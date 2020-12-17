#### EXAMEN FINAL DE APRENDIZAJE DE MAQUINA ####
# Otoño 2020
"El objetivo de este proyecto es aplicar lo aprendido en la clase de aprendizaje de máquina. 
Usaremos una base de datos de clientes disponible en: 
https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients#
Se desea entrenar un modelo para pronosticar si un cliente no pagará el siguiente mes.
Esta información está contenida en la columna de default.payment.next.month, la cual nombraremos 'y' "

#Librerias necesarias ----
library(tidyverse)
library(tidymodels)

#Lectura de datos ----
datos<-read.csv('default of credit card clients.csv', skip=1, colClasses=c('numeric', 'numeric', rep('factor',3),'numeric',
                                                                           rep('factor',6), rep('numeric',12), 'factor'))
datos<-datos%>%mutate(y=default.payment.next.month)%>%select(-ID,-default.payment.next.month)
#Mandamos a other (4) categorías 0, 5 y 6 
datos$EDUCATION = fct_collapse(datos$EDUCATION, "1" = "1", "2" = "2", "3"= "3", 
                               "4" = c("4", "5", "6", "0"))

#Ahora Marriage 0 lo mandamos a 3 (Other)
datos$MARRIAGE = fct_collapse(datos$MARRIAGE, "1" = "1", "2" = "2", "3" = c("0", "3"))

# DA JELL  is -2 -1 y 0, los voy a colapsar en 0 asumiendo que significa que no tuvieron demora en el pago
datos$PAY_0 = fct_collapse(datos$PAY_0, "0" = c("-2","-1","0"))
datos$PAY_2 = fct_collapse(datos$PAY_2, "0" = c("-2","-1","0"))
datos$PAY_3 = fct_collapse(datos$PAY_3, "0" = c("-2","-1","0"))
datos$PAY_4 = fct_collapse(datos$PAY_4, "0" = c("-2","-1","0"))
datos$PAY_5 = fct_collapse(datos$PAY_5, "0" = c("-2","-1","0"))
datos$PAY_6 = fct_collapse(datos$PAY_6, "0" = c("-2","-1","0"))

# Interacción MARRIAGE & SEX
datos = datos %>% mutate(MA = as.factor(as.numeric(SEX) * as.numeric(MARRIAGE)))

# Gasto promedio por mes
datos = datos %>% mutate(avg_exp5 = (BILL_AMT5 - BILL_AMT6 - PAY_AMT5) / LIMIT_BAL) %>%
  mutate(avg_exp4 = 0.5*(BILL_AMT5 - BILL_AMT6 - PAY_AMT5  + 
                           BILL_AMT4 - BILL_AMT5 - PAY_AMT4) / LIMIT_BAL) %>%
  mutate(avg_exp3 = (BILL_AMT5 - BILL_AMT6 - PAY_AMT5  + 
                       BILL_AMT4 - BILL_AMT5 - PAY_AMT4 + 
                       BILL_AMT3 - BILL_AMT4 - PAY_AMT3) / (LIMIT_BAL * 3)) %>%
  mutate(avg_exp2 = (BILL_AMT5 - BILL_AMT6 - PAY_AMT5  + 
                       BILL_AMT4 - BILL_AMT5 - PAY_AMT4 + 
                       BILL_AMT3 - BILL_AMT4 - PAY_AMT3 + 
                       BILL_AMT2 - BILL_AMT3 - PAY_AMT2) / (LIMIT_BAL * 4)) %>% 
  mutate(avg_exp2 = (BILL_AMT5 - BILL_AMT6 - PAY_AMT5  + 
                       BILL_AMT4 - BILL_AMT5 - PAY_AMT4 + 
                       BILL_AMT3 - BILL_AMT4 - PAY_AMT3 + 
                       BILL_AMT2 - BILL_AMT3 - PAY_AMT2 + 
                       BILL_AMT1 - BILL_AMT2 - PAY_AMT1) / (LIMIT_BAL * 5))

# Qué tanto porcentaje queda del límite de crádito
datos = datos %>% mutate(porc_noexp1 = (LIMIT_BAL - BILL_AMT1) / LIMIT_BAL) %>%
  mutate(porc_noexp1 = (LIMIT_BAL - BILL_AMT2) / LIMIT_BAL) %>%
  mutate(porc_noexp2 = (LIMIT_BAL - BILL_AMT3) / LIMIT_BAL) %>%
  mutate(porc_noexp3 = (LIMIT_BAL - BILL_AMT4) / LIMIT_BAL) %>%
  mutate(porc_noexp4 = (LIMIT_BAL - BILL_AMT5) / LIMIT_BAL) %>%
  mutate(porc_noexp5 = (LIMIT_BAL - BILL_AMT6) / LIMIT_BAL)

#Particion (entrenamiento y prueba) ----
set.seed(1234)
datos_particion <- initial_split(datos, 0.85)
entrenamiento<- training(datos_particion)
prueba<- testing(datos_particion)


# Receta ----
default_receta <- recipe(y ~ ., entrenamiento) %>%
  step_dummy(all_nominal(),-y)%>%
  step_normalize(all_numeric())%>%
  prep()

#a<-juice(default_receta)
# Random forest model
set.seed(5784)
default_bosque <- rand_forest(mtry = 2, trees = 1000) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")

# Workflow
flujo <- workflow() %>% 
  add_recipe(default_receta) %>% 
  add_model(default_bosque) 
flujo_ajustado <- fit(flujo, entrenamiento)

# Ajuste
predicciones<-predict(flujo_ajustado , prueba, type = "prob")
chequeo <- predicciones %>% bind_cols(prueba%>%select(y)) %>% bind_cols(predict(flujo_ajustado, prueba))

limite<-0.50
chequeo<-chequeo %>% mutate(prediccion=if_else(.pred_1>=limite,1,0))

matrix<- with(chequeo, table(.pred_class,y))
matrix
matrix[2,2]/sum(matrix[,2])

roc_tbl<-roc_curve(chequeo, truth=y, estimate=.pred_0)
ggplot(roc_tbl, aes(x=1-specificity, y=sensitivity))+geom_path(aes(color=.threshold), size=1.2)+geom_abline()
ggplot(chequeo, aes(x=.pred_1, fill=y))+geom_histogram()

roc_auc(chequeo,truth = as.factor(y), .pred_0)
                                                                  