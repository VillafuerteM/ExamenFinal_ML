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
library(xgboost)

#Lectura de datos ----
datos<-read.csv('default of credit card clients.csv', skip=1, colClasses=c('numeric', 'numeric', rep('factor',3),'numeric',
                                                                           rep('factor',6), rep('numeric',12), 'numeric'))
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
entrenamiento1<- training(datos_particion)
prueba<- testing(datos_particion)
entrenamiento_particion <- initial_split(entrenamiento1, 0.7)
entrenamiento<-training(entrenamiento_particion)
validacion<-testing(entrenamiento_particion)

# Receta ----
default_receta <- recipe(y ~ ., entrenamiento1) %>%
  step_dummy(all_nominal())%>%
  step_normalize(all_numeric())%>%
  prep()

#Preparación para xgboost
y_entrena <- entrenamiento1$y
x_entrena <- juice(default_receta) %>% select(-y) %>% as.matrix
d_entrena <- xgb.DMatrix(data = x_entrena, label= y_entrena)

params = list(
  objective = "binary:logistic",
  eta = 0.2, # tamaño de paso
  max_depth = 25, # profundidad de árboles
  colsample_bynode = 0.2, # prop de variables usadas para candidatos
  lambda = 0)

set.seed(9758)
mod_boost<- xgboost(params = params, 
                    data = d_entrena,
                    nrounds = 500, # número de árboles
                    print_every_n = 1000,
                    nthread=8)

#Se hicieron ajustes de parámetros con validación. Ya después usamos todo el entrenamiento para predecir la prueba
x_validacion<- bake(default_receta,validacion) %>% select(-y) %>% as.matrix 
pred <- predict(mod_boost, x_validacion)
predicciones<-bind_cols(validacion, as.data.frame(pred))
limit<-0.5
predicciones <- predicciones %>% mutate(class=if_else(pred>=limit,1,0))
table(predicciones$class)
matrix<- with(predicciones, table(class,y))
matrix


#Ahora con prueba
x_prueba<- bake(default_receta,prueba) %>% select(-y) %>% as.matrix
pred <- predict(mod_boost, x_prueba)
pred_prueba<-bind_cols(prueba, as.data.frame(pred))
limit<-0.5
pred_prueba <- pred_prueba %>% mutate(class=if_else(pred>=limit,1,0))
matrix<- with(pred_prueba, table(class,y))
matrix
matrix[2,2]/sum(matrix[,2])

pred_prueba<-pred_prueba %>% mutate(prob0=1-pred)
roc_tbl<- roc_curve(pred_prueba, truth = as.factor(y), estimate=prob0)
ggplot(roc_tbl, aes(x=1-specificity,y=sensitivity))+geom_path(aes(color=.threshold), size=1.2)+geom_abline()

roc_auc(pred_prueba,truth = as.factor(y), prob0)

