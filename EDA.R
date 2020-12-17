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
library(corrplot)

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

#Un vistazo a los datos ----
str(datos) #todos los datos parecen ser int
nrow(datos) #tenemos un total de 30,000 datos
datos %>% group_by(y) %>% count() #hay un 22% de casos positivos
conFaltantes<-colSums(is.na(datos)) #Todas las columnas están completas
conFaltantes

#Particion (entrenamiento y prueba) ----
set.seed(1234)
datos_particion <- initial_split(datos, 0.85)
entrenamiento<- training(datos_particion)
prueba<- testing(datos_particion)

#EDA ----
# LIMIT BAL
ggplot(entrenamiento, aes(x=LIMIT_BAL,fill=factor(y)))+geom_histogram()+
  +xlab("Estado Civil")#Parece ser que la distribución de crédito es similar
ggplot(entrenamiento, aes(x=factor(y), y=LIMIT_BAL))+geom_boxplot()

# SEX
entrenamiento %>% group_by(SEX, y) %>% count() %>% mutate(n=n/25500) #Vemos que hay un mayor número de mujeres y eso es lo que explica mayo numero de faulty payments
tabla<- with(entrenamiento, table(SEX, y))
ggplot(as.data.frame(tabla), aes(SEX, Freq, fill = y)) +     
  geom_col(position = 'dodge')+xlab("Sexo")+ylab("Frecuencia")

# EDUCATION (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)
summary(entrenamiento$EDUCATION)
tabla<- with(entrenamiento, table(EDUCATION, y))
ggplot(as.data.frame(tabla), aes(factor(EDUCATION), Freq, fill = y)) +     
  geom_col(position = 'dodge')+xlab("Educacion")+ylab("Frecuencia")

# Marriage (1=married, 2=single, 3=others)
tabla<- with(entrenamiento, table(MARRIAGE, y))
ggplot(as.data.frame(tabla), aes(factor(MARRIAGE), Freq, fill = y)) +     
  geom_col(position = 'dodge') +xlab("Estado Civil")+ylab("Frecuencia")

#AGE
ggplot(entrenamiento, aes(x=AGE,fill=factor(y)))+geom_histogram() +xlab("Edad")+ylab("Frecuencia")
ggplot(entrenamiento, aes(x=factor(y), y=AGE))+geom_boxplot()

# Pay_0 (Mes pasado)  -1 = pay duly; 1 = payment delay for one month; 2 = payment delay for two months
aux<-entrenamiento%>% group_by(PAY_0, y) %>% count()
ggplot(aux, aes(x = PAY_0, y = factor(y), fill = n)) + geom_tile
tabla<- with(entrenamiento, table(PAY_0, y))
ggplot(as.data.frame(tabla), aes(factor(PAY_0), Freq, fill = y)) +     
  geom_col(position = 'dodge')

# PAY_i vs PAY_j
aux<-entrenamiento%>% group_by(PAY_0, PAY_2) %>% count()
ggplot(aux, aes(x = factor(PAY_0), y = factor(PAY_2), fill = n)) + geom_tile() +
  xlab("PAY_0")+ylab("PAY_2")

# BILL_AMT1
ggplot(entrenamiento, aes(x=BILL_AMT1,fill=factor(y)))+geom_histogram()
ggplot(entrenamiento, aes(x=factor(y), y=BILL_AMT4))+geom_boxplot()
ggplot(entrenamiento, aes(x=BILL_AMT1, y=LIMIT_BAL, color=y))+geom_point() #no nos dice nada

# PAY_AMT1
ggplot(entrenamiento%>%filter(PAY_AMT1<=20000), aes(x=PAY_AMT1,fill=factor(y)))+geom_histogram()
# tenemos datos muy jalados a la derecha
ggplot(entrenamiento, aes(x=factor(y), y=PAY_AMT6))+geom_boxplot()
ggplot(entrenamiento, aes(x=PAY_AMT1, y=BILL_AMT2, color=y))+geom_point()
# no parece haber una diferencia en la distribución