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
datos<-read.csv('default of credit card clients.csv', skip=1)
datos<-datos%>%mutate(y=default.payment.next.month)%>%select(-ID,-default.payment.next.month)
datos<- datos%>%mutate(y=as.factor(y))

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
ggplot(entrenamiento, aes(x=LIMIT_BAL,fill=factor(y)))+geom_histogram() #Parece ser que la distribución de crédito es similar
ggplot(entrenamiento, aes(x=factor(y), y=LIMIT_BAL))+geom_boxplot()

# SEX
entrenamiento %>% group_by(SEX, y) %>% count() %>% mutate(n=n/25500) #Vemos que hay un mayor número de mujeres y eso es lo que explica mayo numero de faulty payments

# EDUCATION (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)
summary(entrenamiento$EDUCATION)
tabla<- with(entrenamiento, table(EDUCATION, y))
ggplot(as.data.frame(tabla), aes(factor(EDUCATION), Freq, fill = y)) +     
  geom_col(position = 'dodge')

# Marriage (1=married, 2=single, 3=others)
tabla<- with(entrenamiento, table(MARRIAGE, y))
ggplot(as.data.frame(tabla), aes(factor(MARRIAGE), Freq, fill = y)) +     
  geom_col(position = 'dodge')

#AGE
ggplot(entrenamiento, aes(x=AGE,fill=factor(y)))+geom_histogram() 
ggplot(entrenamiento, aes(x=factor(y), y=AGE))+geom_boxplot()

# Pay_0 (Mes pasado)  -1 = pay duly; 1 = payment delay for one month; 2 = payment delay for two months
aux<-entrenamiento%>% group_by(PAY_0, y) %>% count()
ggplot(aux, aes(x = PAY_0, y = factor(y), fill = n)) + geom_tile
tabla<- with(entrenamiento, table(PAY_0, y))
ggplot(as.data.frame(tabla), aes(factor(PAY_0), Freq, fill = y)) +     
  geom_col(position = 'dodge')

# PAY_i vs PAY_j
aux<-entrenamiento%>% group_by(PAY_3, PAY_4) %>% count()
ggplot(aux, aes(x = factor(PAY_3), y = factor(PAY_4), fill = n)) + geom_tile() 

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

# Receta ----
default_receta <- recipe(y ~ ., entrenamiento)
# Random forest model
default_bosque <- rand_forest(mtry = 6, trees = 1000) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")
# Workflow
flujo <- workflow() %>% 
  add_recipe(default_receta) %>% 
  add_model(default_bosque) 
flujo_ajustado <- fit(flujo, entrenamiento)

# Ajuste
a<-predict(flujo_ajustado , prueba, type = "prob")

