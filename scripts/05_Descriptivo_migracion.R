# Autora: Paula Pareja
# Fecha: Mayo, 2023

# Título: La relación entre género y migración a través del análisis de datos

# Trabajo Final de Máster
# Universidad Carlos III Madrid
# Tutores: Iñaki Ucar y Félix Vacas


########################################################
# 3. ANÁLISIS EXPLORATORIO DE LOS DATOS
########################################################
########################################################

# DESCRIPTIVO MIGRACIÓN
########################################################

# Primero, limpiamos la memoria de trabajo
rm(list = ls())

# Segundo, fijamos el directorio de trabajo de nuevo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Importamos las librerías necesarias
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)
library(cowplot)
library(ggplot2)
library(plotly)
library(tidyverse)
library(hrbrthemes)
library(reshape2)    


########################################################
# 1. LECTURA DE LOS DATASETS
########################################################
########################################################

########################################################


df_economy <- read_excel("data/Migration_genderinequality.xlsx", sheet = 'Economy',col_names = TRUE)
head(df_economy)
df_economy <- df_economy[, -1]

df_gender <- read_excel("data/Migration_genderinequality.xlsx", sheet = 'Gender_violence')
head(df_gender)
df_gender <- df_gender[, -1]

df_migrants <- read_excel("data/Migration_genderinequality.xlsx", sheet = 'Migrants')
head(df_migrants)
df_migrants <- df_migrants[, -1]

df_total <- read_excel("data/Migration_genderinequality.xlsx", sheet = 'Total')
head(df_total)
df_total <- df_total[, -1]



########################################################
# 2. ANÁLISIS EXPLORATORIO
########################################################
########################################################

########################################################

# Análisis migración -------------------------------------------------------------

#Continúo analizando los patrones migratorios 

df_migrants
str(df_migrants)
summary(df_migrants)


# Plot grid de los boxplots
# Transformamos los datos a long format

df_migrantsnum <- df_migrants[,-1]

df_long <- df_migrantsnum %>% 
  gather(key = "variable", value = "value")

#creamos el boxplot
ggplot(df_long, aes(x = "", y = value)) +
  geom_boxplot(alpha = 0.5, color = "black", outlier.shape = NA, fill = "steelblue") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Análisis de los patrones migratorios",
       x = "variables", y = "Valor del indicador")

ggplot(df_long, aes(x = "", y = value)) +
  geom_boxplot(alpha = 0.5, color = "black", outlier.shape = NA, fill = "steelblue") +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(title = "Análisis de los patrones migratorios",
       x = "variables", y = "Valor del indicador")


migrantesnoregistrados <- 50661149 - 17513533
migrantesnoregistrados


#Comenzamos analizando los inmigrantes
names(df_migrants)
boxplot(df_migrants$total_immigrants, col='steelblue', main='Total Inmigrantes',
        ylab="Total",outliercol="black", notchwidth=0.5, alpha=0.5)
summary(df_migrants$total_immigrants)

df_tail10_migration <- df_migrants %>% 
  arrange(desc(total_immigrants)) %>% 
  head(10)
df_tail10_migration$country

df_head10_migration <- df_migrants %>% 
  arrange(total_immigrants) %>% 
  head(10)
df_head10_migration$country


#Análisis emigración

names(df_migrants)
boxplot(df_migrants$total_emigrants, col='steelblue', main='Total Emigrantes',
        ylab="Total",outliercol="black", notchwidth=0.5, alpha=0.5)
summary(df_migrants$total_emigrants)

df_tail10_migration <- df_migrants %>% 
  arrange(desc(total_emigrants)) %>% 
  head(10)
df_tail10_migration$country

df_head10_migration <- df_migrants %>% 
  arrange(total_emigrants) %>% 
  head(10)
df_head10_migration$country


#Boxplot con el número de emigrantes, inmigrantes y porcentaje de cada uno
names(df_migrants)
boxplot_migrantes <- df_migrants[,c(6,11)]

df_long_migrantes <- boxplot_migrantes %>% 
  gather(key = "variable", value = "value")


#creamos el boxplot

ggplot(df_long_migrantes, aes(x = "", y = value)) +
  geom_boxplot(alpha = 0.5, color = "black", outlier.shape = NA, fill = "steelblue") +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  scale_y_continuous(limits = c(0, 90)) +
  labs(title = "Porcentaje de mujeres inmigrantes",
       x = "variables", y = "Valor del indicador")

ggplot(df_long_migrantes, aes(x = "", y = value)) +
  geom_boxplot(alpha = 0.2, color = "black", outlier.shape = NA, fill = "steelblue") +
  geom_point(aes(color = 'black'), size = 0.8, position = position_jitter(width = 0.01)) +
  scale_color_manual(values=c("black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  scale_y_continuous(limits = c(10, 75)) +
  labs(title = "Porcentaje de mujeres inmigrantes",
       x = "variables", y = "Valor del indicador") +
  theme(legend.position = "none")


summary(boxplot_migrantes)




#############################
#ELIMINAR OUTLIERS
############################
#Para poder hacer el análisis correlacional, vamos a identificar los outliers 

summary(df_total)

#Parece que es tan dispar el PIB de los países que siguen reconociéndose outliers que pueden comprometer los análisis 
#Además, teniendo en cuenta que son valores importantes para el análisis no pasaremos a su eliminación
#Vamos a realizar una normalización utilizada para reducir la magnitud de los valores 
#extremadamente grandes en una variable y mejorar su distribución. 


#Con el único df que vamos a trabajar los modelos y análisis correlacionales es con el df_total 
#El cual incluye 27 variables, sobre economía, género y migración


#Nos quedamos solo con las variables numéricas 
names(df_total)
str(df_total)
df_total[, -1] <- apply(df_total[, -1], 2, as.numeric)
str(df_total)

df_model <- df_total[,-1]


##install.packages("caret")
#install.packages("rlang")
#install.packages("vctrs")
#install.packages("tibble")
library(caret)

scaler <- preProcess(df_model, method = 'range')
df_model <- predict(scaler, df_model)

boxplot(df_model, 
        main = "Dataframe total",
        col = "lightblue")

summary(df_model)

str(df_model)

