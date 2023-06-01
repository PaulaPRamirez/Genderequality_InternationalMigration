# Autora: Paula Pareja
# Fecha: Mayo, 2023

# Título: La relación entre género y migración a través del análisis de datos

# Trabajo Final de Máster
# Universidad Carlos III Madrid
# Tutores: Iñaki Ucar y Félix Vacas


########################################################
# ANALISIS POR REGIONES 
########################################################
########################################################



# Primero, limpiamos la memoria de trabajo
rm(list = ls())

# Segundo, fijamos el directorio de trabajo
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



########################################################
#DATAFRAME REGIONES

#Leemos el excel que vamos a usar sobre migración
df_regiones <- read_excel("data/Migrant_stock_age_2019.xlsx", sheet = 'migrant_agesex')
head(df_regiones)


#Eliminamos las primeras 9 filas pues son de información del excel
df_regiones <- slice(df_regiones, 10:n())

ncol(df_regiones)
names(df_regiones)

#Primero me quedo solo con la columna de destino y con las que edad por sexo
df_ages <- df_regiones[, (24:57)]
df_countries <- df_regiones[, 3]

head(df_ages)

#voy a indicar delante de cada columna si corresponde a male o female
column_names <- as.character(df_ages[2,])


# Use a loop to add the prefixes "female_" and "male_" to the column names as needed
for(i in 1:length(column_names)) {
  if(i <= 17) {
    column_names[i] <- paste0("male_", column_names[i])
  } else {
    column_names[i] <- paste0("female_", column_names[i])
  }
}

colnames(df_ages) <- column_names

#Ahora puedo eliminar las dos primeras filas

df_ages <- df_ages[-c(1,2),]


#preparamos el df de los nombres para unirlos
df_countries <- df_countries[-c(1,2),]

colnames(df_countries)[which(colnames(df_countries) == "...3")] <- "destination"
colnames(df_countries)

#Procedemos a unirlos
df_regions <- cbind(df_countries, df_ages)


#Cambio los espacios por "_"
df_regions$destination <- gsub(" ", "_", df_regions$destination)


#Me quedo solo con las regiones
df_regions1 <- df_regions[1:23,]
df_regions2 <- df_regions[c(44,47,54,59,60,78,79,78,87,106,107,113,123,124,132,144,172,181,215,224,278,283,224,225,226,237,251,268),]

#Las elimino para comprobar que no me he dejado ninguna región
df_regions <- df_regions[-c(44,47,54,59,60,78,79,78,87,106,107,113,123,124,132,144,172,181,215,224,278,283,224,225,226,237,251,268),]
df_regions <- slice(df_regions, 24:n())


df_regions <- rbind(df_regions1, df_regions2)

#Elimino las que quedan en blanco por ser filas descriptivas
df_regions <- df_regions %>% 
  rownames_to_column(var = "new_index") %>% 
  select(new_index, everything()) %>%
  mutate(new_index = as.integer(new_index)) %>%
  arrange(new_index)

df_regions <- df_regions[,-1]
df_regions <- df_regions[-c(2,7,14,21,42,43,45,39,51,30),]

df_regions <- df_regions %>% 
  rownames_to_column(var = "new_index") %>% 
  select(new_index, everything()) %>%
  mutate(new_index = as.integer(new_index)) %>%
  arrange(new_index)

df_regions <- df_regions[,-1]
df_regions <- df_regions[-41,]

df_regions <- df_regions %>% 
  rownames_to_column(var = "new_index") %>% 
  select(new_index, everything()) %>%
  mutate(new_index = as.integer(new_index)) %>%
  arrange(new_index)

df_regions <- df_regions[,-1]

library(xlsx)

write.xlsx(df_regions, "data/migration_regions.xlsx", 
           sheetName = "immigration")









