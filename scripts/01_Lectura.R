# Autora: Paula Pareja
# Fecha: Mayo, 2023

# Título: La relación entre género y migración a través del análisis de datos

# Trabajo Final de Máster
# Universidad Carlos III Madrid
# Tutores: Iñaki Ucar y Félix Vacas


########################################################
# 1. LECTURA DE LOS DATASETS
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
#DATAFRAME INMIGRANTES

#Leemos el excel que vamos a usar sobre migración
df_MigrantStock <- read_excel("data/Migrant_stock_age_2019.xlsx", sheet = 'migrant_agesex')
head(df_MigrantStock)


#Eliminamos las primeras 9 filas pues son de información del excel
df_MigrantStock <- slice(df_MigrantStock, 10:n())

ncol(df_MigrantStock)
names(df_MigrantStock)

#Primero me quedo solo con la columna de destino y con las que edad por sexo
df_ages <- df_MigrantStock[, (24:57)]
df_countries <- df_MigrantStock[, 3]

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
df_migrants <- cbind(df_countries, df_ages)

#Voy a quedarme solo con las filas que representan países
#En primer lugar me quedo con las 21 primeras que representan regiones
df_migrants <- slice(df_migrants, 24:n())

#Cambio los espacios por "_"
df_migrants$destination <- gsub(" ", "_", df_migrants$destination)

#Elimino las demás filas que contienen regiones

df_migrants <- df_migrants[-c(31,55,56,189,201,214,239,255,83,90,109,158,228,100,101,203,37,
                              55,63,64,245,201,202,173,174,21,84,121,149),]


#Terminamos el df de migrantes por país
df_migrants <- df_migrants %>% 
  rownames_to_column(var = "new_index") %>% 
  select(new_index, everything()) %>%
  mutate(new_index = as.integer(new_index)) %>%
  arrange(new_index)

df_migrants <- df_migrants[,-1]

#Nos quedamos con un total de 231 países y 44 regiones

#Ahora que ya tenemos el df con el número de migrantes por edades y sexo según país de destino
#Ahora vamos a crear otra tabla con el número de immigrantes por país y sexo, quitando edad

df_migrantsbydestination = df_migrants[,c(1,18,35)]

#ya tengo migrantes hombres y mujeres, voy a ver el total por país para luego sacar %
str(df_migrantsbydestination)
df_migrantsbydestination[, -1] <- apply(df_migrantsbydestination[, -1], 2, as.numeric)

df_migrantsbydestination$Total <- df_migrantsbydestination$female_Total + df_migrantsbydestination$male_Total

str(df_migrantsbydestination)

#Creo una nueva columna con el % de migrantes hombres y mujeres
df_migrantsbydestination$male_percent <- df_migrantsbydestination$male_Total * 100 / df_migrantsbydestination$Total
df_migrantsbydestination$female_percent <- df_migrantsbydestination$female_Total * 100 / df_migrantsbydestination$Total


#Los países con mayor % de migrantes hombres son las Maldivas, Oman, Qatar, Guinea Ecuatorial, 
#Emiratos Árabes, Bahrain, Lybia, Seychelles, y Santa Helena.
#Los países con mayor % de mujeres migrantes son Nepal, China (Hong kong), Latvia
#Montenegro, Kyrgyzstan, Republic_of_Moldova, Armenia, Curaçao, Macedonia del Norte 
#y las Islas Mariana del Norte

#Llama mi atención que países como Nepal y Kyrgyzstan tengan tal % de mujeres migrantes


########################################################
#DATAFRAME EMIGRANTES

df_MigrantStock_male <- read_excel("data/Migrant_stock_origin_2019.xlsx", sheet = 'Table 2')
df_MigrantStock_female <- read_excel("data/Migrant_stock_origin_2019.xlsx", sheet = 'Table 3')

#Comenzamos limpiando el df de migrantes hombres 

colnames(df_MigrantStock_male) <- as.character(unlist(df_MigrantStock_male[6,]))
df_MigrantStock_male <- df_MigrantStock_male[-c(1,2,3,4),-c(1,2,4,5,6)]
names(df_MigrantStock_male)

names(df_MigrantStock_male)[which(is.na(names(df_MigrantStock_male)))] <- "Destination_country"
df_MigrantStock_male <- df_MigrantStock_male[-1,]
df_MigrantStock_male <- df_MigrantStock_male[-1,]

head(df_MigrantStock_male)

#Eliminamos los valores de regiones y creamos un df nuevo con ellos

#Para identificar el número de las regiones voy a crear una columna con números enteros ordenados que luego eliminaré

df_MigrantStock_male <- df_MigrantStock_male %>% mutate(id = row_number())
df_MigrantStock_male <- df_MigrantStock_male %>% select(id, everything())

regions_emigrants <- df_MigrantStock_male[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,44,
                                            113,197,196,47,54,59,60,78,79,87,106,107,113,172,
                                            123,124,132,144,181,224,225,226,237,251,268,278),]

df_MigrantStock_male <- df_MigrantStock_male[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
                                                44,113,197,196,47,54,59,60,78,79,87,106,107,113,172,
                                                123,124,132,144,181,224,225,226,237,251,268,278),]

#Este df nos servirá para hacer análisis de redes neurales con programas como Gephi que permitan ver 
#como son los patrones migratorios y donde se encuentran los canales principales

#Vamos a hacer lo mismo con mujeres y unirlos posteriormente

colnames(df_MigrantStock_female) <- as.character(unlist(df_MigrantStock_female[2,]))
df_MigrantStock_female <- df_MigrantStock_female[-1,-c(1,2,4,5,6)]
names(df_MigrantStock_female)

names(df_MigrantStock_female)[which(is.na(names(df_MigrantStock_female)))] <- "Destination_country"
df_MigrantStock_female <- df_MigrantStock_female[-1,]
head(df_MigrantStock_female)

#Eliminamos los valores de regiones y creamos un df nuevo con ellos

#Para identificar el número de las regiones voy a crear una columna con números enteros ordenados que luego eliminaré

df_MigrantStock_female <- df_MigrantStock_female %>% mutate(id = row_number())
df_MigrantStock_female <- df_MigrantStock_female %>% select(id, everything())

regions_emigrants_female <- df_MigrantStock_female[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,44,
                                                     113,197,196,47,54,59,60,78,79,87,106,107,113,172,
                                                     123,124,132,144,181,224,225,226,237,251,268,278),]

df_MigrantStock_female <- df_MigrantStock_female[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
                                                    44,113,197,196,47,54,59,60,78,79,87,106,107,113,172,
                                                    123,124,132,144,181,224,225,226,237,251,268,278),]


#Ahora descargamos el dataset con el total 
df_MigrantStock_Total <- read_excel("data/Migrant_stock_origin_2019.xlsx", sheet = 'Table 1')
View(df_MigrantStock_Total)

colnames(df_MigrantStock_Total) <- as.character(unlist(df_MigrantStock_Total[11,]))
df_MigrantStock_Total <- df_MigrantStock_Total[-c(1:11),-c(1,2,4,5,6)]


names(df_MigrantStock_Total)

names(df_MigrantStock_Total)[which(is.na(names(df_MigrantStock_Total)))] <- "Destination_country"


df_MigrantStock_Total <- df_MigrantStock_Total %>% mutate(id = row_number())
df_MigrantStock_Total <- df_MigrantStock_Total %>% select(id, everything())

regions_emigrants_total <- df_MigrantStock_Total[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,44,
                                                   113,197,196,47,54,59,60,78,79,87,106,107,113,172,
                                                   123,124,132,144,181,224,225,226,237,251,268,278),]

df_MigrantStock_Total <- df_MigrantStock_Total[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
                                                  44,113,197,196,47,54,59,60,78,79,87,106,107,113,172,
                                                  123,124,132,144,181,224,225,226,237,251,268,278),]


#Ahora creamos un tercer df en el que vamos a escribir el número de emigrantes por país y sexo
#Tenemos dos tablas, una para emigrantes femenina y otra para emigrantes masculina

#Primero, vamos a quedarnos solo con los emigrantes totales, con destino "World"

Male_migrants_origin <- regions_emigrants[1,]
Female_migrants_origin <- regions_emigrants_female[1,]

Male_migrants_origin <- Male_migrants_origin %>% mutate_all(as.integer)
Male_migrants_origin <- pivot_longer(Male_migrants_origin, cols = everything(), names_to = "country", values_to = "male_emigrants")
Male_migrants_origin <- Male_migrants_origin[-c(1:5),]

Female_migrants_origin <- Female_migrants_origin %>% mutate_all(as.integer)
Female_migrants_origin <- pivot_longer(Female_migrants_origin, cols = everything(), names_to = "country", values_to = "female_emigrants")
Female_migrants_origin <- Female_migrants_origin[-c(1:5),]

#Unimos ambos 
df_migrantsbyorigin <- cbind(Male_migrants_origin, female_emigrants = Female_migrants_origin$female_emigrants)


str(df_migrantsbyorigin)

df_migrantsbyorigin$Total <- df_migrantsbyorigin$male_emigrants + df_migrantsbyorigin$female_emigrants

#Creo una nueva columna con el % de migrantes hombres y mujeres
df_migrantsbyorigin$male_percent <- df_migrantsbyorigin$male_emigrants * 100 / df_migrantsbyorigin$Total
df_migrantsbyorigin$female_percent <- df_migrantsbyorigin$female_emigrants * 100 / df_migrantsbyorigin$Total


########################################################
#DATAFRAME GDP

#Continúo descargando el siguiente dataset sobre el PIB (GDP por sus siglas en inglés)
#a través de la base de datos del Banco Mundial

df_GDP <- read_csv("data/GDP.csv")
head(df_GDP)
ncol(df_GDP)
names(df_GDP)

#Eliminamos las columnas "Series Code" ya que son irrelevantes para el análisis
df_GDP <- df_GDP[ ,-c(1,2)]
names(df_GDP)
# "Country Name"  "Country Code"  "2019 [YR2019]"
#Vamos a dejar el código del país por si nos es útil próximamente

#Cambiamos el nombre de la columna 2019 [YR2019] a GDP
df_GDP <- df_GDP %>% rename('GDP' = '2019 [YR2019]')


#Ahora elimino las filas que corresponden a regiones del df_GDP
df_GDP <- df_GDP[-c(218:nrow(df_GDP)),]


df_GDP <- df_GDP %>% 
  mutate(GDP = na_if(GDP, ".."))

filas_con_missing <- which(rowSums(is.na(df_GDP)) > 0)
df_GDP[filas_con_missing, ]

#Faltan los valores de las Islas Vírgenes Británicas, las Islas del Canal, Eritrea, Gibraltar,
#La República Democrática de Corea, San Martin, Sudán del Sur, Venezuela y Yemen

#Vamos a descargar y limpiar un poco todas las listas y una vez tengamos todas pasamos a la 
#limpieza y homogeneización de las tablas



########################################################
#DATAFRAME DESIGUALDAD DE GÉNERO (BASE DE DATOS GID-DB)

#Seguimos descargando la base de datos de violencia contra las mujeres

prueba <- read_csv("data/Violence_against_women.csv")
prueba <- prueba[ ,c(4,7,11)]

prueba <- prueba %>% 
  distinct(Country, VAR, .keep_all = TRUE)

prueba <- pivot_wider(prueba, names_from = VAR, values_from = Value)
names(prueba)


df_genderviolence <- read_csv("data/Violence_against_women.csv")
head(df_genderviolence)
ncol(df_genderviolence)
names(df_genderviolence)

#Nos quedamos con las columnas que necesitamos
df_genderviolence <- df_genderviolence[ ,c(4,7,11)]
head(df_genderviolence)

#La tabla está agrupada por países donde cada fila es un país y variable
#Quiero transformar el df para que cada fila represente un país y cada columna sea una variable

df_genderviolence1 <- df_genderviolence %>% 
  distinct(Country, VAR, .keep_all = TRUE)

df_genderviolence <- pivot_wider(df_genderviolence1, names_from = VAR, values_from = Value)
names(df_genderviolence)

#Vamos a eliminar las columnas que no nos interesan para el análisis o que tienen demasiados valores perdidos
#Para ello en primer lugar vamos a ver los valores perdidos
colSums(is.na(df_genderviolence))

#Teniendo en cuenta que hay un total de 180 países en la base de datos, eliminamos las que tienen más de 90 valores perdidos

df_genderviolence <- df_genderviolence[ ,-c(3,23,30,31,32,33,34)]
ncol(df_genderviolence)
names(df_genderviolence)

df_genderviolence <- df_genderviolence[ ,-c(2,3,4,10,11,12,13,14,15,16,17,18,19,23,26,27)]
names(df_genderviolence)

df_genderviolence <- df_genderviolence %>% rename('Inheritance_law' = 'DF_IN_LAW',
                                                  'Violence_against_women_law' = 'RPI_VAW_LAW',
                                                  'Violence_against_women_attitudes' = 'RPI_VAW_ATT',
                                                  'Violence_against_women%' = 'RPI_VAW_PRACT',
                                                  'Citizenship_rights' = 'RCL_CR_LAW',
                                                  'Inequality_voting_rights' = 'RCL_PV_LAW',
                                                  '%men_parliament' = 'RCL_PV_PRACT',
                                                  'FGM_law' = 'RPI_FGM_LAW',
                                                  'Insecurity_feeling' = 'RCL_FM_PRACT',
                                                  'Access_to_justice' = 'RCL_AJ_LAW',
                                                  'Child_marriage_law' = 'DF_CM_LAW',
                                                  'Girls_married' = 'DF_CM_PRACT')

names(df_genderviolence)
#[1] "Country"                          "Inheritance_law"                 "Violence_against_women_law" 
#[4] "Violence_against_women_attitudes" "Violence_against_%women"          "FMG_law"             
#[7] "Citizenship_rights"               "Inequality_voting_rights"         "%men_parliament"                 
#[10] "Freedom_of_movement"              "Insecurity_feeling"               "Access_to_justice" 
#[13] "Child_marriage_law"               "Girls_married"                   

# Finalmente me he quedado con las siguientes variables: 
#1.  Leyes hereditarias: nivel de igualdad en derechos de herencia de la tierra y otros bienes,
#siendo 0 los mismos derechos y 1 una desigualdad reconocida en la ley
#2.  Leyes en violencia contra las mujeres: nivel de protección de la ley, siendo 0 la mayor y 1 una 
#protección nula
#3.  Violencia de género en actitudes: representa el % de muejres entre 15 y 49 que piensa que está 
#justificado que un hombre pegue a su mujer por alguna razón (como quemar la comida, discutir,
#salir sin avisar, rechazar a los hijos o se niega a tener relaciones sexuales)
#4.  Violence_against_women% : porcentaje de mujeres que han sufrido violencia de género
#5.  FGM_law: legalidad sobre la mutilación genital femenina, siendo 1 legal
#6.  Citizenship_rights: igualdad en las leyes de nacionalidad
#7.  Iequality_voting_rights: desigualdad en el derecho a voto
#8.  %men_parliament: % de de hombres en la camara baja o única del Parlamento
#9.  Insecurity_feeling: % de mujeres que piensan que piensan que es inseguro andar sola por la noche
#en la ciudad o área donde viven
#10. Access_to_justice: leyes igualitarias de acceso a la justicia
#11. Child_marriage_law: igual edad de matrimonio o no, siendo 0 igual y 1 desigual
#12. Girls_married: % de niñas casadas, divorciadas, viudas o en unión informal



#Antes de seguir voy a eliminar las tablas que no nos sirven para limpiar la memoria de trabajo

rm(df_ages)
rm(df_countries)
rm(df_genderviolence1)
rm(df_MigrantStock)
df_migrants_age_sex <- df_migrants
regions_emigrants_male <- regions_emigrants
rm(df_migrants)
rm(regions_emigrants_female)
rm(regions_emigrants_male)
rm(regions_emigrants_total)


########################################################
#DATAFRAME PARTICIPACIÓN LABORAL

#Continuamos con la descarga del dataset sobre Participación Laboral

df_labour <- read_csv("data/Labour_participation_bycountry.csv")
head(df_labour)
ncol(df_labour)
names(df_labour)
df_labour <- df_labour[,c(1,4,7)]

df_labour <- df_labour %>% rename('country' = 'ref_area.label',
                                  'sex' = 'sex.label',
                                  'labour_participation' = 'obs_value')
names(df_labour)
df_labour$sex <- gsub("Sex: ", "", df_labour$sex)

colSums(is.na(df_labour)) #no tiene ningún valor perdido


df_labour_male <- subset(df_labour, sex == "Male")
df_labour_female <- subset(df_labour, sex == "Female")
df_labour_participation <- cbind(df_labour_male, df_labour_female)
head(df_labour_participation)

names(df_labour_participation)

names(df_labour_participation)[3] <- "male_labour_participation"
names(df_labour_participation)[6] <- "female_labour_participation"

df_labour_participation <- df_labour_participation[,c(1,3,6)]



########################################################
#DATAFRAME REMESAS

df_remittances <- read_excel("data/Remittances.xlsx", sheet = 'Data')
head(df_remittances)
names(df_remittances)
df_remittances <- df_remittances[,c(3,4,5)]

df_remittances <- df_remittances %>% rename('Remittances' = '2019 [YR2019]')


df_remittances <- df_remittances %>% 
  mutate(Remittances = na_if(Remittances, ".."))

df_remittances <- slice(df_remittances, 1:217)


#Eliminamos los df para limpiar la consola 

rm(df_labour_female)
rm(df_GDP_regiones)
rm(df_labour_male)
rm(df_labour)
rm(Female_migrants_origin)
rm(Male_migrants_origin)
rm(regions_emigrants)

#Nos quedamos con un total de 10 dataframes 
#1.  df_GDP con el PIB por país
#2.  df_genderviolence, con los indicadores referentes a desigualdad de género
#3.  df_labour_participation, con la tasa de participación laboral por sexo en cada país
#4.  df_migrants_age_sex, con el número de inmigrantes por sexo y edad
#5.  df_migrantsbydestination, con el número de inmigrantes por sexo
#6.  df_migrantsbyorigin, con el número de emigrantes por sexo
#7.  df_MigrantStock_female, con el número de inmigrantes mujeres por origen y destino
#8.  df_MigrantStock_male, con el número de inmigrantes hombres por origen y destino
#9.  df_MigrantStock_total, con el número de inmigrantes por origen y destino de ambos sexos
#10. df_remittances, con el % de las remesas en el PIB de cada país
 





