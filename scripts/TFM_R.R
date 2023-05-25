# author: Paula Pareja
# date: Mayo, 2023

#Proyecto para el Trabajo de FInal de Máster en Estudios Avanzados en Derechos Humanos
#Tutor: Iñaki Ucar y Félix Vacas

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
#install.packages("cowplot")
library(cowplot)
library(ggplot2)
library(plotly)
library(tidyverse)
#install.packages("hrbrthemes")
library(hrbrthemes)


########################################################
# 1. LECTURA Y LIMPIEZA DE LOS DATASETS
########################################################
########################################################

########################################################
#DATAFRAME INMIGRANTES

#Leemos el excel que vamos a usar sobre migración
df_MigrantStock <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Migrant_stock_age_2019.xlsx", sheet = 'migrant_agesex')
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

df_MigrantStock_male <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Migrant_stock_origin_2019.xlsx", sheet = 'Table 2')
df_MigrantStock_female <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Migrant_stock_origin_2019.xlsx", sheet = 'Table 3')

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
df_MigrantStock_Total <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Migrant_stock_origin_2019.xlsx", sheet = 'Table 1')
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

df_GDP <- read_csv("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/GDP.csv")
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

prueba <- read_csv("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Violence_against_women.csv")
prueba <- prueba[ ,c(4,7,11)]

prueba <- prueba %>% 
  distinct(Country, VAR, .keep_all = TRUE)

prueba <- pivot_wider(prueba, names_from = VAR, values_from = Value)
names(prueba)


df_genderviolence <- read_csv("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Violence_against_women.csv")
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

df_labour <- read_csv("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Labour_participation_bycountry.csv")
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

df_remittances <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Remittances.xlsx", sheet = 'Data')
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

#A continuación pasamos a la limpieza y homogeneización de las tablas
########################################################
########################################################

#VALORES PERDIDOS 

#Primero vamos a ver los valores perdidos de cada una de las tablas 

colSums(is.na(df_GDP)) #Tiene 10 valores perdidos
colSums(is.na(df_genderviolence)) #46 valores perdidos en % de violencia y 39 en inseguridad
colSums(is.na(df_labour_participation)) #0 valores perdidos
colSums(is.na(df_migrants_age_sex)) #0 valores perdidos
colSums(is.na(df_migrantsbydestination)) #0 valores perdidos
colSums(is.na(df_migrantsbyorigin)) #0 valores perdidos
colSums(is.na(df_remittances)) #27 valores perdidos

#No comprobamos los valores perdidos de Migrant Stock 
#pues solo están las casillas con migrantes en origen y destino

nrow(df_GDP) #217 países
nrow(df_genderviolence) #180 países
nrow(df_labour_participation) #123 países
nrow(df_migrants_age_sex) #233 países
nrow(df_migrantsbydestination) #233 países
nrow(df_migrantsbyorigin) #232 países
nrow(df_remittances) #217 países



#En primer lugar voy a comprobar por qué el df de migrantes por origen tiene un país menos que el de destino
df_migrantsbydestination <- df_migrantsbydestination %>% rename('country' = 'destination')

df_migrantsbyorigin$country <- gsub(" ", "_", df_migrantsbyorigin$country)


missing_countries <- setdiff(df_migrantsbydestination$country, df_migrantsbyorigin$country)
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#[1] "The following country is missing in df2: Caribbean" 
#[2] "The following country is missing in df2: Melanesia" 
#[3] "The following country is missing in df2: Micronesia"
#[4] "The following country is missing in df2: Polynesia" 

#No había eliminado "Caribe" como país, ya que es una región

df_migrantsbydestination <- slice(df_migrantsbydestination,-108)
nrow(df_migrantsbydestination) #Ahora sí son 232 países

missing_countries <- setdiff(df_migrantsbydestination$country, df_migrantsbyorigin$country)
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#[1] "The following country is missing in df2: Melanesia" 
#[2] "The following country is missing in df2: Micronesia"
#[3] "The following country is missing in df2: Polynesia" 

missing_countries <- setdiff(df_migrantsbyorigin$country, df_migrantsbydestination$country)
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#[1] "The following country is missing in df2: North_Macedonia"         
#[2] "The following country is missing in df2: Northern_Mariana_Islands"
#[3] "The following country is missing in df2: Western_Sahara"  


#Había olvidado eliminar la región del oeste del sáhara
df_migrantsbyorigin <- slice(df_migrantsbyorigin,-229)


#Por ende, el df de emigrantes no tiene los datos de Melanesia, Micronesia ni Polynesia
#y el df de inmigrantes no tiene los datos de Macedonia del Norte ni las islas Mariana del norte

#En ambos nos quedamos con 231 países, aunque 5 de ellos no coinciden entre sí



#Vamos a comparar los df de GDP y remittances, los cuales provienen de la misma fuente
missing_countries <- setdiff(df_GDP$'Country Name', df_remittances$'Country Name')
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#Todos los nombres de países corresponden 


#Compruebo el df_migrants_age_sex ya que tiene las mismas observaciones

missing_countries <- setdiff(df_migrantsbyorigin$'country', df_migrants_age_sex$'destination')
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#[1] "The following country is missing in df2: North_Macedonia"         
#[2] "The following country is missing in df2: Northern_Mariana_Islands"

missing_countries <- setdiff(df_migrants_age_sex$'destination', df_migrantsbyorigin$'country')
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#[1] "The following country is missing in df2: Caribbean" 
#[2] "The following country is missing in df2: Melanesia" 
#[3] "The following country is missing in df2: Micronesia"
#[4] "The following country is missing in df2: Polynesia" 

#Como ocurrió en el anterior tengo que eliminar el Caribe

df_migrants_age_sex <- slice(df_migrants_age_sex, -108)

missing_countries <- setdiff(df_migrants_age_sex$'destination', df_migrantsbyorigin$'country')
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#[1] "The following country is missing in df2: Melanesia" 
#[2] "The following country is missing in df2: Micronesia"
#[3] "The following country is missing in df2: Polynesia"

missing_countries <- setdiff(df_migrants_age_sex$'destination', df_migrantsbydestination$'country')
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#[1] "There are no missing countries."

missing_countries <- setdiff(df_migrantsbydestination$'country', df_migrants_age_sex$'destination')
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#[1] "There are no missing countries."

#Los df de migrantes por edad y sexo y por destino son iguales ya que provenían de la misma fuente
#Aunque esto lo sabíamos a priori lo hemos llevado a cabo para comprobar que la limpieza
#se hizo correctamente


missing_countries <- setdiff(df_migrantsbyorigin$'country', df_migrants_age_sex$'destination')
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

#[1] "The following country is missing in df2: North_Macedonia"         
#[2] "The following country is missing in df2: Northern_Mariana_Islands"


#Por ende, nos encontramos ante la siguiente situación: 

#Países coincidentes
nrow(df_GDP) #217 países
nrow(df_remittances) #217 países

#Países coincidentes
nrow(df_migrants_age_sex) #232 países 
nrow(df_migrantsbydestination) #232 países
#En ambos falta Macedonia del Norte y las islas Marianas del Norte respecto al de emigrantes

nrow(df_migrantsbyorigin) #231 países falta Polynesia, Micronesia y Melanesia respecto a los anteriores


#Tres df con un número menor de países
nrow(df_genderviolence) #180 países
nrow(df_labour_participation) #123 países


df_labour_participation$country <- gsub(" ", "_", df_labour_participation$country)


missing_countries <- setdiff(df_migrantsbyorigin$'country', df_labour_participation$'country')
if (length(missing_countries) > 0) {
  print(paste0("The following country is missing in df2: ", missing_countries))
} else {
  print("There are no missing countries.")
}

missing_countries <- data.frame(missing_countries)


#Voy a intentar descargar otro excel sobre participación laboral 

df_labourforce <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/labourforce_female.xlsx", sheet = 'Data')
head(df_labourforce)

df_labourforce <- df_labourforce[,c(3,4,5)]
names(df_labourforce)
df_labourforce <- df_labourforce %>% rename('labourforce_female' = '2019 [YR2019]')


df_labourforce <- df_labourforce %>% 
  mutate(labourforce_female = na_if(labourforce_female, ".."))

#Elimino a partir de la fila 218 pues corresponde a regiones o valores perdidos
df_labourforce <- df_labourforce[-c(218:nrow(df_labourforce)),]

colSums(is.na(df_labourforce)) #Tiene 30 valores perdidos
filas_con_missing <- which(rowSums(is.na(df_labourforce)) > 0)
df_labourforce[filas_con_missing, ]

#La mayoría son pequeñas islas o países de menor tamaño como Andorra
#Por imposibilidad de encontrar valores de calidad de estos países procedemos a eliminarlos 

df_labourforce <- na.omit(df_labourforce)
nrow(df_labourforce) #Nos quedamos con 187 valores

#Ya que la columna "labourforce_female" representa el % de mujeres en el total de participación laboral
#Voy a crear otra columna en la que incluya la participación de hombres 

str(df_labourforce)
df_labourforce$labourforce_female <- as.numeric(as.character(df_labourforce$labourforce_female))

df_labourforce$labourforce_male <- 100 - df_labourforce$labourforce_female

#Ya tengo el df con la participación laboral de hombres y mujeres respecto al total de participación laboral
#Debemos tener en cuenta que este no considera si hay mas mujeres trabajando, 
#Solo el porcentaje de mujeres y hombres dentro del grupo de personas que trabajan
#Puede que haya más mujeres trabajando en el país en números totales pero sean menos % del total


#Por ende, procedo a usar el dataset sobre participación laboral y elimino el anterior
rm(df_labour_participation)


#Vamos a gestionar los valores perdidos de los siguientes df
colSums(is.na(df_GDP)) #Tiene 10 valores perdidos

filas_con_missing <- which(rowSums(is.na(df_GDP)) > 0)
df_GDP[filas_con_missing, ]

#1 British Virgin Islands    
#2 Channel Islands           
#3 Eritrea                 
#4 Gibraltar                
#5 Korea, Dem. People's Rep.
#6 Sint Maarten (Dutch part) 
#7 South Sudan               
#8 St. Martin (French part)    
#9 Venezuela, RB              
#10 Yemen, Rep.


#Al ser países de menor tamaño o con imposibilidad de obtener datos procedo a eliminarlos
#El caso de Venezuela y Yemen va a ser complicado ya que son países clave para el refugio 
#Sin embargo los últimos datos dispnibles del PIB de Venezuela son de 2014
#Dada la situación económica resulta prácticamente imposible obtener estos datos
#Por ende, usaremos el caso de Venezuela para estudiar patrones pero no podremos incluirlo en los modelos

df_GDP <- na.omit(df_GDP)
nrow(df_GDP) #Nos quedamos con 207 países

colSums(is.na(df_remittances)) #Tiene 27 valores perdidos

filas_con_missing <- which(rowSums(is.na(df_remittances)) > 0)
df_remittances[filas_con_missing, ]

print(df_remittances[filas_con_missing,], n = 27)

#Nos encontramos una situación parecida, son países pequeños o con situaciones de inestabilidad 
#que afecta directamente en la posibilidad de recopilación de datos

df_remittances <- na.omit(df_remittances)
nrow(df_remittances) #Nos quedamos con 190 países

rm(missing_countries)


#Por ende, nos quedamos con la siguiente situación
colSums(is.na(df_GDP)) #Tiene 0 valores perdidos
colSums(is.na(df_genderviolence)) #46 valores perdidos en % de violencia y 39 en inseguridad
colSums(is.na(df_labourforce)) #0 valores perdidos
colSums(is.na(df_migrants_age_sex)) #0 valores perdidos
colSums(is.na(df_migrantsbydestination)) #0 valores perdidos
colSums(is.na(df_migrantsbyorigin)) #0 valores perdidos
colSums(is.na(df_remittances)) #0 valores perdidos

nrow(df_GDP) #207 países
nrow(df_genderviolence) #180 países
nrow(df_labourforce) #187 países
nrow(df_migrants_age_sex) #232 países
nrow(df_migrantsbydestination) #232 países
nrow(df_migrantsbyorigin) #231 países
nrow(df_remittances) #190 países


#Vamos a proseguir gestionando los valores perdidos del df de violencia contra las mujeres

colSums(is.na(df_genderviolence)) 
#46 valores perdidos en % de violencia
#39 valores perdidos en inseguridad
#19 valores perdidos en actitud de violencia contra las mujeres
#En las demás filas solo hay uno 

library(DataExplorer)
plot_missing(df_genderviolence)


#En primer lugar, vamos a eliminar aquellos países que tengan muchos valores perdidos
n_missing <- rowSums(is.na(df_genderviolence))
print(n_missing)


#La fila número 39, que corresponde a Hong Kong tiene todos los valores perdidos 
df_genderviolence <- slice(df_genderviolence,-39)

#Los demás países tienen uno, dos o tres valores perdidos por lo que podemos usarlos

ncol(df_genderviolence)
#Voy a hacer una imputación de valores perdidos teniendo en cuenta las demás variables del df
#A través de una imputación KNN con el paquete "missForest"


#Primero voy a ver la distribución de mis datos
names(df_genderviolence)
boxplot(df_genderviolence$Violence_against_women_attitudes, 
     main = "Violence against women attitudes",
     col = "lightblue")
  #La mayoría tiene valores entre 10 y 40, aunque hay países en los que se supera el 80%

boxplot(df_genderviolence$Insecurity_feeling, 
     main = "Insecurity Feeling",
     col = "lightblue")
  #La mayoría de países tiene valores entre 60 y 70 

boxplot(df_genderviolence$'Violence_against_women%', 
     main = "% of violence against women",
     col = "lightblue")
  #El porcentaje de mujeres que han sufrido violencia va entre el 20% y 38% en la mayoría de países
  #Hay países en los que llega hasta el 60 y algunas excepciones con más del 60%


#Continuamos con la imputación


# load the mice package
#install.packages("mice")
library(mice)

#rename the columns
names(df_genderviolence)
df_genderviolence <- df_genderviolence %>% rename('percentage_violence' = 'Violence_against_women%',
                                                  'percentage_women_parliament' = '%men_parliament')


#Me quedo con las columnas sin valores perdidos y numéricas
df_gender_imputacion_attittudes <- df_genderviolence[,-c(1,2,5,10)]

data_to_impute <- df_gender_imputacion_attittudes$Violence_against_women_attitudes

mice_imputed <- data.frame(original = df_gender_imputacion_attittudes$Violence_against_women_attitudes, 
                           imputed_pmm = complete(mice(df_gender_imputacion_attittudes, method = "pmm"))$Violence_against_women_attitudes)

names(mice_imputed)
df_genderviolence1 <- df_genderviolence
df_genderviolence1$Violence_against_women_attitudes <- mice_imputed$imputed_pmm

#Hacemos lo mismo con la columna de porcentaje de violencia contra las mujeres

df_gender_imputacion_violence <- df_genderviolence[,-c(1,2,4,10)]
data_to_impute <- df_gender_imputacion_violence$percentage_violence

mice_imputed <- data.frame(original = df_gender_imputacion_violence$percentage_violence, 
                           imputed_pmm = complete(mice(df_gender_imputacion_violence, method = "pmm"))$percentage_violence)

names(mice_imputed)
df_genderviolence1$percentage_violence <- mice_imputed$imputed_pmm


#Hacemos lo mismo con la columna de porcentaje de inseguridad

df_gender_imputacion <- df_genderviolence[,-c(1,4,5,10)]
data_to_impute <- df_gender_imputacion$Inheritance_law

mice_imputed <- data.frame(original = df_gender_imputacion$Inheritance_law, 
                           imputed_pmm = complete(mice(df_gender_imputacion, method = "pmm"))$Inheritance_law)

names(mice_imputed)
df_genderviolence1$Inheritance_law <- mice_imputed$imputed_pmm


#Hacemos lo mismo con la columna de porcentaje de inseguridad

df_gender_imputacion <- df_genderviolence[,-c(1,2,4,5)]
data_to_impute <- df_gender_imputacion$Insecurity_feeling

mice_imputed <- data.frame(original = df_gender_imputacion$Insecurity_feeling, 
                           imputed_pmm = complete(mice(df_gender_imputacion, method = "pmm"))$Insecurity_feeling)

names(mice_imputed)
df_genderviolence1$Insecurity_feeling <- mice_imputed$imputed_pmm

colSums(is.na(df_genderviolence1)) 
#Hemos eliminado los valores perdidos de este df

rm(mice_imputed)
rm(df_impute)
rm(df_gender_imputacion)


boxplot(df_genderviolence1$Violence_against_women_attitudes, 
        main = "Violence against women attitudes",
        col = "lightblue")
#La mayoría tiene valores entre 10 y 40, aunque hay países en los que se supera el 80%, se ha mantenido

boxplot(df_genderviolence1$Insecurity_feeling, 
        main = "Insecurity Feeling",
        col = "lightblue")
#La mayoría de países tiene valores entre 0 y 5, parece que hay algún error
#La mayoría de países tiene valores entre 60 y 70 

boxplot(df_genderviolence1$'percentage_violence', 
        main = "% of violence against women",
        col = "lightblue")
#El porcentaje de mujeres que han sufrido violencia va entre el 20% y 38% en la mayoría de países
#Parece igual que el anterior

summary(df_genderviolence$Insecurity_feeling)
#  Min.   1st Qu.  Median   Mean  3rd Qu.  Max.     NA's 
#  29.70   57.80   65.40   63.52   70.20   91.10      38 

summary(df_genderviolence1$Insecurity_feeling)
#   Min.  1st Qu.  Median  Mean   3rd Qu.  Max. 
# 0.0000  0.0000  0.2500  0.3394  0.5000  1.0000 

#Se ha distorsionado completamente vamos a comprobar que ha pasado

df_gender_imputacion <- df_genderviolence[,-c(1,2,4,5)]
data_to_impute <- df_gender_imputacion$Insecurity_feeling

mice_imputed <- data.frame(original = df_gender_imputacion$Insecurity_feeling, 
                           imputed_pmm = complete(mice(df_gender_imputacion, method = "pmm"))$Insecurity_feeling)

names(mice_imputed)
df_genderviolence1$Insecurity_feeling <- mice_imputed$imputed_pmm

summary(df_genderviolence$Insecurity_feeling)
#  Min.   1st Qu.  Median   Mean  3rd Qu.  Max.     NA's 
#  29.70   57.80   65.40   63.52   70.20   91.10      38 

summary(df_genderviolence1$Insecurity_feeling)
#  Min.   1st Qu.  Median  Mean   3rd Qu.   Max. 
#  29.70   58.10   66.10   64.02   70.60   91.10 

boxplot(df_genderviolence1$Insecurity_feeling, 
        main = "Insecurity Feeling",
        col = "lightblue")
#Ha subido un poco la media pero ya no se encuentran distorsionados como antes



#Ahora que tenemos el df limpio, crearemos una última variable que haga un sumatorio de todas 
#las demás. Al ser 12 variables, el número más cerca de 12 indicará una peor igualdad de género 
#El número más cercano a 0 indicará una mayor igualdad de género en todos los aspectos mencionados

df_genderviolence1$total <- rowSums(df_genderviolence1[, -1])

library(scales)
df_scaled <- as.data.frame(sapply(df_genderviolence1[,-1], function(x) rescale(scale(x))))

# find missing values in df_scaled
missing_vals <- is.na(df_scaled)

# loop through each column and replace missing values with column mean
for (col in 1:ncol(df_scaled)) {
  col_vals <- df_scaled[, col]
  col_mean <- mean(col_vals, na.rm = TRUE)
  col_missing <- missing_vals[, col]
  col_missing_count <- sum(col_missing)
  if (col_missing_count > 0) {
    col_vals[col_missing] <- col_mean
    df_scaled[, col] <- col_vals
  }
}

# calculate row sums for df_scaled
#Eliminamos también el % de mujeres en el parlamento ya que este sesgaría los resultados
df_scaled_total <- df_scaled[,-7]
df_genderviolence1$total <- rowSums(df_scaled_total)



rm(df_scaled)
rm(df_scaled_total)
rm(missing_vals)
rm(mice_imputed)
rm(df_gender_imputacion)

#Los países con peores resultados son Yemen, Afghanistan, Syria, Iran,  Bangladesh, 
                                     #Pakistan, Somalia, Bahrain, Guinea y Tanzania

#Valores duplicados

duplicated_rows <- duplicated(df_GDP)
df_unique <- subset(df_GDP, !duplicated_rows)

duplicated_rows <- duplicated(df_genderviolence1)
df_unique <- subset(df_genderviolence1, !duplicated_rows)

duplicated_rows <- duplicated(df_labourforce)
df_unique <- subset(df_labourforce, !duplicated_rows)

duplicated_rows <- duplicated(df_migrants_age_sex)
df_unique <- subset(df_migrants_age_sex, !duplicated_rows)

duplicated_rows <- duplicated(df_migrantsbydestination)
df_unique <- subset(df_migrantsbydestination, !duplicated_rows)

duplicated_rows <- duplicated(df_migrantsbyorigin)
df_unique <- subset(df_migrantsbyorigin, !duplicated_rows)

duplicated_rows <- duplicated(df_remittances)
df_unique <- subset(df_remittances, !duplicated_rows)

#No hay duplicados



#Recordamos las características de nuestros df
colSums(is.na(df_GDP)) #Tiene 0 valores perdidos
colSums(is.na(df_genderviolence1)) #0 valores perdidos
colSums(is.na(df_labourforce)) #0 valores perdidos
colSums(is.na(df_migrants_age_sex)) #0 valores perdidos
colSums(is.na(df_migrantsbydestination)) #0 valores perdidos
colSums(is.na(df_migrantsbyorigin)) #0 valores perdidos
colSums(is.na(df_remittances)) #0 valores perdidos

nrow(df_GDP) #207 países
nrow(df_genderviolence1) #179 países
nrow(df_labourforce) #187 países
nrow(df_migrants_age_sex) #232 países
nrow(df_migrantsbydestination) #232 países
nrow(df_migrantsbyorigin) #231 países
nrow(df_remittances) #190 países


#Vamos primero a llamar a la columna de país "country" en todos los df
df_GDP <- df_GDP %>% rename('country' = 'Country Name')
df_remittances <- df_remittances %>% rename('country' = 'Country Name')
df_genderviolence1 <- df_genderviolence1 %>% rename('country' = 'Country')
df_migrants_age_sex <- df_migrants_age_sex %>% rename('country' = 'destination')
df_MigrantStock_female <- df_MigrantStock_female %>% rename('country' = 'Destination_country')
df_MigrantStock_male <- df_MigrantStock_male %>% rename('country' = 'Destination_country')
df_MigrantStock_Total <- df_MigrantStock_Total %>% rename('country' = 'Destination_country')
df_labourforce <- df_labourforce %>% rename('country' = 'Country Name')

#A continuación vamos a proceder a la homogeneización de los nombres de países 
#Para poder llevar a cabo el análisis exploratorio y las correlaciones

#Lo haremos con el paquete fuzzyjoin
#install.packages("fuzzyjoin")
library(fuzzyjoin)

df_merged <- fuzzy_join <- regex_full_join(df_GDP, df_genderviolence1, by = "country")

#Czech Republic
#Chinese Taipei
#Türkiye
#China (People's Republic of)
#Venezuela
#Côte d'Ivoire
#Kyrgyzstan
#Lao People's Democratic Republic
#Palestinian Authority or West Bank and Gaza Strip
#Viet Nam
#Democratic Republic of the Congo
#Eritrea
#Yemen
#South Sudan

#Estos son los nombres que no coinciden, vamos a modificar los que sí aparecen en el df_GDP

df_genderviolence1$country <- gsub("Czech Republic", "Czechia", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Türkiye", "Turkiye", df_genderviolence1$country)
df_genderviolence1$country <- gsub("China_(People's_Republic_of)", "China", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Côte d'Ivoire", "Cote_dIvoire", df_genderviolence1$country)
df_GDP$country <- gsub("Cote_d'Ivoire", "Cote_dIvoire", df_GDP$country)
df_genderviolence1$country <- gsub("Kyrgyzstan", "Kyrgyz_Republic", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Lao People's Democratic_Republic", "Lao_PDR", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Viet Nam", "Vietnam", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Democratic Republic of the Congo", "Congo_Dem_Rep.", df_genderviolence1$country)

df_genderviolence1$country <- gsub(" ", "_", df_genderviolence1$country)
df_GDP$country <- gsub(" ", "_", df_GDP$country)
df_labourforce$country <- gsub(" ", "_", df_labourforce$country)
df_migrants_age_sex$country <- gsub(" ", "_", df_migrants_age_sex$country)
df_migrantsbydestination$country <- gsub(" ", "_", df_migrantsbydestination$country)
df_migrantsbyorigin$country <- gsub(" ", "_", df_migrantsbyorigin$country)
df_MigrantStock_female$country <- gsub(" ", "_", df_MigrantStock_female$country)
df_MigrantStock_male$country <- gsub(" ", "_", df_MigrantStock_male$country)
df_MigrantStock_Total$country <- gsub(" ", "_", df_MigrantStock_Total$country)
df_remittances$country <- gsub(" ", "_", df_remittances$country)


df_merged <- fuzzy_join <- regex_full_join(df_GDP, df_genderviolence1, by = "country")

df_genderviolence1[67,1] <- 'China'
df_GDP$country <- gsub("Congo,_Dem._Rep.", "Congo_Dem_Rep.", df_GDP$country)
df_genderviolence1$country <- gsub("Cote_dIvoire", "Cote_dIvoire", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Lao_People's_Democratic_Republic", "Lao_PDR", df_genderviolence1$country)


df_merged <- fuzzy_join <- regex_full_join(df_GDP, df_genderviolence1, by = "country")

#Elimino los repetidos 
df_merged1 <- slice(df_merged, -c(39, 51, 73,57, 78,105,128,135))
df_merged1 <- slice(df_merged1, 1:175)
df_merged1 <- df_merged1[,-c(2,4)]

df_genderviolence_GDP <- df_merged1


#Ya tenemos el df para analiar el pib y la violencia por país 


df_remittances$country <- gsub("Congo,_Dem._Rep.", "Congo_Dem_Rep.", df_remittances$country)
df_remittances$country <- gsub("Cote_d'Ivoire", "Cote_dIvoire", df_remittances$country)
df_GDP$country <- gsub("Cote_d'Ivoire", "Cote_dIvoire", df_GDP$country)

df_merged1 <- df_merged1 %>% rename('country' = 'country.x')


df_merged <- fuzzy_join <- regex_full_join(df_remittances, df_GDP, by = "country")
df_merged <- df_merged[-c(50,56,74,79,107,133,141),]
df_merged <- slice(df_merged, 1:190)


df_GDP_remittances <- df_merged
df_GDP_remittances <- df_GDP_remittances[,-c(2,4,5)]
df_GDP_remittances <- df_GDP_remittances[,c(1,3,2)]

df_GDP_remittances <- df_GDP_remittances %>% rename('country' = 'country.x')
df_merged <- fuzzy_join <- regex_full_join(df_GDP_remittances, df_labourforce, by = "country")


df_labourforce$country <- gsub("Congo,_Dem._Rep.", "Congo_Dem_Rep.", df_labourforce$country)
df_labourforce$country <- gsub("Cote_d'Ivoire", "Cote_dIvoire", df_labourforce$country)
df_merged <- fuzzy_join <- regex_full_join(df_GDP_remittances, df_labourforce, by = "country")


df_merged <- slice(df_merged, -c(48,64,69,95,119,126))
df_merged <- slice(df_merged, 1:172)
df_merged <- df_merged[,-c(4,5)]

df_economy <- df_merged
df_economy <- df_economy %>% rename('country' = 'country.x')


df_merged <- fuzzy_join <- regex_full_join(df_genderviolence1, df_economy, by = "country")
df_genderviolence1$country <- gsub("Congo", "Congo,_Rep.", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Egypt", "Egypt,_Arab_Rep.", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Korea", "Korea,_Rep.", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Gambia", "Gambia,_The", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Russia", "Russian_Federation", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Congo,_Rep._Dem_Rep.", "Congo_Dem_Rep.", df_genderviolence1$country)
df_genderviolence1$country <- gsub("Congo,_Rep.,_Rep.", "Congo,_Rep.", df_genderviolence1$country)


df_merged <- fuzzy_join <- regex_full_join(df_economy, df_genderviolence1, by = "country")

df_merged <- slice(df_merged, -c(43,49,64,69,94,117,124))
df_merged <- slice(df_merged, 1:162)
df_merged <- df_merged[,-6]

df_total <- df_merged
df_total <- df_total %>% rename('country' = 'country.x')
df_total_sinmigracion <- df_total

#####################
#Df_migrantes_total 

df_merged <- fuzzy_join <- regex_full_join(df_migrantsbyorigin, df_migrantsbydestination, by = "country")

df_migrantsbyorigin$country <- gsub("Iran_(Islamic_Republic_of)", "Iran", df_migrantsbyorigin$country)
df_migrantsbydestination$country <- gsub("Iran_(Islamic_Republic_of)", "Iran", df_migrantsbydestination$country)
df_migrantsbydestination$country <- gsub("Venezuela_(Bolivarian_Republic_of)", "Venezuela", df_migrantsbydestination$country)
df_migrantsbyorigin$country <- gsub("Venezuela_(Bolivarian_Republic_of)", "Venezuela", df_migrantsbyorigin$country)
df_migrantsbyorigin$country <- gsub("Sint_Maarten_(Dutch_part)", "Sint_Maarten", df_migrantsbyorigin$country)
df_migrantsbydestination$country <- gsub("Sint_Maarten_(Dutch_part)", "Sint_Maarten", df_migrantsbydestination$country)
df_migrantsbyorigin$country <- gsub("Bolivia_(Plurinational_State_of)", "Bolivia", df_migrantsbyorigin$country)
df_migrantsbydestination$country <- gsub("Bolivia_(Plurinational_State_of)", "Bolivia", df_migrantsbydestination$country)
df_migrantsbyorigin$country <- gsub("Falkland_Islands_(Malvinas)", "Falkland_Islands", df_migrantsbyorigin$country)
df_migrantsbydestination$country <- gsub("Falkland_Islands_(Malvinas)", "Falkland_Islands", df_migrantsbydestination$country)

df_merged <- fuzzy_join <- regex_full_join(df_migrantsbyorigin, df_migrantsbydestination, by = "country")


df_migrantsbyorigin[101,1] <- 'Iran'
df_migrantsbydestination[85,1] <- 'Iran'
df_migrantsbyorigin[226,1] <- 'Venezuela'
df_migrantsbydestination[155,1] <- 'Venezuela'
df_migrantsbyorigin[189,1] <- 'Sint_Maarten'
df_migrantsbydestination[130,1] <- 'Sint_Maarten'
df_migrantsbyorigin[25,1] <- 'Bolivia'
df_migrantsbydestination[143,1] <- 'Bolivia'
df_migrantsbyorigin[72,1] <- 'Falkland_Islands'
df_migrantsbydestination[148,1] <- 'Falkland_Islands'

df_merged <- fuzzy_join <- regex_full_join(df_migrantsbyorigin, df_migrantsbydestination, by = "country")

df_merged <- slice(df_merged, -c(5,46,48,62,63,68,74,85,100,161,169,205))
df_merged <- slice(df_merged, 1:229)

df_migrantes_total <- df_merged
df_migrantes_total <- df_migrantes_total[,-7]

df_migrantes_total <- df_migrantes_total %>% rename('country' = 'country.x',
                                                    'total_emigrants' = 'Total.x',
                                                    'male_percent_emigrants' = 'male_percent.x',
                                                    'female_percent_emigrants' = 'female_percent.x',
                                                    'male_immigrants' = 'male_Total',
                                                    'female_immigrants' = 'female_Total',
                                                    'total_immigrants' = 'Total.y',
                                                    'male_percent_immigrants' = 'male_percent.y',
                                                    'female_percent_immigrants' = 'female_percent.y')



###################
#df_total


df_merged <- fuzzy_join <- regex_full_join(df_migrantes_total, df_total, by = "country")

df_migrantes_total$country <- gsub("Congo", "Congo,_Rep.", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Democratic_Republic_of_the_Congo,_Rep.", "Congo_Dem_Rep.", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Côte_d'Ivoire", "Cote_dIvoire", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Lao_People's_Democratic_Republic", "Lao_PDR", df_migrantes_total$country)
df_total$country <- gsub("Gambia,_The", "Gambia", df_total$country)
df_migrantes_total$country <- gsub("Republic_of_Korea", "Korea,_Rep.", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Kyrgyzstan", "Kyrgyz_Republic", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Slovakia", "Slovak_Republic", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Turkey", "Turkiye", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Congo,_Rep._Dem_Rep.", "Congo_Dem_Rep.", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Congo,_Rep.,_Rep.", "Congo,_Rep.", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Egypt", "Egypt,_Arab_Rep.", df_migrantes_total$country)
df_migrantes_total$country <- gsub("Viet_Nam", "Vietnam", df_migrantes_total$country)


df_merged <- fuzzy_join <- regex_full_join(df_migrantes_total, df_total, by = "country")
df_merged <- slice(df_merged, -c(33,34,52,66,114,120,166,145,4))
df_merged <- slice(df_merged, 1:161)

df_countries_total <- df_merged
df_countries_total <- df_countries_total[,-12]
df_countries_total <- df_countries_total %>% rename('country' = 'country.x')


df_merged <- fuzzy_join <- regex_full_join(df_countries_total, df_total_sinmigracion, by = "country")

df_merged <- fuzzy_join <- regex_full_join(df_total_sinmigracion, df_countries_total, by = "country")


#Eliminamos la fila 40 de Corea del Norte ya que nos faltan los datos económicos
df_countries_total <- df_countries_total[-40,]

#La única diferencia es que en el total no tenemos los datos de migración sobre macedonia del norte y gambia




rm(df_merged)
df_genderviolence <- df_genderviolence1
rm(df_genderviolence1)
rm(df_unique)
rm(fuzzy_join)
rm(df_gender_imputacion_attittudes)
rm(df_gender_imputacion_violence)
rm(df_total)


###################3
#Por ende, los df para hacer las correlaciones son los siguientes: 

#df_countries_total : con todas las variables del total de países coincidentes
nrow(df_countries_total) #160 países
ncol(df_countries_total) #28 variables
names(df_countries_total)
#[1] "country"                          "male_emigrants"                  
#[3] "female_emigrants"                 "total_emigrants"                 
#[5] "male_percent_emigrants"           "female_percent_emigrants"        
#[7] "male_immigrants"                  "female_immigrants"               
#[9] "total_immigrants"                 "male_percent_immigrants"         
#[11] "female_percent_immigrants"        "GDP"                             
#[13] "Remittances"                      "labourforce_female"    
#[15] "labourforce_male"                 "Inheritance_law"                 
#[17] "Violence_against_women_law"       "Violence_against_women_attitudes"
#[19] "percentage_violence"              "FGM_law"                         
#[21] "Citizenship_rights"               "Inequality_voting_rights"        
#[23] "percentage_women_parliament"        "Insecurity_feeling"              
#[25] "Access_to_justice"                "Child_marriage_law"              
#[27] "Girls_married"                    "total"                           


#df_migrantes_total : con el número de emigrantes e inmigrantes por país
nrow(df_migrantes_total) #229 países
ncol(df_migrantes_total) #11 variables
names(df_migrantes_total)
#[1] "country"                   "male_emigrants"            "female_emigrants"         
#[4] "total_emigrants"           "male_percent_emigrants"    "female_percent_emigrants" 
#[7] "male_immigrants"           "female_immigrants"         "total_immigrants"         
#[10] "male_percent_immigrants"   "female_percent_immigrants"

#df_economy : con el PIB, remesas y participación laboral por país
nrow(df_economy) #172 países
ncol(df_economy) #5 variables
names(df_economy)
#[1] "country"            "GDP"                "Remittances"        "labourforce_female"
#[5] "labourforce_male" 

#df_genderviolence_GDP : con el PIB, remesas y participación laboral por país
nrow(df_genderviolence_GDP) #175 países
ncol(df_genderviolence_GDP) #15 variables
names(df_genderviolence_GDP)
#[1] "country"                          "GDP"                             
#[3] "Remittances"                      "labourforce_female"              
#[5] "labourforce_male"                 "Inheritance_law"                 
#[7] "Violence_against_women_law"       "Violence_against_women_attitudes"
#[9] "percentage_violence"              "FGM_law"                         
#[11] "Citizenship_rights"               "Inequality_voting_rights"        
#[13] "percentage_women_parliament"        "Insecurity_feeling"              
#[15] "Access_to_justice"                "Child_marriage_law"              
#[17] "Girls_married"                    "total"


#df_total_sinmigracion : con el PIB, remesas y participación laboral por país
nrow(df_total_sinmigracion) #162 países
ncol(df_total_sinmigracion) #18 variables
names(df_total_sinmigracion)
#[1] "country"                          "GDP"                             
#[3] "Remittances"                      "labourforce_female"              
#[5] "labourforce_male"                 "Inheritance_law"                 
#[7] "Violence_against_women_law"       "Violence_against_women_attitudes"
#[9] "percentage_violence"              "FGM_law"                         
#[11] "Citizenship_rights"               "Inequality_voting_rights"        
#[13] "percentage_women_parliament"        "Insecurity_feeling"              
#[15] "Access_to_justice"                "Child_marriage_law"              
#[17] "Girls_married"                    "total"  


########################################################
# 2. ANÁLISIS EXPLORATORIO DE LOS DATOS
########################################################
########################################################


#Vamos a comenzar analizando las diferencias económicas entre regiones y países
#Así como la desigualdad de género laboral 

# Análisis económico -------------------------------------------------------------

#Para ello, vamos a usar el df que creamos anteriormente con aspectos económicos 
head(df_economy)

#Distribución de los valores
str(df_economy)
df_economy[, -1] <- apply(df_economy[, -1], 2, as.numeric)

dfnum <- df_economy[, sapply(df_economy, is.numeric)]
str(dfnum)

plot_list <- lapply(names(dfnum), function(var) {
  ggplot(dfnum, aes(x = 1, y = dfnum[[var]])) +
    geom_boxplot() +
    ggtitle(paste0(var, " Distribution")) +
    xlab(var) +
    ylab("Value") +
    theme(plot.title = element_text(size = 15))
})

library(gridExtra)
grid.arrange(grobs = plot_list, nrow = 2, ncol = 2)

#En GDP tenemos dos outliers que corresponden a Estados Unidos y China
#En Remesas hay muchos outliers, parece que en bastantes países el % de remesas es superior
#Podemos ver una diferencia importante en cuanto a la participación laboral femenina y masculina
#Mientras que la femenina se encuentra entre 40 y 50% teniendo bastantes outliers por debajo
#La participación masculina se encuentra entre 50 y 60 teniendo bastantes outliers por encima


summary(df_economy)
#GDP: 
# -El mínimo es de 727,400,000 dólares
# -El máximo es de 21,380,000,000,000 dólares
# -La media es 657,900,000,000 dólares
# -La mayoría de los países tienen un PIB entre 12,710,000,000 y 25,650,000,000

histogram <- df_economy %>%
  ggplot(aes(x=GDP)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9, bins=172) +
  ggtitle("GDP") +
  theme(plot.title = element_text(size=15))
print(histogram)

ggplot(data=df_economy, aes(x=reorder(country, GDP), y=GDP)) +
  geom_bar(stat = "identity", aes(fill=GDP)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df_economy$GDP), size=1, color="blue") +
  scale_fill_gradient(name="% Difference", low = "red", high = "green") +
  labs(title="PIB por país", x="", y="PIB")


#Como no quiero eliminar ningún país, lo que voy a hacer es crear subgrupos

df_us_china <- df_economy[df_economy$country %in% c("United_States", "China"), ]

#comparamos los cuatro valores de estos dos países
# Convertimos el dataframe a formato largo
df_largo <- pivot_longer(df_us_china, cols = -country, names_to = "variable", values_to = "valor")

# Creamos un plot grid de barplots
ggplot(df_largo, aes(x = country, y = valor)) +
  geom_bar(stat = "identity", color = "darkblue", fill = "lightblue", width = 0.7) +
  facet_wrap(~variable, scales = "free_y", nrow = 2, labeller = labeller(variable = c("GDP" = "PIB", "labourforce_female" = "Participación laboral F", "labourforce_male" = "Participación laboral M", "Remittances" = "Remesas"))) +
  theme_minimal() +
  labs(x = "País", y = "Valor") +
  ggtitle("Análisis comparativo Estados Unidos y China") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major.y = element_line(color = "lightblue", linetype = "dashed"),
        strip.text = element_text(size = 12))

head(df_us_china)

#Ahora voy a crear un df con los outliers (sin incluir EEUU y China), y otro con los demás valores (PIB más bajo)


# Calcular el percentil 70 para cada país
pct_70 <- quantile(df_economy$GDP, 0.7, na.rm = TRUE)
df_outliers <- df_economy[df_economy$GDP > pct_70 & !df_economy$country %in% c("United_States", "China"), ]

# Filtrar los países que no están en df_usa_china ni en df_outliers
df_other <- df_economy[!(df_economy$country %in% df_us_china$country) & !(df_economy$country %in% df_outliers$country), ]


histogram <- df_outliers %>%
  ggplot(aes(x=GDP)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9, bins=172) +
  ggtitle("GDP") +
  theme(plot.title = element_text(size=15))
print(histogram)

ggplot(data=df_outliers, aes(x=reorder(country, GDP), y=GDP)) +
  geom_bar(stat = "identity", aes(fill=GDP)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = median(df_economy$GDP), size=1, color="blue") +
  scale_fill_gradient(name="PIB", low = "#84D2F5", high = "#3F6475") +
  labs(title="PIB por país", x="", y="PIB")

ggplot(data=df_outliers, aes(x=reorder(country, GDP), y=GDP)) +
  geom_bar(stat = "identity", aes(fill=GDP)) +
  theme_light() +
  geom_hline(yintercept = median(df_economy$GDP), size=1, color="blue") +
  scale_fill_gradient(name="PIB", low = "#84D2F5", high = "#3F6475") +
  labs(title="PIB por país", x="", y="PIB") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))


ggplot(data=df_other, aes(x=reorder(country, GDP), y=GDP)) +
  geom_bar(stat = "identity", aes(fill=GDP)) +
  theme_light() +
  geom_hline(yintercept = median(df_other$GDP), size=1, color="blue") +
  scale_fill_gradient(name="PIB", low = "#84D2F5", high = "#3F6475") +
  labs(title="PIB por país", x="", y="PIB") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1,size=6))

ggplot(data=df_other, aes(x=reorder(country, GDP), y=GDP)) +
  geom_bar(stat = "identity", aes(fill=GDP)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = median(df_economy$GDP), size=1, color="blue") +
  scale_fill_gradient(name="PIB", low = "#84D2F5", high = "#3F6475") +
  labs(title="PIB por país", x="", y="PIB") + 
  theme(axis.text.y = element_text(size=5))


df_tail10_gdp <- df_economy %>% 
  arrange(GDP) %>% 
  head(10)
names(df_economy)
head(df_economy)

#Ahora vamos a calcular las diferencias entre la participación laboral de hombres y mujeres
df_economy$diferencia_genero = ((df_economy$labourforce_female - df_economy$labourforce_male)/df_economy$labourforce_male)*100

ggplot(data=df_economy, aes(x=reorder(country, diferencia_genero), y=diferencia_genero)) +
  geom_bar(stat = "identity", aes(fill=GDP)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = median(df_economy$diferencia_genero), size=1, color="blue") +
  scale_fill_gradient(name="Diferencia", low = "#84D2F5", high = "#3F6475") +
  labs(title="Desigualdad de género en el trabajo", x="", y="PIB") + 
  theme(axis.text.y = element_text(size=5))

#Nos damos cuenta de un error importante en el país Uzbekistán ya que los valores están cambiados


df_economy[df_economy$country == "Uzbekistan", "labourforce_female"] <- 36.06811
df_economy[df_economy$country == "Uzbekistan", "labourforce_male"] <-63.931891

df_economy$diferencia_genero = ((df_economy$labourforce_female - df_economy$labourforce_male)/df_economy$labourforce_male)*100

ggplot(data=df_economy, aes(x=reorder(country, diferencia_genero), y=diferencia_genero)) +
  geom_bar(stat = "identity", aes(fill=diferencia_genero)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = median(df_economy$diferencia_genero), size=1, color="blue") +
  scale_fill_gradient(name="Diferencia", low = "#84D2F5", high = "#3F6475") +
  labs(title="Desigualdad de género en el trabajo", x="", y="PIB") + 
  theme(axis.text.y = element_text(size=5))

summary(df_economy$diferencia_genero)
#  Min.  1st Qu.  Median  Mean   3rd Qu.    Max. 
#-83.93  -34.81  -18.29  -25.00  -10.60   10.83 

df_tail10_gdp <- df_economy %>% 
  arrange(diferencia_genero) %>% 
  tail(10)


summary(df_economy$GDP)
df_economy$income_group <- ifelse(df_economy$GDP > 7.013691e+11, "high_income",
                                  ifelse(df_economy$GDP > 4.670e+10, "medium_income", "lowincome"))

library(hrbrthemes)
library(viridis)

# Without transparency (left)
p1 <- ggplot(data=diamonds, aes(x=diferencia_genero, group=cut, fill=cut)) +
  geom_density(adjust=1.5) +
  theme_ipsum()
#p1

# With transparency (right)
p2 <- ggplot(data=df_economy, aes(x=diferencia_genero, group=income_group, fill=income_group)) +
  labs(title = "Participación laboral femenina en comparación con la masculina",
       x = "Porcentaje de diferencia",
       y = "Densidad") +
  theme_minimal() +
  geom_density(adjust=1.5, alpha=.4)
print(p2)



summary(df_economy$Remittances)
#Remittances
# -El mínimo es 0
# -El máximo es un 37% de remesas del PIB
# -La media es 4.6%
# -La mayoría de los países tienen % entre 0.3% y 5.6%
# -La mediana es 2.1%, que corresponde al percentil 50


histogram <- df_economy %>%
  ggplot(aes(x=Remittances)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9, bins=172) +
  ggtitle("Remittances") +
  theme(plot.title = element_text(size=15))
print(histogram)

histogram <- df_economy %>%
  ggplot(aes(x=Remittances)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9, bins=172) +
  ggtitle("Remittances") +
  theme(plot.title = element_text(size=15))
print(histogram)


df_tail10_gdp <- df_economy %>% 
  arrange(Remittances) %>% 
  tail(10)


df_tail10_gdp <- df_economy %>% 
  arrange(Remittances) %>% 
  head(10)

str(df_economy)

#Gráfica comparación de remesas por grupos de países según el PIB

df_economy$income_group <- factor(df_economy$income_group, levels=c("low_income", "medium_income", "high_income"))

df_economy %>%
  ggplot( aes(x=income_group, y=Remittances, fill=income_group)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option="A") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Remesas por grupo económico") +
  xlab("")


df_economy %>%
  ggplot( aes(x=income_group, y=labourforce_female, fill=income_group)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option="A") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Participación laboral femenina por grupo económico") +
  xlab("")


df_sin_uschinajapan <- df_economy[!(df_economy$country %in% c("United_States", "China", "Japan")), ]

# create heatmap
df_num <- df_sin_uschinajapan[,c(2,3,4,5)]
df_num <- data.matrix(df_num)


heatmap(df_num, scale="column",
        Colv=NA,
        col = viridis(100), 
        margins=c(10, 10), 
        xlab = "Variables", ylab = "Observations", 
        main = "Heatmap of Economic Data",
        cex.axis = 0.7, las = 2)

#No sacamos mucha información de aquí, vamos a analizar la correlación entre GDP y remesas
df_num <- df_sin_uschinajapan[,c(2,3,4,5)]
p3 <- ggplot(df_num, aes(x=GDP, y=Remittances)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()
print(p3)



# Análisis violencia de género -------------------------------------------------------------

#Sigo con el análisis del df de violencia de género

names(df_genderviolence)
#eliminamos la variable sobre las leyes de mutilación genial femenina ya que no aparece en el paper metodológico
df_genderviolence <- df_genderviolence[,-6]


head(df_genderviolence)
df_genderviolence$total

boxplot(df_genderviolence$total,
        main = "Gender Violence",
        col = '#84D2F5',
        ylab = "Total", 
        alpha=0.5)

boxplot(df_genderviolence$total, main="Violencia contra las Mujeres", 
        ylab="Total", col="steelblue", 
        border="black", boxwex=0.5, notch=TRUE, 
        medcol="white", whiskcol="black", staplecol="black", 
        outliercol="black", notchwidth=0.5, alpha=0.5)

boxplot.stats(df_genderviolence$total)

library(gridExtra)

df_genderviolencenum <- df_genderviolence[,-c(1,13)]


# Plot grid de los boxplots

# Transformamos los datos a long format
df_long <- df_genderviolencenum %>% 
  gather(key = "variable", value = "value")

#creamos el boxplot
ggplot(df_long, aes(x = "", y = value)) +
  geom_boxplot(alpha = 0.5, color = "black", outlier.shape = NA, fill = "steelblue") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Análisis de los indicadores de violencia contra la mujer",
       x = "variables", y = "Valor del indicador")

summary(df_genderviolencenum)

# Utilizamos la función summarize para calcular los valores estadísticos para cada variable
stats_df <- df_long %>% 
  group_by(variable) %>% 
  summarize(
    min = min(value),
    q1 = quantile(value, probs = 0.25),
    median = median(value),
    q3 = quantile(value, probs = 0.75),
    max = max(value)
  )

# Imprimimos el resultado en la consola
print(stats_df)


names(df_genderviolencenum)
# Representar en un gráfico de barras
#Para ellos quedamos con el número de países de cada valor
access_summary <- df_genderviolencenum %>% 
  group_by(`Access_to_justice`) %>% 
  summarize(count = n())

access_summary$Access_to_justice <- as.character(access_summary$Access_to_justice)

# plot with ggplot
ggplot(access_summary, aes(x=Access_to_justice, y=count)) + 
  geom_bar(stat = "identity", color='black', fill='steelblue') +
  labs(title="Acceso de las mujeres a la justicia", 
       x="Nivel de acceso", y="Número de países")

#Realizamos lo mismo con la variable de leyes de matrimonio infantil

access_summary <- df_genderviolencenum %>% 
  group_by(`Child_marriage_law`) %>% 
  summarize(count = n())

access_summary$Child_marriage_law <- as.character(access_summary$Child_marriage_law)

# plot with ggplot
ggplot(access_summary, aes(x=Child_marriage_law, y=count)) + 
  geom_bar(stat = "identity", color='black', fill='steelblue') +
  labs(title="Leyes de matrimonio infantil", 
       x="Nivel de igualdad", y="Número de países")

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(Child_marriage_law)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(Child_marriage_law) %>% 
  head(10)
df_head10_gender$country

#Analizamos el porcentaje de niñas entre 15 y 19 casadas, divorciadas, viudas o en unión informal
boxplot(df_genderviolence$Girls_married, col='steelblue', main='Porcentaje matrimonio infantil',
        ylab="Total",outliercol="black", notchwidth=0.5, alpha=0.5)
summary(df_genderviolence$Girls_married)

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(Girls_married)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(Girls_married) %>% 
  head(10)
df_head10_gender$country

#inheritance law
access_summary <- df_genderviolencenum %>% 
  group_by(`Inheritance_law`) %>% 
  summarize(count = n())

access_summary$Inheritance_law <- as.character(access_summary$Inheritance_law)

ggplot(access_summary, aes(x=Inheritance_law, y=count)) + 
  geom_bar(stat = "identity", color='black', fill='steelblue') +
  labs(title="Leyes hereditarias", 
       x="Nivel de igualdad", y="Número de países")

summary(df_genderviolencenum$Inheritance_law)
access_summary

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(Inheritance_law)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(Inheritance_law) %>% 
  head(10)
df_head10_gender$country

#Violencia contra las mujeres
access_summary <- df_genderviolencenum %>% 
  group_by(`Violence_against_women_law`) %>% 
  summarize(count = n())

access_summary$Violence_against_women_law <- as.character(access_summary$Violence_against_women_law)

ggplot(access_summary, aes(x=Violence_against_women_law, y=count)) + 
  geom_bar(stat = "identity", color='black', fill='steelblue') +
  labs(title="Protección legal ante la violencia contra las mujeres", 
       x="Nivel de protección", y="Número de países")

summary(df_genderviolencenum$Violence_against_women_law)
access_summary

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(Inheritance_law)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(Inheritance_law) %>% 
  head(10)
df_head10_gender$country


#Actitudes de violencia contra las mujeres
boxplot(df_genderviolence$Violence_against_women_attitudes, col='steelblue', main='Porcentaje mujeres que normalizan la violencia',
        ylab="Total",outliercol="black", notchwidth=0.5, alpha=0.5)
summary(df_genderviolence$Violence_against_women_attitudes)

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(Violence_against_women_attitudes)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(Violence_against_women_attitudes) %>% 
  head(10)
df_head10_gender$country


#Porcentajes de mujeres que han sufrido violencia
boxplot(df_genderviolence$percentage_violence, col='steelblue', main='Porcentaje víctimas de violencia de género',
        ylab="Total",outliercol="black", notchwidth=0.5, alpha=0.5)
summary(df_genderviolence$percentage_violence)

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(percentage_violence)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(percentage_violence) %>% 
  head(10)
df_head10_gender$country


#Leyes sobre nacionalidad
access_summary <- df_genderviolencenum %>% 
  group_by(`Citizenship_rights`) %>% 
  summarize(count = n())

access_summary$Citizenship_rights <- as.character(access_summary$Citizenship_rights)

ggplot(access_summary, aes(x=Citizenship_rights, y=count)) + 
  geom_bar(stat = "identity", color='black', fill='steelblue') +
  labs(title="Leyes sobre Nacionalidad", 
       x="Nivel de igualdad", y="Número de países")

summary(df_genderviolencenum$Citizenship_rights)
access_summary

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(Citizenship_rights)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(Citizenship_rights) %>% 
  head(10)
df_head10_gender$country


#Leyes sobre derecho al voto
access_summary <- df_genderviolencenum %>% 
  group_by(`Inequality_voting_rights`) %>% 
  summarize(count = n())

access_summary$Inequality_voting_rights <- as.character(access_summary$Inequality_voting_rights)

ggplot(access_summary, aes(x=Inequality_voting_rights, y=count)) + 
  geom_bar(stat = "identity", color='black', fill='steelblue') +
  labs(title="Leyes sobre el voto", 
       x="Nivel de igualdad", y="Número de países")

summary(df_genderviolencenum$Inequality_voting_rights)
access_summary

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(Inequality_voting_rights)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(Inequality_voting_rights) %>% 
  head(10)
df_head10_gender$country

#Porcentajes de mujeres en el parlamento
boxplot(df_genderviolence$percentage_women_parliament, col='steelblue', main='Porcentaje de mujeres en el parlamento',
        ylab="Total",outliercol="black", notchwidth=0.5, alpha=0.5)
summary(df_genderviolence$percentage_women_parliament)

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(percentage_women_parliament)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(percentage_women_parliament) %>% 
  head(10)
df_head10_gender$country

#Porcentajes de mujeres que sienten inseguridad en la calle por la noche
boxplot(df_genderviolence$Insecurity_feeling, col='steelblue', main='Sentimiento de inseguridad en la calle por la noche',
        ylab="Porcentaje de mujeres",outliercol="black", notchwidth=0.5, alpha=0.5)
summary(df_genderviolence$Insecurity_feeling)

df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(Insecurity_feeling)) %>% 
  head(10)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(Insecurity_feeling) %>% 
  head(10)
df_head10_gender$country


#Por último, comparamos los resultados totales
df_tail10_gender <- df_genderviolence %>% 
  arrange(desc(total)) %>% 
  head(20)
df_tail10_gender$country

df_head10_gender <- df_genderviolence %>% 
  arrange(total) %>% 
  head(20)
df_head10_gender$country

#La consola va muy lenta por la cantidad de líneas de código, plots y datasets
#Voy a descargar los df y seguir en otro archivo nuevo 


#install.packages("xlsx")
library(xlsx)

write.xlsx(df_economy, "Migración y desigualdad laboral.xlsx", 
           sheetName = "Economy")
write.xlsx(df_genderviolence, "Migración y desigualdad laboral.xlsx", 
           append=TRUE,sheetName = "Gender_violence")
write.xlsx(df_migrantes_total, "Migración y desigualdad laboral.xlsx", 
           append=TRUE,sheetName = "Migrants")
write.xlsx(df_countries_total, "Migración y desigualdad laboral.xlsx", 
           append=TRUE,sheetName = "Total")






###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

