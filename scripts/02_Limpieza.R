# Autora: Paula Pareja
# Fecha: Mayo, 2023

# Título: La relación entre género y migración a través del análisis de datos

# Trabajo Final de Máster
# Universidad Carlos III Madrid
# Tutores: Iñaki Ucar y Félix Vacas


########################################################
# 2. LIMPIEZA DE LOS DATASETS
########################################################
########################################################

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

df_labourforce <- read_excel("data/labourforce_female.xlsx", sheet = 'Data')
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
df_merged <- slice(df_merged, -c(33,34,52,67,115,121,166,146,4))
df_merged <- slice(df_merged, 1:162)

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


###################
#Por ende, los df para hacer las correlaciones son los siguientes: 

#df_countries_total : con todas las variables del total de países coincidentes
nrow(df_countries_total) #162 países
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

#df_genderviolence_GDP : con todas las variables de género por país
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
