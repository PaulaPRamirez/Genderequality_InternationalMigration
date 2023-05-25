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

# DESCRIPTIVO IGUALDAD DE GÉNERO
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


#Comenzamos el análisis 

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


#Voy a descargar los df y seguir en otro archivo nuevo importando los datasets
#La consola está muy llena así que decido hacerlo así para poder borrarla


#install.packages("xlsx")
library(xlsx)

write.xlsx(df_economy, "data/Migration_genderinequality.xlsx", 
           sheetName = "Economy")
write.xlsx(df_genderviolence, "data/Migration_genderinequality.xlsx", 
           append=TRUE,sheetName = "Gender_violence")
write.xlsx(df_migrantes_total, "data/Migration_genderinequality.xlsx", 
           append=TRUE,sheetName = "Migrants")
write.xlsx(df_countries_total, "data/Migration_genderinequality.xlsx", 
           append=TRUE,sheetName = "Total")



