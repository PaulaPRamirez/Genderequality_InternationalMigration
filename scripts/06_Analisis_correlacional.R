# Autora: Paula Pareja
# Fecha: Mayo, 2023

# Título: La relación entre género y migración a través del análisis de datos

# Trabajo Final de Máster
# Universidad Carlos III Madrid
# Tutores: Iñaki Ucar y Félix Vacas


########################################################
# 4. ANÁLISIS CORRELACIONAL
########################################################
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
library(reshape2)    


#Vamos a hacer una matriz de correlación para poder visualizar de manera sencilla las relaciones
cor_matrix <- cor(df_model)

# display the correlation matrix
print(cor_matrix)


# visualize correlation matrix as a heatmap
ggplot(melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1,1), name = "Correlation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Análisis correlacional migración, género y trabajo

names(df_model)
df_model_cor1 <- df_model[,c(3,8,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27)]

cor_matrix <- cor(df_model_cor1)
print(cor_matrix)

melted_cor <- melt(cor_matrix)

correlation <- ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(size = 3, colour = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1,1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap")
correlation

ggsave("visualization/heatmap correlación.jpg", plot = correlation, width = 8, height =6, dpi = 300)


#voy a dividir mi df en dos grupos para poder analizar correctamente las correlaciones 
df_total$income_group <- ifelse(df_total$GDP > 4.670e+10, "highmedium_income", "low_income")



df_highmedium_income <- subset(df_total, income_group == "highmedium_income")
df_low_income <- subset(df_total, income_group == "low_income")



#correlaciones por grupos de países según PIB
ggplot(df_highmedium_income, aes(x = GDP, y = total)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")

ggplot(df_low_income, aes(x = GDP, y = total)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")

cor(df_highmedium_income$GDP, df_highmedium_income$total)
cor(df_low_income$GDP, df_low_income$total)

#No se observa una correlación muy alta directa entre PIB e igualdad de género



#Procedemos al análisis correlacional con el porcentaje de mujeres migrantes
#vamos a eliminar las que son interdependientes
names(df_model)
df_model_cor <- df_model[,c(3,5,8,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26)]

cor_matrix <- cor(df_model_cor)
print(cor_matrix)

melted_cor <- melt(cor_matrix)

correlationmap <- ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(size = 3, colour = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1,1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap")
correlationmap

ggsave("visualization/heatmap de correlación con emigración.jpg", plot = correlationmap, width = 8, height =6, dpi = 300)


#correlación PIB y participación laboral femenina 
cor(df_total$GDP, df_total$labourforce_female)
cor(df_total$labourforce_female, df_total$GDP)

ggplot(df_total, aes(x = GDP, y = labourforce_female)) + 
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df_model_cor, aes(x = GDP, y = labourforce_female)) + 
  geom_point() +
  geom_smooth(method = "lm")

str(df_total)
names(df_total)
scaled_data <- scale(df_total[,-c(1,29)])
scaled_data <- as.data.frame(scaled_data)




ggplot(scaled_data, aes(x = GDP, y = labourforce_female)) + 
  geom_point() +
  geom_smooth(method = "lm")


cor(df_highmedium_income$GDP, df_highmedium_income$labourforce_female)
cor(df_low_income$GDP, df_low_income$labourforce_female)


PIBparticipacion_rentaalta <- ggplot(df_highmedium_income, aes(x = GDP, y = labourforce_female)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")
PIBparticipacion_rentaalta
ggsave("visualization/Correlación PIB y participación países renta alta.jpg", plot = PIBparticipacion_rentaalta, width = 8, height =6, dpi = 300)


PIBparticipacion_rentabaja <- ggplot(df_low_income, aes(x = GDP, y = labourforce_female)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")
PIBparticipacion_rentabaja
ggsave("visualization/Correlación PIB y paritipación países renta baja.jpg", plot = PIBparticipacion_rentabaja, width = 8, height =6, dpi = 300)


#Análisis correlación PIB y migración

#correlación todos los países
ggplot(df_model_cor, aes(x = GDP, y = total_immigrants)) + 
  geom_point() +
  ggtitle("Correlación PIB y migración")+
  geom_smooth(method = "lm")

#correlaciones por grupos de países según PIB
PIBemigrantes_rentaalta <- ggplot(df_highmedium_income, aes(x = GDP, y = total_immigrants)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")
PIBemigrantes_rentaalta
ggsave("visualization/Correlación PIB y emigrantes países renta alta.jpg", plot = PIBemigrantes_rentaalta, width = 8, height =6, dpi = 300)



PIBemigrantes_rentabaja <- ggplot(df_low_income, aes(x = GDP, y = total_immigrants)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")
PIBemigrantes_rentabaja

ggsave("visualization/Correlación PIB y emigrantes países renta baja.jpg", plot = PIBemigrantes_rentabaja, width = 8, height =6, dpi = 300)



cor(df_highmedium_income$GDP, df_highmedium_income$total_immigrants)
cor(df_low_income$GDP, df_low_income$total_immigrants)


#Análisis correlación desigualdad leyes hereditarias y falta de acceso a la justicia

#correlación todos los países
ggplot(df_model_cor, aes(x = Inheritance_law, y = Access_to_justice)) + 
  geom_point() +
  ggtitle("Correlación Acceso a la Justicia y leyes hereditarias")+
  geom_smooth(method = "lm")

#correlaciones por grupos de países según PIB
ggplot(df_highmedium_income, aes(x = Inheritance_law, y = Access_to_justice)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")

ggplot(df_low_income, aes(x = Inheritance_law, y = Access_to_justice)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")

cor(df_highmedium_income$Inheritance_law, df_highmedium_income$Access_to_justice)
cor(df_low_income$Inheritance_law, df_low_income$Access_to_justice)



#Análisis correlación girls married y actitudes sobre violencia contra la mujer

#correlación todos los países
niñascasadas_normalizacon <- ggplot(df_model_cor, aes(x = Girls_married, y = Violence_against_women_attitudes)) + 
  geom_point() +
  ggtitle("Niñas casadas y normalización de la violencia de género")+
  geom_smooth(method = "lm")
niñascasadas_normalizacon
ggsave("visualization/Correlación niñas casadas y normalización violencia.jpg", plot = niñascasadas_normalizacon, width = 8, height =6, dpi = 300)


#correlaciones por grupos de países según PIB
niñascasadas_normalizacon_alta <- ggplot(df_highmedium_income, aes(x = Girls_married, y = Violence_against_women_attitudes)) + 
  geom_point() +
  ggtitle("Niñas casadas y normalización de la violencia de género - Países con mayor PIB")+
  geom_smooth(method = "lm")
niñascasadas_normalizacon_alta
ggsave("visualization/Correlación niñas casadas y normalización violencia países renta alta.jpg", plot = niñascasadas_normalizacon_alta, width = 8, height =6, dpi = 300)

niñascasadas_normalizacon_baja <- ggplot(df_low_income, aes(x = Girls_married, y = Violence_against_women_attitudes)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")
niñascasadas_normalizacon_baja
ggsave("visualization/Correlación niñas casadas y normalización violencia países renta baja.jpg", plot = niñascasadas_normalizacon_baja, width = 8, height =6, dpi = 300)


cor(df_highmedium_income$Girls_married, df_highmedium_income$Violence_against_women_attitudes)
cor(df_low_income$Girls_married, df_low_income$Violence_against_women_attitudes)




#Análisis correlación leyes hereditarias y porcentaje de emigrantes

#correlación todos los países
ggplot(df_model_cor, aes(x = Inheritance_law, y = female_percent_emigrants)) + 
  geom_point() +
  ggtitle("Leyes hereditarias y porcentaje de mujeres emigrantes")+
  geom_smooth(method = "lm")

ggplot(df_model_cor, aes(x = female_percent_emigrants, y = Inheritance_law)) + 
  geom_point() +
  ggtitle("Leyes hereditarias y porcentaje de mujeres emigrantes")+
  geom_smooth(method = "lm")


#correlaciones por grupos de países según PIB
ggplot(df_highmedium_income, aes(x = female_percent_emigrants, y = Inheritance_law)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")

ggplot(df_low_income, aes(x = female_percent_emigrants, y = Inheritance_law)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")

cor(df_highmedium_income$female_percent_emigrants, df_highmedium_income$Inheritance_law)
cor(df_low_income$female_percent_emigrants, df_low_income$Inheritance_law)


#Análisis correlación actitudes de normalización de la violencia contra la mujer y porcentaje de víctimas

#correlación todos los países

normalizacion_victimas <- ggplot(df_model_cor, aes(x = Violence_against_women_attitudes, y = percentage_violence)) + 
  geom_point() +
  ggtitle("Víctimas de violencia de género y normalización de la violencia física en pareja")+
  geom_smooth(method = "lm")
normalizacion_victimas

ggsave("visualization/Correlación normalización de la violencia de género y número de víctimas.jpg", plot = normalizacion_victimas, width = 8, height =6, dpi = 300)



#correlaciones por grupos de países según PIB
normalizacion_victimas_alta <- ggplot(df_highmedium_income, aes(x = Violence_against_women_attitudes, y = percentage_violence)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")

normalizacion_victimas_alta

ggsave("visualization/Correlación normalización de la violencia de género y víctimas países renta alta.jpg", plot = normalizacion_victimas_alta, width = 8, height =6, dpi = 300)


normalizacion_victimas_baja <- ggplot(df_low_income, aes(x = Violence_against_women_attitudes, y = percentage_violence)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")
normalizacion_victimas_baja
ggsave("visualization/Correlación normalización de la violencia de género y víctimas países renta baja.jpg", plot = normalizacion_victimas_baja, width = 8, height =6, dpi = 300)


cor(df_highmedium_income$Violence_against_women_attitudes, df_highmedium_income$percentage_violence)
cor(df_low_income$Violence_against_women_attitudes, df_low_income$percentage_violence)
