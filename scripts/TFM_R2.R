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


df_economy <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Migración y desigualdad laboral.xlsx", sheet = 'Economy',col_names = TRUE)
head(df_economy)
df_economy <- df_economy[, -1]

df_gender <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Migración y desigualdad laboral.xlsx", sheet = 'Gender_violence')
head(df_gender)
df_gender <- df_gender[, -1]

df_migrants <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Migración y desigualdad laboral.xlsx", sheet = 'Migrants')
head(df_migrants)
df_migrants <- df_migrants[, -1]

df_total <- read_excel("C:/Users/Paula Pareja/Instalaciones/Desktop/TFM/R/Migración y desigualdad laboral.xlsx", sheet = 'Total')
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



########################################################
# 2. ANÁLISIS CORRELACIONAL
########################################################
########################################################

########################################################


#Vamos a hacer una matriz de correlación para poder visualizarlo en primer momento
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

ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(size = 3, colour = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1,1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap")


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

ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(size = 3, colour = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1,1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap")


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


ggplot(df_highmedium_income, aes(x = GDP, y = labourforce_female)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")

ggplot(df_low_income, aes(x = GDP, y = labourforce_female)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")


#Análisis correlación PIB y migración

#correlación todos los países
ggplot(df_model_cor, aes(x = GDP, y = total_immigrants)) + 
  geom_point() +
  ggtitle("Correlación PIB y migración")+
  geom_smooth(method = "lm")

#correlaciones por grupos de países según PIB
ggplot(df_highmedium_income, aes(x = GDP, y = total_immigrants)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")

ggplot(df_low_income, aes(x = GDP, y = total_immigrants)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")

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
ggplot(df_model_cor, aes(x = Girls_married, y = Violence_against_women_attitudes)) + 
  geom_point() +
  ggtitle("Niñas casadas y normalización de la violencia de género")+
  geom_smooth(method = "lm")

#correlaciones por grupos de países según PIB
ggplot(df_highmedium_income, aes(x = Girls_married, y = Violence_against_women_attitudes)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")

ggplot(df_low_income, aes(x = Girls_married, y = Violence_against_women_attitudes)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")

cor(df_highmedium_income$Girls_married, df_highmedium_income$Violence_against_women_attitudes)
cor(df_low_income$Girls_married, df_low_income$Violence_against_women_attitudes)




#Análisis correlación girls married y actitudes sobre violencia contra la mujer

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




#Análisis correlación girls married y actitudes sobre violencia contra la mujer

#correlación todos los países

ggplot(df_model_cor, aes(x = Violence_against_women_attitudes, y = percentage_violence)) + 
  geom_point() +
  ggtitle("Víctimas de violencia de género y normalización de la violencia física en pareja")+
  geom_smooth(method = "lm")


#correlaciones por grupos de países según PIB
ggplot(df_highmedium_income, aes(x = Violence_against_women_attitudes, y = percentage_violence)) + 
  geom_point() +
  ggtitle("Países con mayor PIB")+
  geom_smooth(method = "lm")

ggplot(df_low_income, aes(x = Violence_against_women_attitudes, y = percentage_violence)) + 
  geom_point() +
  ggtitle("Países con menor PIB")+
  geom_smooth(method = "lm")

cor(df_highmedium_income$Violence_against_women_attitudes, df_highmedium_income$percentage_violence)
cor(df_low_income$Violence_against_women_attitudes, df_low_income$percentage_violence)






########################################################
# 3. MODELOS
########################################################
########################################################

########################################################


########################################################
# 3.1 CLUSTERIZACIÓN

library(factoextra)

df_model
summary(df_model)


distance <- get_dist(df_model)
fviz_dist(distance)


fviz_nbclust(df_model,kmeans,method="wss") # Elbow
fviz_nbclust(df_model,kmeans,method="silhouette") # Silhouette

a <- kmeans(df_model , 2 , nstart=15)
a$cluster
a$size
a$centers
fviz_cluster(a,data=df_model)



df_total$cluster <- a$cluster

# Resumen por cluster
#aggregate(df_total,list(df_total$cluster),mean)
a$size



#Agrupo los datos por clusters y visualizo su diferente distribución

df_final <- df_model
df_final$country <- df_total$country
df_final$cluster <- df_total$cluster


df_model$cluster <- df_total$cluster

names(df_total)
df_boxplot <- df_model
str(df_boxplot)
df_boxplot <- apply(df_boxplot, 2, as.numeric)
df_boxplot <- as.data.frame(df_boxplot)


# Convertir los datos a formato largo (tidy)
df_boxplot_long <- tidyr::gather(df_boxplot, key = "variable", value = "valor", -cluster)

# Crear el boxplot con dos grupos (hue) según el cluster
ggplot(df_boxplot_long, aes(x = variable, y = valor, fill = factor(cluster))) + 
  geom_boxplot() +
  coord_flip()+
  labs(x = "Variable", y = "Valor", fill = "Cluster")



####################################################################
#Voy a probar una transformación logarítmica
names(df_total)
df_totalnum <- df_total[,-c(1,29,30)]
df_log <- log(df_totalnum)

df_log <- log(df_totalnum + 1)


distance <- get_dist(df_log)
fviz_dist(distance)


fviz_nbclust(df_log,kmeans,method="wss") # Elbow
fviz_nbclust(df_log,kmeans,method="silhouette") # Silhouette

a <- kmeans(df_log , 2 , nstart=15)
a$cluster
a$size
a$centers
fviz_cluster(a,data=df_log)



df_total$cluster <- a$cluster

# Resumen por cluster
#aggregate(df_total,list(df_log$cluster),mean)
a$size



#Agrupo los datos por clusters y visualizo su diferente distribución

df_final <- df_model
df_final$country <- df_total$country
df_final$cluster <- df_total$cluster
df_model$cluster <- df_total$cluster
df_log$cluster <- df_model$cluster

df_boxplot_long <- tidyr::gather(df_log, key = "variable", value = "valor", -cluster)


# Convertir los datos a formato largo (tidy)
df_boxplot_long <- tidyr::gather(df_log, key = "variable", value = "valor", -cluster)

df_boxplot_long <- df_boxplot_long %>% 
  mutate(cluster = factor(cluster, levels = c(1, 2), labels = c("Cluster 1", "Cluster 2")))
df_boxplot_long <- tidyr::gather(df_log, key = "variable", value = "valor", -cluster)


# Crear el boxplot con dos grupos (hue) según el cluster
ggplot(df_boxplot_long, aes(x = variable, y = valor, fill = factor(cluster))) + 
  geom_boxplot() +
  coord_flip()+
  labs(x = "Variable", y = "Valor", fill = "Cluster")



#Voy a analizar más a fondo los países según las variables de género

names(df_final)
names(df_log)
df_final_genero <- df_log[,c(15,16,17,18,19,20,21,22,23,24,25,26,27,28)]

names(df_final_genero)
str(df_final_genero)
df_final_genero <- apply(df_final_genero, 2, as.numeric)
df_final_genero <- as.data.frame(df_final_genero)


# Convertir los datos a formato largo (tidy)
df_final_genero <- tidyr::gather(df_final_genero, key = "variable", value = "valor", -cluster)

# Crear el boxplot con dos grupos (hue) según el cluster
ggplot(df_final_genero, aes(x = variable, y = valor, fill = factor(cluster))) + 
  geom_boxplot() +
  coord_flip()+
  labs(x = "Variable", y = "Valor", fill = "Cluster")


#vamos a visualizar los números y porcentajes reales para entender los resultados
names(df_model)

df_total_gen <- df_model[,c(15,16,17,18,19,20,21,22,23,24,25,26,27,28)]
str(df_total_gen)
df_total_gen <- apply(df_total_gen, 2, as.numeric)
df_total_gen <- as.data.frame(df_total_gen)


# Convertir los datos a formato largo (tidy)
df_total_gen <- tidyr::gather(df_total_gen, key = "variable", value = "valor", -cluster)

# Crear el boxplot con dos grupos (hue) según el cluster
ggplot(df_total_gen, aes(x = variable, y = valor, fill = factor(cluster))) + 
  geom_boxplot() +
  coord_flip()+
  labs(x = "Variable", y = "Valor", fill = "Cluster")



#Vemos los países de cada cluster 

# Filtrar países por cluster 1
cluster_1 <- subset(df_total, cluster == 1)$country
print(cluster_1)

# Filtrar países por cluster 2
cluster_2 <- subset(df_total, cluster == 2)$country
print(cluster_2)

library(xlsx)
write.xlsx(df_total, "Dataframe usado para la clusterización.xlsx")




########################################################
#Voy a hacer una representación en un mapa
########################################################

#install.packages("countrycode")
library(countrycode)
library(maps)

names(df_total)
df_clusters <- df_total[,c(1,30)]

df_clusters$iso3 <- countrycode(df_clusters$country, "country.name", "iso3c")

df_clusters$iso3

df_clusters[150,3] <- "TUR"
df_clusters[156,3] <- "USA"

library(maps)

# Cargar los datos geográficos necesarios
names(df_clusters)
summary(df_clusters)

# Get map data
map_world <- map_data("world")
map_world$iso3 <- countrycode(map_world$region, "country.name", "iso3c")


map_clusters <- merge(map_world, df_clusters, by = "iso3", all.x = TRUE)

map_clusters <- map_clusters %>% 
  mutate(cluster = factor(cluster, levels = c(1, 2), labels = c("Cluster 1", "Cluster 2")))

cluster_colors <- c("Cluster 1" = "#84D2F5", "Cluster 2" = "#3F6475")

#mapa sin color
ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group), fill = "grey80", color = "white") +
  coord_equal() +
  theme_void() +
  theme(panel.border = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

#mapa color por cluster
ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group), fill = "grey80", color = "white", linewidth = 0.05) +
  geom_point(data = map_clusters, aes(x = long, y = lat, color = cluster)) +
  scale_color_manual(values = cluster_colors) +
  coord_equal() +
  theme_void() +
  theme(panel.border = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
#Necesito mejorar el mapa


#vamos a borrar los df que no nos sirven para limpiar la consola 
rm(a)
rm(df_boxplot)
rm(df_boxplot_long)
rm(df_head10_migration)
rm(df_long)
rm(df_migrantsnum)
rm(df_model_cor)
rm(df_model_cor1)
rm(df_tail10_migration)
rm(df_total_gen)
rm(map_clusters)
rm(map_world)
rm(melted_cor)
rm(scaled_data)
rm(scaler)
rm(df_final)



########################################################
# 3.1 MODELO DE REGRESIÓN

##############################
# PREVENCIÓN DEL NÚMERO DE EMIGRANTES MUJERES

#Vamos a crear un modelo de regresión que nos permita predecir el número de mujeres migrantes


names(df_model)
df_reg <- df_model[,-c(1,2,3,4,5,6,8,9,10,14,27,28)]

df_reg$female_emigrants <- df_total$female_emigrants
names(df_reg)
summary(df_reg)


# División en conjuntos de entrenamiento y prueba (70% de entrenamiento, 30% de prueba)
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg), 0.7*nrow(df_reg))
train_data <- df_reg[train_index, ]
test_data <- df_reg[-train_index, ]

# Construcción del modelo de regresión lineal
model <- lm(female_emigrants ~ ., data = train_data)

# Evaluación del modelo
predictions <- predict(model, newdata = test_data) # Predicciones en el conjunto de prueba
rmse <- sqrt(mean((predictions - test_data$female_emigrants)^2)) # Error cuadrático medio
mae <- mean(abs(predictions - test_data$female_emigrants)) # Error absoluto medio
r_squared <- summary(model)$r.squared # Coeficiente de determinación

# Impresión de los resultados
cat("RMSE:", rmse, "\n") #RMSE: 1300394 
cat("MAE:", mae, "\n") #MAE: 675036.8
cat("R-squared:", r_squared, "\n") #R-squared: 0.310281

summary(model)
anova(model)

#Las variables que más influyen son el PIB y el número de inmigrantes
#Estas son las que tienen un F value mayor y un P value menor 


df_reg$predictions <- exp(df_reg$female_emigrants)
df_reg$y_exp <- exp(df_reg$female_emigrants)

allinfo <- data.frame(Realidad = test_data$female_emigrants, Prediccion = predictions)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)
summary(allinfo$Error)

#El modelo se llega a equivocar hasta en unos 6 millones de migrantes



# ---------------------------------------------------------------------------------------
#Vamos a probar a realizar una transformación logarítmica con la V.O. para ver si mejora
summary(df_total)
summary(df_log$female_emigrants)


#Vamos a usar la variable objetivo escalada
#Realizamos una reducción de dimensionalidad quitando las variables interrelacionadas 

names(df_model)
df_reg <- df_model[,-c(1,2,3,4,5,6,8,9,10,14,27,28)]

df_reg$female_emigrants <- df_model$female_emigrants
names(df_reg)
summary(df_reg)


# División en conjuntos de entrenamiento y prueba (70% de entrenamiento, 30% de prueba)
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg), 0.7*nrow(df_reg))
train_data <- df_reg[train_index, ]
test_data <- df_reg[-train_index, ]

# Construcción del modelo de regresión lineal
model <- lm(female_emigrants ~ ., data = train_data)

# Evaluación del modelo
predictions <- predict(model, newdata = test_data) # Predicciones en el conjunto de prueba
rmse <- sqrt(mean((predictions - test_data$female_emigrants)^2)) # Error cuadrático medio
mae <- mean(abs(predictions - test_data$female_emigrants)) # Error absoluto medio
r_squared <- summary(model)$r.squared # Coeficiente de determinación

# Impresión de los resultados
cat("RMSE:", rmse, "\n") #RMSE: 0.2143167  
cat("MAE:", mae, "\n") #MAE: 0.1112522 
cat("R-squared:", r_squared, "\n") #R-squared: 0.310281 
summary(model)
anova(model)

df_reg$predictions <- exp(df_reg$female_emigrants)
df_reg$y_exp <- exp(df_reg$female_emigrants)

allinfo <- data.frame(Realidad = test_data$female_emigrants, Prediccion = predictions)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)

allinfo$realidad_exp <- exp(allinfo$Realidad)
allinfo$pred_exp <- exp(allinfo$Prediccion)
allinfo$Error_real <- allinfo$realidad_exp - allinfo$pred_exp
head(allinfo, 6)
summary(allinfo$Error_real)
#El modelo se llega a equivocar hasta en unos 5.9 millones de migrantes, lo cual son resultados pésimos

df_reg <- df_reg[,-c(18,19)]


# ---------------------------------------------------------------------------------------

#Seguimos probando con un arbol de regresión a ver si mejora


names(df_model)
df_reg <- df_model[,-c(1,2,3,4,5,6,8,9,10,14,27,28)]

df_reg$female_emigrants <- df_model$female_emigrants
names(df_reg)
summary(df_reg)


# Cargamos las librerías necesarias
library(rpart)
#install.packages(("rpart.plot"))
library(rpart.plot)
library(caret)

# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg), 0.7*nrow(df_reg))
train_data <- df_reg[train_index, ]
test_data <- df_reg[-train_index, ]


#creamos el modelo
modelo <- rpart(female_emigrants ~ ., data = train_data, control = rpart.control(maxdepth = 5))

#visualizamos el arbol
rpart.plot(modelo, extra=1, cex=0.6)


# Realizamos las predicciones sobre los datos de prueba
predicciones <- predict(modelo, newdata=test_data)

# Evaluamos el modelo
mse <- mean((test_data$female_emigrants - predicciones)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_emigrants, predicciones)^2

# Imprimimos los resultados
print(paste(mse)) #0.0262216773476279"
print(paste(rmse)) #0.161931088267905"
print(paste(r2)) #0.405664037502967"

summary(modelo)

allinfo <- data.frame(Realidad = test_data$female_emigrants, Prediccion = predicciones)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)
summary(allinfo)




###############################
###############################
#pruebo con la variable total solo de género


names(df_model)

df_reg <- df_model[,c(7,11,12,13,27)]
df_reg$female_emigrants <- df_model$female_emigrants
names(df_reg)
summary(df_reg)


# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg), 0.7*nrow(df_reg))
train_data <- df_reg[train_index, ]
test_data <- df_reg[-train_index, ]

#entrenamos el modelo
modelo <- rpart(female_emigrants ~ ., data = train_data, control = rpart.control(maxdepth = 5))

#visualizamos el arbol
rpart.plot(modelo, extra=1, cex=0.6)

# Realizamos las predicciones sobre los datos de prueba
predicciones <- predict(modelo, newdata=test_data)

# Evaluamos el modelo
mse <- mean((test_data$female_emigrants - predicciones)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_emigrants, predicciones)^2 #coeficiente de determinación

print(paste(mse)) #0.0262216773476279"
print(paste(rmse)) #0.161931088267905"
print(paste(r2)) #0.405664037502967"


allinfo <- data.frame(Realidad = test_data$female_emigrants, Prediccion = predicciones)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)

summary(allinfo)
#Los resultados son prácticamente iguales 


# ---------------------------------------------------------------------------------------

#Vamos a probar con un random forest


names(df_model)
df_reg <- df_model[,-c(1,2,3,4,5,6,8,9,10,14,27,28)]

df_reg$female_emigrants <- df_log$female_emigrants
names(df_reg)
summary(df_reg)


library(randomForest)

# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg), 0.7*nrow(df_reg))
train_data <- df_reg[train_index, ]
test_data <- df_reg[-train_index, ]

set.seed(123)
modelo_rf <- randomForest(female_emigrants ~ ., data = train_data, ntree = 500, mtry = 2)

prediccion_rf <- predict(modelo_rf, newdata = test_data)


# Evaluamos el modelo
mse <- mean((test_data$female_emigrants - prediccion_rf)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_emigrants, prediccion_rf)^2 #coeficiente de determinación

print(paste(mse)) #2.12637738945333
print(paste(rmse)) #1.45821033786396
print(paste(r2)) #0.485007312903761

#El modelo ha mejorado mucho pero aún así sigue teniendo un coeficiente de determinación muy bajo

allinfo <- data.frame(Realidad = test_data$female_emigrants, Prediccion = prediccion_rf)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)

allinfo$realidad_exp <- exp(allinfo$Realidad)
allinfo$pred_exp <- exp(allinfo$Prediccion)
allinfo$Error_real <- allinfo$realidad_exp - allinfo$pred_exp
head(allinfo, 6)
summary(allinfo$Error_real)
#El modelo se llega a equivocar hasta en unos 5.5 millones de migrantes, lo cual son resultados pésimos
#De media se equivoca en unos 146.586  migrantes

# Obtenemos la importancia de las variables predictoras
importancia <- importance(modelo_rf)

# Imprimimos la importancia de cada variable
df_importancia <- data.frame(Variable = names(df_reg[,-17]), importancia = importancia)
str(df_importancia)
df_importancia$importancia <- df_importancia$IncNodePurity
df_importancia$importancia <- as.numeric(df_importancia$importancia)


ggplot(df_importancia, aes(x = reorder(Variable, -importancia), y = importancia)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Variable Importance", x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##############################################################################

#Por último voy a realizar el modelo de random forest solo con la variable total de género
#Usamos la variable objetivo y las independientes escaladas
library(randomForest)

names(df_model)

df_reg_final <- df_model[,c(7,11,12,13,27)]
df_reg_final$female_emigrants <- df_model$female_emigrants
names(df_reg_final)
summary(df_reg_final)


# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg_final), 0.7*nrow(df_reg_final))
train_data <- df_reg_final[train_index, ]
test_data <- df_reg_final[-train_index, ]

modelo_rf1 <- randomForest(female_emigrants ~ ., data = train_data, ntree = 500, mtry = 2)
prediccion_rf <- predict(modelo_rf1, newdata = test_data)


# Evaluamos el modelo
mse <- mean((test_data$female_emigrants - prediccion_rf)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_emigrants, prediccion_rf)^2 #coeficiente de determinación

print(paste(mse)) #0.0234623139269698
print(paste(rmse)) #0.153174129431082
print(paste(r2)) #0.520248675904098


allinfo <- data.frame(Realidad = test_data$female_emigrants, Prediccion = prediccion_rf)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)

# Obtenemos la importancia de las variables predictoras
importancia <- importance(modelo_rf1)
names(df_reg_final)

# Imprimimos la importancia de cada variable
df_importancia <- data.frame(Variable = names(df_reg_final[,-6]), importancia = importancia)
str(df_importancia)
df_importancia$importancia <- df_importancia$IncNodePurity
df_importancia$importancia <- as.numeric(df_importancia$importancia)

ggplot(df_importancia, aes(x = reorder(Variable, -importancia), y = importancia)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Importancia de las variables", x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#################################
#################################
# PREVENCIÓN DEL NÚMERO DE INMIGRANTES MUJERES



names(df_model)
df_reg <- df_log[,-c(1,3,4,5,6,7,8,9,10,14,27,28)]

df_reg$female_immigrants <- df_log$female_immigrants
names(df_reg)
summary(df_reg)


# Cargamos las librerías necesarias
library(rpart)
#install.packages(("rpart.plot"))
library(rpart.plot)
library(caret)

# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg), 0.7*nrow(df_reg))
train_data <- df_reg[train_index, ]
test_data <- df_reg[-train_index, ]


#creamos el modelo
modelo <- rpart(female_immigrants ~ ., data = train_data, control = rpart.control(maxdepth = 5))

#visualizamos el arbol
rpart.plot(modelo, extra=1, cex=0.55)


# Realizamos las predicciones sobre los datos de prueba
predicciones <- predict(modelo, newdata=test_data)

# Evaluamos el modelo
mse <- mean((test_data$female_immigrants - predicciones)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_immigrants, predicciones)^2

# Imprimimos los resultados
print(paste(mse)) #2.21334528979242"
print(paste(rmse)) #1.48773159198574"
print(paste(r2)) #0.523921876874912"

summary(modelo)
#Las variables más influyentes son el PIB, el número de mujeres emigrantes, 
#las actitudes de normalización de la violencia de género, las remesas, 
#el porcentaje de niñas casadas y el porcentaje de mujeres en el parlamento

allinfo <- data.frame(Realidad = test_data$female_immigrants, Prediccion = prediccion_rf)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)

allinfo$realidad_exp <- exp(allinfo$Realidad)
allinfo$pred_exp <- exp(allinfo$Prediccion)
allinfo$Error_real <- allinfo$realidad_exp - allinfo$pred_exp
head(allinfo, 6)
summary(allinfo$Error_real)
#El modelo se llega a equivocar hasta en unos 5.1 millones de migrantes, lo cual son resultados pésimos
#De media se equivoca en unos 4,7 mil  migrantes



# --------------------------------------------------------------------
#RANDOM FOREST


library(randomForest)

names(df_model)

names(df_model)
df_reg <- df_log[,-c(1,3,4,5,6,7,8,9,10,14,27,28)]

df_reg$female_immigrants <- df_log$female_immigrants
names(df_reg)
summary(df_reg)


# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg), 0.7*nrow(df_reg))
train_data <- df_reg[train_index, ]
test_data <- df_reg[-train_index, ]

modelo_rf1 <- randomForest(female_immigrants ~ ., data = train_data, ntree = 500, mtry = 2)
prediccion_rf <- predict(modelo_rf1, newdata = test_data)


# Evaluamos el modelo
mse <- mean((test_data$female_immigrants - prediccion_rf)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_immigrants, prediccion_rf)^2 #coeficiente de determinación

print(paste(mse)) #2.11540347202318
print(paste(rmse)) #1.45444266714889
print(paste(r2)) #0.65349160718253


allinfo <- data.frame(Realidad = test_data$female_immigrants, Prediccion = prediccion_rf)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)


allinfo$realidad_exp <- exp(allinfo$Realidad)
allinfo$pred_exp <- exp(allinfo$Prediccion)
allinfo$Error_real <- allinfo$realidad_exp - allinfo$pred_exp
head(allinfo, 6)
summary(allinfo$Error_real)
#El modelo se llega a equivocar hasta en unos 5.5 millones de migrantes, lo cual son resultados pésimos
#De media se equivoca en unos 2,7 mil  migrantes



# Obtenemos la importancia de las variables predictoras
importancia <- importance(modelo_rf1)
names(df_reg)

# Imprimimos la importancia de cada variable
df_importancia <- data.frame(Variable = names(df_reg[,-17]), importancia = importancia)
str(df_importancia)
df_importancia$importancia <- df_importancia$IncNodePurity
df_importancia$importancia <- as.numeric(df_importancia$importancia)

ggplot(df_importancia, aes(x = reorder(Variable, -importancia), y = importancia)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Importancia de las variables", x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------
#Solo con la variable total de género

names(df_model)
df_reg_final <- df_model[,c(2,11,12,13,27)]
df_reg_final$female_immigrants <- df_log$female_immigrants
names(df_reg_final)
summary(df_reg_final)


# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg_final), 0.7*nrow(df_reg_final))
train_data <- df_reg_final[train_index, ]
test_data <- df_reg_final[-train_index, ]

modelo_rf1 <- randomForest(female_immigrants ~ ., data = train_data, ntree = 500, mtry = 2)
prediccion_rf <- predict(modelo_rf1, newdata = test_data)


# Evaluamos el modelo
mse <- mean((test_data$female_immigrants - prediccion_rf)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_immigrants, prediccion_rf)^2 #coeficiente de determinación

print(paste(mse)) #1.67308341439854
print(paste(rmse)) #1.29347725700862
print(paste(r2)) #0.606023549252731

#El modelo ha mejorado mucho, con un coeficiente del 0.6

allinfo <- data.frame(Realidad = test_data$female_immigrants, Prediccion = prediccion_rf)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)

allinfo$realidad_exp <- exp(allinfo$Realidad)
allinfo$pred_exp <- exp(allinfo$Prediccion)
allinfo$Error_real <- allinfo$realidad_exp - allinfo$pred_exp
head(allinfo, 6)
summary(allinfo$Error_real)
#El modelo se llega a equivocar hasta en unos 5,1 millones de migrantes, lo cual son resultados pésimos
#De media se equivoca en unos 4,7 mil  migrantes


# Obtenemos la importancia de las variables predictoras
importancia <- importance(modelo_rf1)
names(df_reg_final)

# Imprimimos la importancia de cada variable
df_importancia <- data.frame(Variable = names(df_reg_final[,-6]), importancia = importancia)
str(df_importancia)
df_importancia$importancia <- df_importancia$IncNodePurity
df_importancia$importancia <- as.numeric(df_importancia$importancia)


ggplot(df_importancia, aes(x = reorder(Variable, -importancia), y = importancia)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Importancia de las variables", x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#############################################################################
#############################################################################
#############################################################################
# Quisimos ir un paso más allá y es precedir el porecentaje de migrantes mujeres de un país

#############################################################################
# PREDICCIÓN PORCENTAJE EMIGRANTES MUJERES

names(df_model)

df_reg_porcentaje <- df_model[,c(5,10,11,12,13,27)]
names(df_reg_porcentaje)
summary(df_reg_porcentaje)


# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg_porcentaje), 0.7*nrow(df_reg_final))
train_data <- df_reg_porcentaje[train_index, ]
test_data <- df_reg_porcentaje[-train_index, ]

modelo_rf1 <- randomForest(female_percent_immigrants ~ ., data = train_data, ntree = 500, mtry = 2)
prediccion_rf <- predict(modelo_rf1, newdata = test_data)


# Evaluamos el modelo
mse <- mean((test_data$female_percent_immigrants - prediccion_rf)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_percent_immigrants, prediccion_rf)^2 #coeficiente de determinación

print(paste(mse)) #0.00914200088403641
print(paste(rmse)) #0.0956138111573658
print(paste(r2)) #0.220465541715833



#########################
#Los resultados son penosos vamos a probar con la transformación logarítmica
names(df_log)

df_reg_porcentaje <- df_log[,c(5,10,11,12,13,27)]
names(df_reg_porcentaje)
summary(df_reg_porcentaje)


# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg_porcentaje), 0.7*nrow(df_reg_final))
train_data <- df_reg_porcentaje[train_index, ]
test_data <- df_reg_porcentaje[-train_index, ]

modelo_rf1 <- randomForest(female_percent_immigrants ~ ., data = train_data, ntree = 500, mtry = 2)
prediccion_rf <- predict(modelo_rf1, newdata = test_data)


# Evaluamos el modelo
mse <- mean((test_data$female_percent_immigrants - prediccion_rf)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_percent_immigrants, prediccion_rf)^2 #coeficiente de determinación

print(paste(mse)) #0.0208306616594701
print(paste(rmse)) #0.144328312050928
print(paste(r2)) #0.203945806901831

#Aún peores



###################################################
#Por ultimo vamos a probar con todas las variables 


names(df_model)
summary(df_model)

df_reg_porcentaje <- df_model[,c(5,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)]
names(df_reg_porcentaje)
summary(df_reg_porcentaje)



# Dividimos los datos en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train_index <- sample(1:nrow(df_reg_porcentaje), 0.7*nrow(df_reg))
train_data <- df_reg_porcentaje[train_index, ]
test_data <- df_reg_porcentaje[-train_index, ]

set.seed(123)
modelo_rf <- randomForest(female_percent_emigrants ~ ., data = train_data, ntree = 500, mtry = 2)

prediccion_rf <- predict(modelo_rf, newdata = test_data)

# Evaluamos el modelo
mse <- mean((test_data$female_percent_emigrants - prediccion_rf)^2)
rmse <- sqrt(mse)
r2 <- cor(test_data$female_percent_emigrants, prediccion_rf)^2 #coeficiente de determinación

print(paste(mse)) #0.0325633677022144
print(paste(rmse)) #0.180453228572432
print(paste(r2)) #0.304802554170936

#El modelo ha mejorado mucho pero aún así sigue teniendo un coeficiente de determinación muy bajo

allinfo <- data.frame(Realidad = test_data$female_percent_emigrants, Prediccion = prediccion_rf)
allinfo$Error <- allinfo$Realidad - allinfo$Prediccion
head(allinfo, 6)

allinfo$realidad_exp <- exp(allinfo$Realidad)
allinfo$pred_exp <- exp(allinfo$Prediccion)
allinfo$Error_real <- allinfo$realidad_exp - allinfo$pred_exp
head(allinfo, 6)
summary(allinfo$Error_real)
#El modelo se llega a equivocar hasta en unos 5.5 millones de migrantes, lo cual son resultados pésimos
#De media se equivoca en unos 146.586  migrantes

# Obtenemos la importancia de las variables predictoras
importancia <- importance(modelo_rf)

# Imprimimos la importancia de cada variable
df_importancia <- data.frame(Variable = names(df_reg_porcentaje[,-1]), importancia = importancia)
str(df_importancia)
df_importancia$importancia <- df_importancia$IncNodePurity
df_importancia$importancia <- as.numeric(df_importancia$importancia)


ggplot(df_importancia, aes(x = Variable, y = importancia)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Variable Importance", x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






