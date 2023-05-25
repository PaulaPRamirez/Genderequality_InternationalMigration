# Autora: Paula Pareja
# Fecha: Mayo, 2023

# Título: La relación entre género y migración a través del análisis de datos

# Trabajo Final de Máster
# Universidad Carlos III Madrid
# Tutores: Iñaki Ucar y Félix Vacas


########################################################
# 5. MODELOS
########################################################
########################################################


# 5.1 CLUSTERIZACIÓN
########################################################


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
df_totalnum <- df_total[,-c(1,29)]
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


library(tidyverse)
library(dplyr)

#lo mejoro con colores 

map_clusters <- map_world %>%
  left_join(unique(df_clusters), by="iso3") %>%
  mutate(cluster = factor(cluster, levels = c(1, 2), labels = c("Cluster 1", "Cluster 2")))

cluster_colors <- c("Cluster 1" = "#84D2F5", "Cluster 2" = "#3F6475")

ggplot(map_clusters) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=cluster)) +
  coord_map(xlim=c(-180, 180)) +
  scale_fill_manual(values=cluster_colors) +
  theme_map()


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
