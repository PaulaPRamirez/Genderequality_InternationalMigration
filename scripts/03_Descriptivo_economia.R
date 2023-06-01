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

# ANÁLISIS ECONÓMICO Y DE IGUALDAD DE GÉNERO
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
EEUU_China <- ggplot(df_largo, aes(x = country, y = valor)) +
  geom_bar(stat = "identity", color = "darkblue", fill = "lightblue", width = 0.7) +
  facet_wrap(~variable, scales = "free_y", nrow = 2, labeller = labeller(variable = c("GDP" = "PIB", "labourforce_female" = "Participación laboral F", "labourforce_male" = "Participación laboral M", "Remittances" = "Remesas"))) +
  theme_minimal() +
  labs(x = "País", y = "Valor") +
  ggtitle("Análisis comparativo Estados Unidos y China") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major.y = element_line(color = "lightblue", linetype = "dashed"),
        strip.text = element_text(size = 12))
EEUU_China

ggsave("visualization/Gráfica comparativa EEUU - China.png", plot = EEUU_China, width = 10, height = 6, dpi = 300)



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


PIB_alto <- ggplot(data=df_outliers, aes(x=reorder(country, GDP), y=GDP)) +
  geom_bar(stat = "identity", aes(fill=GDP)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = median(df_economy$GDP), size=1, color="blue") +
  scale_fill_gradient(name="PIB", low = "#84D2F5", high = "#3F6475") +
  labs(title="PIB por país", x="", y="PIB")
PIB_alto
ggsave("visualization/PIB por países de renta alta.png", plot = PIB_alto, width = 6, height = 10, dpi = 300)


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


PIB_medio <- ggplot(data=df_other, aes(x=reorder(country, GDP), y=GDP)) +
  geom_bar(stat = "identity", aes(fill=GDP)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = median(df_economy$GDP), size=1, color="blue") +
  scale_fill_gradient(name="PIB", low = "#84D2F5", high = "#3F6475") +
  labs(title="PIB por país", x="", y="PIB") + 
  theme(axis.text.y = element_text(size=5))
PIB_medio
ggsave("visualization/PIB por países de renta media-baja.png", plot = PIB_medio, width = 6, height = 10, dpi = 300)


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

Desigualdad_genero_trabajo <- ggplot(data=df_economy, aes(x=reorder(country, diferencia_genero), y=diferencia_genero)) +
  geom_bar(stat = "identity", aes(fill=diferencia_genero)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = median(df_economy$diferencia_genero), size=1, color="blue") +
  scale_fill_gradient(name="Diferencia", low = "#84D2F5", high = "#3F6475") +
  labs(title="Desigualdad de género en el trabajo", x="", y="PIB") + 
  theme(axis.text.y = element_text(size=5))
Desigualdad_genero_trabajo

ggsave("visualization/Desigualdad de género en el trabajo.png", plot = Desigualdad_genero_trabajo, width = 6, height =10, dpi = 300)



summary(df_economy$diferencia_genero)
#  Min.  1st Qu.  Median  Mean   3rd Qu.    Max. 
#-83.93  -34.81  -18.29  -25.00  -10.60   10.83 

df_tail10_gdp <- df_economy %>% 
  arrange(diferencia_genero) %>% 
  tail(10)


summary(df_economy$GDP)
df_economy$income_group <- ifelse(df_economy$GDP > 7.013691e+11, "high_income",
                                  ifelse(df_economy$GDP > 4.670e+10, "medium_income", "low_income"))

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

ggsave("visualization/Comparación participación laboral y renta.jpg", plot = p2, width = 10, height =6, dpi = 300)



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


Participacionlaboral_grupo <- df_economy %>%
  ggplot( aes(x=income_group, y=labourforce_female, fill=income_group)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.2, option="A") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Participación laboral femenina por grupo económico") +
  xlab("")
Participacionlaboral_grupo

ggsave("visualization/Comparación participación laboral y renta_boxplots.jpg", plot = Participacionlaboral_grupo, width = 10, height =6, dpi = 300)





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
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)
print(p3)




