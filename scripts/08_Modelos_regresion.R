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


# 5.2 MODELO DE REGRESIÓN
########################################################

# --------------------------------------------------------------------------------
# PREDICCIÓN DEL NÚMERO DE EMIGRANTES MUJERES

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





# ------------------------------------------------------------------
# PREDICCIÓN DEL NÚMERO DE INMIGRANTES MUJERES



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



