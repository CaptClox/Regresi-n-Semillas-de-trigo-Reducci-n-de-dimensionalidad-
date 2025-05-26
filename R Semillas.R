install.packages("nnet")
library(nnet)
library(tidyr)
library(ggplot2)
library(EFAtools)
str(datos$clase)
datos$clase = as.factor(datos$clase)
datos <- read.table("C:/Users/pablo/Downloads/seeds_dataset.txt", 
                   header = FALSE,      # TRUE si tiene encabezados
                   sep = "")           # "" = separador por defecto (espacio en blanco)

datos_data= datos[, 1:7]
head(datos_data)

colnames(datos)= c("area", "perimetro", "compacidad", "longGrano",
                   "anchoGrano", "coefiAsimetria", "longSurcoGrano", "clase")
colnames(datos_data)= c("area", "perimetro", "compacidad", "longGrano",
                        "anchoGrano", "coefiAsimetria", "longSurcoGrano")

corr= cor(datos_data[,c("area", "perimetro", "compacidad", "longGrano",
                        "anchoGrano", "coefiAsimetria", "longSurcoGrano")], use="complete")
corr


BARTLETT(corr,N=nrow(datos_data))
KMO(corr)


corrplot(corr, method = "color", type = "upper",
         tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)


set.seed(123)  # Para reproducibilidad
n <- nrow(datos)
indices <- sample(1:n, size = 0.7 * n)  # 70% train

# División en train y test
train_datos <- datos[indices, ]
test_datos <- datos[-indices, ]

train_data <- datos_data[indices, ]
test_data <- datos_data[-indices, ]

res.pca <- PCA(train_data, scale.unit = TRUE, ncp = 7, graph = FALSE)
summary(res.pca)

coord_pca <- as.data.frame(res.pca$ind$coord)
cor(coord_pca)
pairs(coord_pca[, 1:3], main = "Componentes principales", pch = 19, col = '#1e8449')

# Coordenadas del train
coord_train <- as.data.frame(res.pca$ind$coord)

# Agregar clase
coord_train$clase <- train_datos$clase

# Proyectar test sobre el ACP calculado con el train
coord_test <- as.data.frame(predict(res.pca, newdata = test_data)$coord)

# Agregar clase real
coord_test$clase <- test_datos$clase

# Gráfico de individuos (atletas)
fviz_pca_ind(res.pca, 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Gráfico de variables (disciplinas)
fviz_pca_var(res.pca, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

coord_pca <- as.data.frame(res.pca$ind$coord)
coord_pca$clase <- datos$clase

modelo <- multinom(clase ~ Dim.1 + Dim.2, data = coord_train)
summary(modelo)

log(P(clase=2)/P(clase=1)) = -38.54 + 29.66*Dim.1 + 35.99*Dim.2


modelo2 = multinom(clase ~ Dim.1 + Dim.2 + Dim.3, data = coord_train)
summary(modelo2)

# Modelo 1
aic1 <- AIC(modelo)
bic1 <- BIC(modelo)
loglik1 <- logLik(modelo)

# Modelo 2
aic2 <- AIC(modelo2)
bic2 <- BIC(modelo2)
loglik2 <- logLik(modelo2)

# Mostrar resultados
cat("Modelo 1 (2 dimensiones):\n")
cat("  AIC:", aic1, "\n")
cat("  BIC:", bic1, "\n")
cat("  LogLik:", loglik1, "\n\n")

cat("Modelo 2 (3 dimensiones):\n")
cat("  AIC:", aic2, "\n")
cat("  BIC:", bic2, "\n")
cat("  LogLik:", loglik2, "\n")

odds_ratios <- exp(coef(modelo))
print(odds_ratios)
#Hacer predicciones sobre el conjunto de prueba
predicciones <- predict(modelo, newdata = coord_test)
# Evaluar desempeño (matriz de confusión)
table(Predicho = predicciones, Real = coord_test$clase)
# calcular la precisión general:
mean(predicciones == coord_test$clase)




# Creamos la matriz de confusión
conf_mat <- table(Predicho = predicciones, Real = coord_test$clase)

# Convertir a formato largo para ggplot
conf_df <- melt(conf_mat)

# Graficar
ggplot(conf_df, aes(x = Real, y = Predicho)) +
  geom_tile(aes(fill = value), color = "white") +
  geom_text(aes(label = value), size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Matriz de Confusión", x = "Clase Real", y = "Clase Predicha") +
  theme_minimal()




# Boxplot original
c("area", "perimetro", "compacidad", "longGrano",
  "anchoGrano", "coefiAsimetria", "longSurcoGrano", "clase")
library(tidyr)
library(ggplot2)

# Suponiendo que todas excepto "clase" son numéricas
datos_long <- pivot_longer(datos,
                           cols = -clase, # o cols = everything() si no querés agrupar por clase
                           names_to = "Variable",
                           values_to = "Valor")
ggplot(datos_long, aes(x = Variable, y = Valor)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Boxplot de variables",
       x = "Variable",
       y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datos_long, aes(x = Variable, y = Valor, fill = clase)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot por clase",
       x = "Variable",
       y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
