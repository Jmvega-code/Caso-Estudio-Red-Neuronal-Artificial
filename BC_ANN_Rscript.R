#################################
## Redes neuronales Artificiales 
#################################

# Cargar librerias
library("class")
library("knitr")
library("neuralnet")
library("NeuralNetTools")
library("nnet")
library("caret")
library("e1071")
library("kableExtra")

# Tabla de Fortalezas y Debilidades
Fortalezas<-c("Adaptabilidad a problemas de clasificación o predicción numérica",
              "Capacidad de modelar patrones complejos",
              "Localiza relaciones subyacentes")
Debilidades<-c("Lento de entrenar",
               "Propenso a sobreajustar los datos de entrenamiento",
               "Difícil de interpretar")
Table_Info1<-as.data.frame(cbind(Fortalezas,Debilidades))
kable(Table_Info1)

# Exploracion y preparacion de datos
breCan <- read.csv("BreastCancer2.csv")
str(breCan)

set.seed(12345)
shuffle <- sample(nrow(breCan),nrow(breCan))
breCan <- breCan[shuffle,]

# Nos deshacemos del factor para transformarlo
BCan <- breCan[,-10]
# Cambiamos al factor por dos valores B y M cada uno con los valores posibles 0 y 1
mm <- model.matrix(~Class-1, breCan)
head(mm)
colnames(mm) <- c("B","M")

# Normalizamos los datos con la función `lapply()`
BCan_norm <- as.data.frame(lapply(BCan, normalizar))
# Unimos los resultados de ambas transformaciones en una nueva matriz
breCan_norm <- cbind(BCan_norm,mm)
# Comprobamos que todas las variables están dentro de los rangos deseados
str(breCan_norm)

brca_entreno <- breCan_norm[1:455,]
brca_prueba <- breCan_norm[456:683,]
head(brca_entreno)

set.seed(123)
BC_model <- neuralnet( M + B ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, data = brca_entreno, hidden = 1)
plot(BC_model)

resultado_modelo <- compute(BC_model, brca_prueba[1:9])
# De los dos componentes que retorna la función compute() utilizamos $net.result que almacena los valores previstos
breastcancer_prevision <- resultado_modelo$net.result

# Con la funcion cor() vemos la correlación entre dos vectores numéricos.
cor(breastcancer_prevision,brca_prueba$M )

cor(breastcancer_prevision,brca_prueba$B )

set.seed(123)
BC_model2 <- neuralnet( M + B ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, data = brca_entreno, hidden = 3)
plot(BC_model2)

resultado_modelo2 <- compute(BC_model2, brca_prueba[1:9])
# De los dos componentes que retorna la función compute() utilizamos $net.result que almacena los valores previstos
breastcancer_prevision2 <- resultado_modelo2$net.result

# Con la funcion cor() vemos la correlación entre dos vectores numéricos.
cor(breastcancer_prevision2,brca_prueba$M )

cor(breastcancer_prevision2,brca_prueba$B )




