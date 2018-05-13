library(arules)
library(AA.IDT)
library(dplyr)
library(ggplot2)
library(arc)

colnames(TelcoChurn)
str(TelcoChurn)
grep("TotalCharges", colnames(TelcoChurn))
grep("Churn", colnames(TelcoChurn))
TelcoChurn[,19]
discrNumeric(TelcoChurn, "Churn")

#columnas = vector de columnas continuas
#estrategia "igual_ancho" o "igual_cantidad"
discret <- function (dataset, columnas, estrategia, bins){
  for (columna in columnas) {
    if (estrategia == "igual_ancho"){
      dataset[columna] <- discretize(dataset[,columna], "interval", breaks = bins)
    }
    else{
      dataset[columna] <- discretize(dataset[,columna], "frequency", breaks = bins)
    }

  }

  return(dataset)
}

# ----------------------------------------------------
#cargo el dataset completo, porque hay que discretizar antes de dividir en test-validacion
TelcoChurn <- read.csv('Telcochurn.csv')

breaks.vec = seq(1, 20, 1)
ConfidenceFactor.Vector = seq(0.05, 0.5, by = 0.05)
results = data.frame()
trn_size = 0.8
columnas = c("MonthlyCharges", "TotalCharges")

for (method in c("igual_ancho", "igual_freq")) {
  for (brk in breaks.vec) {
    data = discret(TelcoChurn, columnas, method, brk)

    #separo train de validacion
    set.seed(2018)
    train <- sample_frac(data, trn_size)
    sid <- as.numeric(rownames(train)) # because rownames() returns character
    validation <- data[-sid,]

    # ejecuto las corridas en base al CF
    results.cf = execute.J48(ConfidenceFactor.Vector,
                             0,
                             train,
                             validation)

    results = rbind(results,
                    cbind(results.cf, Bins = brk, Method = method))
  }
}

#corrida supervisada
data <- discrNumeric(TelcoChurn, "Churn")
#separo train de validacion
set.seed(2018)
train <- sample_frac(data$Disc.data, trn_size)
sid <- as.numeric(rownames(train)) # because rownames() returns character
validation <- data$Disc.data[-sid,]

# ejecuto las corridas en base al CF
results.cf = execute.J48(ConfidenceFactor.Vector,
                         0,
                         train,
                         validation)

results = rbind(results,
                cbind(results.cf, Bins = 0, Method = "supervisado"))
}
levels(data$Disc.data$MonthlyCharges)

data.igual.ancho <- results[results$Method == "igual_ancho",]
data.igual.freq <- results[results$Method == "igual_freq",]
data.supervisado <- results[results$Method == "supervisado",]
# ------------------------------------------------- TAMAÑO HOJAS -------------------------------------------------


ggplot(data = data.igual.ancho, aes(x = Bins, y = NumberOfLeaves, colour = factor(ConfidenceFactor))) +
geom_line(size = 1) + geom_point(shape = 16, size = 1.5) +
scale_x_continuous(breaks = data.igual.ancho$Bins) +
labs(color="CF") +
ggtitle("Cant Hojas | Bins = Igual Ancho") +
theme(plot.title = element_text(hjust = 0.5))
ggsave("7.1.1- tamaño_hojas_igual_ancho.png", dpi=400, height=6, width=12)



ggplot(data = data.igual.freq, aes(x = Bins, y = NumberOfLeaves, colour = factor(ConfidenceFactor))) +
  geom_line(size = 1) + geom_point(shape = 16, size = 1.5) +
  scale_x_continuous(breaks = data.igual.freq$Bins) +
  labs(color="CF") +
  ggtitle("Cant Hojas | Bins = Igual Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("7.1.2- tamaño_hojas_igual_freq.png", dpi=400, height=6, width=12)

# ------------------------------------------------- TAMAÑO NODOS -------------------------------------------------


ggplot(data = data.igual.ancho, aes(x = Bins, y = NumberOfNodes, colour = factor(ConfidenceFactor))) +
  geom_line(size = 1) + geom_point(shape = 16, size = 1.5) +
  scale_x_continuous(breaks = data.igual.ancho$Bins) +
  labs(color="CF") +
  ggtitle("Cant Nodos | Bins = Igual Ancho") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("7.1.3- tamaño_nodos_igual_ancho.png", dpi=400, height=6, width=12)



ggplot(data = data.igual.freq, aes(x = Bins, y = NumberOfNodes, colour = factor(ConfidenceFactor))) +
  geom_line(size = 1) + geom_point(shape = 16, size = 1.5) +
  scale_x_continuous(breaks = data.igual.freq$Bins) +
  labs(color="CF") +
  ggtitle("Cant Nodos | Bins = Igual Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("7.1.4- tamaño_nodos_igual_freq.png", dpi=400, height=6, width=12)


# ------------------------------------------------- VALIDATION -------------------------------------------------


ggplot(data = data.igual.ancho, aes(x = Bins, y = ValidationAccuracy, colour = factor(ConfidenceFactor))) +
  geom_line(size = 1) + geom_point(shape = 16, size = 1.5) +
  scale_x_continuous(breaks = data.igual.ancho$Bins) +
  ggtitle("Acc Validation | Bins = Igual Ancho") +
  labs(color="CF") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("7.2.1- accuracy_validacion_igual_ancho.png", dpi=400, height=6, width=12)



ggplot(data = data.igual.freq, aes(x = Bins, y = ValidationAccuracy, colour = factor(ConfidenceFactor))) +
  geom_line(size = 1) + geom_point(shape = 16, size = 1.5) +
  scale_x_continuous(breaks = data.igual.freq$Bins) +
  labs(color="CF") +
  ggtitle("Acc Validation | Bins = Igual Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("7.2.2- accuracy_validacion_igual_freq.png", dpi=400, height=6, width=12)

# ------------------------------------------------- ACCURACY TRAIN vs VALID -------------------------------------------------
ggplot(data = data.igual.ancho) + geom_point(size = 2, alpha=0.7, aes(x = Bins, y = TrainningAccuracy, colour = "Entrenamiento")) +
geom_point(size = 2, alpha=0.7, aes(x = Bins, y = ValidationAccuracy, colour = "Validación")) +
scale_x_continuous(breaks = data.igual.ancho$Bins) + theme_minimal() +
ggtitle("Bins Igual Ancho") +
labs(color="") +
theme(plot.title = element_text(hjust = 0.5))
ggsave("accuracy_train_vs_validacion_igual_ancho.png", dpi=400, height=6, width=12)

ggplot(data = data.igual.freq) + geom_point(size = 2, alpha=0.7, aes(x = Bins, y = TrainningAccuracy, colour = "Entrenamiento")) +
  geom_point(size = 2, alpha=0.7, aes(x = Bins, y = ValidationAccuracy, colour = "Validación")) +
  scale_x_continuous(breaks = data.igual.ancho$Bins) + theme_minimal() +
  labs(color="") +
  ggtitle("Bins Igual Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("accuracy_train_vs_validacion_igual_freq.png", dpi=400, height=6, width=12)


# ------------------------------------------------- ACCURACY supervisado VS no supervisado  -------------------------------------------------
#busco los mejores Bins de cada tipo (supervisado es el unico que hay)
best.bins.igual.ancho <- data.igual.ancho[data.igual.freq$ValidationAccuracy == max(data.igual.ancho$ValidationAccuracy),"Bins"]        
best.bins.igual.freq <- data.igual.freq[data.igual.freq$ValidationAccuracy == max(data.igual.freq$ValidationAccuracy),"Bins"]        
best.supervisado <- data.supervisado[data.supervisado$ValidationAccuracy == max(data.supervisado$ValidationAccuracy),]        

comp.igual.ancho <- data.igual.ancho[data.igual.ancho$Bins == best.bins.igual.ancho,]
comp.igual.freq <- data.igual.freq[data.igual.freq$Bins == best.bins.igual.freq,]
comp.supervisado <- data.supervisado


data.comparacion <- rbind(comp.igual.ancho, comp.igual.freq, comp.supervisado)
data.comparacion
ggplot(data = data.comparacion, aes(x = ConfidenceFactor, y = ValidationAccuracy, colour = factor(Method) )) +
geom_line(size = 1) + geom_point(shape = 16, size = 1.5) +
  scale_x_continuous(breaks = data.comparacion$ConfidenceFactor) +
  ggtitle("Acc Validation | Comparacion Métodos (mejores corridas)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Método") 
  
ggsave("7.3- accuracy_validacion_comparacion_metodos.png", dpi=400, height=6, width=12)
