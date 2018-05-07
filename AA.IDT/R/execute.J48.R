library(partykit)
library(RWeka)

execute.J48 = function (Sequence.Vector,
                        cf.value,
                        data.trainning,
                        data.validation) {
  # inicializo los vectores
  NumberOfLeaves.Vector = c()
  NumberOfNodes.Vector = c()
  Accuracy.Trainning.Vector = c()
  Accuracy.Validation.Vector = c()

  vector.index <- 1

  # genero las corridas para los valores del ConfidenceFactorVector
  for (seqValue in Sequence.Vector) {
    # -C <pruning confidence>
    #  Set confidence threshold for pruning.  (default 0.25)
    # -M <minimum number of instances>
    #  Set minimum number of instances per leaf.  (default 2)
    if (cf.value == 0) {
      CF = seqValue
      MNO = 2
    } else {
      CF = 0.3
      MNO = ceiling(seqValue * nrow(data.trainning))
    }
    # genera el árbol en base al set de entrenamiento y el CF indicado
    j48tree = J48(Churn ~ ., data = data.trainning, control = Weka_control(C = CF, M = MNO))
    # evalua el árbol contra el set de validación
    j48validation = evaluate_Weka_classifier(j48tree, newdata = data.validation, class = TRUE)

    # lo convierto para obtener las hojas y los nodos
    partyj48tree <- as.party(j48tree)
    # nodos
    NumberOfNodes.Vector[vector.index] <- length(partyj48tree)
    # hojas
    NumberOfLeaves.Vector[vector.index] <- width(partyj48tree)

    # obtengo el accuracy del entrenamiento y la verificación
    Accuracy.Validation.Vector[vector.index] = get.accuracy(j48validation$string)
    Accuracy.Trainning.Vector[vector.index] = get.accuracy(summary(j48tree)$string)

    # increment subindex
    vector.index <- vector.index + 1
  }

  # generates a dataframe with the results of J48
  results =  data.frame(
    ConfidenceFactor = Sequence.Vector,
    NumberOfNodes = NumberOfNodes.Vector,
    NumberOfLeaves = NumberOfLeaves.Vector,
    TrainningAccuracy = Accuracy.Trainning.Vector,
    ValidationAccuracy = Accuracy.Validation.Vector
  )
}

# # Cargo los datasets
# setwd(
#   'C:/Users/rodri/Google Drive/UBA/Exploración de Datos/Aprendizaje Automático/AA-TP1-Fernandez_Piotti_Rodriguez/Datasets/TelcoChurn'
# )
#
# TelcoChurn.Trainning <- read.csv('TelcoChurn-Trainning.csv')
# TelcoChurn.Validation <- read.csv('TelcoChurn-Validation.csv')
#
# # ---------------------------------------------------- Según Confidence Factor
# # genereta se sequence of confidence factor
# ConfidenceFactor.Vector = seq(0.05, 0.5, by = 0.05)
# # ejecuto las corridas en base al CF
# results.cf = execute.J48(ConfidenceFactor.Vector,
#                          0,
#                          TelcoChurn.Trainning,
#                          TelcoChurn.Validation)
#
# plot.treeSize(results.cf, "Confidence Factor", ConfidenceFactor.Vector)
# plot.performance(results.cf, "Confidence Factor", ConfidenceFactor.Vector)
#
# # ---------------------------------------------------- Según MinNumObj
# MinNumObj.Vector = seq(0.005, 0.1, by = 0.005)
# # ejecuto las corridas en base al CF
# results.mno = execute.J48(MinNumObj.Vector,
#                           0.3,
#                           TelcoChurn.Trainning,
#                           TelcoChurn.Validation)
#
# plot.treeSize(results.mno, "MinNumObj (%)", MinNumObj.Vector)
# plot.performance(results.mno, "MinNumObj (%)", MinNumObj.Vector)
