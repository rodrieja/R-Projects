library(ggplot2)
library(grid)
library(gridExtra)

plot.missing.accuracy <- function(results, scale.values) {
  results.class.false = results[results$ClassMode == FALSE, ]
  results.class.true = results[results$ClassMode == TRUE, ]
  # ------------------------------------------------- TRAINNING -------------------------------------------------
  trainning.performance.plot.false = ggplot(data = results.class.false, aes(
    x = Missings,
    y = TrainningAccuracy,
    colour = factor(ConfidenceFactor)
  ))
  trainning.performance.plot.false = trainning.performance.plot.false + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  trainning.performance.plot.false = trainning.performance.plot.false + scale_x_continuous(breaks = scale.values)
  trainning.performance.plot.false = trainning.performance.plot.false + ggtitle("Entrenamiento - Moda del atributo") + theme_minimal()
  trainning.performance.plot.false = trainning.performance.plot.false + theme(plot.title = element_text(hjust = 0.5),
                                                                              axis.text.x = element_text(angle = 90, hjust = 1))
  trainning.performance.plot.false = trainning.performance.plot.false + labs(colour = "CF", x = "Faltantes (%)", y = "Accuracy")


  trainning.performance.plot.true = ggplot(data = results.class.true, aes(
    x = Missings,
    y = TrainningAccuracy,
    colour = factor(ConfidenceFactor)
  ))
  trainning.performance.plot.true = trainning.performance.plot.true + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  trainning.performance.plot.true = trainning.performance.plot.true + scale_x_continuous(breaks = scale.values)
  trainning.performance.plot.true = trainning.performance.plot.true + ggtitle("Entrenamiento - Moda de la clase") + theme_minimal()
  trainning.performance.plot.true = trainning.performance.plot.true + theme(plot.title = element_text(hjust = 0.5),
                                                                            axis.text.x = element_text(angle = 90, hjust = 1))
  trainning.performance.plot.true = trainning.performance.plot.true + labs(colour = "CF", x = "Faltantes (%)", y = "Accuracy")
  # ------------------------------------------------- VALIDATION -------------------------------------------------
  validation.performance.plot.false = ggplot(data = results.class.false,
                                             aes(
                                               x = Missings,
                                               y = ValidationAccuracy,
                                               colour = factor(ConfidenceFactor)
                                             ))
  validation.performance.plot.false = validation.performance.plot.false + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  validation.performance.plot.false = validation.performance.plot.false + scale_x_continuous(breaks = scale.values) + theme_minimal()
  validation.performance.plot.false = validation.performance.plot.false + ggtitle("Validación - Moda del atributo")
  validation.performance.plot.false = validation.performance.plot.false + theme(plot.title = element_text(hjust = 0.5),
                                                                                axis.text.x = element_text(angle = 90, hjust = 1))
  validation.performance.plot.false = validation.performance.plot.false + labs(colour = "CF", x = "Faltantes (%)", y = "Accuracy")

  validation.performance.plot.true = ggplot(data = results.class.true,
                                            aes(
                                              x = Missings,
                                              y = ValidationAccuracy,
                                              colour = factor(ConfidenceFactor)
                                            ))
  validation.performance.plot.true = validation.performance.plot.true + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  validation.performance.plot.true = validation.performance.plot.true + scale_x_continuous(breaks = scale.values) + theme_minimal()
  validation.performance.plot.true = validation.performance.plot.true + ggtitle("Validación - Moda de la clase")
  validation.performance.plot.true = validation.performance.plot.true + theme(plot.title = element_text(hjust = 0.5),
                                                                              axis.text.x = element_text(angle = 90, hjust = 1))
  validation.performance.plot.true = validation.performance.plot.true + labs(colour = "CF", x = "Faltantes (%)", y = "Accuracy")

  # ------------------------------------------------- ACCURACY -------------------------------------------------
  accuracy.plot.false = ggplot(data = results.class.false) + geom_point(size = 2,
                                                                        aes(x = Missings, y = TrainningAccuracy, colour = "Entrenamiento"))
  accuracy.plot.false = accuracy.plot.false + geom_point(size = 2,
                                                         aes(x = Missings, y = ValidationAccuracy, colour = "Validación"))
  accuracy.plot.false = accuracy.plot.false + scale_x_continuous(breaks = scale.values) + theme_minimal()
  accuracy.plot.false = accuracy.plot.false + ggtitle("Moda del atributo")
  accuracy.plot.false = accuracy.plot.false + theme(plot.title = element_text(hjust = 0.5),
                                                    axis.text.x = element_text(angle = 90, hjust = 1))
  accuracy.plot.false = accuracy.plot.false + labs(colour = "Dataset", x = "Faltantes (%)", y = "Accuracy")


  accuracy.plot.true = ggplot(data = results.class.true) + geom_point(size = 2,
                                                                      aes(x = Missings, y = TrainningAccuracy, colour = "Entrenamiento"))
  accuracy.plot.true = accuracy.plot.true + geom_point(size = 2,
                                                       aes(x = Missings, y = ValidationAccuracy, colour = "Validación"))
  accuracy.plot.true = accuracy.plot.true + scale_x_continuous(breaks = scale.values) + theme_minimal()
  accuracy.plot.true = accuracy.plot.true + ggtitle("Moda de la clase")
  accuracy.plot.true = accuracy.plot.true + theme(plot.title = element_text(hjust = 0.5),
                                                  axis.text.x = element_text(angle = 90, hjust = 1))
  accuracy.plot.true = accuracy.plot.true + labs(colour = "Dataset", x = "Faltantes (%)", y = "Accuracy")

  # ------------------------------------------------- EN PARALELO -------------------------------------------------
  grid.arrange(
    trainning.performance.plot.false,
    validation.performance.plot.false,
    accuracy.plot.false,
    trainning.performance.plot.true,
    validation.performance.plot.true,
    accuracy.plot.true,
    ncol = 3,
    nrow = 2
  )

  grid.arrange(
    trainning.performance.plot.false,
    trainning.performance.plot.true,
    validation.performance.plot.false,
    validation.performance.plot.true,
    ncol = 2
  )

  grid.arrange(accuracy.plot.false,
               accuracy.plot.true,
               ncol = 2)

}
