

library(ggplot2)
library(grid)
library(gridExtra)

plot.noise.accuracy <- function(results, scale.values) {
  # ------------------------------------------------- TRAINNING -------------------------------------------------
  trainning.performance.plot = ggplot(data = results, aes(
    x = Noise,
    y = TrainningAccuracy,
    colour = factor(ConfidenceFactor)
  ))
  trainning.performance.plot = trainning.performance.plot + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  trainning.performance.plot = trainning.performance.plot + scale_x_continuous(breaks = scale.values) + theme_minimal()
  trainning.performance.plot = trainning.performance.plot + ggtitle("Dataset de Entrenamiento")
  trainning.performance.plot = trainning.performance.plot + theme(plot.title = element_text(hjust = 0.5))
  trainning.performance.plot = trainning.performance.plot + labs(colour = "CF", x = "Ruido (%)", y = "Accuracy")

  # ------------------------------------------------- VALIDATION -------------------------------------------------
  validation.performance.plot = ggplot(data = results, aes(
    x = Noise,
    y = ValidationAccuracy,
    colour = factor(ConfidenceFactor)
  ))
  validation.performance.plot = validation.performance.plot + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  validation.performance.plot = validation.performance.plot + scale_x_continuous(breaks = scale.values) + theme_minimal()
  validation.performance.plot = validation.performance.plot + ggtitle("Dataset de Validación")
  validation.performance.plot = validation.performance.plot + theme(plot.title = element_text(hjust = 0.5))
  validation.performance.plot = validation.performance.plot + labs(colour = "CF", x = "Ruido (%)", y = "Accuracy")

  # ------------------------------------------------- ACCURACY -------------------------------------------------
  accuracy.plot = ggplot(data = results) + geom_point(size = 2,
                                                      aes(x = Noise, y = TrainningAccuracy, colour = "Entrenamiento"))
  accuracy.plot = accuracy.plot + geom_point(size = 2,
                                             aes(x = Noise, y = ValidationAccuracy, colour = "Validación"))
  accuracy.plot = accuracy.plot + scale_x_continuous(breaks = noise.seq) + theme_minimal()
  accuracy.plot = accuracy.plot + ggtitle("Entrenamiento vs Validación")
  accuracy.plot = accuracy.plot + theme(plot.title = element_text(hjust = 0.5),
                                        axis.text.x = element_text(angle = 90, hjust = 1))
  accuracy.plot = accuracy.plot + labs(colour = "Dataset", x = "Ruido (%)", y = "Accuracy")

  # ------------------------------------------------- COMPARANDO RESULTADOS -------------------------------------------------
  parallel.plot = grid.arrange(
    trainning.performance.plot,
    validation.performance.plot,
    accuracy.plot,
    nrow = 2,
    layout_matrix = rbind(c(1, 2),
                          c(3, 3))
  )

  return(parallel.plot)

}
