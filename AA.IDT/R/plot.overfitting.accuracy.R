library(ggplot2)

plot.overfitting.accuracy <- function(results, x.value, x.scale) {
  accuracyResults.lineplot = ggplot()
  accuracyResults.lineplot = accuracyResults.lineplot + geom_line(
    data = results,
    aes(x = ConfidenceFactor,
        y = TrainningAccuracy,
        colour = "Entrenamiento"),
    size = 1
  )
  accuracyResults.lineplot = accuracyResults.lineplot + geom_line(
    data = results,
    aes(x = ConfidenceFactor,
        y = ValidationAccuracy,
        colour = "Validation"),
    size = 1
  )
  accuracyResults.lineplot = accuracyResults.lineplot + geom_point(
    data = results,
    shape = 16,
    size = 1.5,
    aes(x = ConfidenceFactor,
        y = TrainningAccuracy,
        colour = "Entrenamiento")
  )
  accuracyResults.lineplot = accuracyResults.lineplot + geom_point(
    data = results,
    shape = 16,
    size = 1.5,
    aes(x = ConfidenceFactor,
        y = ValidationAccuracy,
        colour = "Validation")
  )
  accuracyResults.lineplot = accuracyResults.lineplot + scale_x_continuous(breaks = x.scale) + theme_minimal()
  accuracyResults.lineplot = accuracyResults.lineplot + ggtitle(paste(c("Relación Performace ", x.value), collapse = ""))
  accuracyResults.lineplot = accuracyResults.lineplot + theme(plot.title = element_text(hjust = 0.5))
  accuracyResults.lineplot = accuracyResults.lineplot + labs(colour = "Dataset", x = x.value, y = "Performance (%)")
  accuracyResults.lineplot
}
