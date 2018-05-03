library(ggplot2)

plot.performance <- function(results, x.value, x.scale) {
  accuracyResults.lineplot <-
    ggplot() + geom_line(
      data = results,
      size = 1.5,
      aes(x = ConfidenceFactor, y = TrainningAccuracy, colour = "Entrenamiento")
    ) + geom_line(
      data = results,
      size = 1.5,
      aes(x = ConfidenceFactor, y = ValidationAccuracy, colour = "Validación")
    )
  accuracyResults.lineplot = accuracyResults.lineplot + scale_x_continuous(breaks = x.scale)
  accuracyResults.lineplot = accuracyResults.lineplot + ggtitle(paste(c("Relación Performace ", x.value), collapse = ""))
  accuracyResults.lineplot = accuracyResults.lineplot + theme(plot.title = element_text(hjust = 0.5))
  accuracyResults.lineplot = accuracyResults.lineplot + labs(colour = "Dataset", x = x.value, y = "Performance (%)")
  accuracyResults.lineplot
}
