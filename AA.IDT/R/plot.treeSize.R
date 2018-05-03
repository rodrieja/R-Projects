library(ggplot2)

plot.treeSize <- function(results,
                          x.value,
                          x.scale) {
  results.lineplot = ggplot()  + geom_line(
    data = results,
    size = 1.5,
    aes(x = ConfidenceFactor, y = NumberOfNodes, colour = "Nodos")
  )
  results.lineplot = results.lineplot + geom_line(
    data = results,
    size = 1.5,
    aes(x = ConfidenceFactor, y = NumberOfLeaves, colour = "Hojas")
  )
  results.lineplot = results.lineplot + scale_x_continuous(breaks = x.scale)
  results.lineplot = results.lineplot + ggtitle(paste(c("Relación ", x.value, " - Tamaño del árbol"), collapse = ""))
  results.lineplot = results.lineplot + theme(plot.title = element_text(hjust = 0.5))
  results.lineplot = results.lineplot + labs(colour = "Contador",
                                             x = x.value,
                                             y = "Cantidad de Elementos")
  results.lineplot
}
