library(ggplot2)
library(grid)
library(gridExtra)

plot.noise.size <- function(results, scale.values) {
  # ------------------------------------------------- HOJAS -------------------------------------------------
  leaves.plot = ggplot(data = results, aes(
    x = Noise,
    y = NumberOfLeaves,
    colour = factor(ConfidenceFactor)
  ))
  leaves.plot = leaves.plot + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  leaves.plot = leaves.plot + scale_x_continuous(breaks = scale.values) + theme_minimal()
  leaves.plot = leaves.plot + ggtitle("Inducción de Ruido en el Dataset")
  leaves.plot = leaves.plot + theme(plot.title = element_text(hjust = 0.5))
  leaves.plot = leaves.plot + labs(colour = "CF", x = "Ruido (%)", y = "Hojas")

  # ------------------------------------------------- NODOS -------------------------------------------------
  nodes.plot = ggplot(data = results, aes(
    x = Noise,
    y = NumberOfNodes,
    colour = factor(ConfidenceFactor)
  ))
  nodes.plot = nodes.plot + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  nodes.plot = nodes.plot + scale_x_continuous(breaks = scale.values) + theme_minimal()
  nodes.plot = nodes.plot + ggtitle("Inducción de Ruido en el Dataset")
  nodes.plot = nodes.plot + theme(plot.title = element_text(hjust = 0.5))
  nodes.plot = nodes.plot + labs(colour = "CF", x = "Ruido (%)", y = "Nodos")

  # ------------------------------------------------- EN PARALELO -------------------------------------------------
  grid.arrange(leaves.plot,
               nodes.plot,
               nrow = 2)

}
