library(ggplot2)
library(grid)
library(gridExtra)

plot.missing.size <- function(results, scale.values) {
  results.class.false = results[results$ClassMode == FALSE, ]
  results.class.true = results[results$ClassMode == TRUE, ]

  # ------------------------------------------------- HOJAS -------------------------------------------------
  leaves.plot.false = ggplot(data = results.class.false, aes(
    x = Missings,
    y = NumberOfLeaves,
    colour = factor(ConfidenceFactor)
  ))
  leaves.plot.false = leaves.plot.false + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  leaves.plot.false = leaves.plot.false + scale_x_continuous(breaks = scale.values)
  leaves.plot.false = leaves.plot.false + ggtitle("Moda del atributo") + theme_minimal()
  leaves.plot.false = leaves.plot.false + theme(plot.title = element_text(hjust = 0.5),
                                                axis.text.x = element_text(angle = 90, hjust = 1))
  leaves.plot.false = leaves.plot.false + labs(colour = "CF", x = "Faltantes (%)", y = "Hojas")

  leaves.plot.true = ggplot(data = results.class.true, aes(
    x = Missings,
    y = NumberOfLeaves,
    colour = factor(ConfidenceFactor)
  ))
  leaves.plot.true = leaves.plot.true + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  leaves.plot.true = leaves.plot.true + scale_x_continuous(breaks = scale.values)
  leaves.plot.true = leaves.plot.true + ggtitle("Moda de la clase") + theme_minimal()
  leaves.plot.true = leaves.plot.true + theme(plot.title = element_text(hjust = 0.5),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
  leaves.plot.true = leaves.plot.true + labs(colour = "CF", x = "Faltantes (%)", y = "Hojas")

  # ------------------------------------------------- NODOS -------------------------------------------------
  nodes.plot.false = ggplot(data = results.class.false, aes(
    x = Missings,
    y = NumberOfNodes,
    colour = factor(ConfidenceFactor)
  ))
  nodes.plot.false = nodes.plot.false + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  nodes.plot.false = nodes.plot.false + scale_x_continuous(breaks = scale.values)
  nodes.plot.false = nodes.plot.false + ggtitle("Moda del atributo") + theme_minimal()
  nodes.plot.false = nodes.plot.false + theme(plot.title = element_text(hjust = 0.5),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
  nodes.plot.false = nodes.plot.false + labs(colour = "CF", x = "Faltantes (%)", y = "Nodos")

  nodes.plot.true = ggplot(data = results.class.true, aes(
    x = Missings,
    y = NumberOfNodes,
    colour = factor(ConfidenceFactor)
  ))
  nodes.plot.true = nodes.plot.true + geom_line(size = 1) + geom_point(shape = 16, size = 1.5)
  nodes.plot.true = nodes.plot.true + scale_x_continuous(breaks = scale.values)
  nodes.plot.true = nodes.plot.true + ggtitle("Moda de la clase") + theme_minimal()
  nodes.plot.true = nodes.plot.true + theme(plot.title = element_text(hjust = 0.5),
                                            axis.text.x = element_text(angle = 90, hjust = 1))
  nodes.plot.true = nodes.plot.true + labs(colour = "CF", x = "Faltantes (%)", y = "Nodos")

  # ------------------------------------------------- EN PARALELO -------------------------------------------------
  parallel.plot = grid.arrange(
    leaves.plot.false,
    leaves.plot.true,
    nodes.plot.false,
    nodes.plot.true,
    ncol = 2,
    nrow = 2
  )

  return (parallel.plot)
}
