get.discret <- function (dataset, columns, strategy, bins) {
  # strategy puede ser "interval" o "frequency"
  if (strategy == "interval" || strategy == "frequency") {
    for (col in columns) {
      dataset[col] <- discretize(dataset[, col], strategy, breaks = bins)
    }
  }

  return(dataset)
}
