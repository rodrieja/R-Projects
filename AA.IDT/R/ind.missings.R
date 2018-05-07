ind.missings <- function(data, class.column, miss.perc, by.class = FALSE) {
  data.miss = generate.na(data, miss.perc, class.column)
  result = calculate.mode.values(data.miss, class.column, by.class)

  return(result)
}
