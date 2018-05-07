get.mode <- function(values) {
  available.values = values[!is.na(values)]

  uniqx = unique(available.values)
  mode = uniqx[which.max(tabulate(match(available.values, uniqx)))]

  return(mode)
}
