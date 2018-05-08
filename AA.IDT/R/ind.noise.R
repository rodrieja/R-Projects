ind.noise <- function(x, class.column, noise.perc) {
  # calculo la cantidad de elementos a modificar
  nRow = nrow(x)
  noise.count = ceiling(nRow * noise.perc)
  noise.idx = sample(nRow, noise.count)
  # obtengo los valores modificados
  switched.values = switch.value(x[noise.idx, class.column])
  # reemplazo en el dataset original
  x[noise.idx, class.column] = switched.values

  return (x)
}
