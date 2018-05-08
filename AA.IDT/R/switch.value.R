switch.value <- function(x) {
  cls.values = levels(x)

  if (length(x) > 0) {
    for (idx in 1:length(x)) {
      if (x[idx] == cls.values[1]) {
        x[idx] = cls.values[2]
      }
      else {
        x[idx] = cls.values[1]
      }
    }
  }

  return(x)
}
