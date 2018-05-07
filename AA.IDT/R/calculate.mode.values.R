calculate.mode.values <-
  function(data, class.column, by.class = FALSE) {
    inf.data = data
    # genero la moda de todos los atributos
    for (df.col in c(1:ncol(data))[-class.column]) {
      if (by.class == TRUE) {
        # calcula la moda por clase
        for (cls.value in levels(data[, class.column])) {
          data.class = data[data[class.column] == cls.value, df.col]
          mode.value = get.mode(data.class)
          # obtengo los valores NA para la clase y le asigno la moda
          na.values = which(inf.data[, class.column] == cls.value &
                              is.na(inf.data[, df.col]), arr.ind = TRUE)

          if (length(na.values) > 0) {
            inf.data[na.values, df.col] = mode.value
          }
        }
      }
      else {
        na.values = is.na(inf.data[, df.col])
        mode.value = get.mode(inf.data[, df.col])

        if (sum(na.values) > 0) {
          inf.data[na.values, df.col] = mode.value
        }
      }

    }

    return(inf.data)
  }


# setwd(
#   'E:/GoogleDrive/UBA/Exploración de Datos/Aprendizaje Automático/AA-TP1-Fernandez_Piotti_Rodriguez/Datasets/TelcoChurn'
# )
# TelcoChurn.Trainning <- read.csv('TelcoChurn-Trainning.csv')
# data = TelcoChurn.Trainning
# class.column = ncol(data)
# data = generate.na(data, 0.5, class.column)
# result = calculate.mode.values(data, class.column, byClass = TRUE)
# sum(is.na(result))
