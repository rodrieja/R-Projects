generate.na <-
  # se pasa un data frame, y el porcentaje a reemplazar
  function (original.data,
            original.perc,
            class.column) {
    # genero una copia del data.frame para modificarlo
    data = original.data

    # Calculo la cantidad de elementos en el data.frame
    data.ncol = ncol(data) - 1
    data.nrow = nrow(data)
    total.elements = data.ncol * data.nrow

    na.in.data = is.na(original.data[-class.column])
    # obtengo el porcentaje de NAs
    original.na.perc = length(data[na.in.data]) / total.elements

    # calculo la cantidad de elementos a reemplazar
    elements.to.replace = as.integer((original.perc - original.na.perc) * total.elements)

    if (elements.to.replace > 0) {
      #indices disponibles (no NA), con [-class.column] no incluyo al atributo Clase
      available.ix = which(!na.in.data, arr.ind = TRUE)

      #genero lista de indices
      ix.to.rep = available.ix[sample(nrow(available.ix), elements.to.replace),]

      #reemplazo
      data[ix.to.rep] = NA
    }

    return (data)
  }
