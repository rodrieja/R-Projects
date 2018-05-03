generate.na <-
  # se pasa un data frame, y el porcentaje a reemplazar
  function (original.data, original.perc) {
    # TODO: debería ser una constante o parámetro
    class.column = ncol(original.data)

    # genero una copia del data.frame para modificarlo
    data = original.data

    # Calculo la cantidad de elementos en el data.frame
    data.ncol = ncol(data)
    data.nrow = nrow(data)
    total.elements = data.ncol * data.nrow

    # obtengo el porcentaje de NAs
    original.na.perc = length(data[na.in.data]) / total.elements

    # calculo la cantidad de elementos a reemplazar
    elements.to.replace = as.integer((original.perc - original.na.perc) * total.elements)

    #indices disponibles (no NA), con [-class.column] no incluyo al atributo Clase
    available.ix = which(!is.na(original.data[-class.column]), arr.ind = TRUE)

    #genero lista de indices
    ix.to.rep = available.ix[sample(nrow(available.ix), elements.to.replace), ]

    #reemplazo
    data[ix.to.rep] = NA

    return (data)
  }
