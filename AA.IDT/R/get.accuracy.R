get.accuracy = function (validationString) {
  # divido el summary en líneas
  splitedByLines = strsplit(validationString, "\\n")
  # el 4to elemento contiene los valores clasificados correctamente (Correctly Classified Instances)
  splitedBySpaces = strsplit(splitedByLines[[1]][4], "\\s")
  # el sexto elemento es el porcentaje ("Correctly", "Classified", "Instances", "", "1122", "79.7441", "%")
  accuracy = as.double(unique(splitedBySpaces[[1]])[6])

  return (accuracy)
}
