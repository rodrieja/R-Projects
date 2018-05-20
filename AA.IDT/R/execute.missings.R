execute.missings <- function(ds.path, ds.trainning, ds.validation, class.column, missings.perc, ConfidenceFactor.Vector) {
  setwd(ds.path)

  dataset.Trainning <- read.csv(ds.trainning)
  dataset.Validation <- read.csv(ds.validation)

  results = data.frame()

  for (by.class in c(TRUE, FALSE)) {
    for (miss.perc in missings.perc) {
      data = ind.missings(dataset.Trainning, class.column, miss.perc, by.class)

      # ejecuto las corridas en base al CF
      results.cf = execute.J48(ConfidenceFactor.Vector,
                               0,
                               data,
                               dataset.Validation)

      results = rbind(results,
                      cbind(results.cf, Missings = miss.perc, ClassMode = by.class))
    }
  }

  return(results)
}
