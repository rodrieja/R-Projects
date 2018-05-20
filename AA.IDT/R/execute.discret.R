library(arules)
library(dplyr)
library(arc)

execute.discret <- function(ds.path,
                            ds.complete,
                            train.size,
                            columns,
                            class.attr,
                            bins,
                            ConfidenceFactor.Vector) {
  setwd(ds.path)
  dataset <- read.csv(ds.complete)

  results = data.frame()

  for (method in c("interval", "frequency")) {
    for (brk in bins) {
      data = get.discret(TelcoChurn, columns, method, brk)

      #separo train de validacion
      set.seed(2018)
      trainning <- sample_frac(data, trn_size)
      sid <-
        as.numeric(rownames(trainning)) # because rownames() returns character
      validation <- data[-sid,]

      # ejecuto las corridas en base al CF
      results.cf = execute.J48(ConfidenceFactor.Vector,
                               0,
                               trainning,
                               validation)

      results = rbind(results,
                      cbind(results.cf, Bins = brk, Method = method))
    }
  }

  #corrida supervisada
  data <- discrNumeric(dataset, class.attr)

  #separo train de validacion
  set.seed(2018)
  trainning <- sample_frac(data$Disc.data, train.size)
  sid <-
    as.numeric(rownames(trainning)) # because rownames() returns character
  validation <- data$Disc.data[-sid, ]

  # ejecuto las corridas en base al CF
  results.cf = execute.J48(ConfidenceFactor.Vector,
                           0,
                           trainning,
                           validation)

  results = rbind(results,
                  cbind(results.cf, Bins = 0, Method = "supervised"))
}
