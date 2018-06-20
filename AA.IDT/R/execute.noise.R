execute.noise <- function(ds.path,
                          ds.trainning,
                          ds.validation,
                          class.column,
                          noise.seq,
                          ConfidenceFactor.Vector) {
  setwd(ds.path)

  dataset.Trainning <- read.csv(ds.trainning)
  dataset.Validation <- read.csv(ds.validation)

  results = data.frame()

  for (noise.perc in noise.seq) {
    noise.ds = ind.noise(x = dataset.Trainning,
                         class.column = class.column,
                         noise.perc = noise.perc)

    # ejecuto las corridas en base al CF
    results.cf = execute.J48(ConfidenceFactor.Vector,
                             0,
                             noise.ds,
                             dataset.Validation)

    results = rbind(results,
                    cbind(results.cf, Noise = noise.perc))
  }

  return(results)
}
