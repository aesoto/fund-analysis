source(here('functions', 'dateFuncs.R'))
source(here('functions', 'getFuncs.R'))
source(here('functions', 'cleanFuncs.R'))
source(here('functions', 'prepFuncs v2.R'))
source(here('functions', 'attribFuncs v2.R'))
source(here('functions', 'saveFuncs.R'))

main <- function() {
  isTesting <- FALSE
  isGeometric <- TRUE
  date <- getDate()
  rawData <- getData(date, isTesting)
  cleanedData <- cleanData(rawData)
  preparedData <- prepData(cleanedData)
  output <- getAttribution(preparedData, isGeometric)
  saveData(output, date)
}