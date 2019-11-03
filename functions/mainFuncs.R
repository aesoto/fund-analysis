source(here('functions', 'inputFuncs.R'))
source(here('functions', 'getFuncs.R'))
source(here('functions', 'cleanFuncs.R'))
source(here('functions', 'prepFuncs v2.R'))
source(here('functions', 'attribFuncs.R'))
source(here('functions', 'saveFuncs.R'))

main <- function() {
  input <- getInputs()
  rawData <- getData(input)
  cleanedData <- cleanData(rawData)
  preparedData <- prepData(cleanedData)
  output <- getAttribution(preparedData, isGeometric)
  saveData(output, date)
}
