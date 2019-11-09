source(here('functions', 'inputFuncs.R'))
source(here('functions', 'getFuncs.R'))
source(here('functions', 'cleanFuncs.R'))
source(here('functions', 'prepFuncs.R'))
source(here('functions', 'attribFuncs.R'))
source(here('functions', 'saveFuncs.R'))
source(here('functions', 'bottomUpFuncs.R'))

main <- function() {
  inputs <- getInputs()
  rawData <- getData(inputs)
  cleanedData <- cleanData(rawData, inputs)
  preparedData <- prepData(cleanedData)
  outputTopDown <- getAttribution(preparedData, inputs)
  outputBottomUp <- getBottomsUp(preparedData, inputs)
  saveData(outputTopDown, outputBottomUp, inputs$date)
}
