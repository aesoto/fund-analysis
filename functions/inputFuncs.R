source(here('functions', 'dateFuncs.R'))
source(here('functions', 'schemaFuncs.R'))

#browser()
#debug(getInputs)
getInputs <- function () {
  isTesting <- FALSE
  isGeometric <- TRUE
  date <- getDate()
  
  schema <- getSchema()
  
  inputs <- list(date=date, schema=schema, isTesting=isTesting, isGeometric=isGeometric)
  
  return(inputs)
}
