#Source files dateFuncs.R and schemaFuncs.R are made available to inputFuncs.R for processing of getDate() and getSchema(), respectively
source(here('functions', 'dateFuncs.R'))
source(here('functions', 'schemaFuncs.R'))

#User is prompted to specify Date, Schema [isTesting and isGeometric are 'globally' set to FALSE and TRUE, respectively within mainFuncs.R]
#Find intelligent way to first prompt user to supply Date and then supply Schema

browser()
debug(getInputs)
getInputs <- function () {
  #dataFuncs.R is called by getDate(), resulting in the user-supplied date to pass to the date object
  browser()
  date <- getDate()
  return(date)

  #schemaFuncs.R is called by getSchema(), resulting in the user-supplied schema to pass to the schema object
  browser()
  schema <- getSchema()
  return(schema)
}

