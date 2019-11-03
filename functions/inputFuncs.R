###### AES pasted a version we discusssed 10/31/2019

#Source files dateFuncs.R and schemaFuncs.R are made available to inputFuncs.R for processing of getDate() and getSchema(), respectively
source(here('functions', 'dateFuncs.R'))
source(here('functions', 'schemaFuncs.R'))


#browser()
#debug(getInputs)
getInputs <- function () {
  isTesting <- FALSE
  isGeometric <- TRUE
  date <- getDate()
  
  schema <- getSchema()
  
  #The line immediately below seems to serve no purpose.
  inputs <- list(date=date, schema=schema, isTesting=isTesting, isGeometric=isGeometric)
  
  return(inputs)
}
