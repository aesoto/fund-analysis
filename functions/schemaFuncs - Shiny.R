# Pasting version from our dicussion today, 10/31/2019

source(here('functions', 'SchemaDashboard.R'))

  askUS <- function() { return(readline(prompt='Choose US Equity schema: '))}
askIntl <- function() { return(readline(prompt='Choose Intl Equity schema: '))}
askBond <- function() { return(readline(prompt='Choose bond schema: '))}

getSchema <- function() {
  #filename <-"C:\\UpWork\\Data Analytics for Investment Portfolio\\Schema Summary.csv"
  #schemaData <- as.matrix(fread(filename))
  schemas <- data.frame(AssetClass=c('US Equity', 'US Equity', 'US Equity', 'Intl Equity', 'Intl Equity', 'Intl Equity', 'Bonds', 'Bonds'),
                        Schema=c('GICS','MktCap','Country', 'Country', 'GICS', 'MktCap', 'AssetType','Duration'))
  
  #print(schemas)
  
  USschema <- askUS()
  intlSchema <- askIntl()
  bondSchema <- askBond()
  
  schema<-getShiny()
  
  # schema is now a dataframe with 3 rows; one row is the US Equity schema, a 2nd row is the Intl Equity schema, and 3rd row is FI schema
  
  print(schema)
  return(schema)

}



