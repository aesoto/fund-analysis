# Pasting version from our dicussion today, 10/31/2019
  
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
  
  #Default schema for US Equity
  if(USschema=='GICS') {
    #print(USschema)
    US <- data.frame(Level1='Equity', Level2='US',Level3='GICS_1',
                     Level4='GICS_2',Level5='GICS_2',Level6='Selection',
                     stringsAsFactors = FALSE)
  } 
  #Alternative schema for US Equity
  if (USschema=='Mkt_Cap') {
    US <- data.frame(Level1='Equity', Level2='US',Level3='US',
                     Level4='Mkt_Cap',Level5='Mkt_Cap',Level6='Selection',
                     stringsAsFactors = FALSE)
  } 
  #Alternative schema for US Equity
  if (USschema=='Country') {
    US <- data.frame(Level1='Equity', Level2='US',Level3='US',
                     Level4='GICS_1',Level5='GICS_2',Level6='Selection',
                     stringsAsFactors = FALSE)
  }
  #Default schema for International Equity
  if(intlSchema=='Country') {
    intl <- data.frame(Level1='Equity', Level2='International',Level3='DM/EM',
                     Level4='Continent',Level5='Country',Level6='Selection',
                     stringsAsFactors = FALSE)
  } 
  #Alternative schema for International Equity
  if (intlSchema=='GICS'){
    intl <- data.frame(Level1='Equity', Level2='International',Level3='DM/EM',
                       Level4='GICS_1',Level5='GICS_2',Level6='Selection',
                       stringsAsFactors = FALSE)
  } 
  #Alternative schema for International Equity
  if (intlSchema=='Mkt_Cap'){
    intl <- data.frame(Level1='Equity', Level2='International',Level3='DM/EM',
                       Level4='Mkt_Cap',Level5='Mkt_Cap',Level6='Selection',
                       stringsAsFactors = FALSE)  
  }
  #Default schema for Fixed Income
  if(bondSchema=='AssetType') {
    bonds<- data.frame(Level1='Fixed Income', Level2='Fixed Income',Level3='Sec_Group',
                       Level4='Sec_Type',Level5='Sec_Type',Level6='Selection',
                       stringsAsFactors = FALSE)   
  }
  #Alternative schema for Fixed Income
  if(bondSchema=='Duration') {
    bonds<- data.frame(Level1='Fixed Income', Level2='Fixed Income',Level3='Dur',
                       Level4='Dur',Level5='Dur',Level6='Selection',
                       stringsAsFactors = FALSE)  
  }
  schema <- rbind(US, intl)
  schema <- rbind(schema, bonds)
  
  # schema is now a dataframe with 3 rows; one row is the US Equity schema, a 2nd row is the Intl Equity schema, and 3rd row is FI schema
  
  print(schema)
  return(schema)
  
}





