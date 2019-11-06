# 11/6/2019 - I noticed that there is an offset of the levels that has to happen if
# non-default options are selected.  So I handled it this way.
# Also, the saving of data needs to be modified if not default so the isDefault variable needs to tag along.
askDefault <- function() { return(readline(prompt='Would you like to use the default schema (y/n)? '))}
askEquity <- function() { return(readline(prompt='Choose Equity schema: '))}
askBond <- function() { return(readline(prompt='Choose bond schema: '))}


# askUS <- function() { return(readline(prompt='Choose US Equity schema: '))}
# askIntl <- function() { return(readline(prompt='Choose Intl Equity schema: '))}
# askBond <- function() { return(readline(prompt='Choose bond schema: '))}

getSchema <- function() {
  schemas <- data.frame(Options=c('Default', 'Equity', 'Equity', 'Fixed Income', 'Fixed Income'),
                        Schema=c('Default', 'GICS', 'Mkt Cap', 'Asset Type', 'Duration'))
  
  #schemas <- data.frame(AssetClass=c('US Equity', 'US Equity', 'Intl Equity', 'Intl Equity', 'Intl Equity', 'Bonds', 'Bonds'),
  #                      Schema=c('GICS','MktCap','Country', 'GICS', 'MktCap', 'AssetType','Duration'),
  #                      stringsAsFactors = FALSE)
  print('Schema Menu:')
  print(schemas)
  
  defaultAnswer <- askDefault()
  defaultAnswer <- tolower(defaultAnswer)
  if(defaultAnswer=='y') {
    USschema <- 'GICS1'
    intlSchema <- 'Country'
    bondSchema <- 'AssetType'
  } else {
    equityAnswer <- askEquity()
    bondAnswer <- askBond()
    
    equityAnswer <- tolower(equityAnswer)
    bondAnswer <- tolower(bondAnswer)
    
    if(equityAnswer=='gics') {
      USschema <- 'GICS2'
      intlSchema <- 'GICS'
    } else {
      USschema <- 'Mkt_Cap'
      intlSchema <- 'Mkt_Cap'
    }
    if(bondAnswer=='asset type') {
      bondSchema <-'AssetType'
    } else {
      bondSchema <- 'Duration'
    }
  }
  
  #USschema <- askUS()
  #intlSchema <- askIntl()
  #bondSchema <- askBond()
  
  # Default schema for US Equity
  if(USschema=='GICS1') {
    #print(USschema)
    US <- data.frame(AssetClass='US',Level1='Equity', Level2='US',Level3='GICs_1',
                     Level4='GICS_2',Level5='GICS_2',Level6='Selection',
                     stringsAsFactors = FALSE)
  }
  # Alternative GICS schema for US Equity
  if(USschema=='GICS2') {
    #print(USschema)
    US <- data.frame(AssetClass='US',Level1='Equity', Level2='US',Level3='US',
                     Level4='GICS_1',Level5='GICS_2',Level6='Selection',
                     stringsAsFactors = FALSE)
  } 
  #Alternative Market Cap schema for US Equity
  if (USschema=='Mkt_Cap') {
    US <- data.frame(AssetClass='US',Level1='Equity', Level2='US',Level3='US',
                     Level4='Mkt_Cap',Level5='Mkt_Cap',Level6='Selection',
                     stringsAsFactors = FALSE)
  } 
  #Default schema for International Equity - MSCI Categorization
  if(intlSchema=='Country') {
    intl <- data.frame(AssetClass='intl',Level1='Equity', Level2='International',Level3='DM/EM',
                       Level4='Continent',Level5='Country',Level6='Selection',
                       stringsAsFactors = FALSE)
  } 
  #Alternative schema for International Equity - GICS
  if (intlSchema=='GICS'){
    intl <- data.frame(AssetClass='intl',Level1='Equity', Level2='International',Level3='DM/EM',
                       Level4='GICS_1',Level5='GICS_2',Level6='Selection',
                       stringsAsFactors = FALSE)
  } 
  #Alternative schema for International Equity - Mkt Cap
  if (intlSchema=='Mkt_Cap'){
    intl <- data.frame(AssetClass='intl',Level1='Equity', Level2='International',Level3='DM/EM',
                       Level4='Mkt_Cap',Level5='Mkt_Cap',Level6='Selection',
                       stringsAsFactors = FALSE)  
  }
  #Default schema for Fixed Income
  if(bondSchema=='AssetType') {
    bonds <- data.frame(AssetClass='bonds',Level1='Fixed Income', Level2='Fixed Income',Level3='Sec_Group',
                        Level4='Sec_Type',Level5='Sec_Type',Level6='Selection',
                        stringsAsFactors = FALSE)   
  }
  #Alternative schema for Fixed Income - Duration
  if(bondSchema=='Duration') {
    bonds <- data.frame(AssetClass='bonds',Level1='Fixed Income', Level2='Fixed Income',Level3='Dur',
                        Level4='Dur',Level5='Dur',Level6='Selection',
                        stringsAsFactors = FALSE)  
  }
  schema <- rbind(US, intl)
  schema <- rbind(schema, bonds)
  
  # schema is now a dataframe with 3 rows;
  # row 1 is the US Equity schema, a 2nd row is the Intl Equity schema, and 3rd row is FI schema
  #print(schema)
  return(schema)
}
