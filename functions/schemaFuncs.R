library(tcltk2)
filename <-"C:\\UpWork\\Data Analytics for Investment Portfolio\\Schema Summary.csv"
schemaData <- as.matrix(fread(filename))

#R function browser() halts execution and invokes an environment browser when it is called. 
#You can put browser() anywhere in your code to stop at that point in the code for debugging

for (i in 1:ncol(schemaData)){
  #R function debug() allows user to step through execution of a function, line by line. 
  #At any point, we can print out values of variables or produce a graph of results in the function.
  #While debugging, we can simply type "c" to continue to the end of the current section of code.
  
  debug(askSchema)
  browser()
  askSchema <- function(userInputLevel){       
    uniqueLevel <- unique(as.character(strsplit(schemaData[,i], ",")),FALSE)
    inputLevel <- as.array(uniqueLevel)
    
  for (j in 1:length(inputLevel)){
    userInputLevel <- as.character(inputLevel[j])
    response <- tkmessageBox(
      message = paste0('Is your choice ', userInputLevel, '?'), 
      icon="question", 
      type = "yesno") 
      #default = "yes")

    if (as.character(response)[1]=="yes") {
      continue = FALSE 
      break()
    }
    
    if (as.character(response)[1]=="no") {
      continue = TRUE 
    } 

  }
    
    browser()
    userInputLevel <- as.array(userInputLevel)
    #return(userInputLevel)
    
    browser()
    debug(getSchema)
    schema <- getSchema(userInputLevel)

  }
 
 getSchema <- function(userInputLevel){
  
  setInputLevel <- userInputLevel
  schemaData<-as.data.frame(schemaData)
  schema <- dplyr::filter(schemaData, grepl(setInputLevel,inputLevel))
  schemaData<-as.matrix(schemaData)
  return(schema)
  
 }
 
  
# Pasting version from our dicussion today, 10/31/2019
  
  askUS <- function() { return(readline(prompt='Choose US Equity schema: '))}
askIntl <- function() { return(readline(prompt='Choose Intl Equity schema: '))}
askBond <- function() { return(readline(prompt='Choose bond schema: '))}

getSchema <- function() {
  #filename <-"C:\\UpWork\\Data Analytics for Investment Portfolio\\Schema Summary.csv"
  #schemaData <- as.matrix(fread(filename))
  schemas <- data.frame(AssetClass=c('US Equity', 'US Equity', 'Intl Equity', 'Intl Equity', 'Intl Equity', 'Bonds', 'Bonds'),
                        Schema=c('GICS', 'MktCap', 'Country', 'GICS_1', 'MktCap', 'assetType','Duration'))
  
  print(schemas)
  
  USschema <- askUS()
  intlSchema <- askIntl()
  bondSchema <- askBond()
  
  US <- data.frame(stringsAsFactors = FALSE)
  if(USschema=='GICS') { 
    US$Level1 <- 'Equity'
    US$Level2 <- 'GICS_1'
    US$Level3 <- 'GICS_2'
    # etc.
  } else if (USschema=='MktCap') {
    US$Level2 <- 'MktCap'
    #etc
  }
  intl <- data.frame(stringsAsFactors = FALSE)
  if(intlSchema=='Country') {
    #define data frame
  } else if (intlSchema=='GICS')  # 3 choices in intl
  
  # other asset class designations - bonds are left

  bonds <- data.frame(stringsAsFactors = FALSE)
  #if statements to define bond schema line
  
  schema <- rbind(US, intl)
  schema <- rbind(schema, bond)
  
  # schema is now a dataframe with 3 rows.  1st row is the US equity schema, 2nd row intl, 3rd is bond
  
  return(schema)
}





#R function browser() halts execution and invokes an environment browser when it is called. 
#You can put browser() anywhere in your code to stop at that point in the code for debugging

for (i in 1:ncol(schemaData)){
  #R function debug() allows user to step through execution of a function, line by line. 
  #At any point, we can print out values of variables or produce a graph of results in the function.
  #While debugging, we can simply type "c" to continue to the end of the current section of code.
  
  debug(askSchema)
  browser()
  askSchema <- function(userInputLevel){       
    uniqueLevel <- unique(as.character(strsplit(schemaData[,i], ",")),FALSE)
    inputLevel <- as.array(uniqueLevel)
    
    for (j in 1:length(inputLevel)){
      userInputLevel <- as.character(inputLevel[j])
      response <- tkmessageBox(
        message = paste0('Is your choice ', userInputLevel, '?'), 
        icon="question", 
        type = "yesno") 
      #default = "yes")
      
      if (as.character(response)[1]=="yes") {
        continue = FALSE 
        break()
      }
      
      if (as.character(response)[1]=="no") {
        continue = TRUE 
      } 
      
    }
    
    browser()
    userInputLevel <- as.array(userInputLevel)
    #return(userInputLevel)
    
    browser()
    debug(getSchema)
    schema <- getSchema(userInputLevel)
    
  }
  
  getSchema <- function(userInputLevel){
    
    setInputLevel <- userInputLevel
    schemaData<-as.data.frame(schemaData)
    schema <- dplyr::filter(schemaData, grepl(setInputLevel,inputLevel))
    schemaData<-as.matrix(schemaData)
    return(schema)
    
  }
  
}

  
}

