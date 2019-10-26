# functions for cleaning benchmarks

cleanBenchmarksData <- function(benchmarksData, fundsData, countryData, addlCountryData, addlCompanyData, benchmarkKeys, futuresData) {
  result <- list()
  benchmarks <- unique(benchmarksData$Fund)
  for(benchmark in benchmarks) {
    # print(benchmark)
    reportedRet <- getBenchmarkReturn(benchmark, benchmarksData, benchmarkKeys)
    reportedNMV <- getBenchmarkNMV(benchmark, benchmarksData, benchmarkKeys)
    #attributionType <- getBenchmarkAttributonType(benchmark, fundsData, benchmarkKeys)
    benchmarkData <- getBenchmarkData(benchmark, benchmarksData)
    benchmarkData <- cleanBenchmark(benchmark, benchmarkData, benchmarkKeys, countryData, addlCountryData, addlCompanyData, reportedRet, reportedNMV)
    
    
    ################################    
    #benchmarkData <- setBenchmarkLevels(benchmarkData, attributionType, countryData, addlCountryData, addlCompanyData)
    #benchmarkData <- adjustBenchmarkTable(benchmarkData)
    totRetCont <- sum(benchmarkData$Ret_Cont)
    tempData <- list(reportedRet=reportedRet, totRetCont=totRetCont, benchmarkData=benchmarkData)
    result[[benchmark]] <- tempData
  }
  return(result)
}

cleanBenchmark <- function(benchmark, benchmarkData, benchmarkKeys, countryData, addlCountryData, addlCompanyData, reportedRet, reportedNMV) {
  benchmarkData <- removeBenchmarkNoise(benchmark, benchmarkData, benchmarkKeys)
  benchmarkData <- parseBenchmark(benchmarkData, benchmark, countryData, addlCountryData, addlCompanyData)
  benchmarkData <- setBenchmarkReturns(benchmark, benchmarkData, benchmarkKeys, reportedRet, reportedNMV)
  return(benchmarkData)
}

parseBenchmark <- function(benchmarkData, benchmark, countryData, addlCountryData, addlCompanyData) {
  benchmarkData <- fillBenchmarkCountryData(benchmarkData, benchmark, addlCountryData)
  benchmarkData <- sortBenchmark(benchmarkData)
  benchmarkData <- fillBenchmarkCompanyData(benchmarkData, addlCompanyData)
  benchmarkData <- setBenchmarkLevels(benchmarkData, countryData)
  return(benchmarkData)
}

getBenchmarkReturn <- function(benchmark, benchmarksData, benchmarkKeys) {
  key <- benchmarkKeys$`Aladdin ID`[benchmarkKeys$'Fund'==benchmark]
  reportedRet <- benchmarksData$Ret_Cont[benchmarksData$'Aladdin ID'==key]
  # if(is.na(reportedRet)) { reportedRet <- sum(benchmarkData$) }
  return(reportedRet)
}

getBenchmarkNMV <- function(benchmark, benchmarksData, benchmarkKeys) {
  key <- benchmarkKeys$`Aladdin ID`[benchmarkKeys$'Fund'==benchmark]
  reportedNMV <- benchmarksData$NMV[benchmarksData$'Aladdin ID'==key]
  # if(is.na(reportedNMV)) { reportedNMV <- sum(benchmarkData$NMV) }
  return(reportedNMV)
}

getBenchmarkAttributonType <-function(benchmark, fundsData, benchmarkKeys) {
  if(benchmark %in% fundsData$Benchmark1) {
    attributionType <- fundsData$AttribType1[fundsData$Benchmark1 == benchmark]
  } else if(benchmark %in% fundsData$Benchmark2) {
    attributionType <- fundsData$AttribType2[fundsData$Benchmark2 == benchmark]
  } else if(benchmark %in% fundsData$Benchmark3) {
    attributionType <- fundsData$AttribType3[fundsData$Benchmark3 == benchmark]
  } else if(benchmark %in% benchmarkKeys$Fund) { 
    attributionType <- benchmarkKeys$AttribType[benchmarkKeys$Fund == benchmark]
  } else {
    print(paste0('Benchmark ', benchmark, ' has no attribution data.  Please correct metadata file.'))
  }
  return(attributionType[1]) # may contain duplicated values so [1] must be here
}

getBenchmarkData <- function(benchmark, benchmarksData) {
  benchmarkData <- filter(benchmarksData, Fund==benchmark)
  return(benchmarkData)
}

removeBenchmarkNoise <- function(benchmark, benchmarkData, benchmarkKeys) {
  #columns <- c(4, 11, 11)
  columns <- c('Ret_Cont', 'Ret', 'NMV', 'NMVPct')
  benchmarkData[, columns] <- apply(benchmarkData[ , columns], 2, function(x) as.numeric(as.character(x)))
  key <- benchmarkKeys$`Aladdin ID`[benchmarkKeys$'Fund'==benchmark]
  benchmarkData <- benchmarkData[!(benchmarkData$'Aladdin ID'==key),]
  # if(benchmark == 'R3K') {
  #   benchmarkData <- benchmarkData[!(benchmarkData$'Aladdin ID' == 'RS3000'),]
  # } else if(benchmark=='SPX') {
  #   benchmarkData <- benchmarkData[!(benchmarkData$'Aladdin ID' == 'SNP500'),]
  # } else if(benchmark=='AGG') {
  #   benchmarkData <- benchmarkData[!(benchmarkData$'Aladdin ID' == 'LEH_AGG'),]
  # } else if(benchmark=='EAFE') {
  #   benchmarkData <- benchmarkData[!(benchmarkData$'Aladdin ID' == 'MSCIEAFEN'),]
  # } else {
  #   benchmarkData <- benchmarkData[!(benchmarkData$NMVPct == 1),]
  # }
  benchmarkData$Ret_Cont[is.na(benchmarkData$Ret_Cont)] <- 0
  benchmarkData$Ret[is.na(benchmarkData$Ret)] <- 0
  #benchmarkData <- benchmarkData[complete.cases(benchmarkData),]
  #benchmarkData[is.na(benchmarkData)] <- 0
  benchmarkData <- benchmarkData[(benchmarkData$NMV!=0),]
  benchmarkData <- benchmarkData[complete.cases(benchmarkData),]
  return(benchmarkData)
}

setBenchmarkReturns <- function(benchmark, benchmarkData, benchmarkKeys, reportedRet, reportedNMV) {
  if(is.na(reportedNMV)) { reportedNMV <- sum(benchmarkData$NMV) }
  if(is.na(reportedRet)) { reportedRet <- sum(benchmarkData$Ret_Cont, na.rm=T) / 2 }
  
  benchmarkData$Ret <- benchmarkData$Ret_Cont / benchmarkData$NMVPct
  benchmark <- benchmarkData$Fund[1]
  NMVPctTotal <- sum(benchmarkData$NMVPct)
  if((NMVPctTotal - 1) > 0.01){
    totalNMV <- sum(benchmarkData$NMV)
    marginPct <- 1 - totalNMV/reportedNMV
    marginBal <- reportedNMV - totalNMV
    marginData <- data.table(Fund=benchmark,
                             'Aladdin ID'='Margin',
                             Sec_Des='Margin as Plug',
                             Sec_Group='Margin',
                             Sec_Type='Margin',
                             GICS_1='Margin',
                             GICS_2='Margin',
                             Country='Margin',
                             assetClass='Other',
                             Level1='Other',
                             Level2='Margin',
                             Level3='Margin',
                             Level4='Margin',
                             Level5='Margin',
                             NMV=marginBal,
                             Ret=0,
                             NMVPct=marginPct,
                             Ret_Cont=0
    )
    
    benchmarkData <- rbind(benchmarkData, marginData)
    totalNMV <- sum(benchmarkData$NMV)
    benchmarkData$NMVPct <- benchmarkData$NMV / totalNMV
  }
  
  if((reportedRet - sum(benchmarkData$Ret_Cont)) > 0.0001) {
    print(paste0('Benchmark ', benchmark, ' requires reconciliation of total return'))
  }
  return(benchmarkData)
}

setBenchmarkLevels <- function(benchmarkData, countryData) {
  #benchmarkData <- applyBenchmarkAttribution(benchmarkData, attributionType, countryData, addlCountryData, addlCompanyData)
  suppressWarnings <- TRUE
  assetClasses <- c('intl', 'US', 'bond', 'Other')
  
  result <- data.frame()
  for(class in assetClasses) {
    if(class=='intl') {
      temp <- benchmarkData %>% 
        filter(assetClass==class) %>% 
        select('Aladdin ID', Country)
      temp <- merge(x = temp, y = countryData,
                    by.x = 'Country', by.y = 'CountryCd',
                    all.x = TRUE)
      temp$Country <- NULL
      
      if(!suppressWarnings) {
        missing <- temp[is.na(temp$Level1),]
        missing <- missing[!missing$`Aladdin ID`=='USD_CCASH',]
        if(nrow(missing)>0) {
          print('The following are missing country attribution data')
          print('Update the countryData file if needed.')
          print(missing)
        }
      }
      temp$Level1[is.na(temp$Level1)] <- 'Other'
      temp$Level2[is.na(temp$Level2)] <- 'Other'
      temp$Level3[is.na(temp$Level3)] <- 'Other'
      
      colnames(temp)[colnames(temp)=='Level3'] <- 'Level5'
      colnames(temp)[colnames(temp)=='Level1'] <- 'Level3'
      colnames(temp)[colnames(temp)=='Level2'] <- 'Level4'
      if(nrow(temp) > 0) {
        temp$Level1 <- 'Equity'
        temp$Level2 <- 'Intl'
      }
      temp <- temp %>%
        mutate(Level1 = ifelse(Level3=='Other', 'Other', Level1)) %>%
        mutate(Level2 = ifelse(Level3=='Other', 'Other', Level2))
      temp <- temp[c('Aladdin ID', 'Level1', 'Level2', 'Level3', 'Level4', 'Level5')]
      
      benchmarkData <- merge(x=benchmarkData, y=temp,
                             by.x='Aladdin ID', by.y='Aladdin ID',
                             all.x=TRUE)
    } else if (class=='US') {
      benchmarkData <- benchmarkData %>%
        mutate(Level1= ifelse((Country=='US' | Country=='PR'), 'Equity', Level1)) %>%
        mutate(Level2= ifelse((Country=='US' | Country=='PR'), 'US', Level2)) %>%
        mutate(Level3= ifelse((Country=='US' | Country=='PR'), GICS_1, Level3)) %>%
        mutate(Level4= ifelse((Country=='US' | Country=='PR'), GICS_2, Level4))
      
      temp <- benchmarkData %>% 
        filter(assetClass==class) %>% 
        select('Aladdin ID', Sec_Des, GICS_1, GICS_2)
      
      if(!suppressWarnings) {
        missing <- temp[(is.na(temp$GICS_1) | is.na(temp$GICS_2)),]
        missing <- missing[!missing$`Aladdin ID`=='USD_CCASH',]
        if(nrow(missing)>0) { 
          print('The following are missing bond attribution data')
          print('Update the core data file if needed.')
          print(missing)
        }
      }
    } else if (class=='bond') {
      benchmarkData <- benchmarkData %>%
        mutate(Level1= ifelse(assetClass=='bond', 'Fixed Income', Level1)) %>%
        mutate(Level2= ifelse(assetClass=='bond', 'Fixed Income', Level2)) %>%
        mutate(Level3= ifelse(assetClass=='bond', Sec_Group, Level3)) %>%
        mutate(Level4= ifelse(assetClass=='bond', Sec_Type, Level4))
      
      temp <- benchmarkData %>% 
        filter(assetClass==class) %>% 
        select('Aladdin ID', Sec_Des, Sec_Group, Sec_Type)
      
      if(!suppressWarnings) {
        missing <- temp[(is.na(temp$Sec_Group) | is.na(temp$Sec_Type)),]
        missing <- missing[!missing$`Aladdin ID`=='USD_CCASH',]
        if(nrow(missing)>0) { 
          print('The following are missing bond attribution data')
          print('Update the core data file if needed.')
          print(missing)
        }
      }
    } else if (class=='Other') {
      benchmarkData$Level1[benchmarkData$assetClass=='Other'] <- 'Other'
      benchmarkData$Level2[benchmarkData$assetClass=='Other'] <- 'Other'
      benchmarkData$Level3[benchmarkData$assetClass=='Other'] <- 'Other'
      benchmarkData$Level4[benchmarkData$assetClass=='Other'] <- 'Other'
      
      benchmarkData <- benchmarkData %>%
        mutate(Level1= ifelse(Sec_Group=='CASH', 'Other', Level1)) %>%
        mutate(Level2= ifelse(Sec_Group=='CASH', 'Cash', Level2)) %>%
        mutate(Level3= ifelse(Sec_Group=='CASH', 'Cash', Level3)) %>%
        mutate(Level4= ifelse(Sec_Group=='CASH', 'Cash', Level4))
      
      benchmarkData <- benchmarkData %>%
        mutate(Level1= ifelse(Sec_Group=='CASH' & Sec_Type=='CASH', 'Other', Level1)) %>%
        mutate(Level2= ifelse(Sec_Group=='CASH' & Sec_Type=='CASH', 'Cash', Level2)) %>%
        mutate(Level3= ifelse(Sec_Group=='CASH' & Sec_Type=='CASH', 'Cash', Level3)) %>%
        mutate(Level4= ifelse(Sec_Group=='CASH' & Sec_Type=='CASH', 'Cash', Level4))
      
      benchmarkData <- benchmarkData %>%
        mutate(Level1= ifelse(Sec_Group=='FUND' & Sec_Type=='STIF', 'Other', Level1)) %>%
        mutate(Level2= ifelse(Sec_Group=='FUND' & Sec_Type=='STIF', 'Cash', Level2)) %>%
        mutate(Level3= ifelse(Sec_Group=='FUND' & Sec_Type=='STIF', 'Cash', Level3)) %>%
        mutate(Level4= ifelse(Sec_Group=='FUND' & Sec_Type=='STIF', 'Cash', Level4))
    }
  }
  
  benchmarkData <- benchmarkData %>%
    mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  benchmarkData <- benchmarkData %>%
    mutate(GICS_1= ifelse(Sec_Group=='CASH', 'Other', Sec_Group)) %>%
    mutate(GICS_2= ifelse(Sec_Group=='CASH', 'Other', Sec_Group))
  
  
  
  
  return(benchmarkData)
}
#   
#   benchmarkData <- distinct(benchmarkData)
#   if(attributionType == 'DM-EM_Country') { 
#     benchmarkData <- merge(x = benchmarkData, y = countryData,
#                            by.x = 'Country', by.y = 'CountryCd',
#                            all.x = TRUE)
#     benchmarkData$Level1[is.na(benchmarkData$Level1)] <- 'Other'
#     benchmarkData$Level2[is.na(benchmarkData$Level2)] <- 'Other'
#     benchmarkData$Level3[is.na(benchmarkData$Level3)] <- 'Other'
#     colnames(benchmarkData)[colnames(benchmarkData)=='Level3'] <- 'Level5'
#     colnames(benchmarkData)[colnames(benchmarkData)=='Level1'] <- 'Level3'
#     colnames(benchmarkData)[colnames(benchmarkData)=='Level2'] <- 'Level4'
#     benchmarkData$Level1 <- 'Equity'
#     benchmarkData$Level2 <- 'Intl'
#     benchmarkData <- benchmarkData %>%
#       mutate(Level1 = ifelse(Level3=='Other', 'Other', Level1)) %>%
#       mutate(Level2 = ifelse(Level3=='Other', 'Other', Level2))
# 
#     temp <- benchmarkData[benchmarkData$Level2=='Intl',]
#     missing <- temp[temp$Country=='',]
#     if(nrow(missing)>1) {
#       print('EAFE benchmark misssing country Data')
#       print(missing)
#     }
#     
#     
#   } else if(attributionType == 'GICS') { # For equity
#     benchmarkData$Level1 <- 'Equity'
#     benchmarkData$Level2 <- 'US'
#     setnames(benchmarkData, old=c('GICS_1', 'GICS_2'), new=c('Level3', 'Level4'))
#     benchmarkData$Level1[benchmarkData$Level1 == ''] <- 'Other'
#     benchmarkData$Level2[benchmarkData$Level2 == ''] <- 'Other'
#     benchmarkData <- benchmarkData %>%
#       mutate(Level1 = ifelse(Level3=='Other', 'Other', Level1)) %>%
#       mutate(Level2 = ifelse(Level3=='Other', 'Other', Level2))
#     
#     temp <- benchmarkData[benchmarkData$Level2=='US',]
#     missing <- temp[temp$Level3=='',]
#     if(nrow(missing)>1) {
#       print('US equity benchmark missing GICS Data')
#       print(missing)
#     }
#   } else if(attributionType == 'Sec_Group-Type') { # For fixed income
#     benchmarkData$Level1 <- 'Fixed Income'
#     benchmarkData$Level2 <- 'Fixed Income'
#     setnames(benchmarkData, old=c('Sec_Group', 'Sec_Type'), new=c('Level3', 'Level4'))
#     benchmarkData$Level1[benchmarkData$Level1 == ''] <- 'Other'
#     benchmarkData$Level2[benchmarkData$Level2 == ''] <- 'Other'
#     benchmarkData <- benchmarkData %>%
#       mutate(Level1 = ifelse(Level3=='Other', 'Other', Level1)) %>%
#       mutate(Level2 = ifelse(Level3=='Other', 'Other', Level2))
#     
#     temp <- benchmarkData[benchmarkData$Level1=='Fixed Income',]
#     missing <- temp[temp$Level3=='',]
#     if(nrow(missing)>1) {
#       print('Fixed Income benchmark missing Sec_Group Data')
#       print(missing)
#     }
#   } else {
#     print('Attribution type is unknown for :')
#     print(benchmarkData$Fund[1])
#   }
#   return(benchmarkData)
# }

applyBenchmarkAttribution <- function(benchmarkData, attributionType, countryData, addlCountryData, addlCompanyData) {
  if(attributionType == 'DM-EM_Country') {
    addlCountryData <- addlCountryData %>% select(AladdinID, AddlCountry)
    benchmarkData <- benchmarkData %>%
      left_join(addlCountryData, by=c('Aladdin ID' = 'AladdinID'))
    benchmarkData <- benchmarkData %>%
      mutate(Country = ifelse(Country == '', AddlCountry, Country))
    benchmarkData$AddlCountry <- NULL
    benchmarkData <- benchmarkData %>%
      mutate(Country = ifelse(Sec_Group == 'CASH', '', Country))
    benchmarkData <- benchmarkData %>%
      mutate(Country = ifelse(Sec_Group == 'OPTION', '', Country))
    #benchmarkData <- benchmarkData[!apply(is.na(benchmarkData), 1, all),]
    
    missing <- benchmarkData[(benchmarkData$Country == '' & benchmarkData$Sec_Type == 'EQUITY'),]
    missing <- missing[(!complete.cases(missing[,'Country']) & complete.cases(missing[,'Aladdin ID'])),]
    if(nrow(missing) > 0) {
      print('The following are missing country attribution data in the EAFE Benchmark')
      print(missing)
      print('Update addlCountryData file')
    }
    benchmarkData <- benchmarkData[complete.cases(benchmarkData[,'Country']),]
  } else if(attributionType=='GICS') {
    addlCountryData <- addlCountryData %>% select(AladdinID, AddlCountry)
    benchmarkData <- benchmarkData %>%
      left_join(addlCountryData, by=c('Aladdin ID' = 'AladdinID'))
    benchmarkData <- benchmarkData %>%
      mutate(Country = ifelse(Country == '', AddlCountry, Country))
    benchmarkData$AddlCountry <- NULL
    addlCompanyData <- addlCompanyData %>% select(AladdinID, AddlGICS_1, AddlGICS_2)
    benchmarkData <- benchmarkData %>%
      left_join(addlCompanyData, by=c('Aladdin ID' = 'AladdinID'))
    benchmarkData <- benchmarkData %>%
      mutate(GICS_1= ifelse(GICS_1 == '', AddlGICS_1, GICS_1)) %>%
      mutate(GICS_2= ifelse(GICS_2 == '', AddlGICS_2, GICS_2))
    benchmarkData$AddlGICS_1 <- NULL
    benchmarkData$AddlGICS_2 <- NULL
    benchmarkData <- benchmarkData %>%
      mutate(Sec_Group = ifelse(Sec_Group == 'CASH', 'Other', Sec_Group))
    benchmarkData <- benchmarkData %>%
      mutate(Sec_Type = ifelse(Sec_Type == 'CASH', 'Other', Sec_Type))
    benchmarkData <- benchmarkData %>%
      mutate(GICS_1 = ifelse(Sec_Type == 'Other', 'Other', GICS_1))
    benchmarkData <- benchmarkData %>%
      mutate(GICS_2 = ifelse(Sec_Type == 'Other', 'Other', GICS_2))
    
    
    missing <- benchmarkData[(is.na(benchmarkData$GICS_1) & benchmarkData$Sec_Type == 'EQUITY'),]
    missing <- missing[(!complete.cases(missing[,'GICS_1']) & complete.cases(missing[,'Aladdin ID'])),]
    if(nrow(missing) > 0) {
      print('The following are missing GICS data in the R3K Benchmark')
      print(missing)
      print('Update addlCountryData file')
    }
    benchmarkData <- benchmarkData[complete.cases(benchmarkData[,'GICS_1']),]
  } else if(attributionType == 'Sec_Group-Type') {
    benchmarkData <- benchmarkData %>%
      mutate(Sec_Group = ifelse(Sec_Group == 'CASH', 'Other', Sec_Group))
    benchmarkData <- benchmarkData %>%
      mutate(Sec_Type = ifelse(Sec_Type == 'CASH', 'Other', Sec_Type))
    
    missing <- benchmarkData[(is.na(benchmarkData$Sec_Group) & benchmarkData$Sec_Type != 'EQUITY'),]
    missing <- missing[(!complete.cases(missing[,'Sec_Group']) & complete.cases(missing[,'Aladdin ID'])),]
    if(nrow(missing) > 0) {
      print('The following are missing Sec_Group data in the AGG Benchmark')
      print(missing)
      print('Update bond data')
    } 
    benchmarkData <- benchmarkData[complete.cases(benchmarkData[,'Sec_Group']),]
  } else {
    print('Attribution type is unknown for :')
    print(benchmarkData$Fund[1])
  }
  
  #print('Working on benchmark')
  #print(benchmarkData$Fund[1])
  
  return(benchmarkData)
}

fillBenchmarkCountryData <- function(benchmarkData, benchmark, addlCountryData) {
  tempCountry <- select(addlCountryData, c('AladdinID', 'AddlCountry'))
  benchmarkData <- benchmarkData %>%
    left_join(tempCountry, by=c('Aladdin ID'='AladdinID'))
  benchmarkData <- benchmarkData %>%
    mutate(Country = ifelse((Country == '' | is.na(Country)), AddlCountry, Country))
  benchmarkData$AddlCountry <- NULL
  benchmarkData <- benchmarkData[!duplicated(benchmarkData),]
  benchmarkData <- benchmarkData %>%
    mutate(Country = ifelse(Sec_Group == 'CASH', 'Other', Country))
  
  usIndices <- c('R1K', 'R1KG', 'R1KV', 'R3K', 'R2K', 'R2KG', 'R2KV', 'RMCG', 'RMCV')
  isUSBenchmark <- ifelse(benchmark %in% usIndices, TRUE, FALSE)
  if(isUSBenchmark) {
    benchmarkData <- benchmarkData %>%
      mutate(Country= ifelse((is.na(Country) & Sec_Group=='EQUITY'), 'US', Country))
  } else {}
  
  
  
  # if(!supressWarnings) {
  #   missing <- accountData[(accountData$Sec_Group=='EQUITY' & is.na(accountData$Country)),]
  #   if(nrow(missing)>0) {
  #     print('The following holdings are missing country data. Probably because they are ETFs.')
  #     print(missing)
  #     print('Update Addl Country Data file if applicable.')
  #   }
  benchmarkData <- benchmarkData[!duplicated(benchmarkData$`Aladdin ID`),]
  
  return(benchmarkData)
}

adjustBenchmarkTable <- function(benchmarkData) {
  benchmarkData <- benchmarkData %>%
    mutate(Country= ifelse(is.na(Country), Level2, Country))
  benchmarkData <- benchmarkData %>%
    mutate(Level2= ifelse(Country != Level2, Country, Level2))
  
  
  
  
  return(benchmarkData)
}

sortBenchmark <- function(benchmarkData) {
  bondTypes <- c('ABS', 'ARM', 'BND', 'CMBS', 'CMO', 'MBS', 'IBND', 'LOAN')
  benchmarkData <- benchmarkData %>%
    mutate(assetClass= ifelse(Sec_Group %in% bondTypes, 'bond', NA))
  benchmarkData <- benchmarkData %>%
    mutate(assetClass= ifelse(Sec_Group=='EQUITY' & (Country=='US' | Country=='PR'), 'US', assetClass))
  benchmarkData <- benchmarkData %>%  
    mutate(assetClass= ifelse(Sec_Group=='EQUITY' & Country!='US' & Country!='PR', 'intl', assetClass))
  benchmarkData <- benchmarkData %>%  
    mutate(assetClass= ifelse(is.na(assetClass), 'Other', assetClass))
  benchmarkData <- benchmarkData[!duplicated(benchmarkData),]
  return(benchmarkData)
}

fillBenchmarkCompanyData <- function(benchmarkData, addlCompanyData) {
  suppressWarnings <- FALSE
  addlCompanyData <- addlCompanyData %>% select(AladdinID, AddlGICS_1, AddlGICS_2)
  temp <- benchmarkData[benchmarkData$assetClass=='US',]
  temp[is.na(temp)] <- ''
  benchmarkData <- benchmarkData[!benchmarkData$assetClass=='US',]
  temp <- temp %>%
    left_join (addlCompanyData, by=c('Aladdin ID'= 'AladdinID'))
  temp <- temp %>%
    mutate(GICS_1 = ifelse(GICS_1=='' & Sec_Group=='EQUITY', AddlGICS_1, GICS_1)) %>%
    mutate(GICS_2 = ifelse(GICS_2=='' & Sec_Group=='EQUITY', AddlGICS_2, GICS_2))
  temp$AddlGICS_1 <- NULL
  temp$AddlGICS_2 <- NULL
  
  if(!suppressWarnings) {
    missing <- temp[(temp$GICS_1 == '' | temp$GICS_2 == ''),]
    if(nrow(missing) > 0) {
      print('The following US companies are missing GICS data.')
      print(missing)
      print('Update benchmark addl company data file')
    } 
  }
  benchmarkData <- rbind(benchmarkData, temp)
  return(benchmarkData)
}


# for(benchmark in benchmarks) {
#   benchmarkData <- filter(benchmarksData, Fund==benchmark)
#   key <- benchmarkKeys$`Aladdin ID`[benchmarkKeys$'Fund'==benchmark]
#   reportedRet <- benchmarkData$Ret_Cont[benchmarkData$'Aladdin ID'==key]
#   reportedNMV <- benchmarkData$NMV[benchmarkData$'Aladdin ID'==key]
#   print(reportedRet)
#   print(reportedNMV)
# }
