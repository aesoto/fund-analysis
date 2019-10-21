# functions for cleaning accounts

cleanAccountsData <- function(accountsData, fundsData, countryData, addlCountryData, addlCompanyData, benchmarks, investments, etfData, futuresData) {
  supressWarnings <- FALSE
  result <- list()
  for(account in fundsData$Account) {
    #print(paste0('Working on account ', account))
    
    
    
    fundMV <- getAUM(account, fundsData)
    fundBenchData <- getFundBenchData(account, fundsData, benchmarks)
    accountData <- setAccountData(account, accountsData) # must be here as expense items are reconciled
    adjustments <- setAdjustmentsData(accountData, countryData, addlCountryData, addlCompanyData, supressWarnings)
    
    accountData <- cleanAccount(account, accountData, fundMV, investments, 
                                countryData, addlCountryData, addlCompanyData, etfData, benchmarks,
                                futuresData, supressWarnings)
    derivativeAdjustments <- getDerivativeAdjustments(accountData)
    accountData <- removeDerivativeAdjustments(accountData)
    adjustments <- addDerivativeAdjustments(derivativeAdjustments, adjustments)
    
    accountData <- setFundReturns(accountData, fundMV)
    
    fundData <- list(benchData=fundBenchData, 
                     accountData=accountData, 
                     adjustments=adjustments, 
                     fundMV=fundMV, 
                     investments=investments)
    account <- as.character(account)
    result[[account]] <- fundData
  }
  return(result)
}

setAccountData <- function(account, accountsData) {
  accountData <- getFundData(account, accountsData)
  accountData <- putIntoDecimal(account, accountData) # must be kept here so that adjustments are accurate
  # an options adjustment needs to be added here....the top line NMV will change....
  accountData <- optionsAdjustment(account)
  
  return(accountData)
}

setAdjustmentsData <- function(accountData, countryData, addlCountryData, addlCompanyData, supressWarnings) {
  adjustments <- getAdjustments(accountData)
  adjustments <- parseAdjustments(adjustments, countryData, addlCountryData, addlCompanyData, supressWarnings)
  return(adjustments)
}

cleanAccount <- function(account, accountData, fundMV, investments, 
                         countryData, addlCountryData, addlCompanyData, etfData, benchmarks,
                         futuresData, supressWarnings) {
  accountData <- removeFundNoise(accountData)
  accountData <- parseAccount(accountData, investments, countryData, addlCountryData, addlCompanyData, etfData, benchmarks, futuresData, supressWarnings)
  return(accountData)
}

parseAccount <- function(accountData, investments, countryData, addlCountryData, addlCompanyData, etfData, benchmarks, futuresData, supressWarnings, adjustments) {
  accountData <- setMoneyMarket(accountData, investments) 
  accountData <- fillCountryData(accountData, addlCountryData, supressWarnings) # must be in this order to properly sort...
  accountData <- sortAssets(accountData)
  accountData <- fillCompanyData(accountData, addlCompanyData, supressWarnings)
  
  accountData <- mapBonds(accountData)
  accountData <- consolidatePositions(accountData)
  accountData <- setAccountLevels(accountData, countryData, supressWarnings)
  # map futures and ETFs
  accountData <- mapETFs(accountData, etfData, benchmarks, addlCountryData, addlCompanyData, supressWarnings)
  accountData <- mapIndexFutures(accountData, futuresData, benchmarks, addlCountryData, addlCompanyData, supressWarnings)
  # map options
  accountData <- mapOptions(accountData, benchmarks, addlCountryData, addlCompanyData, supressWarnings)
  
  accountData <- consolidatePositionsWithNMVPct(accountData)
  return(accountData)
}

mapOptions <- function(accountData, benchmarks, addlCountryData, addlCompanyData, supressWarnings) {
  fundNMV <- sum(accountData$`BOD NMV`)
  benchmarkNames <- names(benchmarks)
  optionsData <- data.frame(benchmark='SPX',
                            keyword='SPX',
                            stringsAsFactors = FALSE)
  accountIDs <- accountData[,c('Aladdin ID', 'ID')]
  allOptions <- accountData[(accountData$Sec_Group=='OPTION' & accountData$Sec_Type=='EQUITY'),]
  #allOptions <- consolidatePositions(allOptions)
  
  word <- 'SPX'
  tempOptions <- filter(allOptions, grepl(word, Sec_Des)) 
  
  if(nrow(allOptions) != nrow(tempOptions)) { print('There is a non-SPX Option. Must be re-categorized') }
  
  options <- options[!duplicated(options),]
  dupes <- dupes[!duplicated(dupes),]
  
  # remove just your selected options from accountData
  accountData <- rbind(accountData, dupes)
  accountData <- accountData[!(duplicated(accountData) | duplicated(accountData, fromLast = TRUE)), ]
  #accountData <- accountData[!duplicated(accountData,fromLast = FALSE) & !duplicated(accountData,fromLast = TRUE),] 
  
  # consolidate indexFutures
  options <- consolidateOptions(options)
  
  NMV <- options$`BOD NMV`[1]
  NMVPct <- options$NMVPct[1]
  totPctCont <- options$TotPctCont[1]
  tempBench <- 'SPX'
  tempParent <- options$Parent_ID[1]
  tempFund <- options$Fund[1]
  index <- 'SPX'
  
  tempBenchData <- benchmarks[[tempBench]]
  indexReturn <- tempBenchData$totRetCont
  tempBenchData <- tempBenchData$benchmarkData
  tempBenchData$NMV <- tempBenchData$NMVPct * NMV
  tempBenchData$Ret_Cont <- tempBenchData$Ret * tempBenchData$NMV / fundNMV
  tempBenchData$NMVPct <- tempBenchData$NMVPct * NMVPct
  
  optionsPlug <- totPctCont - sum(tempBenchData$Ret_Cont)
  
  tempBenchData$Parent_ID <- tempParent
  tempBenchData$Fund <- tempFund
  
  tempBenchData <- tempBenchData %>%
    select(Fund, everything())
  tempBenchData <- tempBenchData %>%
    select(Parent_ID, everything())
  
  tempBenchData <- tempBenchData %>%
    select(-NMV, -NMVPct, -Ret, everything())
  tempBenchData <- tempBenchData %>%
    select(-Ret_Cont, everything())
  
  names(tempBenchData)[colnames(tempBenchData)=='Ret_Cont'] <- 'TotPctCont'
  names(tempBenchData)[colnames(tempBenchData)=='NMV'] <- 'BOD NMV'
  tempBenchData <- tempBenchData %>%
    mutate(Country= ifelse(Country=='', NA, Country))
  
  tempBenchData <- tempBenchData %>%
    left_join(accountIDs, by='Aladdin ID')
  tempBenchData <- tempBenchData[!duplicated(tempBenchData),]
  
  tempBenchData <- tempBenchData %>%
    select(Parent_ID, Fund, ID, everything())
  
  adjustmentData <- options
  adjustmentData$TotPctCont <- optionsPlug
  adjustmentData$`BOD NMV` <- 0
  adjustmentData$Sec_Des <- paste0(adjustmentData$Sec_Des, ' - Options Adjustment')
  adjustmentData$Ret <- 0
  adjustmentData$NMVPct <- 0
  adjustmentData$GICS_1 <- NA
  adjustmentData$GICS_2 <- NA
  
  tempBenchData <- rbind(setDT(tempBenchData), setDT(adjustmentData), fill=TRUE)
  
  tempBenchData <- tempBenchData %>%
    mutate(ID= ifelse(is.na(ID), `Aladdin ID`, ID))
  
  accountData <- rbind(accountData, tempBenchData)
  return(accountData)
}



# ############################  
#   mapData <- data.frame(stringsAsFactors = FALSE)
#   for(option in options$'Aladdin ID') {
#     NMV <- options$`BOD NMV`[options$`Aladdin ID`==option]
#     NMVPct <- options$NMVPct[options$`Aladdin ID`==option]
#     totPctCont <- options$TotPctCont[options$`Aladdin ID`==option]
#     tempBench <- options$Index[options$`Aladdin ID`==option]
#     tempParent <- options$Parent_ID[options$`Aladdin ID`==option]
#     tempFund <- options$Fund[options$`Aladdin ID`==option]
#     index <- options$Index[options$`Aladdin ID`==option]
#     
#     tempBenchData <- benchmarks[[tempBench]]
#     indexReturn <- tempBenchData$totRetCont
#     tempBenchData <- tempBenchData$benchmarkData
#     tempBenchData$NMV <- tempBenchData$NMVPct * NMV
#     tempBenchData$Ret_Cont <- tempBenchData$Ret * tempBenchData$NMV / fundNMV
#     tempBenchData$NMVPct <- tempBenchData$NMVPct * NMVPct
#     
#     #tempBenchData$NMVPctTemp <- tempBenchData$NMVPct
#     # tempBenchData$NMVPct <- tempBenchData$NMV / fundMV
#     
#     optionsPlug <- totPctCont - sum(tempBenchData$Ret_Cont)
#     #tempBenchData$futuresAdj <- tempBenchData$NMVPctTemp * futuresPlug
#     #tempBenchData$NMVPctTemp <- NULL
#     
#     tempBenchData$Parent_ID <- tempParent
#     tempBenchData$Fund <- tempFund
#     
#     tempBenchData <- tempBenchData %>%
#       select(Fund, everything())
#     tempBenchData <- tempBenchData %>%
#       select(Parent_ID, everything())
#     
#     tempBenchData <- tempBenchData %>%
#       select(-NMV, -NMVPct, -Ret, everything())
#     tempBenchData <- tempBenchData %>%
#       select(-Ret_Cont, everything())
#     
#     names(tempBenchData)[colnames(tempBenchData)=='Ret_Cont'] <- 'TotPctCont'
#     names(tempBenchData)[colnames(tempBenchData)=='NMV'] <- 'BOD NMV'
#     tempBenchData <- tempBenchData %>%
#       mutate(Country= ifelse(Country=='', NA, Country))
#     
#     tempBenchData <- tempBenchData %>%
#       left_join(accountIDs, by='Aladdin ID')
#     tempBenchData <- tempBenchData[!duplicated(tempBenchData),]
#     
#     tempBenchData <- tempBenchData %>%
#       select(Parent_ID, Fund, ID, everything())
#     
#     adjustmentData <- options[options$`Aladdin ID`==option,]
#     adjustmentData$TotPctCont <- optionsPlug
#     adjustmentData$`BOD NMV` <- 0
#     adjustmentData$Sec_Des <- paste0(adjustmentData$Sec_Des, ' - Derivative Adjustment')
#     adjustmentData$Ret <- 0
#     adjustmentData$NMVPct <- 0
#     adjustmentData$Index <- NULL
#     adjustmentData$GICS_1 <- NA
#     adjustmentData$GICS_2 <- NA
#     
#     tempBenchData <- rbind(setDT(tempBenchData), setDT(adjustmentData), fill=TRUE)
#     
#     mapData <- rbind(mapData, tempBenchData)
#   }
#   mapData <- mapData %>%
#     mutate(ID= ifelse(is.na(ID), `Aladdin ID`, ID))
#   
#   accountData <- rbind(accountData, mapData)
#   return(accountData)
# }

getAUM <- function(account, fundsData) {
  MV <- fundsData$AUM[fundsData$Account == account]
  return(MV)
}

getFundBenchData <- function(account, fundsData, benchmarks) {
  temp <- fundsData[fundsData$Account==account,]
  temp <- temp %>% mutate_all(na_if,'')
  fundBenchmark <- data.frame(Bench=c(temp$Benchmark1, temp$Benchmark2, temp$Benchmark3),
                              AttribType=c(temp$AttribType1, temp$AttribType2, temp$AttribType3),
                              Wt=c(temp$BenchWt1, temp$BenchWt2, temp$BenchWt3),
                              stringsAsFactors = FALSE)
  tempDF <- data.frame(Ret=c(benchmarks[[temp$Benchmark1]]$totRetCont,
                             benchmarks[[temp$Benchmark2]]$totRetCont,
                             benchmarks[[temp$Benchmark3]]$totRetCont), stringsAsFactors = FALSE)
  fundBenchmark <- cbind.fill(fundBenchmark, tempDF, fill=NA)
  fundBenchmark <- fundBenchmark[complete.cases(fundBenchmark),]
  fundBenchmark$WtRet <- fundBenchmark$Wt * fundBenchmark$Ret
  fundBenchmark <- fundBenchmark %>% mutate_if(is.factor, as.character)
  return(fundBenchmark)
}

getFundData <- function(account, accountsData) {
  accountData <- filter(accountsData, Parent_ID == account)
  return(accountData)
}

removeFundNoise <- function(accountData) {
  accountData <- accountData[!((accountData$`BOD NMV`==0) & (accountData$TotPctCont!=0)),] # adjustment items, stored earlier
  accountData$TotPctCont[accountData$TotPctCont == 0 & accountData$'BOD NMV' == 0] <- NA # empty observations with no information
  
  #accountData <- accountData[complete.cases(accountData),]
  accountData <- accountData[!(is.na(accountData$TotPctCont) & accountData$`BOD NMV`==0),]
  
  totNMV <- sum(accountData$`BOD NMV`)
  
  accountData <- accountData %>%
    mutate(Ret= ifelse(is.na(Ret) & TotPctCont==0, 0, Ret))
  
  accountData <- accountData %>%
    mutate(Ret= ifelse(is.na(Ret), TotPctCont/`BOD NMV`/totNMV, Ret))
  
  accountData <- accountData %>%
    mutate(Sec_Des = ifelse(`Aladdin ID`=='', ID, Sec_Des))
  accountData <- accountData %>%
    mutate(`Aladdin ID` = ifelse(`Aladdin ID`=='', ID, `Aladdin ID`))
  return(accountData)
}

setFundReturns <- function(accountData, fundMV) {
  account <- accountData$Parent_ID[1]
  # return calculated first to ensure integrity of metric
  colnames(accountData)[colnames(accountData)=='BOD NMV'] <- 'BOD_NMV'
  totalNMV <- sum(accountData$BOD_NMV) 
  
  # you are here.....
  # Goal: calculate a return that is internally consistent with the portfolio, may be used for price adjustment later
  accountData$impliedRet <- accountData$TotPctCont * totalNMV / accountData$BOD_NMV 
  
  #accountData$tempWt <- NULL
  accountData$Ret[is.na(accountData$impliedRet)] <- 0
  accountData$Ret[is.infinite(accountData$impliedRet)] <- 0
  
  #marginPct <- 1 - totalNMV/fundMV
  marginBal <- fundMV - totalNMV
  marginData <- data.table(Parent_ID=account,
                           Fund=account,
                           ID='Margin',
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
                           BOD_NMV=marginBal,
                           Ret=0,
                           NMVPct=marginBal/fundMV,
                           TotPctCont=0,
                           impliedRet=0
  )
  accountData$NMVPct <- accountData$BOD_NMV / fundMV
  
  #accountData$impliedRet2 <- accountData$Ret_Cont / accountData$NMVPct
  
  accountData <- rbind(accountData, marginData)
  #newTotalNMV <- sum(accountData$BOD_NMV)
  #accountData$NMVPct <- accountData$BOD_NMV / newTotalNMV
  #accountData$TotPctCont <- accountData$TotPctCont * (newTotalNMV / totalNMV) 
  #accountData$Ret <- accountData$TotPctCont / accountData$NMVPct
  
  names(accountData)[names(accountData) == 'TotPctCont'] <- 'Ret_Cont'
  return(accountData)
}

parseAccountData <- function(category, accountData) {
  # obtain data by category
  # perform require adjustments
  return(accountData)
}

getAdjustments <- function(accountData) {
  adjItems <- accountData[(accountData$`BOD NMV`==0) & (accountData$TotPctCont!=0),]
  #adjItems <- adjItems %>% select(Parent_ID, Fund, ID, 'Aladdin ID', Sec_Des, TotPctCont)
  adjItems$`BOD NMV` <- NULL
  adjEffect <- sum(adjItems$TotPctCont)
  #adjItems <- sortAdjItems(adjItems)
  adjustments <- list(Items=adjItems, Effect=adjEffect)
  return(adjustments)
}

parseAdjustments <- function(adjustments, countryData, addlCountryData, addlCompanyData, supressWarnings) {
  adjEffect <- adjustments$Effect
  adjItems <- adjustments$Items
  temp <- adjItems %>% group_by(ID) %>% summarize(ConslTotPctCont=sum(TotPctCont))
  adjItems <- adjItems %>% full_join(temp, by='ID')
  adjItems$TotPctCont <- adjItems$ConslTotPctCont
  adjItems$ConslTotPctCont <- NULL
  adjItems$Parent_ID <- NULL
  adjItems$Fund <- NULL
  adjItems <- adjItems[!duplicated(adjItems),]
  
  adjItems <- fillCountryData(adjItems, addlCountryData, supressWarnings) # must be in this order to properly sort...
  adjItems <- sortAssets(adjItems)
  adjItems <- fillCompanyData(adjItems, addlCompanyData, supressWarnings)
  #adjItems <- consolidatePositions(adjItems)
  adjItems <- setAdjustmentLevels(adjItems, countryData, supressWarnings)
  
  adjustments <- list(Items=adjItems, Effect=adjEffect)
  return(adjustments)
}

setAccountLevels <- function(accountData, countryData, supressWarnings) {
  assetClasses <- c('intl', 'US', 'bond', 'Other')
  
  #result <- data.frame()
  for(class in assetClasses) {
    if(class=='intl') {
      temp <- accountData %>% 
        filter(assetClass==class) %>% 
        select('Aladdin ID', Country)
      temp <- merge(x = temp, y = countryData,
                    by.x = 'Country', by.y = 'CountryCd',
                    all.x = TRUE)
      temp$Country <- NULL
      
      if(!supressWarnings) {
        missing <- temp[is.na(temp$Level1),]
        if(nrow(missing)>0) {
          print('The following are missing country attribution data')
          print('Update the countryData file if needed.')
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
      
      accountData <- merge(x=accountData, y=temp,
                           by.x='Aladdin ID', by.y='Aladdin ID',
                           all.x=TRUE)
    } else if (class=='US') {
      accountData <- accountData %>%
        mutate(Level1= ifelse((Country=='US' | Country=='PR'), 'Equity', Level1)) %>%
        mutate(Level2= ifelse((Country=='US' | Country=='PR'), 'US', Level2)) %>%
        mutate(Level3= ifelse((Country=='US' | Country=='PR'), GICS_1, Level3)) %>%
        mutate(Level4= ifelse((Country=='US' | Country=='PR'), GICS_2, Level4))
      
      temp <- accountData %>% 
        filter(assetClass==class) %>% 
        select('Aladdin ID', Sec_Des, GICS_1, GICS_2)
      
      if(!supressWarnings) {
        missing <- temp[(is.na(temp$GICS_1) | is.na(temp$GICS_2)),]
        if(nrow(missing)>0) { 
          print('The following are missing bond attribution data')
          print('Update the core data file if needed.')
        }
      }
    } else if (class=='bond') {
      accountData <- accountData %>%
        mutate(Level1= ifelse(assetClass=='bond', 'Fixed Income', Level1)) %>%
        mutate(Level2= ifelse(assetClass=='bond', 'Fixed Income', Level2)) %>%
        mutate(Level3= ifelse(assetClass=='bond', Sec_Group, Level3)) %>%
        mutate(Level4= ifelse(assetClass=='bond', Sec_Type, Level4))
      
      temp <- accountData %>% 
        filter(assetClass==class) %>% 
        select('Aladdin ID', Sec_Des, Sec_Group, Sec_Type)
      
      if(!supressWarnings) {
        missing <- temp[(is.na(temp$Sec_Group) | is.na(temp$Sec_Type)),]
        if(nrow(missing)>0) { 
          print('The following are missing bond attribution data')
          print('Update the core data file if needed.')
        }
      }
    } else if (class=='Other') {
      accountData$Level1[accountData$assetClass=='Other' & accountData$Sec_Group != 'CASH'] <- 'Other'
      accountData$Level2[accountData$assetClass=='Other' & accountData$Sec_Group != 'CASH'] <- 'Other'
      accountData$Level3[accountData$assetClass=='Other' & accountData$Sec_Group != 'CASH'] <- 'Other'
      accountData$Level4[accountData$assetClass=='Other' & accountData$Sec_Group != 'CASH'] <- 'Other'
      
      accountData <- accountData %>%
        mutate(Level1= ifelse(Sec_Group=='CASH', 'Other', Level1)) %>%
        mutate(Level2= ifelse(Sec_Group=='CASH', 'Cash', Level2)) %>%
        mutate(Level3= ifelse(Sec_Group=='CASH', 'Cash', Level3)) %>%
        mutate(Level4= ifelse(Sec_Group=='CASH', 'Cash', Level4))
      
      accountData <- accountData %>%
        mutate(Level1= ifelse(Sec_Group=='CASH' & Sec_Type=='CASH', 'Other', Level1)) %>%
        mutate(Level2= ifelse(Sec_Group=='CASH' & Sec_Type=='CASH', 'Cash', Level2)) %>%
        mutate(Level3= ifelse(Sec_Group=='CASH' & Sec_Type=='CASH', 'Cash', Level3)) %>%
        mutate(Level4= ifelse(Sec_Group=='CASH' & Sec_Type=='CASH', 'Cash', Level4))
      
      accountData <- accountData %>%
        mutate(Level1= ifelse(Sec_Group=='FUND' & Sec_Type=='STIF', 'Other', Level1)) %>%
        mutate(Level2= ifelse(Sec_Group=='FUND' & Sec_Type=='STIF', 'Cash', Level2)) %>%
        mutate(Level3= ifelse(Sec_Group=='FUND' & Sec_Type=='STIF', 'Cash', Level3)) %>%
        mutate(Level4= ifelse(Sec_Group=='FUND' & Sec_Type=='STIF', 'Cash', Level4))
    }
  }
  
  accountData <- accountData %>%
    mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  
  totalNMV <- sum(accountData$`BOD NMV`)
  accountData$NMVPct <- accountData$`BOD NMV` / totalNMV
  
  accountData <- accountData %>%
    select(Parent_ID, Fund, ID, everything())
  accountData <- accountData %>%
    select(-'BOD NMV', -NMVPct, -Ret, everything())
  accountData <- accountData %>%
    select(-TotPctCont, everything())
  
  return(accountData)
}


setMoneyMarket <- function(accountData, investments) {
  moneyFunds <- investments$Account[investments$Benchmark1=='MM']
  temp <- accountData %>% group_by(Fund) %>% summarise(TotPctCont=sum(TotPctCont), `BOD NMV`=sum(`BOD NMV`))
  temp <- temp[temp$Fund %in% moneyFunds,]
  
  if(nrow(temp) > 0) {
    for (fund in moneyFunds) {
      parentID <- accountData$Parent_ID[1]
      # print('Consolidating money market')
      # print(parentID)
      tempDF <- data.frame(Parent_ID=parentID, 
                           Fund=fund, ID='Money Market', 
                           "Aladdin ID"='Money Market',
                           TotPctCont=temp$TotPctCont[temp$Fund==fund], 
                           "BOD NMV"=temp$`BOD NMV`[temp$Fund==fund], 
                           Sec_Des='Money Market',
                           Sec_Group='CASH', 
                           Sec_Type='CASH', 
                           GICS_1='Other', 
                           GICS_2='Other', 
                           Country='US', 
                           Ret=0, 
                           stringsAsFactors = FALSE)
      colnames(tempDF)[colnames(tempDF)=='Aladdin.ID'] <- 'Aladdin ID'
      colnames(tempDF)[colnames(tempDF)=='BOD.NMV'] <- 'BOD NMV'
      accountData <- accountData[!accountData$Fund==fund,]
      
      accountData <- rbind(accountData, tempDF)
    }
  }
  return(accountData)
}

consolidatePositions <- function(portfolio) {
  temp <- portfolio[,c('Aladdin ID', 'TotPctCont', 'BOD NMV')]
  temp <- temp %>% group_by(`Aladdin ID`) %>% summarize_each(list(~sum(.)))
  # temp <- temp %>% group_by(`Aladdin ID`) %>% summarize_each(funs(sum)) #n.b. positions must be grouped by Aladdin ID as it is unique. Not the case with ID
  temp <- data.frame(temp, stringsAsFactors = FALSE)
  names(temp) <- c('Aladdin ID', 'ConsTotPctCont', 'ConsBOD_NMV')
  
  portfolio <- portfolio %>%
    left_join(temp, by='Aladdin ID')
  portfolio <- portfolio %>%
    mutate(TotPctCont = ifelse(TotPctCont==ConsTotPctCont, TotPctCont, ConsTotPctCont)) %>%
    mutate(`BOD NMV` = ifelse('BOD NMV'==ConsBOD_NMV, `BOD NMV`, ConsBOD_NMV))
  portfolio$ConsTotPctCont <- NULL
  portfolio$ConsBOD_NMV <- NULL
  portfolio <- portfolio[!duplicated(portfolio$`Aladdin ID`),]
  return(portfolio)
}

consolidatePositionsWithNMVPct <- function(portfolio) {
  temp <- portfolio[,c('Aladdin ID', 'TotPctCont', 'BOD NMV', 'NMVPct')]
  temp <- temp %>% group_by(`Aladdin ID`) %>% summarize_each(list(~sum(.)))
  # temp <- temp %>% group_by(`Aladdin ID`) %>% summarize_each(funs(sum)) #n.b. positions must be grouped by Aladdin ID as it is unique. Not the case with ID
  temp <- data.frame(temp, stringsAsFactors = FALSE)
  names(temp) <- c('Aladdin ID', 'ConsTotPctCont', 'ConsBOD_NMV', 'ConsNMVPct')
  
  portfolio <- portfolio %>%
    left_join(temp, by='Aladdin ID')
  portfolio <- portfolio %>%
    mutate(TotPctCont = ifelse(TotPctCont==ConsTotPctCont, TotPctCont, ConsTotPctCont)) %>%
    mutate(`BOD NMV` = ifelse('BOD NMV'==ConsBOD_NMV, `BOD NMV`, ConsBOD_NMV)) %>%
    mutate(NMVPct= ifelse(NMVPct==ConsNMVPct, NMVPct, ConsNMVPct))
  portfolio$ConsTotPctCont <- NULL
  portfolio$ConsBOD_NMV <- NULL
  portfolio$ConsNMVPct <- NULL
  portfolio <- portfolio[!duplicated(portfolio$`Aladdin ID`),]
  return(portfolio)
}

fillCountryData <- function(accountData, addlCountryData, supressWarnings) {
  tempCountry <- select(addlCountryData, c('AladdinID', 'AddlCountry'))
  accountData <- accountData %>%
    left_join(tempCountry, by=c('Aladdin ID'='AladdinID'))
  accountData <- accountData %>%
    mutate(Country= ifelse((Country == '' | is.na(Country)), AddlCountry, Country))
  accountData <- accountData %>%
    mutate(Country= ifelse((!is.na(Country) & !is.na(AddlCountry) & AddlCountry != Country), AddlCountry, Country))
  
  accountData$AddlCountry <- NULL
  accountData <- accountData[!duplicated(accountData),]
  if(!supressWarnings) {
    missing <- accountData[(accountData$Sec_Group=='EQUITY' & is.na(accountData$Country)),]
    if(nrow(missing)>0) {
      print('The following holdings are missing country data. Probably because they are ETFs.')
      print(missing)
      print('Update Addl Country Data file if applicable.')
    }
  }
  return(accountData)
}

fillCompanyData <- function(accountData, addlCompanyData, supressWarnings) {
  addlCompanyData <- addlCompanyData %>% select(AladdinID, AddlGICS_1, AddlGICS_2)
  temp <- accountData[accountData$assetClass=='US',]
  temp[is.na(temp)] <- ''
  accountData <- accountData[!accountData$assetClass=='US',]
  temp <- temp %>%
    left_join (addlCompanyData, by=c('Aladdin ID'= 'AladdinID'))
  temp <- temp %>%
    mutate(GICS_1 = ifelse(GICS_1=='' & Sec_Group=='EQUITY', AddlGICS_1, GICS_1)) %>%
    mutate(GICS_2 = ifelse(GICS_2=='' & Sec_Group=='EQUITY', AddlGICS_2, GICS_2))
  temp$AddlGICS_1 <- NULL
  temp$AddlGICS_2 <- NULL
  
  if(!supressWarnings) {
    missing <- temp[(temp$GICS_1 == '' | temp$GICS_2 == ''),]
    if(nrow(missing) > 0) {
      print('The following US companies are missing GICS data.')
      print(missing)
      print('Update account addl company data file')
    } 
  }
  accountData <- rbind(accountData, temp)
  return(accountData)
}

sortAssets <- function(accountData) {
  bondTypes <- c('ABS', 'ARM', 'BND', 'CMBS', 'CMO', 'MBS', 'IBND', 'LOAN')
  accountData <- accountData %>%
    mutate(assetClass= ifelse(Sec_Group %in% bondTypes, 'bond', NA))
  accountData <- accountData %>%
    mutate(assetClass= ifelse(Sec_Group=='EQUITY' & (Country=='US' | Country=='PR'), 'US', assetClass))
  accountData <- accountData %>%  
    mutate(assetClass= ifelse(Sec_Group=='EQUITY' & Country!='US' & Country!='PR', 'intl', assetClass))
  accountData <- accountData %>%  
    mutate(assetClass= ifelse(is.na(assetClass), 'Other', assetClass))
  accountData <- accountData[!duplicated(accountData),]
  return(accountData)
}

putIntoDecimal <- function(account, accountData) {
  # The account file stores contribution in percent form, so it is off by a factor of 100 
  # compared to the benchmark.  The exception is the expenses line which for some reason is in decimal form.
  # Further customized adjustments may be placed in this function
  parentExpense <- accountData[(accountData$Parent_ID==account & accountData$Fund==account & accountData$ID=='EXPENSES'),]
  accountData <- accountData[!(accountData$Parent_ID==account & accountData$Fund==account & accountData$ID=='EXPENSES'),]
  accountData$TotPctCont <- accountData$TotPctCont / 100
  accountData <- rbind(accountData, parentExpense)
  return(accountData)
}

# not used
consolidateETFs <- function(ETFs) {
  temp <- portfolio[,c('Aladdin ID', 'TotPctCont', 'BOD NMV')]
  temp <- temp %>% group_by(`Aladdin ID`) %>% summarize_each(list(~sum(.)))
  # temp <- temp %>% group_by(`Aladdin ID`) %>% summarize_each(funs(sum)) #n.b. positions must be grouped by Aladdin ID as it is unique. Not the case with ID
  temp <- data.frame(temp, stringsAsFactors = FALSE)
  names(temp) <- c('Aladdin ID', 'ConsTotPctCont', 'ConsBOD_NMV')
  
  portfolio <- portfolio %>%
    left_join(temp, by='Aladdin ID')
  portfolio <- portfolio %>%
    mutate(TotPctCont = ifelse(TotPctCont==ConsTotPctCont, TotPctCont, ConsTotPctCont)) %>%
    mutate(`BOD NMV` = ifelse('BOD NMV'==ConsBOD_NMV, `BOD NMV`, ConsBOD_NMV))
  portfolio$ConsTotPctCont <- NULL
  portfolio$ConsBOD_NMV <- NULL
  portfolio <- portfolio[!duplicated(portfolio$`Aladdin ID`),]
  return(portfolio)
}

mapETFs <- function(accountData, etfData, benchmarks, addlCountryData, addlCompanyData, supressWarnings) {
  fundNMV <- sum(accountData$`BOD NMV`)
  benchmarkNames <- names(benchmarks)
  etfIDs <- etfData[,c('Aladdin ID', 'Benchmark')]
  accountIDs <- accountData[,c('Aladdin ID', 'ID')]
  ETFs <- accountData[accountData$`Aladdin ID` %in% etfIDs$`Aladdin ID`,]
  ETFs <- consolidatePositions(ETFs)
  # remove ETFs from accountData
  accountData <- accountData[!(accountData$`Aladdin ID` %in% etfIDs$`Aladdin ID`),]
  
  mapData <- data.frame(stringsAsFactors = FALSE)
  for(ETF in ETFs$'Aladdin ID') {
    NMV <- ETFs$`BOD NMV`[ETFs$`Aladdin ID`==ETF]
    NMVPct <- ETFs$NMVPct[ETFs$`Aladdin ID`==ETF]
    totPctCont <- ETFs$TotPctCont[ETFs$`Aladdin ID`==ETF]
    tempBench <- etfData$Benchmark[etfData$`Aladdin ID`==ETF]
    tempParent <- ETFs$Parent_ID[ETFs$`Aladdin ID`==ETF]
    tempFund <- ETFs$Fund[ETFs$`Aladdin ID`==ETF]
    index <- etfIDs$Benchmark[etfIDs$`Aladdin ID`==ETF] #same as tempBench....
    
    tempBenchData <- benchmarks[[tempBench]]
    indexReturn <- tempBenchData$totRetCont
    tempBenchData <- tempBenchData$benchmarkData
    tempBenchData$NMV <- tempBenchData$NMVPct * NMV
    tempBenchData$Ret_Cont <- tempBenchData$Ret * tempBenchData$NMV / fundNMV
    tempBenchData$NMVPct <- tempBenchData$NMVPct * NMVPct
    
    etfPlug <- totPctCont - sum(tempBenchData$Ret_Cont)
    
    tempBenchData$Parent_ID <- tempParent
    tempBenchData$Fund <- tempFund
    
    tempBenchData <- tempBenchData %>%
      select(Fund, everything())
    tempBenchData <- tempBenchData %>%
      select(Parent_ID, everything())
    
    tempBenchData <- tempBenchData %>%
      select(-NMV, -NMVPct, -Ret, everything())
    tempBenchData <- tempBenchData %>%
      select(-Ret_Cont, everything())
    
    names(tempBenchData)[colnames(tempBenchData)=='Ret_Cont'] <- 'TotPctCont'
    names(tempBenchData)[colnames(tempBenchData)=='NMV'] <- 'BOD NMV'
    tempBenchData <- tempBenchData %>%
      mutate(Country= ifelse(Country=='', NA, Country))
    
    tempBenchData <- tempBenchData %>%
      left_join(accountIDs, by='Aladdin ID')
    tempBenchData <- tempBenchData[!duplicated(tempBenchData),]
    
    tempBenchData <- tempBenchData %>%
      select(Parent_ID, Fund, ID, everything())
    
    adjustmentData <- ETFs[ETFs$`Aladdin ID`==ETF,]
    adjustmentData$TotPctCont <- etfPlug
    adjustmentData$`BOD NMV` <- 0
    adjustmentData$Sec_Des <- paste0(adjustmentData$Sec_Des, ' - ETF Adjustment')
    adjustmentData$Ret <- 0
    adjustmentData$NMVPct <- 0
    adjustmentData$Index <- NULL
    adjustmentData$GICS_1 <- NA
    adjustmentData$GICS_2 <- NA
    
    tempBenchData <- rbind(setDT(tempBenchData), setDT(adjustmentData), fill=TRUE)
    
    mapData <- rbind(mapData, tempBenchData)
  }
  mapData <- mapData %>%
    mutate(ID= ifelse(is.na(ID), `Aladdin ID`, ID))
  
  accountData <- rbind(accountData, mapData)
  return(accountData)
}

mapIndexFutures <- function(accountData, futuresData, benchmarks, addlCountryData, addlCompanyData, supressWarnings) {
  fundNMV <- sum(accountData$`BOD NMV`)
  benchmarkNames <- names(benchmarks)
  accountIDs <- accountData[,c('Aladdin ID', 'ID')]
  allFutures <- accountData[(accountData$Sec_Group=='FUTURE' & accountData$Sec_Type=='INDEX'),]
  
  indexFutures <- data.frame()
  dupes <- data.frame()
  for(benchmark in benchmarkNames) {
    keyWords <- futuresData[futuresData$Index==benchmark,]
    keyWords$Index <- NULL
    keyWords <- as.vector(keyWords[1,])
    keyWords <- keyWords[, !apply(keyWords, 2, function(x) all(is.na(x)))]
    for(word in keyWords) {
      tempFutures <- filter(allFutures, grepl(word, Sec_Des))
      dupes <- rbind(dupes, tempFutures)
      if(nrow(tempFutures)>0) { tempFutures$Index <- benchmark }
      indexFutures <- rbind(indexFutures, tempFutures)
    }
  }
  indexFutures <- indexFutures[!duplicated(indexFutures),]
  dupes <- dupes[!duplicated(dupes),]
  
  # remove just your selected futures from accountData
  accountData <- rbind(accountData, dupes)
  accountData <- accountData[!(duplicated(accountData) | duplicated(accountData, fromLast = TRUE)), ]
  #accountData <- accountData[!duplicated(accountData,fromLast = FALSE) & !duplicated(accountData,fromLast = TRUE),] 
  
  # consolidate indexFutures
  indexFutures <- consolidatePositions(indexFutures)
  
  mapData <- data.frame(stringsAsFactors = FALSE)
  for(indexFuture in indexFutures$'Aladdin ID') {
    NMV <- indexFutures$`BOD NMV`[indexFutures$`Aladdin ID`==indexFuture]
    NMVPct <- indexFutures$NMVPct[indexFutures$`Aladdin ID`==indexFuture]
    totPctCont <- indexFutures$TotPctCont[indexFutures$`Aladdin ID`==indexFuture]
    tempBench <- indexFutures$Index[indexFutures$`Aladdin ID`==indexFuture]
    tempParent <- indexFutures$Parent_ID[indexFutures$`Aladdin ID`==indexFuture]
    tempFund <- indexFutures$Fund[indexFutures$`Aladdin ID`==indexFuture]
    index <- indexFutures$Index[indexFutures$`Aladdin ID`==indexFuture]
    
    tempBenchData <- benchmarks[[tempBench]]
    indexReturn <- tempBenchData$totRetCont
    tempBenchData <- tempBenchData$benchmarkData
    tempBenchData$NMV <- tempBenchData$NMVPct * NMV
    tempBenchData$Ret_Cont <- tempBenchData$Ret * tempBenchData$NMV / fundNMV
    tempBenchData$NMVPct <- tempBenchData$NMVPct * NMVPct
    
    #tempBenchData$NMVPctTemp <- tempBenchData$NMVPct
    # tempBenchData$NMVPct <- tempBenchData$NMV / fundMV
    
    futuresPlug <- totPctCont - sum(tempBenchData$Ret_Cont)
    #tempBenchData$futuresAdj <- tempBenchData$NMVPctTemp * futuresPlug
    #tempBenchData$NMVPctTemp <- NULL
    
    tempBenchData$Parent_ID <- tempParent
    tempBenchData$Fund <- tempFund
    
    tempBenchData <- tempBenchData %>%
      select(Fund, everything())
    tempBenchData <- tempBenchData %>%
      select(Parent_ID, everything())
    
    tempBenchData <- tempBenchData %>%
      select(-NMV, -NMVPct, -Ret, everything())
    tempBenchData <- tempBenchData %>%
      select(-Ret_Cont, everything())
    
    names(tempBenchData)[colnames(tempBenchData)=='Ret_Cont'] <- 'TotPctCont'
    names(tempBenchData)[colnames(tempBenchData)=='NMV'] <- 'BOD NMV'
    tempBenchData <- tempBenchData %>%
      mutate(Country= ifelse(Country=='', NA, Country))
    
    tempBenchData <- tempBenchData %>%
      left_join(accountIDs, by='Aladdin ID')
    tempBenchData <- tempBenchData[!duplicated(tempBenchData),]
    
    tempBenchData <- tempBenchData %>%
      select(Parent_ID, Fund, ID, everything())
    
    adjustmentData <- indexFutures[indexFutures$`Aladdin ID`==indexFuture,]
    adjustmentData$TotPctCont <- futuresPlug
    adjustmentData$`BOD NMV` <- 0
    adjustmentData$Sec_Des <- paste0(adjustmentData$Sec_Des, ' - Derivative Adjustment')
    adjustmentData$Ret <- 0
    adjustmentData$NMVPct <- 0
    adjustmentData$Index <- NULL
    adjustmentData$GICS_1 <- NA
    adjustmentData$GICS_2 <- NA
    
    tempBenchData <- rbind(setDT(tempBenchData), setDT(adjustmentData), fill=TRUE)
    
    mapData <- rbind(mapData, tempBenchData)
  }
  mapData <- mapData %>%
    mutate(ID= ifelse(is.na(ID), `Aladdin ID`, ID))
  
  accountData <- rbind(accountData, mapData)
  return(accountData)
}

mapBonds <- function(accountData) {
  # quick patch to change LQD from an equity to Fixed Income
  LQD <- accountData[accountData$`Aladdin ID`=='464287242',]
  accountData <- accountData[!accountData$`Aladdin ID`=='464287242',]
  if(nrow(LQD)>0) {
    LQD$Sec_Group <- 'BND'
    LQD$Sec_Type <- 'CORP'
    LQD$Country <- 'US'
    LQD$assetClass <- 'bond'
    accountData <- rbind(accountData, LQD)
  }
  
  bondKeywords <- c('NOTE', 'BOND', 'T-BOND')
  bondFutures <- data.frame()
  for(word in bondKeywords) {
    tempBondFutures <- filter(accountData, grepl(word, Sec_Des), grepl('US', Sec_Des),
                              !grepl('AUST', Sec_Des), assetClass=='Other', Sec_Group=='FUTURE')
    if(nrow(tempBondFutures)>0) { bondFutures <- rbind(bondFutures, tempBondFutures) }
  }
  bondFutures <- unique(bondFutures$`Aladdin ID`)
  
  accountData <- accountData %>%
    mutate(Sec_Group= ifelse(`Aladdin ID` %in% bondFutures, 'BND', Sec_Group)) %>%
    mutate(Sec_Type= ifelse(`Aladdin ID` %in% bondFutures, 'GOVT', Sec_Type)) %>%
    mutate(Country= ifelse(`Aladdin ID` %in% bondFutures, 'US', Country)) %>%
    mutate(assetClass= ifelse(`Aladdin ID` %in% bondFutures, 'bond', assetClass))
  return(accountData)
}

# not used
consolidateIndexFutures <- function(indexFutures) {
  temp <- portfolio[,c('Aladdin ID', 'TotPctCont', 'BOD NMV')]
  temp <- temp %>% group_by(`Aladdin ID`) %>% summarize_each(list(~sum(.)))
  # n.b. positions must be grouped by Aladdin ID as it is unique. Not the case with ID
  # temp <- temp %>% group_by(`Aladdin ID`) %>% summarize_each(funs(sum)) 
  temp <- data.frame(temp, stringsAsFactors = FALSE)
  names(temp) <- c('Aladdin ID', 'ConsTotPctCont', 'ConsBOD_NMV')
  
  portfolio <- portfolio %>%
    left_join(temp, by='Aladdin ID')
  portfolio <- portfolio %>%
    mutate(TotPctCont = ifelse(TotPctCont==ConsTotPctCont, TotPctCont, ConsTotPctCont)) %>%
    mutate(`BOD NMV` = ifelse('BOD NMV'==ConsBOD_NMV, `BOD NMV`, ConsBOD_NMV))
  portfolio$ConsTotPctCont <- NULL
  portfolio$ConsBOD_NMV <- NULL
  portfolio <- portfolio[!duplicated(portfolio$`Aladdin ID`),]
  return(portfolio)
}

tansformBenchmark <- function(benchmarks, tempBench, NMV, totPctCont) {
  tempBenchData <- benchmarks[[tempBench]]
  tempBenchData <- tempBenchData$benchmarkData
  tempBenchData$NMV <- tempBenchData$NMVPct * NMV
  tempBenchData$Ret_Cont <- tempBenchData$NMVPct * totPctCont
  names(tempBenchData)[colnames(tempBenchData)=='Ret_Cont'] <- 'TotPctCont'
  names(tempBenchData)[colnames(tempBenchData)=='NMV'] <- 'BOD NMV'
  tempBenchData <- tempBenchData %>%
    mutate(Country= ifelse(Country=='', NA, Country))
  return(tempBenchData)
}

setAdjustmentLevels <- function(adjItems, countryData, supressWarnings) {
  assetClasses <- c('intl', 'US', 'bond', 'Other')
  
  #result <- data.frame()
  for(class in assetClasses) {
    if(class=='intl') {
      temp <- adjItems %>% 
        filter(assetClass==class) %>% 
        select('Aladdin ID', Country)
      temp <- merge(x = temp, y = countryData,
                    by.x = 'Country', by.y = 'CountryCd',
                    all.x = TRUE)
      temp$Country <- NULL
      
      if(!supressWarnings) {
        missing <- temp[is.na(temp$Level1),]
        if(nrow(missing)>0) {
          print('The following are missing country attribution data')
          print('Update the countryData file if needed.')
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
      
      adjItems <- merge(x=adjItems, y=temp,
                        by.x='Aladdin ID', by.y='Aladdin ID',
                        all.x=TRUE)
    } else if (class=='US') {
      adjItems <- adjItems %>%
        mutate(Level1= ifelse((Country=='US' | Country=='PR'), 'Equity', Level1)) %>%
        mutate(Level2= ifelse((Country=='US' | Country=='PR'), 'US', Level2)) %>%
        mutate(Level3= ifelse((Country=='US' | Country=='PR'), GICS_1, Level3)) %>%
        mutate(Level4= ifelse((Country=='US' | Country=='PR'), GICS_2, Level4))
      
      temp <- adjItems %>% 
        filter(assetClass==class) %>% 
        select('Aladdin ID', Sec_Des, GICS_1, GICS_2)
      
      if(!supressWarnings) {
        missing <- temp[(is.na(temp$GICS_1) | is.na(temp$GICS_2)),]
        if(nrow(missing)>0) { 
          print('The following are missing bond attribution data')
          print('Update the core data file if needed.')
        }
      }
    } else if (class=='bond') {
      adjItems <- adjItems %>%
        mutate(Level1= ifelse(assetClass=='bond', 'Fixed Income', Level1)) %>%
        mutate(Level2= ifelse(assetClass=='bond', 'Fixed Income', Level2)) %>%
        mutate(Level3= ifelse(assetClass=='bond', Sec_Group, Level3)) %>%
        mutate(Level4= ifelse(assetClass=='bond', Sec_Type, Level4))
      
      temp <- adjItems %>% 
        filter(assetClass==class) %>% 
        select('Aladdin ID', Sec_Des, Sec_Group, Sec_Type)
      
      if(!supressWarnings) {
        missing <- temp[(is.na(temp$Sec_Group) | is.na(temp$Sec_Type)),]
        if(nrow(missing)>0) { 
          print('The following are missing bond attribution data')
          print('Update the core data file if needed.')
        }
      }
    } else if (class=='Other') {
      adjItems$Level1[adjItems$assetClass=='Other'] <- 'Other'
      adjItems$Level2[adjItems$assetClass=='Other'] <- 'Other'
      adjItems$Level3[adjItems$assetClass=='Other'] <- 'Other'
      adjItems$Level4[adjItems$assetClass=='Other'] <- 'Other'
    }
  }
  
  adjItems <- adjItems %>%
    mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  
  adjItems <- adjItems %>%
    select(-TotPctCont, everything())
  
  return(adjItems)
}

getDerivativeAdjustments <- function(accountData) {
  derivativeAdjustments <- accountData[accountData$'BOD NMV'==0,]
  return(derivativeAdjustments)
}

removeDerivativeAdjustments <- function(accountData){
  accountData <- accountData[!accountData$'BOD NMV'==0,]
  return(accountData)
}

addDerivativeAdjustments <- function(derivativeAdjustments, adjustments) {
  adjustmentItems <- adjustments$Items
  adjustmentEffect <- adjustments$Effect
  
  derivativeAdjustments$Parent_ID <- NULL
  derivativeAdjustments$Fund <- NULL
  derivativeAdjustments$NMVPct <- NULL
  derivativeAdjustments$'BOD NMV' <- NULL
  #colnames(adjustmentItems)[colnames(adjustmentItems) =='TotPctCont'] <- 'Ret_Cont'
  
  adjustmentItems <- rbind(derivativeAdjustments, adjustmentItems)
  
  adjustmentEffect <- adjustmentEffect + sum(derivativeAdjustments$Ret_Cont)
  
  adjustments <- list(Items=adjustmentItems, Effect=adjustmentEffect)
  return(adjustments)
}

optionsAdjustment <- function(accountData){
  # remove options, calculate metadata, return, adjust....
  startNMV <- sum(accountData$`BOD NMV`)
  options <- accountData[(accountData$Sec_Group=='OPTION' & accountData$Sec_Type=='EQUITY'),]
  accountData <- accountData[!(accountData$Sec_Group=='OPTION' & accountData$Sec_Type=='EQUITY'),]
  #NMVChange <- sum(options$<DeltaAdjustedNMV>) - sum(options$`BOD NMV`)
  #options$`BOD NMV` <- options$<DeltaAdjustedNMV>
  accountData <- rbind(accountData, options)
  endNMV <- startNMV + NMVChange
  accountData$NMVPct <- accountData$`BOD NMV` / endNMV
  return(accountData)
}

consolidateOptions <- function(options){
  optionPosition <- options[1,]
  optionPosition$Sec_Des[1] <- 'SPX Option Position'
  optionPosition$`BOD NMV`[1] <- sum(options$`BOD NMV`)
  optionPosition$TotPctCont[1] <- sum(options$TotPctCont)
  optionPosition$Ret[1] <- 0
  optionPosition$NMVPct[1] <- sum(optionPosition$NMVPct)
  return(optionPosition)
}
