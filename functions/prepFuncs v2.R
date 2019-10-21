source(here('functions/panelMaking', 'makePanels.R'))
# source(here('functions/testing', 'testPrep.R'))

prepData <- function(cleanedData) {
  fundsData <- cleanedData$funds
  accountsData <- cleanedData$accountsData
  benchmarksData <- cleanedData$benchmarksData
  panels <- createPanels(fundsData, accountsData, benchmarksData)
  return(panels)
}

createPanels <- function(fundsData, accountsData, benchmarksData) {
  panels <- list()
  for(account in names(accountsData)) {
    accountPacket <- accountsData[[account]]
    accountData <- accountPacket$accountData
    accountData <- data.frame(accountData, stringsAsFactors = FALSE)
    colnames(accountData)[colnames(accountData)=='Aladdin.ID'] <- 'Aladdin ID'
    benchData <- accountPacket$benchData
    adjustments <- accountPacket$adjustments
    adjEffect <- adjustments$Effect
    fundMV <- accountPacket$fundMV
    
    benchmarks <- getBenchmarks(account, fundsData)
    benchmarkData <- makeBenchmark(account, benchmarksData, benchData, fundsData, benchmarks)
    
    panel <- makePanel(accountData, benchmarkData, account, fundsData, benchmarks)
    #print(paste0('PriceAdjmnts for account ', account))
    priceAdjmnts <- getPriceAdjmnts(accountData, benchmarkData, benchmarks)
    
    tempPanel <- list(Panel=panel, NAV=fundMV, adjmts=adjustments, priceAdjmnts=priceAdjmnts)
    panels[[account]] <- tempPanel
  }
  return(panels)
}

makeBenchmark <- function(account, benchmarksData, benchData, fundsData, benchmarks) {
  weights <- getWeights(account, fundsData)
  benchmarks <- data.frame(Bench=benchmarks,
                           Wt=weights,
                           stringsAsFactors = FALSE)
  benchmarkData <- data.frame()
  for(benchmark in benchmarks$Bench) {
    tempData <- benchmarksData[[benchmark]]$benchmarkData 
    tempData$NMVPct <- tempData$NMVPct * benchmarks$Wt[benchmarks$Bench==benchmark]
    tempData$NMV <- tempData$NMV * benchmarks$Wt[benchmarks$Bench==benchmark]
    tempData$Ret_Cont <- tempData$Ret_Cont * benchmarks$Wt[benchmarks$Bench==benchmark]
    benchmarkData <- rbind(setDT(benchmarkData), setDT(tempData), fill=TRUE)
  }
  benchmarkData <- data.frame(benchmarkData, stringsAsFactors = FALSE)
  colnames(benchmarkData)[colnames(benchmarkData)=='Aladdin.ID'] <- 'Aladdin ID'
  benchmarkData <- completeBenchmarkLevels(benchmarkData, benchmarks)
  return(benchmarkData)
}

# change so that adjustment decision is based on return differential
getPriceAdjmnts <- function(accountData, benchmarkData, benchmarks) {
  levels <- accountData%>% dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levels <- length(levelnames)
  
  actRets <- accountData %>%
    dplyr::select('Aladdin ID', 'Sec_Des', starts_with('Level'), Ret_Cont, NMVPct)
  actRets <- rename(actRets, ActRet_Cont=Ret_Cont, ActNMVPct=NMVPct)
  actRets$ActRet <- actRets$ActRet_Cont / actRets$ActNMVPct
  actRets <- actRets %>% select('Aladdin ID', 'Sec_Des', c(levelnames), ActNMVPct, ActRet)
  benchRets <- benchmarkData %>%
    select('Aladdin ID', 'Sec_Des', Ret_Cont, NMVPct)
  benchRets <- rename(benchRets, BenchRet_Cont=Ret_Cont, BenchNMVPct=NMVPct)
  benchRets$BenchRet <- benchRets$BenchRet_Cont / benchRets$BenchNMVPct
  benchRets <- benchRets %>% select('Aladdin ID', BenchRet)
  intersection <- actRets %>% inner_join(benchRets, by='Aladdin ID')
  # taking only EAFE related price differences, others are caused by index rescaling
  # intersection <- intersection[intersection$Level2=='Intl',]
  
  intersection$priceEffect <- (intersection$BenchRet - intersection$ActRet) * intersection$ActNMVPct
  priceEffect <- sum(intersection$priceEffect)
  intersection <- intersection %>% select('Aladdin ID', 'Sec_Des', c(levelnames), 'priceEffect')
  # intersection <- intersection[,c(1:(levels+2),(levels+6))]
  items <- intersection %>%
    left_join(accountData, by=(c('Aladdin ID', 'Sec_Des', levelnames)))
  
  items <- reconcileLevels(items, benchmarkData, levelnames, benchmarks)
  
  temp <- list(priceAdjItems=items, priceEffect=priceEffect, priceAdjDetails=intersection)
  return(temp)
}

completeBenchmarkLevels <- function(benchmarkData, benchmarks) {
  levels <- 6
  levelnames <- c('Level1', 'Level2', 'Level3', 'Level4', 'Level5')
  
  benchmarks <- as.vector(benchmarks$Bench)
  
  benchLevels <- getLevels(benchmarks)
  benchLevelnames <- getLevelNames(levels)
  
  benchmarkData <- benchmarkData %>% 
    mutate(Level4= ifelse(is.na(Level4), Level3, Level4))
  
  if(benchLevels==5) { 
    benchmarkData$Level5 <- benchmarkData$Level4 
  } else {
    benchmarkData <- benchmarkData %>% 
      mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  }
  return(benchmarkData)
}

getBenchmarks <- function(account, fundsData){
  benchmarks <- fundsData %>%
    filter(Account==account) %>%
    select(starts_with('Benchmark'))
  benchmarks[benchmarks==''] <- NA
  benchmarks <- benchmarks[ , ! apply( benchmarks , 2 , function(x) all(is.na(x)) ) ]
  benchmarks <- as.vector(t(benchmarks))
  return(benchmarks)
}

getWeights <- function(account, fundsData){
  weights <- fundsData %>%
    filter(Account==account) %>%
    select(starts_with('BenchWt'))
  weights <- weights[ , ! apply( weights , 2 , function(x) all(is.na(x)) ) ]
  weights <- as.vector(t(weights))
  return(weights)
}
