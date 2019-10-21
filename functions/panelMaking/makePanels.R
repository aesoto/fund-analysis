makePanel <- function(accountData, benchmarkData, account, fundsData, benchmarks) {
  # benchLevels <- getLevels(benchmarks)
  # benchLevelnames <- getLevelNames(levels)
  # 
  # accountLevels <- accountData %>% 
  #   dplyr::select(starts_with('Level'))
  # accountLevelNames <- names(accountLevels)
  # accountLevels <- length(accountLevelNames) + 1
  # 
  # levels <- max(benchLevels, accountLevels)
  # if(levels==benchLevels) { levelnames <- benchLevelNames 
  # } else if (levels==accountLevels) {levelnames <- accountLevelNames }
  # 
  # if(accountLevels > 5) {
  #   accountData <- accountData %>% 
  #     mutate(Level4= ifelse(is.na(Level4), Level3, Level4))
  #   accountData <- accountData %>% 
  #     mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  # } else {
  #   accountData <- accountData %>% 
  #     mutate(Level4= ifelse(is.na(Level4), Level3, Level4))
  # }
  # 
  # 
  # if(benchLevels > 5) {
  #   benchmarkData <- benchmarkData %>% 
  #     mutate(Level4= ifelse(is.na(Level4), Level3, Level4))
  #   benchmarkData <- benchmarkData %>% 
  #     mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  # } else {
  #   benchmarkData <- benchmarkData %>% 
  #     mutate(Level4= ifelse(is.na(Level4), Level3, Level4))
  # }
  # 
  
  
  levels <- 6
  levelnames <- c('Level1', 'Level2', 'Level3', 'Level4', 'Level5')
  
  # benchLevels <- getLevels(benchmarks)
  # benchLevelnames <- getLevelNames(levels)
  #  
  accountLevels <- accountData %>% 
    dplyr::select(starts_with('Level'))
  accountLevelNames <- names(accountLevels)
  accountLevels <- length(accountLevelNames) + 1
  
  # benchmarkData <- benchmarkData %>% 
  #   mutate(Level4= ifelse(is.na(Level4), Level3, Level4))
  # if(benchLevels==5) { 
  #   benchmarkData$Level5 <- benchmarkData$Level4 
  # } else {
  #   benchmarkData <- benchmarkData %>% 
  #     mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  # }
  
  accountData <- accountData %>% 
    mutate(Level4= ifelse(is.na(Level4), Level3, Level4))
  if(accountLevels==5) { 
    accountData$Level5 <- accountData$Level4 
  } else {
    accountData <- accountData %>% 
      mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  }
  #print(paste0('Reconciling ', account))
  accountData <- reconcileLevels(accountData, benchmarkData, levelnames, benchmarks)
  
  
  allWts <- panelWeights(accountData, benchmarkData, levelnames)
  allRets <- panelReturns(accountData, benchmarkData, levelnames)
  
  headers <- append('Level0', levelnames)
  headers <- append(headers, 'Aladdin ID')
  headers <- append(headers, 'Sec_Des')
  panel <- allWts %>% full_join(allRets, by = c(headers))
  return(panel)
}

getLevels <- function(benchmarks){
  levels <- 0
  for(bench in benchmarks) {
    if(bench=='EAFE') {levels<-max(levels, 6)
    } else if(bench=='R3K') {levels<-max(levels, 5)
    } else if(bench=='SPX') {levels<-max(levels, 5)
    } else if(bench=='AGG') {levels<-max(levels, 5)
    } else {
      if(nchar(bench)>2) {
        print('There is an unidentified benchmark.  Please update benchmark attribution types')
        print(bench)
      }
    }
  }
  return(levels)
}

getLevelNames <- function(levels) {
  levelnames <- vector()
  for(i in 1:(levels-1)) {
    this.level <- paste0('Level', i)
    levelnames <- append(levelnames, this.level)
  }
  return(levelnames)
}

panelWeights <- function(accountData, benchmarkData, levelnames) {
  levels <- length(levelnames) + 1
  names <- append('Level0', levelnames)
  names <- append(names, 'Aladdin ID')
  names <- append(names, 'Sec_Des')
  
  actWts <- getPanelActWeights(accountData, levelnames)
  bchWts <- getPanelBenchWeights(benchmarkData, levelnames)
  
  
  allWts <- actWts %>% full_join(bchWts, by = c(names))
  
  
  allWts <- allWts %>%
    mutate(PortWt = if_else(is.na(PortWt), 0, PortWt)) %>%
    mutate(BenchWt = if_else(is.na(BenchWt), 0, BenchWt))
  
  allWts <-  allWts %>% arrange(.dots = !!! rlang::syms(names))
  return(allWts)
}

getPanelActWeights <- function(accountData, levelnames) {
  headers <- c('ID', 'Aladdin ID', 'Sec_Des', 'NMVPct')
  headers <- append(headers, c(levelnames))
  accountData <- subset(accountData, select=headers)
  
  descriptions <- accountData[c('Aladdin ID', 'Sec_Des')]
  accountData$Sec_Des <- NULL
  
  #accountData <- accountData[, headers]
  accountData <- accountData %>% arrange(!!! rlang::syms(levelnames))
  
  resultnames <- append('Level0', levelnames)
  resultnames <- append(resultnames, 'Aladdin ID')
  #resultnames <- append(resultnames, 'Sec_Des')
  resultnames <- append(resultnames, 'PortWt')
  result <- setNames(data.table(matrix(ncol = length(resultnames), nrow=0)), resultnames)
  temp <- data.table(Level0 = 'Total', 
                     PortWt = 1.00)
  result <- rbind(result, temp, fill = TRUE)
  
  panelnames <- append(levelnames, 'Aladdin ID')
  #panelnames <- append(panelnames, 'Sec_Des')
  
  for(i in 1:length(panelnames)) {
    if(i == 1) {
      currentlevels <- panelnames[i]
      temp <- accountData %>% 
        group_by(!! rlang::sym(currentlevels)) %>% 
        summarise(PortWt = sum(NMVPct))
      result <- rbind(result, temp, fill = TRUE)
    } else {
      currentlevels <- panelnames[1:i]
      temp <- accountData %>%
        group_by(!!! rlang::syms(currentlevels)) %>% 
        summarise(PortWt = sum(NMVPct))
      result <- bind_rows(result, temp)
    }
  }
  result <- result %>% 
    left_join(descriptions, by='Aladdin ID')
  result <- result[!duplicated(result),]
  resultnames <- c('Level0', levelnames, 'Aladdin ID', 'Sec_Des', 'PortWt')
  result <- result[c(resultnames)]
  result <- result %>% replace(is.na(.), '')
  result <-  result %>% arrange(.dots = !!! rlang::syms(panelnames))
  return(result)
}

getPanelBenchWeights <- function(benchmarkData, levelnames) {
  headers <- c('Aladdin ID', 'Sec_Des', 'NMVPct')
  headers <- append(headers, c(levelnames))
  benchmarkData <- subset(benchmarkData, select=headers)
  
  descriptions <- benchmarkData[c('Aladdin ID', 'Sec_Des')]
  benchmarkData$Sec_Des <- NULL
  
  #benchmarkData <- benchmarkData[, headers]
  benchmarkData <- benchmarkData %>% arrange(!!! rlang::syms(levelnames))
  
  resultnames <- append('Level0', levelnames)
  resultnames <- append(resultnames, 'Aladdin ID')
  resultnames <- append(resultnames, 'BenchWt')
  result <- setNames(data.table(matrix(ncol = length(resultnames), nrow=0)), resultnames)
  temp <- data.table(Level0 = 'Total', 
                     BenchWt = 1.00)
  result <- rbind(result, temp, fill = TRUE)
  
  panelnames <- append(levelnames, 'Aladdin ID')
  
  for(i in 1:length(panelnames)) {
    currentlevel <- panelnames[i]
    if(i == 1) {
      temp <- benchmarkData %>% 
        group_by(!! rlang::sym(currentlevel)) %>%
        summarise(BenchWt = sum(NMVPct))
      result <- rbind(result, temp, fill = TRUE)
    } else {    
      currentlevels <- panelnames[1:i]
      temp <- benchmarkData %>% 
        group_by(!!! rlang::syms(currentlevels)) %>%
        summarise(BenchWt = sum(NMVPct))
      result <- bind_rows(result, temp)
    }  
  }
  result <- result %>% 
    left_join(descriptions, by='Aladdin ID')
  result <- result[!duplicated(result),]
  resultnames <- c('Level0', levelnames, 'Aladdin ID', 'Sec_Des', 'BenchWt')
  result <- result[c(resultnames)]
  result <- result %>% replace(is.na(.), '')
  result <-  result %>% arrange(.dots = !!! rlang::syms(panelnames))
  return(result)
}

# examine discrepancy here in level 4
getPanelActReturns <- function(accountData, levelnames) {
  headers <- c('ID', 'Aladdin ID', 'Sec_Des', 'NMVPct', 'Ret', 'Ret_Cont')
  headers <- append(headers, c(levelnames))
  accountData<- subset(accountData, select=headers)
  
  descriptions <- accountData[c('Aladdin ID', 'Sec_Des')]
  accountData$Sec_Des <- NULL
  #accountData <- accountData[, headers]
  accountData <- accountData %>% arrange(!!! rlang::syms(levelnames))
  #accountData$WtRet <- accountData$NMVPct * accountData$Ret
  
  resultnames <- append('Level0', levelnames)
  resultnames <- append(resultnames, 'Aladdin ID')
  resultnames <- append(resultnames, 'PortRet')
  result <- setNames(data.table(matrix(ncol = length(resultnames), nrow=0)), resultnames)
  temp <- data.table(Level0 = 'Total', 
                     PortRet = as.numeric(sum(accountData$Ret_Cont)))
  result <- rbind(result, temp, fill = TRUE)
  
  panelnames <- append(levelnames, 'Aladdin ID')
  
  for(i in 1:length(panelnames)) {
    currentlevel <- panelnames[i]
    if(i == 1) {
      temp <- accountData %>% 
        group_by(!! rlang::sym(currentlevel)) %>%
        summarise(PortRet = sum(Ret_Cont) / sum(NMVPct))
      result <- rbind(result, temp, fill = TRUE)
    } else {
      currentlevels <- panelnames[1:i]
      temp <- accountData %>% 
        group_by(!!! rlang::syms(currentlevels)) %>%
        summarise(PortRet = sum(Ret_Cont) / sum(NMVPct))
      result <- bind_rows(result, temp)
    }
  }
  result <- result %>% 
    left_join(descriptions, by='Aladdin ID')
  result <- result[!duplicated(result),]
  resultnames <- c('Level0', levelnames, 'Aladdin ID', 'Sec_Des', 'PortRet')
  result <- result[c(resultnames)]
  result <- result %>% replace(is.na(.), '')
  result <- result %>% arrange(.dots = !!! rlang::syms(panelnames))
  return(result)
}

getPanelBenchReturns <- function(benchmarkData, levelnames) {
  headers <- c('Aladdin ID', 'Sec_Des', 'Ret_Cont', 'NMVPct')
  headers <- append(headers, c(levelnames))
  benchmarkData <- subset(benchmarkData, select=headers)
  #benchmarkData <- benchmarkData[, headers]
  
  descriptions <- benchmarkData[c('Aladdin ID', 'Sec_Des')]
  benchmarkData$Sec_Des <- NULL
  
  benchmarkData <- benchmarkData %>% arrange(!!! rlang::syms(levelnames))
  resultnames <- append('Level0', levelnames)
  resultnames <- append(resultnames, 'Aladdin ID')
  resultnames <- append(resultnames, 'BenchRet')
  result <- setNames(data.table(matrix(ncol = length(resultnames), nrow=0)), resultnames)
  
  panelnames <- append(levelnames, 'Aladdin ID')
  
  temp <- data.table(Level0 = 'Total', 
                     BenchRet = as.numeric(sum(benchmarkData$Ret_Cont))) 
  result <- rbind(result, temp, fill = TRUE)
  for(i in 1:length(panelnames)) {
    currentlevel <- panelnames[i]
    if(i == 1) {
      temp <- benchmarkData %>% 
        group_by(!! rlang::sym(currentlevel)) %>%
        summarise(BenchRet = sum(Ret_Cont) / sum(NMVPct)) 
      temp <- temp[complete.cases(temp),]
      result <- rbind(result, temp, fill = TRUE)
    } else {
      currentlevels <- panelnames[1:i]
      temp <- benchmarkData %>% 
        group_by(!!! rlang::syms(currentlevels)) %>%
        summarise(BenchRet = sum(Ret_Cont) / sum(NMVPct)) 
      temp <- temp[complete.cases(temp),]
      #temp <- data.frame(temp, stringsAsFactors=FALSE)
      result <- bind_rows(result, temp)
    }
  }
  
  result <- result %>% 
    left_join(descriptions, by='Aladdin ID')
  result <- result[!duplicated(result),]
  resultnames <- c('Level0', levelnames, 'Aladdin ID', 'Sec_Des', 'BenchRet')
  result <- result[c(resultnames)]
  
  result <- result %>% replace(is.na(.), '')
  result <- result %>% arrange(.dots = !!! rlang::syms(levelnames))
  return(result)
}

panelReturns <- function(accountData, benchmarkData, levelnames) {
  levels <- length(levelnames) + 1
  names <- append('Level0', levelnames)
  names <- append(names, 'Aladdin ID')
  names <- append(names, 'Sec_Des')
  
  actRets <- getPanelActReturns(accountData, levelnames)
  bchRets <- getPanelBenchReturns(benchmarkData, levelnames)
  
  allRets <- actRets %>% full_join(bchRets, by = c(names))
  allRets <- allRets %>%
    mutate(PortRet = ifelse(is.na(PortRet), 0, PortRet)) %>%
    mutate(BenchRet = ifelse(is.na(BenchRet), 0, BenchRet))
  
  allRets <-  allRets %>% arrange(.dots = !!! rlang::syms(names))
  return(allRets)
}

reconcileLevels <- function(accountData, benchmarkData, levelnames, benchmarks) {
  #categories <- append('Aladdin ID', levelnames)
  #tempAccountData <- accountData[,c(categories)]
  #tempBenchmarkData <- benchmarkData[,c(categories)]
  
  levels <- 6
  levelnames <- c('Level1', 'Level2', 'Level3', 'Level4', 'Level5')
  
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
  
  accountLevels <- accountData %>% 
    dplyr::select(starts_with('Level'))
  accountLevelNames <- names(accountLevels)
  accountLevels <- length(accountLevelNames) + 1
  
  accountData <- accountData %>% 
    mutate(Level4= ifelse(is.na(Level4), Level3, Level4))
  if(accountLevels==5) { 
    accountData$Level5 <- accountData$Level4 
  } else {
    accountData <- accountData %>% 
      mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  }
  
  accountData$Level1Bench <- benchmarkData$Level1[match(accountData$`Aladdin ID`, benchmarkData$`Aladdin ID`)]
  accountData <- accountData %>%
    mutate(Level1= if_else((!is.na(Level1Bench) & Level1!=Level1Bench), Level1Bench, Level1))
  accountData$Level1Bench <- NULL
  
  accountData$Level2Bench <- benchmarkData$Level2[match(accountData$`Aladdin ID`, benchmarkData$`Aladdin ID`)]
  accountData <- accountData %>%
    mutate(Level2= if_else((!is.na(Level2Bench) & Level2!=Level2Bench), Level2Bench, Level2))
  accountData$Level2Bench <- NULL
  
  accountData$Level3Bench <- benchmarkData$Level3[match(accountData$`Aladdin ID`, benchmarkData$`Aladdin ID`)]
  accountData <- accountData %>%
    mutate(Level3= if_else((!is.na(Level3Bench) & Level3!=Level3Bench), Level3Bench, Level3))
  accountData$Level3Bench <- NULL
  
  accountData$Level4Bench <- benchmarkData$Level4[match(accountData$`Aladdin ID`, benchmarkData$`Aladdin ID`)]
  accountData <- accountData %>%
    mutate(Level4= if_else((!is.na(Level4Bench) & Level4!=Level4Bench), Level4Bench, Level4))
  accountData$Level4Bench <- NULL
  
  if(length(levelnames)==5) { 
    accountData$Level5Bench <- benchmarkData$Level5[match(accountData$`Aladdin ID`, benchmarkData$`Aladdin ID`)]
    accountData <- accountData %>%
      mutate(Level5= if_else((!is.na(Level5Bench) & Level5!=Level5Bench), Level5Bench, Level5))
    accountData$Level5Bench <- NULL
  }
  
  accountData$Sec_DesBench <- benchmarkData$Sec_Des[match(accountData$`Aladdin ID`, benchmarkData$`Aladdin ID`)]
  accountData <- accountData %>%
    mutate(Sec_Des= if_else((!is.na(Sec_DesBench) & Sec_Des!=Sec_DesBench), Sec_DesBench, Sec_Des))
  accountData$Sec_DesBench <- NULL
  
  return(accountData)
}

# 
# 
# 
# test <- allWts[duplicated(allWts$`Aladdin ID`),]
# test <- test[test$`Aladdin ID`!='',]
# dupes <- test$`Aladdin ID`
# duplications <- allWts[allWts$`Aladdin ID` %in% dupes,]
# duplications
