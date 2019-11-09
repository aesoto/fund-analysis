makePanel <- function(accountData, benchmarkData, account, fundsData, benchmarks) {
  levels <- names(accountData)[grepl('Level', names(accountData))]
  selection <- c('Aladdin ID', 'Sec_Des')
  levels <- c(levels, selection)
  
  totPortRet <- sum(accountData$Ret_Cont)
  totBenchRet <- sum(benchmarkData$Ret_Cont)
  
  CTRPanel <- makeCTRPanel(accountData, benchmarkData, levels)
  completePanel <- makeCompletePanel(CTRPanel, levels)
  returnPanel <- makeReturnPanel(completePanel, levels)
  panel <- addTotals(returnPanel, totPortRet, totBenchRet)
  checkPanel(panel, levels, account)
  return(panel)
}

makeCTRPanel <- function(accountData, benchmarkData, levels) {
  tempAccountData <- accountData[c(levels, 'NMVPct', 'Ret_Cont')]
  colnames(tempAccountData)[names(tempAccountData)=='NMVPct'] <- 'PortWt'
  colnames(tempAccountData)[names(tempAccountData)=='Ret_Cont'] <- 'PortCTR'
  
  tempBenchmarkData <- benchmarkData[c(levels, 'NMVPct', 'Ret_Cont')]
  colnames(tempBenchmarkData)[names(tempBenchmarkData)=='NMVPct'] <- 'BenchWt'
  colnames(tempBenchmarkData)[names(tempBenchmarkData)=='Ret_Cont'] <- 'BenchCTR'
  
  CTRPanel <- tempAccountData %>%
    full_join(tempBenchmarkData, by=levels)
  CTRPanel <- CTRPanel[!duplicated(CTRPanel),]
  
  CTRPanel <- CTRPanel %>% 
    mutate_if(is.numeric,~ ifelse(is.na(.), 0, .))
  
  if(sum(CTRPanel$PortWt) != 1) {
    adjustment <- 1 - sum(CTRPanel$PortWt)
    CTRPanel$PortWt[CTRPanel$Level2=='Margin'] <- CTRPanel$PortWt[CTRPanel$Level2=='Margin'] + adjustment
  }
  
  CTRPanel <- CTRPanel %>% 
    arrange(.dots = !!! rlang::syms(levels))
  return(CTRPanel)
}

makeReturnPanel <- function(completePanel, levels) {
  returnPanel <- completePanel
  returnPanel$PortRet <- returnPanel$PortCTR / returnPanel$PortWt
  returnPanel$BenchRet <- returnPanel$BenchCTR / returnPanel$BenchWt
  returnPanel$PortCTR <- NULL
  returnPanel$BenchCTR <- NULL
  returnPanel <- returnPanel %>% 
    mutate_if(is.numeric,~ ifelse(is.na(.), 0, .))
  returnPanel <- returnPanel[c(levels, 'PortWt', 'PortRet', 'BenchWt', 'BenchRet')]
  returnPanel <- returnPanel %>% 
    arrange(.dots = !!! rlang::syms(levels))
  return(returnPanel)
}

makeCompletePanel <- function(CTRPanel, levels) {
  completePanel <- CTRPanel
  for(i in 1:(length(levels)-2)) {
    if(i == 1) {
      currentlevels <- levels[i]
      temp <- completePanel %>% 
        group_by(!! rlang::sym(currentlevels)) %>% 
        summarise_if(is.numeric, sum) 
      temp <- data.frame(temp, stringsAsFactors = FALSE)
      completePanel <- rbind(setDT(completePanel), setDT(temp), fill = TRUE)
      completePanel <- completePanel %>% 
        mutate_if(is.character,~ ifelse(is.na(.), '', .))
      completePanel <- completePanel %>% 
        arrange(.dots = !!! rlang::syms(levels))
    } else {
      currentlevels <- levels[1:i]
      temp <- completePanel %>%
        group_by(!!! rlang::syms(currentlevels)) %>% 
        summarise_if(is.numeric, sum)
      temp <- data.frame(temp, stringsAsFactors = FALSE)
      temp <- temp[temp[i]!='',]
      completePanel <- rbind(setDT(completePanel), setDT(temp), fill = TRUE)
      completePanel <- completePanel %>% 
        mutate_if(is.character,~ ifelse(is.na(.), '', .))
      completePanel <- completePanel %>% 
        arrange(.dots = !!! rlang::syms(levels))
    }
  }
  return(completePanel)
}

addTotals <- function(returnPanel, totPortRet, totBenchRet) {
  total <- head(returnPanel, 1)
  total <- total %>% 
    mutate_if(is.character,~ ifelse(.!='', '', .))
  total <- total %>% 
    mutate_if(is.numeric,~ ifelse(.!=0, 0, .))
  total$Level0 <- 'Total'
  total <- select(total, Level0, everything())
  total$PortWt <- 1
  total$PortRet <- totPortRet
  total$BenchWt <- 1
  total$BenchRet <- totBenchRet
  panel <- rbind(setDT(total), setDT(returnPanel), fill = TRUE)
  panel <- panel %>% 
    mutate_if(is.character,~ ifelse(is.na(.), '', .))
  return(panel)
}

checkPanel <- function(panel, levels, account) {
  totals <- panel[panel$Level0=='Total',]
  values <- panel[!panel$Level0=='Total',]
  
  if((sum(panel$PortWt)/length(levels) - 1) > 0.0001) {
    print(paste0('Portfolio weight discrepancy in account ', account))
  }
  if((sum(panel$BenchWt)/length(levels) - 1) > 0.0001) {
    print(paste0('Benchmark weight discrepancy in account ', account))
  }
  values$WtdPortRet <- values$PortWt * values$PortRet
  values$WtdBenchRet <- values$BenchWt * values$BenchRet
  
  endPortRet <- sum(values$WtdPortRet) / (length(levels)-1)
  endBenchRet <- sum(values$WtdBenchRet) / (length(levels)-1)
  
  if(endPortRet - totals$PortRet > 0.0001) {
    print(paste0('Portfolio return discrepancy in account ', account))
  }
  if(endBenchRet - totals$BenchRet > 0.0001) {
    print(paste0('Benchmark return discrepancy in account ', account))
  }
}
