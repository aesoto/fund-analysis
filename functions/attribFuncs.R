# source(here('functions/testing','testAttrib.R'))

getAttribution <- function(preparedData, isGeometric) {
  output <- createAttribution(preparedData, isGeometric)
  return(output)
}

createAttribution <- function(preparedData, isGeometric) {
  results <- list()
  metrics <- data.frame()
  #gaps <- list()
  for(account in names(preparedData)) {
    accountPacket <- preparedData[[account]]
    #prePanel <- accountPacket$prePanel
    #portRet <- accountPacket$PortRet
    #fundPacket <- accountPacket$fundPanels[[fund]]
    #fund <- as.numeric(accountPacket$account)
    fundMV <- accountPacket$NAV
    panel <- accountPacket$Panel
    adjmts <- accountPacket$adjmts
    adjmtsItems <- adjmts$Items
    adjmtsItems <- fillLevels(adjmtsItems)
    adjmtsEffect <- sum(adjmtsItems$TotPctCont)
    priceAdjmntsEffect <- as.numeric(accountPacket$priceAdjmnts$priceEffect)
    priceAdjItems <- accountPacket$priceAdjmnts$priceAdjItems # Levels need to be filled out, per panel algorithm
    priceAdjItems <- fillLevels(priceAdjItems)
    priceAdjDetails <- accountPacket$priceAdjmnts$priceAdjDetails # Levels need to be filled out, per panel algorithm
    priceAdjDetails <- data.frame(priceAdjDetails, stringsAsFactors = FALSE)
    priceAdjDetails <- fillLevels(priceAdjDetails)
    priceAdjDetails <- priceAdjDetails %>% select_if(~sum(!is.na(.)) > 0)
    totals <- panel[panel$Level0 == 'Total',]
    
    attribPanel <- makeAttributionPanel(panel)
    attribPanel <- addAdjustments(attribPanel, adjmtsEffect, priceAdjmntsEffect, totals, fundMV)
    attribPanel <- applyPriceAdjmts(attribPanel, priceAdjDetails) # adds other level data
    
    attribPanel <- addPriceAdjItems(attribPanel, priceAdjItems) # adds selection level
    
    attribPanel <- addOtherAdjItems(attribPanel, adjmtsItems)
    attribPanel <- fillColumns(attribPanel)
    
    dolAttribPanel <- getDollarPanel(attribPanel, fundMV)
    
    if(isGeometric) { attribPanel <- addGeometric(attribPanel) }
    
    finalPanel <- mergePanels(attribPanel, dolAttribPanel)
    finalPanel <- cleanFinalPanel(finalPanel)
    finalPanel <- formatPanel(finalPanel)
    
    tempMetrics <- data.frame(account=account,
                              NAV=finalPanel$NAV[1],
                              targetBasis=finalPanel$basis[1],
                              gap=finalPanel$gap[1],
                              basisVal=finalPanel$basisVal[1],
                              gapVal=finalPanel$gapVal[1],
                              PctExplained=finalPanel$actvRet[1]/finalPanel$basis[1]
    )
    
    metrics <- rbind(metrics, tempMetrics)
    
    tempData <- list(account=account, 
                     finalPanel=finalPanel,
                     attribPanel=attribPanel, 
                     dolAttribPanel=dolAttribPanel,
                     fundMV=fundMV,
                     adjmts=adjmts,
                     priceAdjmnts=accountPacket$priceAdjmnts
    )
    #print(paste0(account, ': ', finalPanel$gap[1], ' : ', finalPanel$gapVal[1]))
    
    results[[account]] <- tempData
  }
  print(metrics)
  results[['metrics']] <- metrics
  #print(summary(gaps))
  return(results)
}

makeAttributionPanel <- function(panel) {
  totalData <- panel[panel$Level0 == 'Total',]
  benchRet <- totalData$BenchRet
  panel$Level0 <- NULL
  
  levels <- panel %>% dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levelnames <- append(levelnames, 'Selection')
  levels <- length(levelnames) 
  
  descriptions <- panel[c('Aladdin ID', 'Sec_Des')]
  descriptions <- descriptions[!apply(descriptions == "", 1, all),]
  descriptions <- descriptions[!duplicated(descriptions),]
  
  weights <- panel
  weights <- weights[!apply(descriptions == "", 1, all),]
  weights <- weights[!weights$Level1=='',]
  colnames(weights)[colnames(weights)=='Aladdin ID'] <- 'Selection'
  
  colnames(panel)[colnames(panel)=='Aladdin ID'] <- 'Selection'
  #levelnames <- names(panel)[1:levels]
  
  attributionResult <- makeResultPanel(levelnames)
  attributionPanel <- createAttributionPanel(panel, attributionResult, benchRet, levels)  # <--- all the results are here. the rest is organization
  attributionPanel <- organizePanel(attributionPanel, levels, descriptions, weights, levelnames)
  attributionPanel <- cleanPanel(attributionPanel, levels)
  return(attributionPanel)
}

getCurrentLevelData <- function(panel, levelnames, lvl) {
  if(lvl == (length(levelnames)-1)) {
    temp <- panel
    temp[temp==''] <- NA
    temp <- na.omit(temp)
    temp$PortRet <- NULL
  } else {
    temp <- panel
    for(i in 1:(lvl)) {
      temp <- temp[!(temp[i] == ''),]
    }
    for(i in (lvl+1):(length(levelnames)-1)) {
      temp <- temp[(temp[i] == ''),]
    }
    temp[temp==''] <- NA
    temp <- temp %>% select_if(~!all(is.na(.)))
    temp$PortRet <- NULL
  }
  return(temp)
}

getPreviousLevelData <- function(panel, levelnames, lvl) {
  newLevel <- lvl-1
  temp <- getCurrentLevelData(panel, levelnames, newLevel)
  names <-colnames(temp)
  oldNames <- names[0:newLevel]
  newNames <- paste0('Prev', colnames(temp)[(newLevel+1):length(colnames(temp))])
  colnames(temp) <- c(oldNames, newNames)
  return(temp)
}

sortAttributionResult <- function(attributionResult, levelnames, totalData) {
  groupingNames <- levelnames[1:(length(levelnames)-1)]
  groupingNames <- append(groupingNames, 'SelectionAttr')
  selection <- attributionResult
  selection$Selection <- NULL
  selection <- selection %>% filter(SelectionAttr > 0)
  
  tempSelection <- setNames(data.table(matrix(ncol = length(groupingNames), nrow=0)), groupingNames)
  tempSelection <- tempSelection %>% mutate_if(is.logical, as.character)
  tempSelection$SelectionAttr <- as.numeric(tempSelection$SelectionAttr)
  
  for(i in 2:(length(levelnames)-1)) {
    temp <- data.frame(selection, stringsAsFactors = FALSE)
    temp <- temp %>% group_by(!!! rlang::syms(levelnames[2:i])) %>%
      summarise(SelectionAttr = sum(SelectionAttr))
    tempSelection <- tempSelection %>% full_join(temp, by=c(levelnames[2:i], 'SelectionAttr'))
  }
  
  tempOther <- attributionResult %>% filter(is.na(SelectionAttr))
  tempOther <- tempOther %>% select_if(~sum(!is.na(.)) > 0)
  
  result <- tempOther %>% full_join(tempSelection, by=c(levelnames[2:(length(levelnames)-1)]))
  levels <- levelnames[2:(length(levelnames)-1)]
  
  totalData$'Aladdin ID' <- NULL
  totalData$PortWt <- NULL
  totalData$BenchWt <- NULL
  
  result <- result %>% 
    mutate_if(is.character,~ ifelse(is.na(.), '', .))
  result <- result %>% arrange(.dots =!!! rlang::syms(levels))
  result <- result %>% 
    mutate_if(is.numeric,~ ifelse(is.na(.), 0, .))
  result <- rbind(setDT(result), setDT(totalData), fill=TRUE)
  result <- result %>% select(Level0, everything())
  
  result <- rbind(tail(result, 1), head(result, nrow(result)-1))
  return(result)
}

getSumData <- function(attributionResult, panel, levelnames, lvl, benchRet) {
  names <- paste0(levelnames, 'Attr')
  for(i in 1:(length(names)-1)) {
    temp <- getLevelAttribution(panel, levelnames, lvl=i, benchRet)
    attribution <- sum(temp[i+1])
    attributionResult[attributionResult$Level0 == 'Total', names[i+1]] <- attribution
  }
  return(attributionResult)
}

addAdjustments <- function(attribPanel, adjmtsEffect, priceAdjmntsEffect, totals, fundMV) {
  adjmts <- adjmtsEffect
  
  levels <- totals %>% dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levelnames <- append(levelnames, 'Selection')
  levels <- length(levelnames)-1
  levelnames <- levelnames[1:(levels+1)]
  
  tempTotNames <- levelnames[1:(length(levelnames)-1)]
  tempTotNames <- append(tempTotNames, 'Aladdin ID')
  tempTotNames <- append(tempTotNames, 'Sec_Des')
  
  tempTotals <- totals[,c(tempTotNames, 'PortRet', 'BenchRet')]
  colnames(tempTotals)[colnames(tempTotals)=='PortRet'] <- 'AttribCTR'
  colnames(tempTotals)[colnames(tempTotals)=='BenchRet'] <- 'benchRet'
  
  portRet <- tempTotals$AttribCTR + adjmts
  
  tempTotals$AttribCTR <- portRet - adjmts + priceAdjmntsEffect
  tempTotals$basis <- tempTotals$AttribCTR[1] - tempTotals$benchRet[1]
  setnames(tempTotals, old='Level0', new='NAV')
  tempTotals$NAV <- fundMV
  
  #portRet <- tempTotals$AttribRet + priceAdjmnts + adjmts
  
  portDF <- data.frame(PortfolioCTR=portRet,
                       otherAdj=-adjmts,
                       priceAdj=priceAdjmntsEffect
  )
  portDFnames <- names(portDF)
  
  attrNames <- names(attribPanel)[(length(names(attribPanel))-levels+1):length(names(attribPanel))]
  attribTotals <- attribPanel[,attrNames]
  attribTotals <- attribTotals %>% summarise_all(sum)
  totActvRet <- rowSums(attribTotals)[1]  #formerly totalAttib
  tempTotals <- cbind(tempTotals, attribTotals)
  attrBasis <- totActvRet#-priceAdjmnts#+adjmts
  
  
  subTot <- data.frame(actvRet=totActvRet,
                       #priceAdj=-priceAdjmnts,
                       #otherAdj=adjmts,
                       #attrBasis=attrBasis,
                       gap=tempTotals$basis - attrBasis)
  tempTotals <- cbind(tempTotals, subTot)
  
  tempTotNames <- replace(tempTotNames, tempTotNames=='Level0', 'NAV')
  temp1 <- tempTotals[, names(tempTotals) %in% tempTotNames]
  temp2 <- tempTotals[, !names(tempTotals) %in% tempTotNames]
  
  weightItems <- totals[,c('PortWt', 'BenchWt', 'PortRet', 'BenchRet')]
  
  temp1 <- cbind(temp1, weightItems)
  tempTotals <- cbind(temp1, portDF)
  tempTotals <- cbind(tempTotals, temp2)
  colnames(tempTotals)[colnames(tempTotals)=='Aladdin ID'] <- 'Selection'
  
  attribPanel <- rbind(setDT(tempTotals), setDT(attribPanel), fill=TRUE)
  
  attribPanel$benchRet <- attribPanel$BenchRet * attribPanel$BenchWt
  
  colnames(attribPanel)[colnames(attribPanel)=='benchRet'] <- 'BenchCTR'  ######
  #attribNames <- names(attribPanel)
  #attribNames <- attribNames[c(1:12, 12:13, 11, 14:ncol(attribPanel))]
  #attribPanel <- attribPanel %>% select(attribNames)
  
  attribPanel$PortfolioCTR1 <- attribPanel$PortWt * attribPanel$PortRet
  attribPanel <- subset(attribPanel, select=c(NAV:otherAdj, PortfolioCTR1, priceAdj:gap))
  
  attribPanel[is.na(attribPanel)] <- 0
  
  #colnames(attribPanel)[colnames(attribPanel)=='PortfolioRet'] <- 'PortfolioCTR1'
  
  return(attribPanel)
}

addGeometric <- function(attribPanel) {
  start <- which(names(attribPanel) == 'Level1Attr')
  end <- which(names(attribPanel) == 'SelectionAttr')
  benchRet <- attribPanel$BenchRet[1]
  portRet <- attribPanel$AttribCTR[1]
  
  attribPanel$basis[1] <- (1+portRet)/(1+benchRet) - 1 
  # temp <- attribPanel %>% dplyr::select(starts_with('Level'))
  # levels <- length(names(temp))/2 + 1
  # levelnames <- names(attribPanel)
  # start <- levels+1
  # end <- 2*levels
  # benchRet <- attribPanel$BenchRet[1]
  
  EGTot <- 0
  R_H <- benchRet
  totAttr <- 0
  for(i in start:end) {
    if(i == start) { 
      attribPanel[i] <- attribPanel[i] / (1 + R_H)
      EGTot <- attribPanel[1,i]
      totAttr <- 1 + EGTot
    }
    else {
      R_H <- EGTot * (1 + R_H) + R_H
      attribPanel[i] <- attribPanel[i] / (1 + R_H)
      EGTot <- attribPanel[1,i]
      totAttr <- totAttr * (1 + EGTot)
    }
  }
  totAttr <- totAttr - 1
  attribPanel$actvRet[1] <- totAttr
  attribPanel$gap[1] <- attribPanel$basis[1] - totAttr
  return(attribPanel)
}

modifyPanel <- function(tempPanel) {
  tempPanel <- tempPanel %>%
    mutate(BenchRet = ifelse(BenchWt==0, PortRet, BenchRet))
  return(tempPanel)
}

getLev1Attribution <- function(tempPanel, benchRet) {
  tempPanel$Level1Attr <- (tempPanel$PortWt - tempPanel$BenchWt) * (tempPanel$BenchRet - benchRet)
  tempPanel <- tempPanel[,c(1,6)]
  return(tempPanel)
}

getLevAttribution <- function(tempPanel, prevWts, prevRet, lvl, levels) {
  tempPanel <- merge(tempPanel, prevWts, by.x = c(1:(lvl-1)), by.y = c(1:(lvl-1)), all.x = T, all.y = F ) # the base R syntax is more intuitive here
  tempPanel$PrevWtFactor <- ifelse(tempPanel$PrevBenchWt == 0, 0, tempPanel$PrevPortWt / tempPanel$PrevBenchWt)
  tempPanel$LevelAttr <- (tempPanel$PortWt - (tempPanel$PrevWtFactor) * tempPanel$BenchWt) * (tempPanel$BenchRet - tempPanel$PrevBenchRet)
  tempPanel <- tempPanel[,c(c(1:lvl),c(lvl+10))]
  if(lvl < levels) { name <- paste0('Level', lvl, 'Attr')
  } else { name <- 'SelectionAttr'}
  names(tempPanel)[lvl+1] <- name
  return(tempPanel)
}

getLevelReturn <- function(levelData, prevRet, lvl) {
  newRet <- sum(levelData[lvl+1]) * (1 + prevRet)
  return(newRet)
}

renamePanel <- function(tempPanel) {
  colnames(tempPanel) <- paste0('Prev', colnames(tempPanel))
  return(tempPanel)
}

getCurrLevel <- function(panel, lvl, start, end) {
  tempPanel <- panel[,c(c(1:(lvl+1)),c(start:end))]
  tempPanel <- tempPanel[(tempPanel[lvl+1] == ''),]
  tempPanel[lvl+1] <- NULL
  tempPanel <- tempPanel[(tempPanel[lvl] != ''),]
  return(tempPanel)
}

getSelLevel <- function(panel, levels, start, end) {
  tempPanel <- panel[,c(c(0:levels),c(start:end))]
  tempPanel <- tempPanel[(tempPanel[levels] != ''),]
}

makeResultPanel <- function(levelnames) {
  resultnames <- paste0(levelnames, 'Attr')
  
  attributionResult1 <- setNames(data.frame(matrix(ncol = length(levelnames), nrow=0)), levelnames)
  attributionResult1 <- attributionResult1 %>% mutate_if(is.logical, as.character)
  
  attributionResult2 <- setNames(data.frame(matrix(ncol = length(resultnames), nrow=0)), resultnames)
  attributionResult2 <- attributionResult2 %>% mutate_if(is.logical, as.numeric)
  
  attributionResult <- cbind(attributionResult1, attributionResult2)
  
  return(attributionResult)
}

createAttributionPanel <- function(panel, attributionResult, benchRet, levels) {
  prevRet <- benchRet
  start <- levels + 2
  end <- levels + 5
  for(lvl in 1:levels) {
    if(lvl < levels) { tempPanel <- getCurrLevel(panel, lvl, start, end)
    } else { tempPanel <- getSelLevel(panel, levels, start, end) }
    tempPanel <- modifyPanel(tempPanel)
    if(lvl==1) { levelAttrData <- getLev1Attribution(tempPanel, prevRet) 
    } else { levelAttrData <- getLevAttribution(tempPanel, prevWts, prevRet, lvl, levels) }
    prevWts <- renamePanel(tempPanel)
    prevRet <- getLevelReturn(levelAttrData, prevRet, lvl)
    attributionResult <- attributionResult %>%
      full_join(levelAttrData, by=c(names(levelAttrData)))
  }
  return(attributionResult)
}

organizePanel <- function(attributionPanel, levels, descriptions, weights, levelnames) {
  panelData <- attributionPanel %>%
    left_join(descriptions, by= c('Selection' = 'Aladdin ID'))
  #names(selectionData)[levels] <- 'Selection'
  panelNames <- names(panelData)
  panelNames <- panelNames[c(1:levels, length(panelNames), (levels+1):(length(panelNames)-1))]
  panelData <- panelData[c(panelNames)]
  
  newNames <- append(levelnames, 'Sec_Des')
  weights[weights==''] <- NA
  panelData[panelData==''] <- NA
  
  panelData <- panelData %>%
    full_join(weights, by= c(newNames))
  #left_join(weights, by= c(newNames))
  
  panelNames <- names(panelData)
  categories <- panelNames[1:(levels+1)]
  
  weightNames <- c('PortWt', 'BenchWt', 'PortRet', 'BenchRet')
  attributionNames <- panelNames[(levels+2):(length(panelNames)-length(weightNames))]
  
  newPanelNames <- c(categories, weightNames, attributionNames)
  
  panelData <- panelData[newPanelNames]
  
  # An alternative approach if you want to roll up the Selection Attributioin data into one line....
  # categories <- names(attributionPanel)[1:levels]
  # panelData <- attributionPanel[is.na(attributionPanel$Selection),]
  # panelData$Selection<- NULL
  # selectionData <- attributionPanel[!is.na(attributionPanel$Selection),]
  # selectionData$Selection <- NULL
  # selectionData <- selectionData %>% group_by(!!! rlang::syms(categories)) %>% summarise(SelectionAttr = sum(SelectionAttr))
  #   selectionData <- selectionData[c(selectionNames)]
  # panelData <- panelData %>% full_join(selectionData, by=c(categories))
  # panelData$SelectionAttr <- NULL
  return(panelData)
}


# selectionData <- selectionData %>%
#   left_join(descriptions, by= c('Selection' = 'Aladdin ID'))
# #names(selectionData)[levels] <- 'Selection'
# selectionNames <- names(selectionData)
# selectionNames <- selectionNames[c(1:levels, length(selectionNames), (levels+1):(length(selectionNames)-1))]
# 





cleanPanel <- function(attributionPanel, levels) {
  categories <- names(attributionPanel)[1:(levels-1)]
  attributionPanel <- attributionPanel %>% 
    mutate_if(is.character,~ ifelse(is.na(.), '', .)) #%>%
  #mutate_if(is.numeric,~ ifelse(is.na(.), 0, .))
  attributionPanel[is.na(attributionPanel)] <- 0
  attributionPanel <- attributionPanel %>% arrange(.dots = !!! rlang::syms(categories))
  return(attributionPanel)
}

getDollarPanel <- function(attribPanel, fundMV) {
  # clear out chr columns, multiply by scalar, put back chr columns....
  # or multiply by scalar on condition of being num....
  levels <- attribPanel%>% dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levels <- length(levelnames) / 2
  levelnames <- levelnames[1:levels]
  
  temp <- attribPanel %>% 
    mutate_if(is.numeric,~ ifelse(is.na(.), 0, . * fundMV))
  temp$PortWt <- NULL
  temp$BenchWt <- NULL
  temp$PortRet <- NULL
  temp$BenchRet <- NULL
  
  temp$NAV[1] <- fundMV
  temp <- temp %>%
    #mutate_if(is.numeric,~ sprintf(.,fmt='%#.2f'))
    mutate_if(is.numeric,~ round(.,2))
  temp <- temp %>%
    rename_at(vars(-c('NAV', levelnames, 'Selection', 'Sec_Des')), ~ paste0(.,'Val'))
  temp$NAV <- as.numeric(temp$NAV)
  #temp <- temp %>%
  #mutate(NAV= ifelse(NAV==0, NA, NAV))
  return(temp)
}

applyPriceAdjmts <- function(attribPanel, priceAdjDetails) { # adds higher level price adjustments, i.e. not 'Selection level'
  levels <- attribPanel %>% dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levels <- length(levelnames) / 2
  groupingNames <- levelnames[1:levels]
  groupingNames <- append(groupingNames, 'Selection')
  #tempPriceAdj <- setNames(data.table(matrix(ncol = length(groupingNames), nrow=0)), groupingNames)
  #tempPriceAdj <- tempPriceAdj %>% mutate_if(is.logical, as.character)
  #tempPriceAdj$priceAdj <- as.numeric(tempPriceAdj$priceAdj)
  
  for(i in 1:levels) {
    currentLevel <- groupingNames[i]
    nextLevel <- groupingNames[i+1]
    temp <- data.frame(priceAdjDetails, stringsAsFactors = FALSE)
    #colnames(temp)[colnames(temp)=='Aladdin.ID'] <- 'Aladdin ID'
    names(temp)[1] <- 'Aladdin ID'
    temp$'Aladdin ID' <- NULL
    temp <- temp %>% group_by(!!! rlang::syms(groupingNames[1:i])) %>%
      summarise(tempPriceAdj = sum(priceEffect))
    colnames(temp)[colnames(temp)=='priceAdj'] <- 'tempPriceAdj'
    attribPanel <- attribPanel %>% full_join(temp, by=c(groupingNames[1:i])) #examine effect of full join here....
    attribPanel <- attribPanel %>%
      mutate(priceAdj= ifelse((!! rlang::sym(currentLevel)!='' & !! rlang::sym(nextLevel)==''), tempPriceAdj, priceAdj))
    attribPanel$tempPriceAdj <- NULL
  }
  #tempPriceAdj <- tempPriceAdj[, c((levels+1), (levels+2))]  
  # tempPriceAdj <- tempPriceAdj[complete.cases(tempPriceAdj),] #comment this out if you want more detail
  # tempPriceAdj$priceAdjTemp <- tempPriceAdj$priceAdj
  # tempPriceAdj$priceAdj <- NULL
  # attribPanel <- attribPanel %>% left_join(tempPriceAdj, by=groupingNames[1:levels])  
  # attribPanel <- attribPanel %>% mutate(priceAdj = ifelse(is.na(priceAdj & ), priceAdjTemp, priceAdj))  
  # attribPanel$priceAdjTemp <- NULL
  return(attribPanel)
}

mergePanels <- function(attribPanel, dolAttribPanel) {
  levels <- attribPanel %>% 
    dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levels <- length(levelnames) / 2
  levelnames <- levelnames[1:levels]
  
  if(attribPanel$NAV[1] != dolAttribPanel$NAV[1]) { dolAttribPanel$NAV[1] <- attribPanel$NAV[1] }
  
  attribPanel <- attribPanel %>% 
    mutate_if(is.numeric,~ ifelse(is.na(.), 0, .))
  
  
  
  result <- attribPanel %>%
    full_join(dolAttribPanel, by=c('NAV', levelnames, 'Selection', 'Sec_Des'))
  return(result)
}

addPriceAdjItems <- function(attribPanel, priceAdjItems) {  # adds Selection level price adjustment data
  levels <- attribPanel %>% 
    dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levels <- length(levelnames) / 2 + 1
  levelnames <- levelnames[1:(levels-1)]
  levelnames <- append(levelnames, 'Selection')
  levelnames <- append(levelnames, 'Sec_Des')
  
  temp <- priceAdjItems %>%
    select('Aladdin ID', priceEffect)
  #select('Aladdin ID', Sec_Des, starts_with('Level'), priceEffect)
  
  colnames(temp)[colnames(temp)=='priceEffect'] <- 'tempPriceAdj'
  colnames(temp)[colnames(temp)=='Aladdin ID'] <- 'Selection'
  #temp$tempPriceAdj <- temp$tempPriceAdj #* -1
  
  #if(levels==5) { temp$Level5 <- NULL }
  
  attribPanel <- attribPanel %>% full_join(temp, by='Selection')
  attribPanel <- attribPanel %>%
    mutate(priceAdj= if_else(!is.na(tempPriceAdj), tempPriceAdj, priceAdj))
  attribPanel$tempPriceAdj <- NULL
  
  attribPanel <- attribPanel %>% arrange(.dots = !!! rlang::syms(c(levelnames, 'Sec_Des')))
}

fillLevels <- function(items) {
  levels <- items %>% 
    dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levels <- length(levelnames) + 1
  
  if(levels>5) {
    items <- items %>% 
      mutate(Level4= ifelse(is.na(Level4), Level3, Level4))
    items <- items %>% 
      mutate(Level5= ifelse(is.na(Level5), Level4, Level5))
  }
  colnames(items)[colnames(items)=='Aladdin.ID'] <- 'Aladdin ID'
  return(items)
}

addOtherAdjItems <- function(attribPanel, adjmtsItems) {
  levels <- attribPanel %>% 
    dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levels <- length(levelnames) / 2
  levelnames <- levelnames[1:levels]
  
  temp <- adjmtsItems %>%
    mutate(`Aladdin ID`= ifelse(`Aladdin ID`=='', ID, `Aladdin ID`))
  temp <- temp %>%
    select('Aladdin ID', Sec_Des, TotPctCont)
  colnames(temp)[colnames(temp)=='TotPctCont'] <- 'tempOtherAdj'
  colnames(temp)[colnames(temp)=='Aladdin ID'] <- 'Selection'
  colnames(temp)[colnames(temp)=='Sec_Des'] <- 'tempSec_Des'
  #tempColNames <- c(levelnames, 'Aladdin ID', 'Sec_Des', 'otherAdj')
  #temp <- temp[,c(tempColNames)]
  temp$tempOtherAdj <- temp$tempOtherAdj * -1
  #if(levels==4) { temp$Level5 <- NULL }
  
  attribPanel <- attribPanel %>% full_join(temp, by='Selection')
  attribPanel <- attribPanel %>%
    mutate(otherAdj= if_else(!is.na(tempOtherAdj), tempOtherAdj, otherAdj)) %>%
    mutate(Sec_Des= if_else(!is.na(tempSec_Des), tempSec_Des, Sec_Des))
  attribPanel$tempOtherAdj <- NULL
  attribPanel$tempSec_Des <- NULL
  #attribPanel <- rbind(setDT(attribPanel), setDT(temp), fill=TRUE)
  attribPanel <- attribPanel %>% arrange(.dots = !!! rlang::syms(c(levelnames, 'Selection')))
  return(attribPanel)
}

cleanFinalPanel <- function(finalPanel) {
  finalPanel <- finalPanel %>% 
    mutate_if(is.character,~ ifelse(is.na(.), '', .))
  return(finalPanel)
}

formatPanel <- function(finalPanel) {
  levels <- finalPanel %>% 
    dplyr::select(starts_with('Level'))
  levelnames <- names(levels)
  levels <- length(levelnames) / 3 + 1
  levelnames <- levelnames[1:(levels-1)]
  levelnames <- append(levelnames, 'Selection')
  formatnames <- levelnames[2:levels]
  
  # tempNAV <- finalPanel$NAV[1]
  
  finalPanel$Index <- NA
  finalPanel <- finalPanel %>%
    select(Index, NAV, everything())
  for(level in formatnames) {
    finalPanel <- finalPanel %>%
      mutate(Index= ifelse((!! rlang::sym(level)=='' & is.na(Index)), match(level, formatnames), Index))
  }
  
  finalPanel <- finalPanel %>%
    mutate(Index= ifelse((Selection != '' & is.na(Index)), levels, Index))
  finalPanel <- finalPanel %>%
    mutate(Index= ifelse((Selection != '' & otherAdj!=0 & SelectionAttr==0), 'otherAdj', Index))
  
  finalPanel$Index[1] <- 'Totals'
  finalPanel <- finalPanel[!duplicated(finalPanel),]
  return(finalPanel)
}



# TODo attribPanel$PortfolioRet <- attribPanel$PortCTR - attribPanel$otherAdj
# attribPanel$AttribRet <- attribPanel$PortCTR + attribPanel$priceAdj
# attribPanel$basis <- attribPanel$AttribRet - attribPanel$BenchCTR
# attribPanel$actvRet<- attribPanel$Level1Attr + attribPanel$Level2Attr + attribPanel$Level3Attr +
#   attribPanel$Level4Attr + attribPanel$Level5Attr + attribPanel$SelectionAttr


fillColumns <- function(attribPanel) {
  totals <- head(attribPanel,1)
  attribPanel <- attribPanel[-1,]
  
  attribPanel$PortfolioCTR1[is.na(attribPanel$PortfolioCTR1)] <- 0
  attribPanel$PortfolioCTR <- attribPanel$PortfolioCTR1 - attribPanel$otherAdj
  
  attribPanel$priceAdj[is.na(attribPanel$priceAdj)] <- 0
  attribPanel$AttribCTR <- attribPanel$PortfolioCTR1 + attribPanel$priceAdj
  
  
  #attribPanel[is.na(attribPanel$OfficialCTR)] <- 0
  #attribPanel[is.na(attribPanel$OfficialCTR)] <- 0
  
  attribPanel$basis <- attribPanel$AttribCTR - attribPanel$BenchCTR
  
  attribPanel$BenchCTR[is.na(attribPanel$BenchCTR)] <- 0
  attribPanel$basis[is.na(attribPanel$basis)] <- 0
  
  #attribPanel$PortfolioRet <- (attribPanel$PortRet * attribPanel$PortWt) - attribPanel$otherAdj
  #attribPanel$AttribRet <- (attribPanel$PortRet * attribPanel$PortWt) + attribPanel$priceAdj
  #attribPanel$basis <- attribPanel$AttribRet - (attribPanel$BenchRet * attribPanel$BenchWt)
  attribPanel$actvRet <- attribPanel$Level1Attr + attribPanel$Level2Attr + attribPanel$Level3Attr +
    attribPanel$Level4Attr + attribPanel$Level5Attr + attribPanel$SelectionAttr
  #attribPanel$gap <- attribPanel$Level1Attr + attribPanel$Level2Attr + attribPanel$Level3Attr +
  #attribPanel$Level4Attr + attribPanel$Level5Attr + attribPanel$SelectionAttr - attribPanel$actvRet
  
  
  attribPanel <- rbind(totals, attribPanel)
  # check totals.....
  return(attribPanel)
}
