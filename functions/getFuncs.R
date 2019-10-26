getData <- function(date, isTesting) {
  FoFTypes <- c('EPN', 'MRF', 'MVF', 'USFF')
  datasources <- getSources(date)
  fundData <- getFundsData(datasources, isTesting)
  AUMs <- getAUMs(datasources, fundData)
  fundData <- mergeAUMData(fundData, AUMs)
  fundData <- getBenchAllocations(fundData, datasources)
  accounts <- getAccountsData(datasources, fundData, FoFTypes)
  etfData <- getETFData(datasources)
  futuresBenchmarks <- getFuturesBenchmarks(datasources) # the futures benchmarks are in addition to account benchmarks
  benchmarks <- getBenchmarksData(datasources, fundData, etfData, futuresBenchmarks)
  countryData <- getCountryData(datasources)
  addlCountryData <- getAddlCountryData(datasources)
  addlCompanyData <- getAddlCompanyData(datasources)
  benchmarkKeys <- getBenchmarkKeys(datasources)
  
  futuresData <- getFuturesData(datasources)
  investments <- getInvestmentData(datasources)
  collectedData <- list(funds=fundData,
                        accountsData=accounts,
                        benchmarksData=benchmarks,
                        countryData=countryData,
                        addlCountryData=addlCountryData,
                        addlCompanyData=addlCompanyData,
                        etfData=etfData,
                        futuresData=futuresData,
                        investments=investments,
                        benchmarkKeys=benchmarkKeys)
  
  return(collectedData)
}

getSources <- function(date) {
  data.path <- 'S:/Unit10230/CTI_CMS Project/'
  metadata.path <- 'S:/Unit10230/CTI_CMS Project/Scripts for Data Push/Development/'
  reportDate <- formatDate(date)
  #reportDate <- format(date, format='%Y.%m.%d')
  prevDate <- getPrevDate(date)
  mapDate <- formatDate(prevDate)
  #mapDate <- format(prevDate, format='%Y.%m.%d')
  
  sources <- data.frame(DataCat=c('AUM',
                                  'EPN',
                                  'MRF',
                                  'MVF',
                                  'USFF',
                                  'Benchmarks',
                                  'CountryTaxonomy',
                                  'AddlCountryData',
                                  'Metadata',
                                  'AddlCompanyData',
                                  'etfData',
                                  'futuresData',
                                  'benchmarkKeys',
                                  'futuresBenchmarks'),
                        FilePath=c(paste0(data.path, 'EPN_MVF Map/Archive/'),
                                   paste0(data.path, 'Parent Fund Attribution/Archive/'),
                                   paste0(data.path, 'Parent Fund Attribution/Archive/'),
                                   paste0(data.path, 'Parent Fund Attribution/Archive/'),
                                   paste0(data.path, 'Parent Fund Attribution/Archive/'),
                                   paste0(data.path, 'InvestOne_&_Aladdin Combined Reports/Archive/'),
                                   rep(metadata.path, 8)
                        ),
                        FileName=c(paste0(mapDate, '_EPN_MVF_Map.csv'),
                                   paste0(reportDate, '_EPN_Attribution.csv'),
                                   paste0(reportDate, '_MRF_Attribution.csv'),
                                   paste0(reportDate, '_MVF_Attribution.csv'),
                                   paste0(reportDate, '_USFF_Attribution.csv'),
                                   paste0(reportDate, '_InvestOne_Aladdin_Daily Attribution Benchmark file.csv'),
                                   'Country Taxonomy.csv',
                                   'Addl Country Data.csv',
                                   'Fund List v3.csv',
                                   'Addl Company Data.csv',
                                   'etfData.csv',
                                   'futuresData.csv',
                                   'benchmark Key Data.csv',
                                   'futuresBenchmarks.csv'))
  return(sources)
}

getFundsData <- function(datasources, isTesting) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='Metadata'], datasources$FileName[datasources$DataCat=='Metadata'])
  metadata <- read.csv(filename, stringsAsFactors = FALSE)[ ,c('Account', 'AccountType', 'Description', 'IndexSplit',
                                                               'Benchmark1', 'BenchWt1', 'AttribType1', 'Benchmark2', 'BenchWt2', 'AttribType2', 'Benchmark3', 
                                                               'BenchWt3', 'AttribType3')]
  funds <- metadata[(metadata$AccountType=='EPN' | metadata$AccountType=='MVF' | metadata$AccountType=='MVF_2'),]
  funds$Account <- as.numeric(as.character(funds$Account))
  funds$Benchmark1 <- as.character(funds$Benchmark1)
  funds$Benchmark2 <- as.character(funds$Benchmark2)
  funds$Benchmark3 <- as.character(funds$Benchmark3)
  funds$AttribType1 <- as.character(funds$AttribType1)
  funds$AttribType2 <- as.character(funds$AttribType2)
  funds$AttribType3 <- as.character(funds$AttribType3)
  funds <- funds %>% mutate_if(is.factor, as.character)
  if(isTesting) { funds <- filter(funds, Account==237000 | Account==419000 | Account==491000) }
  return(funds)
}

getAUMs <- function(datasources, fundData) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='AUM'], datasources$FileName[datasources$DataCat=='AUM'])
  mapData <- fread(filename, select = c('ID_1', 'AUM'))  # This should be the 'top' account with all of the assets - see 'Parent_Fund' as other column
  mapData <- mapData[!(mapData$ID_1=='')]
  AUM <- data.frame(Account = as.character(fundData$Account), stringsAsFactors = FALSE)
  #mapData$ID_1 <- as.numeric(as.character(mapData$ID_1))
  AUM <- AUM %>%
    left_join(mapData, by= c('Account' = 'ID_1'))  # Parent_Fund is the other column here
  AUM$Account <- as.numeric(AUM$Account)
  return(AUM)
}

mergeAUMData <- function(fundData, AUMs) {
  mergedData <- fundData %>%
    full_join(AUMs, by=c('Account'))
  return(mergedData)
}

getAccountsData <- function(datasources, fundData, FoFTypes) {
  accountsData <- data.frame(stringsAsFactors = FALSE)
  for(fundType in FoFTypes) {
    filename <- paste0(datasources$FilePath[datasources$DataCat==fundType], datasources$FileName[datasources$DataCat==fundType])
    tempData <- fread(filename, select = c('Parent_ID', 'Fund', 'ID', 'Aladdin ID', 'Total_%Cont',
                                           'BOD NMV', 'Ret', 'Sec_Des', 'Sec_Group', 'Sec_Type',
                                           'GICS_1', 'GICS_2', 'Country'), stringsAsFactors = FALSE)
    setnames(tempData, old=c('Total_%Cont'), new=c('TotPctCont'))
    tempData$TotPctCont <- as.numeric(gsub('%', '', tempData$TotPctCont))
    tempData$Country <- as.character(tempData$Country)
    tempData <- data.frame(tempData, stringsAsFactors = FALSE)
    colnames(tempData)[colnames(tempData)=='Aladdin.ID'] <- 'Aladdin ID'
    colnames(tempData)[colnames(tempData)=='BOD.NMV'] <- 'BOD NMV'
    accountsData <- rbind(accountsData, tempData)
  }
  
  return(accountsData)
}

getBenchmarksData <- function(datasources, fundData, etfData) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='Benchmarks'], datasources$FileName[datasources$DataCat=='Benchmarks'])
  benchmarks <- as.vector(as.matrix(fundData[,c('Benchmark1', 'Benchmark2', 'Benchmark3')]))
  etfBenchmarks <- as.vector(etfData$Benchmark)
  benchmarks <- c(benchmarks, etfBenchmarks)
  benchmarks <- benchmarks[benchmarks!='']
  benchmarks <- unique(benchmarks)
  benchmarksData <- fread(filename, select = c('Fund', 'Aladdin ID', 'Sec_Des', 'Ret_Cont', 'Ret',
                                               'Sec_Group', 'Sec_Type', 'GICS_1', 'GICS_2',
                                               'Country', 'NMV', 'NMV%'), stringsAsFactors = FALSE)
  benchmarksData <- filter(benchmarksData, Fund %in% c(benchmarks))
  setnames(benchmarksData, old=c('NMV%'), new=c('NMVPct'))
  benchmarksData$Fund <- as.character(benchmarksData$Fund)
  benchmarksData$Country <- as.character(benchmarksData$Country)
  benchmarksData$NMVPct <-as.numeric(as.character(benchmarksData$NMVPct))
  return(benchmarksData)
}

getCountryData <- function(datasources) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='CountryTaxonomy'], datasources$FileName[datasources$DataCat=='CountryTaxonomy'])
  countryData <- read.csv(filename, stringsAsFactors=FALSE)
  countryData$X <- NULL
  names(countryData) <- c('CountryCd', 'Level1', 'Level2', 'Level3')
  countryData$CountryCd[is.na(countryData$CountryCd)] <- 'NA'
  return(countryData)
}

getAddlCountryData <- function(datasources) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='AddlCountryData'], datasources$FileName[datasources$DataCat=='AddlCountryData'])
  addlCountryData <- read.csv(filename, stringsAsFactors = FALSE)
  addlCountryData <- addlCountryData[!map_lgl(addlCountryData, ~all(is.na(.)))]
  colnames(addlCountryData)[colnames(addlCountryData)=='Country'] <- 'AddlCountry'
  return(addlCountryData)
}

getAddlCompanyData <- function(datasources) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='AddlCompanyData'], datasources$FileName[datasources$DataCat=='AddlCompanyData'])
  addlCompanyData <- read.csv(filename, stringsAsFactors = FALSE)
  addlCompanyData <- addlCompanyData[!map_lgl(addlCompanyData, ~all(is.na(.)))]
  colnames(addlCompanyData)[colnames(addlCompanyData)=='GICS_1'] <- 'AddlGICS_1'
  colnames(addlCompanyData)[colnames(addlCompanyData)=='GICS_2'] <- 'AddlGICS_2'
  return(addlCompanyData)
}

getFuturesBenchmarks <- function(datasources) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='futuresBenchmarks'], datasources$FileName[datasources$DataCat=='futuresBenchmarks'])
  futuresBenchmarks <- read.csv(filename, stringsAsFactors = FALSE)
  names(futuresBenchmarks)[1] <-'Futures'
  futuresBenchmarks <-futuresBenchmarks[!map_lgl(futuresBenchmarks, ~all(is.na(.)))]
  futuresBenchmarks <- futuresBenchmarks[complete.cases(futuresBenchmarks),]
  return(futuresBenchmarks)
}

getETFData <- function(datasources) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='etfData'], datasources$FileName[datasources$DataCat=='etfData'])
  etfData <- read.csv(filename, stringsAsFactors = FALSE)
  names(etfData)[1] <-'Aladdin ID'
  etfData <-etfData[!map_lgl(etfData, ~all(is.na(.)))]
  etfData <- etfData[complete.cases(etfData),]
  return(etfData)
}

getFuturesData <- function(datasources) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='futuresData'], datasources$FileName[datasources$DataCat=='futuresData'])
  futuresData <- read.csv(filename, stringsAsFactors = FALSE)
  names(futuresData)[1] <-'Index'
  futuresData <-futuresData[!map_lgl(futuresData, ~all(is.na(.)))]
  #futuresData <- futuresData[complete.cases(futuresData),]
  return(futuresData)
}

getBenchAllocations <- function(fundData, datasources) {
  # 29 August 2019 - Agreed by BVirginia to defer for later version
  # Revise datasources to point to benchmark allocation data file
  # Read data file and apply needed benchmark to fundData table
  return(fundData)
}

getInvestmentData <- function(datasources) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='Metadata'], datasources$FileName[datasources$DataCat=='Metadata'])
  metadata <- read.csv(filename, stringsAsFactors = FALSE)[, c('Account', 'AccountType', 'Description', 'Benchmark1')]
  investments <- metadata[metadata$AccountType=='FOF',]
  investments <- investments[!(investments$Benchmark1 ==''),]
  investments$Account <- as.numeric(as.character(investments$Account))
  investments <- investments %>% mutate_if(is.factor, as.character)
  return(investments)
}

getBenchmarkKeys <- function(datasources) {
  filename <- paste0(datasources$FilePath[datasources$DataCat=='benchmarkKeys'], datasources$FileName[datasources$DataCat=='benchmarkKeys'])
  benchmarkKeys <- read.csv(filename, stringsAsFactors = FALSE)
  names(benchmarkKeys)[1] <-'Fund'
  names(benchmarkKeys)[2] <-'Aladdin ID'
  benchmarkKeys <-benchmarkKeys[!map_lgl(benchmarkKeys, ~all(is.na(.)))]
  benchmarkKeys <- benchmarkKeys[complete.cases(benchmarkKeys),]
  return(benchmarkKeys)
}
