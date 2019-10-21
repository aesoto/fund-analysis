source(here('functions/cleaning', 'cleanBenchmarks.R'))
source(here('functions/cleaning', 'cleanAccounts.R'))
# source(here('functions/testing','testCleaning.R'))

cleanData <- function(rawData) {
  fundsData <- rawData$funds
  accountsData <- rawData$accountsData
  benchmarksData <- rawData$benchmarksData
  countryData <- rawData$countryData
  addlCountryData <- rawData$addlCountryData
  addlCompanyData <- rawData$addlCompanyData
  etfData <- rawData$etfData
  futuresData <- rawData$futuresData
  investments <- rawData$investments
  benchmarkKeys <- rawData$benchmarkKeys
  
  benchmarks <- cleanBenchmarksData(benchmarksData, fundsData, countryData, addlCountryData,
                                    addlCompanyData, benchmarkKeys)
  accounts <- cleanAccountsData(accountsData, fundsData, countryData, addlCountryData, 
                                addlCompanyData, benchmarks, investments, etfData, futuresData)  
  cleanedData <- list(funds=fundsData,
                      accountsData=accounts,
                      benchmarksData=benchmarks)
  return(cleanedData)
}
