saveData <- function(output, date) {
  fileData <- list()
  for(account in names(output)) {
    accountPacket <- output[[account]]
    if(account=='metrics') { 
      fileData[[account]] <- accountPacket
    } else {
      accountData <- accountPacket$finalPanel
      fileData[[account]] <- accountData
    }
  }
  
  reportDate <- format(date, format='%Y.%m.%d')
  this.file.name <- paste0(reportDate, ' FoF Top Down Attribution.xlsx')
  #output.path <- here('Archive', this.file.name)
  output.path <- paste0('S:/Unit10230/CTI_CMS Project/Top Down Attribution/Archive FoFs/', this.file.name)
  write_xlsx(fileData, path=output.path, col_names=TRUE, format_headers=TRUE)
  
  # Open file, apply grouping algorithm
  
  
}


# ######## Revised version of this function ########
# ####  Keep for some other application ####
# saveData <- function(output, date) {
#   account <- '216'
#  
#   outwb <- createWorkbook()
#   csTableRowNames <- CellStyle(outwb) + Font(outwb, isBold=FALSE)
#   csTableColNames <- CellStyle(outwb) + Font(outwb, isBold=TRUE) +
#     Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black",
#     position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
#   csValColumn <- CellStyle(outwb, dataFormat=DataFormat("0.00"))
#   csRetColumn <- CellStyle(outwb, dataFormat=DataFormat("0.00000"))
# 
#   attribution.colRatio = list(
#    '7'=csRetColumn, '8'=csRetColumn, '9'=csRetColumn, '10'=csRetColumn,
#    '11'=csRetColumn, '12'=csRetColumn, '13'=csRetColumn, '14'=csRetColumn,
#    '15'=csRetColumn, '16'=csRetColumn, '17'=csRetColumn
#    )
#   attribution.colPerc =list(
#    '1'=csValColumn, '18'=csValColumn, '19'=csValColumn, '20'=csValColumn,
#    '21'=csValColumn, '22'=csValColumn, '23'=csValColumn, '24'=csValColumn,
#    '25'=csValColumn, '26'=csValColumn, '27'=csValColumn, '28'=csValColumn,
#    '29'=csValColumn
#    )
#   sheet <- createSheet(outwb, sheetName = account)
#   addDataFrame(output$`216`$finalPanel, sheet, startRow=1,
#                startColumn=1, 
#                colStyle= c(attribution.colPerc, attribution.colRatio),
#                colnamesStyle=csTableColNames,
#                rownamesStyle=csTableRowNames
#                )
#   setColumnWidth(sheet, colIndex=c(2:15), colWidth=11)
#   setColumnWidth(sheet, colIndex=16, colWidth=13)
#   setColumnWidth(sheet, colIndex=17, colWidth=6)
#   setColumnWidth(sheet, colIndex=1, colWidth= 0.8*max(length(rownames(x.RiskStats)))) 
#   
#   saveWorkbook(outwb, "DJUBS Commodities Performance Summary.xlsx")
# }
