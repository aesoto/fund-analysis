# Top Down Attribution - Funds
# 7 August 2019

attributionLibraries <- c('data.table', 'dplyr', 'tidyr', 'lubridate', 'purrr', 'bizdays', 'rowr', 'here', 'writexl', 'tcltk2')
lapply(attributionLibraries, require, character.only=TRUE)

source(here('functions', 'mainFuncs.R'))

if (sys.nframe() == 0) {
  main()
}
