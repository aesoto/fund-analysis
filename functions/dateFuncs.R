askDate <- function() { return(readline(prompt='Enter report date: ')) }

setDate <- function(input) {
  date <- mdy(input)
  return(date)
}

getPrevDate <- function(date) {
  create.calendar(name='mycal', weekdays=c('saturday', 'sunday'))
  prevDate <- bizdays::offset(date, -1, 'mycal')
  return(prevDate)
}

formatDate <- function(date) { return(format(date, format='%Y.%m.%d')) }

getDate <- function() {
  input <- askDate()
  date <- setDate(input)
  return(date)
}