
prep_datetime_fun <- function(datetime){
  datetime <- strftime(as.POSIXct(datetime), format = '%b %d %I:%M %p')

  plot_fun <- function(){
    plot(datetime, add = TRUE, col = 'grey')
  }
  return(plot_fun)
}
