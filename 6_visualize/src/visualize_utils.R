
fetch_read <- function(ind_file) readRDS(sc_retrieve(ind_file))

format_start_date <- function(storm_data) {
  storm_data <- arrange(storm_data, dateTime)
  start_date <- storm_data$dateTime[1]
  return(start_date)
}
