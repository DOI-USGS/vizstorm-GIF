
POSIX_from_filename <- function(filename){
  date_char <- gsub("\\[|\\]", "", regmatches(filename, gregexpr("\\[.*?\\]", filename))[[1]][1])
  posix_out <- as.POSIXct(date_char, format = '%Y%m%d-%H', tz = "UTC")
  if (is.na(posix_out)){
    stop('Parsing error with', filename, call. = FALSE)
  }
  return(posix_out)
}

write_put_fun <- function(plot_fun, ind_file){
  data_file <- as_data_file(ind_file)
  saveRDS(plot_fun, data_file)
  gd_put(ind_file, data_file)
}
