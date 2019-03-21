#' @param ind_file scipiper indicator file
#' @param ymd_str task_name string of year month day, e.g. "20190319"
#' @param tmp_dir directory to store these intermediate files
fetch_snow_data <- function(ind_file, ymd_str, tmp_dir) {
  
  # construct URL
  date_format <- as.Date(ymd_str, format="%Y%m%d") 
  year <- format(date_format, "%Y")
  month <- format(date_format, "%m")
  month_str <- month.abb[as.numeric(month)]
  
  # Identify download location & tmp save location
  ftp_base <- "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/unmasked/%s/%s"
  ftp_folder <- sprintf(ftp_base, year, paste(month, month_str, sep = "_"))
  url_tar <- sprintf('%s/SNODAS_unmasked_%s.tar', ftp_folder, ymd_str)
  fn_tar <- file.path(tmp_dir, sprintf("%s.tar", ymd_str))
  
  # Download and unzip the snow data
  download.file(url_tar, destfile = fn_tar, mode = "wb")
  untar(fn_tar, exdir = tmp_dir)
  
  # Find the snow depth file and unzip
  fn_tmp <- list.files(tmp_dir, full.names = TRUE)
  fn_snow <- fn_tmp[grep("zz_ssmv11036", fn_tmp)]
  fn_snow_dat <- fn_snow[grep(".dat", fn_snow)]
  fn_zip <- fn_snow_dat[grep(ymd_str, fn_snow_dat)]
  fn_out <- as_data_file(ind_file)
  R.utils::gunzip(fn_zip, destname = fn_out)
  
  gd_put(remote_ind=ind_file, local_source=fn_out, mock_get='none')
}
