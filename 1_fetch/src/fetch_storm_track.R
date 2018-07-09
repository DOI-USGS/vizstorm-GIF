#' Download a storm track from the National Hurricane Center
#'
#' Go to https://www.nhc.noaa.gov/gis/archive_besttrack.php?year=2018 to see a list of options
#'
#' @param out_file character file name where the output should be saved
#' @param storm_code the 8-digit character ID identifying a storm by the codes used at https://www.nhc.noaa.gov/gis/archive_besttrack.php?year=2018. The components are a 2-digit ocean ID ('al' or 'ep'), a 2-digit storm number (counts up from 01 throughout the year), and a 4-digit year. Go to the NHC website to identify the right code.
#' year: 2017
fetch_storm_track <- function(
  ind_file,
  cfg
) {

  # get from NHC
  url <- sprintf("http://www.nhc.noaa.gov/gis/best_track/%s_best_track.zip", cfg$storm_code)
  httr::GET(url, httr::write_disk(ind_file, overwrite=TRUE))

  # post to Google Drive
  data_file <- as_data_file(ind_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}
