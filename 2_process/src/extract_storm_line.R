#' Make an sfc_LINESTRING object from the storm track data (for plotting the
#' path as a line)
#' @param ind_file the indicator file describing the desired output from this
#'   function
#' @param storm_shp_ind the indicator file of the storm shp zip file
#' @param cfg a list of configuration information including (at least) a CRS
#'   field called `projection`
extract_storm_line <- function(ind_file, storm_shp_ind, cfg){

  # get the shp zip file from Drive if we don't already have it
  storm_shp_file <- sc_retrieve(storm_shp_ind, remake_file = getOption("scipiper.remake_file"))

  if(file.size(storm_shp_file) == 0) {
    track_proj <- NULL
  } else {
    # unzip into a temporary directory and read from there
    tmp_path <- file.path(tempdir(), 'storm_track')
    if (!dir.exists(tmp_path)) dir.create(tmp_path)
    unzip(storm_shp_file, exdir = tmp_path)
    layer <- tools::file_path_sans_ext(list.files(tmp_path, pattern='lin\\.shp$'))
    track <- sf::read_sf(tmp_path, layer=layer)
    unlink(tmp_path, recursive = TRUE)

    # convert to the vizzy projection
    track_proj <- sf::st_transform(track, crs=sf::st_crs(cfg$projection))
  }

  # write the data and return an indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(track_proj, data_file)
  gd_put(ind_file, data_file)
}
