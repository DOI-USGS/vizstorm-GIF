#' Make an sf POINT collection with hurricane locations at each of the
#' timepoints in the `timesteps` viz item
#'
#' @param ind_file the indicator file describing the desired output from this
#'   function
#' @param storm_shp_ind the indicator file of the storm shp zip file
#' @param cfg a list of configuration information including (at least) a CRS
extract_storm_points <- function(ind_file, storm_shp_ind, cfg){

  # get the shp zip file from Drive if we don't already have it
  storm_shp_file <- sc_retrieve(storm_shp_ind)

  # unzip into a temporary directory
  tmp_path <- file.path(tempdir(), 'storm_track')
  if (!dir.exists(tmp_path)) dir.create(tmp_path)
  unzip(storm_shp_file, exdir = tmp_path)

  # read the unzipped files. the pattern name is based on the NHC convention and
  # could change without warning
  layer <- tools::file_path_sans_ext(list.files(tmp_path, pattern='pts\\.shp$'))
  points <- sf::read_sf(tmp_path, layer=layer)

  # delete our very temporary directory now that the shapefiles are read in
  unlink(tmp_path, recursive = TRUE)

  # filter to only those obs where the storm was identified as the modal storm
  # name (sometimes they change names early on). Or should we keep all points?
  stormname <- names(which.max(table(points$STORMNAME)))
  points_mainstorm <- points %>% filter(STORMNAME==stormname)

  # parse the times
  as.time <- function(YEAR, MONTH, DAY, HHMM){
    as.POSIXct(sprintf('%s-%s-%s %s', YEAR, MONTH, DAY, HHMM), format='%Y-%m-%d %H%M', tz="America/Puerto_Rico")
  }
  points_parsed <- points_mainstorm %>%
    mutate(DateTime = as.time(YEAR, MONTH, DAY, HHMM))

  # select just the columns we need
  points_simpler <- points_parsed %>%
    select(LAT, LON, DateTime, TAU, MSLP, STORMTYPE, INTENSITY, SS)

  # transform to the shared proj.string
  track_proj <- sf::st_transform(track, crs=cfg$projection)
  points_proj <- sf::st_transform(points_simpler, cfg$projection)

  # write the sf POINTS object to file
  data_file <- as_data_file(ind_file)
  saveRDS(points_proj, data_file)
  gd_put(ind_file, data_file)
}
