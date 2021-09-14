#' Make an sf POINT collection with hurricane locations at each of the
#' timepoints in the `timesteps` viz item
#'
#' @param ind_file the indicator file describing the desired output from this
#'   function
#' @param storm_shp_ind the indicator file of the storm shp zip file
#' @param cfg a list of configuration information including (at least) a CRS
extract_storm_points <- function(ind_file, storm_shp_ind, cfg){

  # get the shp zip file from Drive if we don't already have it
  storm_shp_file <- sc_retrieve(storm_shp_ind, remake_file = getOption("scipiper.remake_file"))

  if(file.size(storm_shp_file) == 0) {
    points_proj <- NULL
  } else {
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
    points_mainstorm <- points %>% dplyr::filter(STORMNAME==stormname)

    # parse the times - assume UTC based on the definition of the DTG column in
    # al092017_pts.shp.xml, which is "the valid date and time of the data in UTC",
    # and which matches HHMM in its last 2 digits
    points_parsed <- points_mainstorm %>%
      mutate(
        DateTimeString = sprintf('%s-%s-%s %s', YEAR, MONTH, DAY, HHMM),
        DateTime = as.POSIXct(DateTimeString, format='%Y-%b-%d %H%M', tz='UTC'))
    if(any(is.na(points_parsed$DateTime))) {
      points_parsed <- points_mainstorm %>%
        mutate(
          # test with numeric month instead of abbreviation
          DateTimeString = sprintf('%s-%s-%s %s', YEAR, MONTH, DAY, HHMM),
          DateTime = as.POSIXct(DateTimeString, format='%Y-%m-%d %H%M', tz='UTC'))
      if(any(is.na(points_parsed$DateTime))) {
        bad_dts <- points_parsed %>%
          filter(is.na(points_parsed$DateTime)) %>%
          pull(DateTimeString)
        stop(paste(
          "Unable to parse datetime for these DateTimeStrings:",
          paste(bad_dts, collapse=', ')
        ))
      }
    }

    # select just the columns we need
    points_simpler <- points_parsed %>% select(
      DateTime,
      MSLP,
      # MSLP: the estimated sea level pressure at the center of a tropical cyclone
      # (the lowest pressure in the system in Millibars)
      STORMTYPE,
      # STORMTYPE: category of the tropical cyclone according to the initial
      # intensity
      INTENSITY,
      # INTENSITY: the highest 1-minute average wind (at an elevation of 10 meters
      # without an unobstructed exposure) associated with a tropical cyclone at a
      # particular point in time (knots)
      SS
      # SS: The Saffir Simpson Hurricane Scale. The Saffir-Simpson Hurricane Scale
      # is a 1-5 rating based on the hurricane's present intensity. This is used
      # to give an estimate of the potential property damage and flooding expected
      # along the coast from a hurricane landfall. Wind speed is the determining
      # factor in the scale, as storm surge values are highly dependent on the
      # slope of the continental shelf and the shape of the coastline, in the
      # landfall region. Note that all winds are using the U.S. 1-minute average.
    )

    # transform to the shared proj.string
    points_proj <- sf::st_transform(points_simpler, crs=sf::st_crs(cfg$projection))
  }

  # write the sf POINTS object to file
  data_file <- as_data_file(ind_file)
  saveRDS(points_proj, data_file)
  gd_put(ind_file, data_file)
}
