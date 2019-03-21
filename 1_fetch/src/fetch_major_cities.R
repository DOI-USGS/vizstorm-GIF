
fetch_major_cities <- function(ind_file, city_config, within = NULL, crs = sf::st_crs(within)){

  # cities should be a combo of maps::us.cities data (selected based on pop_min)
  # and custom config cities (with their lat/long drawn from maps::us.cities
  # unless specified)
  custom_cities <- vapply(city_config$custom, `[[`, 'name', FUN.VALUE='')
  cities_df <- maps::us.cities %>%
    dplyr::filter(pop >= city_config$pop_min | name %in% custom_cities) %>%
    select(name, pop, lat, long)
  cities_df <- cities_df %>%
    # add any cities that weren't listed above
    bind_rows(data_frame(name=setdiff(custom_cities, cities_df$name))) %>%
    # remove cities requested for exclusion
    filter(!(name %in% city_config$exclude)) %>%
    # add formatting defaults
    mutate(
      label = substring(name, 1, nchar(name) - 3),
      dot_bg = city_config$dot$bg,
      dot_col = city_config$dot$col,
      dot_pch = city_config$dot$pch,
      dot_cex = city_config$dot$cex,
      text_col = city_config$text$col,
      text_font = city_config$text$font,
      text_cex = city_config$text$cex,
      text_angle = city_config$text$angle,
      text_dist = city_config$text$dist
    )
  # overwrite maps::us.cities and default info if and only if a different value
  # is specified
  custom_df <- bind_rows(lapply(city_config$custom, function(custom_info) {
    info <- cities_df %>% filter(name == custom_info$name)
    if(nrow(info) != 1) browser()
    for(col in setdiff(colnames(info), 'name')) {
      if(exists(col, custom_info)) info[[col]] <- custom_info[[col]]
    }
    return(info)
  }))
  # merge the custom info into the maps/default info
  cities_df <- bind_rows(
    filter(cities_df, !(name %in% custom_cities)),
    custom_df)

  # format as sf so we can do spatial filtering & projection
  cities_sf <- cities_df %>%
    sf::st_as_sf(
      coords = c('long', 'lat'),
      crs = sf::st_crs("+init=epsg:4326")) # maps::us.cities data always arrive with EPSG 4326

  # filter cities to intersections w/ `within`, using the same crs for both
  if (!is.null(within) && nrow(cities_sf) > 0) {
    cities_trans <- sf::st_transform(cities_sf, crs = sf::st_crs(within))
    subset_idx <- sf::st_within(cities_trans, within, sparse = F)
    cities_sf <- cities_sf[subset_idx, ]
  }

  # transform to the final crs, which has a default but must always be non-NA
  if(!'crs' %in% class(crs)) stop('crs must be specified')
  if(nrow(cities_sf) > 0) {
    cities <- sf::st_transform(cities_sf, crs = crs)
  } else {
    cities <- sf::st_set_crs(cities_sf, value = crs)
  }

  # save and post data, write indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(cities, as_data_file(ind_file))
  gd_put(ind_file)
}
