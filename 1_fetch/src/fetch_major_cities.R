
fetch_major_cities <- function(ind_file, city_pop_min, crs = sf::st_crs(within), within = NULL){

  cities <- maps::us.cities %>%
    dplyr::filter(pop > city_pop_min)

  cities <- sf::st_as_sf(cities, coords = c('long', 'lat'))

  if (!is.na(crs)) {
    # crs will be NA if within is NULL
    cities <- sf::st_transform(cities, crs = crs)
  }
  if (!is.null(within)) {
    # filter cities to intersections w/ `within`
    subset_idx <- sf::st_within(cities, within, sparse = F)
    cities <- cities[subset_idx, ]
  }

  # save and post data, write indicator file
  data_file = as_data_file(ind_file)
  saveRDS(cities, data_file)
  gd_put(ind_file, data_file)
}
