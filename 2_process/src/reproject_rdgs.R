reproject_rdgs <- function(ind_file, rdg_ind_file, proj_str) {

  rdgs <- readRDS(sc_retrieve(rdg_ind_file))

  if(length(rdgs) !=  0) {
    rdgs <- rdgs %>%
      sf::st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
      sf::st_transform(crs = proj_str) %>%
      select(site_no, geometry)
  }

  # Write the data file and the indicator file
  saveRDS(rdgs, as_data_file(ind_file))
  gd_put(ind_file)
}
