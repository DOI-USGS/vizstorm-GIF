
#' @param ind_file scipiper indicator file
#' @param view_config with `projection`, a valid input to sf::st_crs
#' and `bbox` e.g. [-83, 25, -80, 28]
fetch_precip_spatial <- function(ind_file, view_config) {

  nc <- ncdf4::nc_open("https://cida.usgs.gov/thredds/dodsC/stageiv_combined")

  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")

  x <- matrix(rep(c(1:nrow(lon)), ncol(lon)),
              nrow = nrow(lon), ncol = ncol(lon),
              byrow = FALSE)

  y <- matrix(rep(c(1:ncol(lon)), nrow(lon)),
              nrow = nrow(lon), ncol = ncol(lon),
              byrow = TRUE)

  sf_points <- data.frame(x = matrix(x, ncol = 1),
                          y = matrix(y, ncol = 1),
                          lon = matrix(lon, ncol = 1),
                          lat = matrix(lat, ncol = 1))

  sf_points <- sf::st_as_sf(sf_points,
                            coords = c("lon", "lat"),
                            crs = "+init=epsg:4326",
                            agr = "constant")

  bbox <- bbox_to_polygon(bbox = view_config$bbox,
                          return_crs = view_config$projection)

  sf_points_sub <- sf::st_intersection(
    sf::st_transform(sf_points,
                     view_config$projection),
    bbox)

  sf_polygons <- sf::st_sf(
    sf::st_intersection(
    sf::st_cast(
      sf::st_voronoi(
        sf::st_union(
          sf_points_sub$geometry))), bbox))

  sf_polygons <- sf::st_join(sf_polygons, sf_points_sub, join = sf::st_contains)

  data_file <- as_data_file(ind_file)
  saveRDS(sf_polygons, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')

}
