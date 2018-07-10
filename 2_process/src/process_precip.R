library(sf)
library(dplyr)
library(fasterize)

process_precip_rasters <- function(ind_file, precip_spatial_ind,
                                   precip_values_ind,
                                   view_polygon,
                                   view_config) {

  precip_spatial <- readRDS(sc_retrieve(precip_spatial_ind)) %>%
    dplyr::select(-x, -y)

  precip <- readRDS(sc_retrieve(precip_values_ind))


  time <- dplyr::select(precip, time) %>%
    distinct() %>%
    arrange() %>%
    mutate(time_id = 1:n())

  precip <- left_join(precip, time, by = "time") %>%
    dplyr::select(-time)

  # Should get from some config?
  ncol <- view_config$width

  bbox <- sf::st_bbox(view_polygon)

  y_range <- as.numeric(bbox$ymax - bbox$ymin)
  x_range <- as.numeric(bbox$xmax - bbox$xmin)

  aspect_ratio <- x_range / y_range

  nrow <- round(ncol / aspect_ratio)

  raster_template <- raster::raster(ncols = ncol, nrows = nrow,
                                    xmn =  bbox$xmin, xmx =  bbox$xmax,
                                    ymn =  bbox$ymin, ymx =  bbox$ymax,
                                    crs = sf::st_crs(view_polygon)$proj4string)

  rasterize_precip <- function(t_step, precip_spatial, precip, raster_template, r_fun) {
    library(dplyr)
    library(sf)
    library(fasterize)
    try({
      fil <- precip$time_id == t_step
      p_poly <- left_join(precip_spatial, filter(precip, fil), by = "id")
      return(list(fasterize(p_poly, raster_template, field = "precip", fun = r_fun)))},
      silent = TRUE)
    return(t_step)
  }

  cl <- parallel::makeCluster(rep("localhost", 4), type = "SOCK")

  rasters <- snow::parSapply(cl, setNames(as.list(time$time_id), time$time),
                             rasterize_precip,
                             precip_spatial = precip_spatial,
                             precip = precip,
                             raster_template = raster_template,
                             r_fun = "max")

  parallel::stopCluster(cl)

  data_file <- as_data_file(ind_file)
  saveRDS(rasters, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')

}
