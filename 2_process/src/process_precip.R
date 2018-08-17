
process_precip_rasters <- function(ind_file, precip_data_ind,
                                   view_polygon,
                                   view_config) {

  precip_data <- readRDS(sc_retrieve(precip_data_ind))


  precip_spatial <- precip_data$spatial %>%
    dplyr::select(-x, -y)

  precip <- precip_data$values


  time <- dplyr::select(precip, time) %>%
    distinct() %>%
    arrange() %>%
    mutate(time_id = 1:n())

  in_per_mm <- 0.0393700787

  precip <- left_join(precip, time, by = "time") %>%
    dplyr::select(-time) %>%
    group_by(id) %>%
    arrange(time_id, .by_group = TRUE) %>%
    mutate(precip = cumsum(precip) * in_per_mm) %>%
    ungroup()

  ncol <- view_config$width
  nrow <- view_config$height

  bbox <- sf::st_bbox(view_polygon)

  y_range <- as.numeric(bbox$ymax - bbox$ymin)
  x_range <- as.numeric(bbox$xmax - bbox$xmin)

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
