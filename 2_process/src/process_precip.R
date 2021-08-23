process_precip_spatial <- function(ind_file, precip_data_nc_ind, view_polygon) {

  nc <- ncdf4::nc_open(sc_retrieve(precip_data_nc_ind, remake_file = getOption("scipiper.remake_file")))

  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")

  x <- matrix(rep(c(1:ncol(lon)), nrow(lon)),
              nrow = nrow(lon), ncol = ncol(lon),
              byrow = TRUE)

  y <- matrix(rep(c(1:nrow(lon)), ncol(lon)),
              nrow = nrow(lon), ncol = ncol(lon),
              byrow = FALSE)

  sf_points <- data.frame(x = matrix(x, ncol = 1),
                          y = matrix(y, ncol = 1),
                          lon = matrix(lon, ncol = 1),
                          lat = matrix(lat, ncol = 1))
  sf_points <- sf::st_as_sf(sf_points,
                            coords = c("lon", "lat"),
                            crs = "+init=epsg:4326",
                            agr = "constant")

  sf_points_sub <- sf::st_intersection(
    sf::st_transform(sf_points,
                     sf::st_crs(view_polygon)),
    view_polygon)

  sf_polygons <- sf::st_sf(
    sf::st_intersection(
      sf::st_cast(
        sf::st_voronoi(
          sf::st_union(
            sf_points_sub$geometry))), view_polygon))

  sf_polygons <- sf::st_join(sf_polygons, sf_points_sub, join = sf::st_contains)

  sf_polygons <- mutate(sf_polygons, id = 1:nrow(sf_polygons))

  data_file <- as_data_file(ind_file)
  saveRDS(sf_polygons, file = data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')

}


process_precip_values <- function(ind_file, precip_data_nc_ind,
                                  precip_spatial_ind, dates, view_polygon) {

  nc <- ncdf4::nc_open(sc_retrieve(precip_data_nc_ind, remake_file = getOption("scipiper.remake_file")))

  precip_spatial <- readRDS(sc_retrieve(precip_spatial_ind, remake_file = getOption("scipiper.remake_file")))

  t_vals <- get_time_nc(nc$dim$time)
  # Strip out the `dim` attribute to avoid binding issues later,
  #   see https://github.com/r-lib/vctrs/issues/1329
  dim(t_vals) <- NULL

  start_date <- as.POSIXct(dates$start, tz = 'UTC')
  end_date <- as.POSIXct(dates$end, tz = 'UTC')

  x_inds <- seq(min(precip_spatial$x), max(precip_spatial$x), 1)
  y_inds <- seq(min(precip_spatial$y), max(precip_spatial$y), 1)
  t_inds <- which(t_vals >= start_date & t_vals <= end_date)

  # ensures we make our request in the right axis order.
  dimid_order <- match(nc$var$Total_precipitation_surface_1_Hour_Accumulation$dimids,
                       c(nc$dim$x$id, nc$dim$y$id, nc$dim$time$id))

  precip <- array(dim = c(length(x_inds), length(y_inds), length((t_inds))))
  t_counter <- 1

  for(step in seq(min(t_inds), max(t_inds), 1)) {
    precip[,,t_counter] <-
      ncdf4::ncvar_get(nc, nc$var$Total_precipitation_surface_1_Hour_Accumulation,
                       start <- c(min(x_inds),
                                  min(y_inds),
                                  step)[dimid_order],
                       count <- c(length(x_inds),
                                  length(y_inds),
                                  1)[dimid_order])

    if (t_counter %% 10 == 0){
      message(paste(t_counter, 'of', length(t_inds)))
    }

    t_counter <- t_counter + 1
  }
  nc_close(nc)
  precip <- arrayhelpers::array2df(precip)

  x_inds <- data.frame(d1 = seq_len(length(x_inds)), x = x_inds)
  y_inds <- data.frame(d2 = seq_len(length(y_inds)), y = y_inds)
  t_inds <- data.frame(d3 = seq_len(length(t_inds)), t = t_inds)

  precip <- precip %>%
    left_join(x_inds, by = "d1") %>%
    left_join(y_inds, by = "d2") %>%
    left_join(t_inds, by = "d3") %>%
    inner_join(sf::st_set_geometry(precip_spatial, NULL),
               by = c("x", "y")) %>%
    left_join(data.frame(time = t_vals,
                         t = seq_len(length(t_vals))),
              by = "t") %>%
    dplyr::select(precip, time, id)

  data_file <- as_data_file(ind_file)
  saveRDS(precip, file = data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}

get_time_nc <- function(t_dim) {
  time_units<-strsplit(t_dim$units, " ")[[1]]
  time_step<-time_units[1]
  date_origin<-time_units[3]

  if(grepl("hour", time_step, ignore.case = TRUE)) {
    multiplier <- (60^2)
  } else {
    stop("only hour time steps supported so far")
  }

  origin <- as.POSIXct(strptime(date_origin,
                                format = "%Y-%m-%dT%H:%M:%SZ",
                                tz = "UTC"),
                       tz = "UTC")

  as.POSIXct(t_dim$vals * multiplier,
             origin = origin,
             tz = "UTC")
}


process_precip_rasters <- function(ind_file, precip_spatial_ind,
                                   precip_values_ind,
                                   view_polygon,
                                   view_config) {

  precip <- readRDS(sc_retrieve(precip_values_ind, remake_file = getOption("scipiper.remake_file")))


  precip_spatial <- readRDS(sc_retrieve(precip_spatial_ind, remake_file = getOption("scipiper.remake_file"))) %>%
    dplyr::select(-x, -y)

  time <- dplyr::select(precip, time) %>%
    distinct() %>%
    arrange() %>%
    mutate(time_id = 1:n())

  in_per_mm <- 0.0393700787

  precip <- precip %>%
    left_join(time, by = "time") %>%
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

  rasters <- snow::parSapply(cl, setNames(as.list(time$time_id), time$date_time),
                             rasterize_precip,
                             precip_spatial = precip_spatial,
                             precip = precip,
                             raster_template = raster_template,
                             r_fun = "max")

  parallel::stopCluster(cl)

  # Name the rasters the same as the timestep they represent
  #   8.23.2021: this part used to just happen and downstream
  #   steps depend on the naming to be like this.
  names(rasters) <- as.character(time$time)

  data_file <- as_data_file(ind_file)
  saveRDS(rasters, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')

}
