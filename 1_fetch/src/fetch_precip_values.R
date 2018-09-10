precip_spatial <- function(nc, view_polygon) {

  if(!is.null(nc$var$RAINRATE)) { # NWM Precip Forcing Method (Tested with medium range)

    nc_proj <- "+proj=lcc +lat_1=30 +lat_2=60 +x_0=0 +y_0=0 +units=m +lat_0=40.0000076294 +lon_0=-97 +a=6370000 +b=6370000 +pm=0 +no_defs"
    # From: ncdfgeom::get_prj(ncatt_get(nc, "ProjectionCoordinateSystem"))

    x_coord <- nc$dim$x$vals
    y_coord <- nc$dim$y$vals
    rows <- length(y_coord)
    cols <- length(x_coord)

    x_vals <- matrix(x_coord, nrow = rows, ncol = cols, byrow = TRUE)
    y_vals <- matrix(y_coord, nrow = rows, ncol = cols, byrow = FALSE)
    x <- matrix(rep(c(1:cols), rows), nrow = rows, ncol = cols, byrow = TRUE)
    y <- matrix(rep(c(1:rows), cols), nrow = rows, ncol = cols, byrow = FALSE)
    id <- matrix(c(1:(rows*cols)), nrow = rows, ncol = cols, byrow = TRUE)

    sf_points <- st_as_sf(data.frame(x_coord = matrix(x_vals, ncol = 1),
                                     y_coord = matrix(y_vals, ncol = 1),
                                     x_ind = matrix(x, ncol = 1),
                                     y_ind = matrix(y, ncol = 1),
                                     id = matrix(id, ncol = 1)),
                          coords = c("x_coord", "y_coord"),
                          crs = nc_proj,
                          agr = "constant")
  } else {

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

  }

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

  return(sf_polygons)

}


get_precip_values <- function(precip_cube_nc_file, dates, view_polygon) {


  nc <- ncdf4::nc_open(precip_cube_nc_file)

  precip_spatial <- precip_spatial(nc, view_polygon)

  t_vals <- get_time_nc(nc$dim$time)

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

  return(list(values = precip, spatial = precip_spatial))
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
