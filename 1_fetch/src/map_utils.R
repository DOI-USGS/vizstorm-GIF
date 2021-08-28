#' turn a bbox vector into a polygon
#'
#' @param bbox a length 4 numeric vector of xmin, ymin, xmax, ymax
#' @param bbox_crs the coordinate reference system the `bbox` is using
#' @param return_crs the coordinate reference system to convert the data to
#'
#' @return an sf polygon that based on coordinates of `bbox`
bbox_to_polygon <- function(bbox, bbox_crs = "+init=epsg:4326", return_crs = NULL) {
  names(bbox) <- c("xmin","ymin","xmax","ymax")
  class(bbox) <- "bbox"
  bbox_poly <- sf::st_as_sfc(bbox)
  st_crs(bbox_poly) <- st_crs(bbox_crs)
  if (!is.null(return_crs)) {
    sf::st_transform(bbox_poly, return_crs)
  }
  else {
    bbox_poly
  }
}


#' Create a polygon for the plotting view
#'
#'
#' @param view_config a list that contains the numeric vector `bbox`, `height`, `width` and `projection` (optional)
#'
#' @details if `projection` is missing, it is assumed to be "+init=epsg:4326"
#' @return an sf polygon that contains the `bbox` specfied at the aspect ratio specified
as_view_polygon <- function(view_config) {

  # the projected polygon specified by the user
  bbox_projected <- bbox_to_polygon(bbox = unlist(view_config$bbox), return_crs = view_config$projection)
  poly_bbox <- as.numeric(sf::st_bbox(bbox_projected))
  aspect <- view_config$width / view_config$height
  bbox_aspect <- diff(poly_bbox[c(1,3)]) / diff(poly_bbox[c(2,4)])
  if (bbox_aspect > aspect){ # flatter than it should be
    new_y_diff <- diff(poly_bbox[c(1,3)]) / aspect
    y_buffer <- new_y_diff - diff(poly_bbox[c(2,4)])
    poly_bbox[2] <- poly_bbox[2] - y_buffer / 2
    poly_bbox[4] <- poly_bbox[4] + y_buffer / 2

  } else { # taller than it should be
    # new x dimension
    new_x_diff <- diff(poly_bbox[c(2,4)]) * aspect
    # new x dimension - existing y dimension
    x_buffer <- new_x_diff - diff(poly_bbox[c(1,3)])
    # subtract half the buffer from min x, add half to max x
    poly_bbox[1] <- poly_bbox[1] - x_buffer / 2
    poly_bbox[3] <- poly_bbox[3] + x_buffer / 2
  }

  view_poly <- bbox_to_polygon(poly_bbox, bbox_crs = st_crs(bbox_projected))
  return(view_poly)
}

#' create a view polygon from the configuration info and then write it to a file
#' and push it to Drive so that we can use a shared version of the polygon
#' across OSes / GDAL versions
post_view_polygon <- function(ind_file, view_config) {
  data_file <- as_data_file(ind_file)
  view_polygon <- as_view_polygon(view_config)
  saveRDS(view_polygon, file=data_file)
  gd_put(ind_file, data_file)
}

#' download and read in the view polygon
get_view_polygon <- function(view_poly_ind) {
  readRDS(sc_retrieve(view_poly_ind,
                      remake_file = getOption("scipiper.remake_file")))
}


#' fetch and merge geometries from maps and mapdata packages
#'
#' @param geoms_config a list that includes named arguments for maps::map(...) function calls
#' @param crs an option st_crs object, or it is taken from `within` argument
#' @param within a `sf` polygon to check for intersections (outside are excluded)
#'
#' @return an sf object with geometries filtered according to input arguments
fetch_geoms <- function(ind_file, geoms_config, crs = sf::st_crs(within), within = NULL){

  fetch_sf_geoms <- function(...){
    # get data from maps package
    map_data <- sf::st_as_sf(maps::map(..., fill = TRUE, plot = FALSE))
    if (!is.na(crs)) {
      # crs will be NA if within is NULL
      map_data <- sf::st_transform(map_data, crs)
    }
    if (!is.null(within)) {
      # filter polygons to intersections w/ `within`
      subset_idx <- sf::st_intersects(map_data, within, sparse = F)
      map_data <- map_data[subset_idx, ]
    }
    return(map_data)
  }

  # call for all features specified
  geoms_list <- lapply(geoms_config[[1]], function(x) do.call(fetch_sf_geoms, x))

  geoms_out <- geoms_list[[1]]
  # merge if there are more than one
  if (length(geoms_list) > 1){
    for (i in 2:length(geoms_list)){
      geoms_out <- rbind(geoms_out, geoms_list[[i]])
    }
  }

  # save and post data, write indicator file
  saveRDS(geoms_out, as_data_file(ind_file))
  gd_put(ind_file, ind_file)
}
