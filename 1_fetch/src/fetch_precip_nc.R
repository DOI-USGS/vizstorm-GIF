#' @param ind_file scipiper indicator file
#' @param view_polygon from the view_polygon target
#' @param times the start and end time of the request
fetch_precip_data <- function(ind_file, view_polygon, times) {
  fabric <- webdata(
    url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined',
    variables = "Total_precipitation_surface_1_Hour_Accumulation",
    times = as.POSIXct(c(times$start,times$end), tz = 'UTC'))

  knife <- webprocess(algorithm = list('OPeNDAP Subset' = "gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"),
                      REQUIRE_FULL_COVERAGE = FALSE, wait = TRUE, OUTPUT_TYPE = 'netcdf')

  # create the stencil from the bbox of the view_polygon
  crs <- st_crs(view_polygon)
  bbox <- st_bbox(view_polygon)

  precip_boundary <- "POLYGON ((-80.74672 19.80368, -119.023 23.117, -134.043 53.51192, -124.9736 55.90588, -114.5051 57.4236, -102.7265 57.82357, -91.28471 56.95466, -81.85043 55.19577, -73.63054 52.69872, -66.4331 49.53428, -59.95137 45.6187, -80.74672 19.80368))" %>%
    st_as_sfc(crs = st_crs("+proj=longlat +datum=WGS84"))

  stencil <- st_transform(view_polygon, "+proj=longlat +datum=WGS84") %>%
    st_intersection(precip_boundary)

  stencil <- as_Spatial(stencil)
  stencil@proj4string <- sp::CRS("+proj=longlat +datum=WGS84") # geoknife complains otherwise
  stencil <- geoknife::simplegeom(stencil)

  # run the job and retrieve the results
  job <- geoknife(stencil = stencil, fabric = fabric, knife = knife)
  nc_file <- as_data_file(ind_file)
  download(.Object = job, destination = nc_file, overwrite = TRUE)

  gd_put(remote_ind=ind_file, local_source=nc_file, mock_get='none')
}

#######################################################################
# Code to create the precip boundary above.                           #
# The GDP does not like stencils way outside the domain of the data?  #
#######################################################################
# nc <- nc_open("https://cida.usgs.gov/thredds/dodsC/stageiv_combined")
# lon <- ncdf4::ncvar_get(nc, "lon")
# lat <- ncdf4::ncvar_get(nc, "lat")
# x <- matrix(rep(c(1:ncol(lon)), nrow(lon)),
#             nrow = nrow(lon), ncol = ncol(lon),
#             byrow = TRUE)
#
# y <- matrix(rep(c(1:nrow(lon)), ncol(lon)),
#             nrow = nrow(lon), ncol = ncol(lon),
#             byrow = FALSE)
#
# sf_points <- data.frame(x = matrix(x, ncol = 1),
#                         y = matrix(y, ncol = 1),
#                         lon = matrix(lon, ncol = 1),
#                         lat = matrix(lat, ncol = 1))
# sf_points <- sf::st_as_sf(sf_points,
#                           coords = c("lon", "lat"),
#                           crs = "+init=epsg:4269",
#                           agr = "constant")
# boundary <- st_convex_hull(st_union(sf_points))
# bsimp <- st_transform(st_simplify(st_transform(boundary, 5070), dTolerance = 10000),4269)
# st_as_text(bsimp)

fetch_precip_notGDP <- function(ind_file, view_polygon, times) {
  library(ncmeta)
  library(RNetCDF)
  library(intersectr)

  nc_file <- "https://cida.usgs.gov/thredds/dodsC/stageiv_combined"

  nc_var <- nc_vars(nc_file)
  variable_name <- nc_var$name[1]

  nc_coord_vars <- nc_coord_var(nc_file, variable_name)

  #variable names
  x_var <- nc_coord_vars$X
  y_var <- nc_coord_vars$Y
  t_var <- nc_coord_vars$T

  nc <- RNetCDF::open.nc(nc_file)
  #actual coord values, unprojected
  x_coords <- RNetCDF::var.get.nc(nc, x_var, unpack = TRUE)
  y_coords <- RNetCDF::var.get.nc(nc, y_var, unpack = TRUE)
  #time, in UTC
  dates <- RNetCDF::utcal.nc(unitstring = att.get.nc(nc, t_var, "units"),
                             value = var.get.nc(nc, t_var, unpack = TRUE),
                             type = "c")

  in_prj <- "+init=epsg:4326" #unprojected WGS84

  #just a box for view
  geom <- view_polygon %>%
    st_transform(in_prj)

  #have stencil
  cell_geometry <- suppressWarnings(
    create_cell_geometry(X_coords = x_coords, Y_coords = y_coords,
                         prj = in_prj, geom = geom,
                         buffer_dist = 0.1, #degrees
                         regularize = TRUE))
  X_inds <- seq(min(cell_geometry$X_ind), max(cell_geometry$X_ind), 1)
  Y_inds <- seq(min(cell_geometry$Y_ind), max(cell_geometry$Y_ind), 1)
  T_inds <- seq(which(dates == times$start), which(dates == times$end))
  #run with nccopy using system, and variables by name
  #dims are time,y,x
  x_range_brackets <- paste0("[",min(X_inds) - 1, ":1:", max(X_inds) -1, "]")
  y_range_brackets <- paste0("[",min(Y_inds) - 1, ":1:", max(Y_inds) - 1, "]")
  t_range_brackets <- paste0("[",min(T_inds) - 1, ":1:", max(T_inds) - 1, "]")

  nccopy_command <- paste0("nccopy ", nc_file, "?", variable_name,
                            t_range_brackets, x_range_brackets, y_range_brackets, ",",
                            t_var, t_range_brackets, ",",
                           x_var, y_range_brackets, x_range_brackets, ",",
                           y_var, y_range_brackets, x_range_brackets,
                           " ", as_data_file(ind_file)
                           )
  message("Running nccopy against thredds...")
  system(nccopy_command)
  message("Done")
  #now indicate and push to drive
  gd_put(ind_file)
}
