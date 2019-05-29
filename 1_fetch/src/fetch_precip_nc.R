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
  library(intersectr)
  
  nc_url <- "https://cida.usgs.gov/thredds/dodsC/stageiv_combined"
  
  #################
  # NEED TO GET NetCDF file download happening.
  #################
  
  # nc_var <- nc_vars(nc_file)
  # variable_name <- nc_var$name[1]
  # 
  # nc_coord_vars <- nc_coord_var(nc_file, variable_name)
  # 
  # x_var <- nc_coord_vars$X
  # y_var <- nc_coord_vars$Y
  # t_var <- nc_coord_vars$T
  # 
  # nc <- RNetCDF::open.nc(nc_file)
  # 
  # x <- RNetCDF::var.get.nc(nc, x_var, unpack = TRUE)
  # y <- RNetCDF::var.get.nc(nc, y_var, unpack = TRUE)
  
  # geom <- view_polygon %>%
  #   st_transform(5070)
  # 
  # geom <- geom[5, ]
  # 
  # in_prj <- "+init=epsg:4326"
  # 
  # cell_geometry <- suppressWarnings(
  #   create_cell_geometry(x, y, in_prj, geom))
  # 
  # expect(nrow(cell_geometry) == 340)
  # 
  # cell_geometry <- suppressWarnings(
  #   create_cell_geometry(x, y, in_prj, geom, 10000))
  # 
  # expect(nrow(cell_geometry) == 700)
  # 
  # data_source_cells <- st_sf(select(cell_geometry, grid_ids))
  # target_polygons <- st_sf(select(geom, CNTY_ID))
  # 
  # sf::st_agr(data_source_cells) <- "constant"
  # sf::st_agr(target_polygons) <- "constant"
  # 
  # area_weights <- calculate_area_intersection_weights(
  #   data_source_cells,
  #   target_polygons)
  # 
  # suppressWarnings(intersected <- execute_intersection(nc_file, variable_name, area_weights,
  #                                                      cell_geometry, x_var, y_var, t_var))
  
  nc_file <- as_data_file(ind_file)
  gd_put(remote_ind=ind_file, local_source=nc_file, mock_get='none')
}
