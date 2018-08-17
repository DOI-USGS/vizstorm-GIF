#' @param ind_file scipiper indicator file
#' @param view_polygon from the view_polygon target
#' @param times the start and end time of the request
fetch_precip_nc <- function(ind_file, view_polygon, times) {
  fabric <- webdata(url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined',
                    variables = "Total_precipitation_surface_1_Hour_Accumulation",
                    times = as.POSIXct(c(times$start,times$end), tz = 'UTC'))

  knife <- webprocess(algorithm = list('OPeNDAP Subset' = "gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"),
                      REQUIRE_FULL_COVERAGE = FALSE, wait = TRUE, OUTPUT_TYPE = 'netcdf')

  crs <- st_crs(view_polygon)
  bbox <- st_bbox(view_polygon)

  as.stencil_pt <- function(box1, box2, col_name){
    st_sf(st_sfc(st_point(c(bbox[[box1]], bbox[[box2]]), dim = "XY")), crs = crs) %>%
      st_transform(crs = "+init=epsg:4326") %>%
      st_coordinates() %>% as.vector() %>% data.frame(x = .) %>% setNames(col_name)
  }

  stencil_pts <- as.stencil_pt('xmin', 'ymax', 'up_left')

  stencil_pts <- as.stencil_pt('xmax', 'ymax', 'up_right') %>% cbind(stencil_pts)

  stencil_pts <- as.stencil_pt('xmax', 'ymin', 'low_right') %>% cbind(stencil_pts)

  stencil_pts <- as.stencil_pt('xmin', 'ymin', 'low_left') %>% cbind(stencil_pts)

  job <- geoknife(stencil = stencil_pts, fabric = fabric, knife = knife)
  data_file <- as_data_file(ind_file)
  download(.Object = job, destination = data_file, overwrite = TRUE)

  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}
