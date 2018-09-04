#' @param ind_file scipiper indicator file
#' @param view_polygon from the view_polygon target
#' @param times the start and end time of the request
fetch_precip_data <- function(ind_file, view_polygon, times) {

  knife <- webprocess(algorithm = list('OPeNDAP Subset' = "gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"),
                      REQUIRE_FULL_COVERAGE = FALSE, wait = TRUE, OUTPUT_TYPE = 'netcdf')

  # create the stencil from the bbox of the view_polygon
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

  # break times into chunks to avoid WAF rejections of queries that are "too big"
  dateseq <- unique(c(
    seq(as.POSIXct(times$start, tz='UTC'), as.POSIXct(times$end, tz='UTC'), by=as.difftime(3, units='days')),
    as.POSIXct(times$end, tz='UTC')
  ))
  precip_data_list <- lapply(seq_len(length(dateseq)-1), function(i) {
    message(sprintf("pulling precip for %s to %s", dateseq[i], dateseq[i+1]))
    # define the fabric, subset by time
    fabric <- webdata(
      url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined',
      variables = "Total_precipitation_surface_1_Hour_Accumulation",
      times = c(dateseq[i], dateseq[i+1]-as.difftime(1, units='mins')))

    # run the job and retrieve the results
    job <- geoknife(stencil = stencil_pts, fabric = fabric, knife = knife)
    nc_file <- file.path(tempfile(fileext = '.nc'))
    download(.Object = job, destination = nc_file, overwrite = TRUE)

    # process the nc file into precipitation values
    precip_dat <- get_precip_values(nc_file, dates = times, view_polygon = view_polygon)
    return(precip_dat)
  })
  precip_values <- rbind(lapply(precip_data_list, function(pdat) { pdat$values }))
  precip_spatial <- rbind(lapply(precip_data_list, function(pdat) { pdat$spatial }))
  precip_data <- list(values=precip_values, spatial=precip_spatial)

  data_file <- as_data_file(ind_file)
  saveRDS(precip_data, file = data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}
