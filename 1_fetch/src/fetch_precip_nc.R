#' @param ind_file scipiper indicator file
#' @param view_polygon from the view_polygon target
#' @param times the start and end time of the request
fetch_precip_data <- function(ind_file, view_polygon, times) {

  now <- Sys.time()
  attributes(now)$tzone <- "UTC"
  start <- as.POSIXct(times$start, tz = "UTC", format = "%Y-%m-%d")
  end <- as.POSIXct(times$end, tz = "UTC", format = "%Y-%m-%d")
  if(start  > now) {
    # http://thredds.hydroshare.org/thredds/dodsC/nwm/forcing_medium_range/20180910/nwm.t12z.medium_range.forcing.f240.conus.nc
    base_url <- "http://thredds.hydroshare.org/thredds/dodsC/nwm/forcing_medium_range/"
    try_days <- c(format(now-(24*60*60), "%Y%m%d", tz = "UTC"), format(now, "%Y%m%d", tz = "UTC"))
    try_runs <- c( "t00z", "t06z", "t12z","t18z")

    for(try_day in try_days) { # Finds the most recent available model run.
      for(try_run in try_runs) {
        url <- paste0(base_url, try_day, "/nwm.", try_run, ".medium_range.forcing.")
        if(httr::HEAD(paste0(url, "f001.conus.nc.html"))$status_code == 200) {
          run <- try_run
          day <- try_day
          run_time <- as.POSIXct(paste0(day, run), format = "%Y%m%dt%Hz", tz = "UTC")
          url_base <- url
        }
      }
    }

    start_hour <- round(as.numeric(start - run_time) * 24)
    end_hour <- round(as.numeric(end - run_time) * 24)
    if(end_hour > 240) end_hour <- 240
    hours <- sprintf("f%03d", c(start_hour:end_hour))
    urls <- sprintf("%s%s.conus.nc", url_base, hours)

    precip_data <- get_precip_values(urls, dates = times, view_polygon = view_polygon)

  } else {

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

  # break times into chunks to avoid WAF rejections of queries that are "too
  # big". use dateformat and some string-POSIXct back and forth to retain the
  # UTC timezone despite calls to c(), which always switches you back into
  # user's local timezone
  try_formats <- c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d")
  dateformat <- try_formats[sapply(try_formats, function(fmt) all(!is.na(as.POSIXct(unlist(times), format=fmt))))]
  dateseq <- as.POSIXct(unique(c(
    format(seq(as.POSIXct(times$start, tz='UTC'), as.POSIXct(times$end, tz='UTC'), by=as.difftime(3, units='days')), dateformat),
    times$end
  )), tz='UTC')
  precip_data_list <- lapply(seq_len(length(dateseq)-1), function(i) {
    message(sprintf("pulling precip for %s to %s", dateseq[i], dateseq[i+1]))
    # define the fabric, subset by time
    times_subset <- if(i == length(dateseq)-1) {
      dateseq[i:(i+1)]
    } else {
      as.POSIXct(
        c(format(dateseq[i], "%Y-%m-%d %H:%M:%OS"),
          format(dateseq[i+1]-as.difftime(1, units='mins'), "%Y-%m-%d %H:%M:%OS")),
        tz='UTC')
    }
    fabric <- webdata(
      url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined',
      variables = "Total_precipitation_surface_1_Hour_Accumulation",
      times = times_subset)

    # run the job and retrieve the results
    job <- geoknife(stencil = stencil_pts, fabric = fabric, knife = knife)
    nc_file <- file.path(tempfile(fileext = '.nc'))
    download(.Object = job, destination = nc_file, overwrite = TRUE)

    # process the nc file into precipitation values
    precip_dat <- get_precip_values(nc_file, dates = times, view_polygon = view_polygon)
    return(precip_dat)
  })
  precip_values <- do.call(rbind, lapply(precip_data_list, function(pdat) { pdat$values }))
  precip_spatial <- do.call(rbind, lapply(precip_data_list, function(pdat) { pdat$spatial }))
  precip_data <- list(values=precip_values, spatial=precip_spatial)
  }

  data_file <- as_data_file(ind_file)
  saveRDS(precip_data, file = data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}
