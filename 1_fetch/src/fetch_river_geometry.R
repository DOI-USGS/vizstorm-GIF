fetch_major_river_geoms <- function(ind_file, view_polygon, fetch_streamorder) {

  bbox_sf <- st_as_sfc(st_bbox(view_polygon))

  sf_major_rivers <- get_nhdplus(AOI = bbox_sf,
                           realization = "flowline",
                           streamorder = fetch_streamorder$fetch_streamorder)

  data_file <- as_data_file(ind_file)
  saveRDS(sf_major_rivers, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}

fetch_waterbody_geoms <- function(ind_file, view_polygon, fetch_waterbody_areasqkm) {

  bbox_sf <- st_as_sfc(st_bbox(view_polygon))

  sf_waterbodies <- get_waterbodies(AOI = bbox_sf,
                                 buffer = 0) %>%
    filter(areasqkm > fetch_waterbody_areasqkm$fetch_waterbody_areasqkm) %>%
    ms_simplify()

  data_file <- as_data_file(ind_file)
  saveRDS(sf_waterbodies, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}

fetch_gage_river_geoms <- function(ind_file, view_polygon, sites_ind) {

  sites <- readRDS(sc_retrieve(sites_ind, remake_file = getOption("scipiper.remake_file")))

  if(nrow(sites) > 30) {
    set.seed(42)
    sites <- sites[sample(seq_len(nrow(sites)), 30),]
  }

  site_list <- paste0("USGS-", sites$site_no)

  site_list_DM <- lapply(site_list, navigate_nldi, f_source = "nwissite", mode = "DM")
  site_list_UM <- lapply(site_list, navigate_nldi, f_source = "nwissite", mode = "UM")
  names(site_list_DM) <- names(site_list_UM) <- site_list

  sf_DM <- sf_converter(site_list_DM) %>%
    name_adder("down_main")
  sf_UM <- sf_converter(site_list_UM) %>%
    name_adder("up_main")

  sf_gage_rivers <- rbind(do.call(rbind, sf_UM), do.call(rbind, sf_DM)) %>%
    st_transform(st_crs(view_polygon))

  data_file <- as_data_file(ind_file)
  saveRDS(sf_gage_rivers, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}

name_adder <- function(x, updn) {

  lapply(seq_along(x), function(y, n, r, updn) {
    print(y)
    mutate(y[[r]], site_id = n[[r]]) %>%
    mutate(up_down = updn)
    },
    n = names(x), y = x, updn = updn)
}

sf_converter <- function(x)
  sapply(x, function(x) try(read_sf(x), silent = TRUE),
         USE.NAMES = TRUE, simplify = FALSE)

navigate_nldi <- function(f_id, f_source, mode = "UM",
                          d_source = NULL, distance = NULL) {

  url <- paste("https://labs.waterdata.usgs.gov/api/nldi/linked-data", f_source, f_id, "navigate", mode, d_source,
               sep = "/")

  if(!is.null(distance)) {
    url <- paste0(url, "?distance=", distance)
  }
  return(rawToChar(httr::GET(url)$content))
}
