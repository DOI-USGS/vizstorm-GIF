fetch_major_river_geoms <- function(ind_file, view_polygon, fetch_streamorder) {

  bbox <- st_bbox(st_transform(view_polygon, 4326))

  postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"

  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="application/json" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      '<wfs:Query xmlns:feature="http://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
                      '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:And>',
                      '<ogc:PropertyIsGreaterThan>',
                      '<ogc:PropertyName>streamorde</ogc:PropertyName>',
                      '<ogc:Literal>',fetch_streamorder,'</ogc:Literal>',
                      '</ogc:PropertyIsGreaterThan>',
                      '<ogc:BBOX>',
                      '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                      '<gml:Envelope>',
                      '<gml:lowerCorner>',bbox[2]," ",bbox[1],'</gml:lowerCorner>',
                      '<gml:upperCorner>',bbox[4]," ",bbox[3],'</gml:upperCorner>',
                      '</gml:Envelope>',
                      '</ogc:BBOX>',
                      '</ogc:And>',
                      '</ogc:Filter>',
                      '</wfs:Query>',
                      '</wfs:GetFeature>')

  out <- httr::POST(postURL, body = filterXML)

  sf_major_rivers <- read_sf(rawToChar(out$content)) %>%
    st_transform(st_crs(view_polygon))

  data_file <- as_data_file(ind_file)
  saveRDS(sf_major_rivers, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}

which_nwm_files_to_use <- function(file_names, fetch_streamorder) {
  file_orders <- stringr::str_extract_all(basename(file_names), "[0-9]") %>% unlist() %>% as.numeric()
  files_to_keep <- file_names[which(file_orders >= fetch_streamorder[[1]])]
  return(files_to_keep)
}

fetch_major_river_geoms_from_in <- function(ind_file, view_polygon, conus_nwm_files) {

  nwm_sf_list <- lapply(conus_nwm_files, function(fn) {
    readRDS(fn) %>% st_as_sf()
  })

  sf_major_rivers <- do.call(rbind, nwm_sf_list) %>%
    st_transform(st_crs(view_polygon)) %>%
    st_crop(view_polygon)

  data_file <- as_data_file(ind_file)
  saveRDS(sf_major_rivers, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}

fetch_waterbody_geoms <- function(ind_file, view_polygon, fetch_waterbody_areasqkm) {

  bbox <- st_bbox(st_transform(view_polygon, 4326))

  postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"

  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="application/json" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      '<wfs:Query xmlns:feature="http://gov.usgs.cida/nhdplus" typeName="feature:nhdwaterbody" srsName="EPSG:4326">',
                      '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:And>',
                      '<ogc:PropertyIsGreaterThan>',
                      '<ogc:PropertyName>areasqkm</ogc:PropertyName>',
                      '<ogc:Literal>',fetch_waterbody_areasqkm,'</ogc:Literal>',
                      '</ogc:PropertyIsGreaterThan>',
                      '<ogc:BBOX>',
                      '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                      '<gml:Envelope>',
                      '<gml:lowerCorner>',bbox[2]," ",bbox[1],'</gml:lowerCorner>',
                      '<gml:upperCorner>',bbox[4]," ",bbox[3],'</gml:upperCorner>',
                      '</gml:Envelope>',
                      '</ogc:BBOX>',
                      '</ogc:And>',
                      '</ogc:Filter>',
                      '</wfs:Query>',
                      '</wfs:GetFeature>')

  out <- httr::POST(postURL, body = filterXML)

  sf_waterbodies <- read_sf(rawToChar(out$content)) %>%
    st_transform(st_crs(view_polygon))

  data_file <- as_data_file(ind_file)
  saveRDS(sf_waterbodies, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}

fetch_gage_river_geoms <- function(ind_file, view_polygon, sites_ind) {

  sites <- readRDS(sc_retrieve(sites_ind))

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
    mutate(y[[r]], site_id = n[[r]]) %>%
    mutate(up_down = updn)},
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
