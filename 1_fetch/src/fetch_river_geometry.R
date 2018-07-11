fetch_river_geoms <- function(ind_file, view_polygon, sites, streamorder) {

  sites <- readRDS(sc_retrieve(sites))

  if(nrow(sites) > 30) {
    sites <- sites[sample(seq_len(nrow(sites)), 30),]
  }

  bbox <- sf::st_bbox(sf::st_transform(view_polygon, 4326))

  postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"

  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="application/json" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      '<wfs:Query xmlns:feature="http://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
                      '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:And>',
                      '<ogc:PropertyIsGreaterThan>',
                      '<ogc:PropertyName>streamorde</ogc:PropertyName>',
                      '<ogc:Literal>',streamorder,'</ogc:Literal>',
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

  sf_major_rivers <- sf::read_sf(rawToChar(out$content)) %>%
    sf::st_transform(sf::st_crs(view_polygon)) %>%
    sf::st_simplify(5000)

  site_list <- paste0("USGS-", sites$site_no)

  site_list_DM <- lapply(site_list, navigate_nldi, f_source = "nwissite", mode = "DM")
  site_list_UP <- lapply(site_list, navigate_nldi, f_source = "nwissite", mode = "UM")
  names(site_list_DM) <- names(site_list_UP) <- site_list

  sf_DM <- sf_converter(site_list_DM)
  sf_UM <- sf_converter(site_list_UP)
  sf_gage_rivers <- rbind(do.call(rbind, sf_UM), do.call(rbind, sf_DM)) %>%
    sf::st_transform(sf::st_crs(view_polygon)) %>%
    sf::st_simplify(5000)

  data_file <- as_data_file(ind_file)
  saveRDS(list(sf_gage_rivers = sf_gage_rivers, sf_major_rivers = sf_major_rivers), data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}


sf_converter <- function(x)
  sapply(x, function(x) try(sf::read_sf(x), silent = TRUE),
         USE.NAMES = TRUE, simplify = FALSE)

navigate_nldi <- function(f_id, f_source, mode = "UM",
                          d_source = NULL, distance = NULL, tier = "prod") {

  url <- paste("https://cida.usgs.gov/nldi", f_source, f_id, "navigate", mode, d_source,
               sep = "/")

  if(!is.null(distance)) {
    url <- paste0(url, "?distance=", distance)
  }

  return(rawToChar(httr::GET(url)$content))
}

get_nldi_url <- function(tier = "prod") {
  if(tier=="prod") {
    "https://cida.usgs.gov/nldi"
  } else if(tier=="test") {
    "https://cida-test.er.usgs.gov/nldi"
  } else if(tier=="local") {
    "http://localhost:8080/nldi"
  }
}
