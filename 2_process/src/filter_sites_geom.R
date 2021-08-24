#' Keep only sites that are within the focus geoms and view box
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for a data.frame with at least
#'    the following columns: site_no, station_nm, dec_lat_va, dec_long_va, site_no_nws, flood_stage
#' @param view_polygon indicator file for an sf object of the bounding box of the viz view
#' @param focus_geoms_ind indicator file for an sf object of states to include
#' @param proj_str string of projection to transform to
filter_sites_geom <- function(ind_file, sites_ind, view_polygon, focus_geoms_ind, proj_str) {

  focus_geoms <- readRDS(sc_retrieve(focus_geoms_ind, remake_file = getOption("scipiper.remake_file")))
  sites_df <- readRDS(sc_retrieve(sites_ind, remake_file = getOption("scipiper.remake_file")))

  # get sites as sf object
  sites_df <- sites_df %>%
    dplyr::filter(!is.na(dec_lat_va)) %>%
    dplyr::filter(!is.na(dec_long_va))
  sites_sf <- sf::st_as_sf(sites_df, coords = c("dec_long_va", "dec_lat_va"), crs = 4326)
  sites_sf <- sf::st_transform(sites_sf, crs = proj_str)

  # get only sites that exist within focus_geoms:
  sites_sf_focus_i <- sf::st_intersection(sites_sf, st_buffer(focus_geoms, 0))
  sites_sf_focus <- sites_sf[sites_sf_focus_i,]

  # now subset to those within the view box:
  sites_sf_view_i <- sf::st_intersection(sf::st_geometry(sites_sf_focus), view_polygon)
  sites_sf_view <- sites_sf_focus[sites_sf_view_i,]

  # not sure how this happens, but we've seen a site (08161000) get duplicated
  # during this function, which makes the hydrograph plot funny
  sites_sf_distinct <- distinct(sites_sf_view)

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(sites_sf_view, data_file)
  gd_put(ind_file, data_file)
}
