
prep_storm_sites_fun <- function(storm_sites_ind_file, viz_config){
  storm_sites_sf <- readRDS(sc_retrieve(storm_sites_ind_file))

  plot_fun <- function(){
    plot(sf::st_geometry(storm_sites_sf), add = TRUE,
         pch = 21, bg = viz_config$gage_norm_col, col = NA)
  }
  return(plot_fun)
}
