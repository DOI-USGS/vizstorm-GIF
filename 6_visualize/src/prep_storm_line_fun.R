
prep_storm_line_fun <- function(storm_line_ind_file){
  storm_line_sf <- readRDS(sc_retrieve(storm_line_ind_file))

  plot_fun <- function(){
    plot(sf::st_geometry(storm_line_sf), add = TRUE, col = 'black', lwd = 2)
  }
  return(plot_fun)
}
