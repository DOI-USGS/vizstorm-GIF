
prep_storm_line_fun <- function(storm_line_ind_file, storm_line_cfg){
  storm_line_sf <- readRDS(sc_retrieve(storm_line_ind_file))
  plot_fun <- if(is.null(storm_line_sf)) {
    function() {} # storms are optional
  } else {
    function(){
      plot(sf::st_geometry(storm_line_sf), add = TRUE, col = storm_line_cfg$storm_line_col, lwd = 2)
    }
  }
  return(plot_fun)
}
