
prep_basemap_fun <- function(ind_file, focus_geoms, secondary_geoms = NULL, detail_geoms = NULL){

  saveRDS(function() {
    plot(mydata, add=TRUE)
  }, fun_filename)
  basemap_fun <- function(){
    if (!is.null(secondary_geoms))
      sf::plot(secondary_geoms, add = TRUE, lwd = 0.3, col = 'grey80') # should style args be in a config?

    if (!is.null(detail_geoms))
      sf::plot(detail_geoms, add = TRUE, lwd = 0.3, col = NA, border = 'grey95') # should style args be in a config?

    sf::plot(focus_geoms, add = TRUE, col = NA, border = 'grey40')
  }

  data_file <- as_data_file(ind_file)
  saveRDS(basemap_fun, data_file)
  gd_put(ind_file, data_file)
}
