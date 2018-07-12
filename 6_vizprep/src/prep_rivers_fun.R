
prep_rivers_fun <- function(river_ind){

  sf_rivers <- readRDS(sc_retrieve(river_ind))

  plot_fun <- function(){
    plot(sf_rivers$sf_gage_rivers$geometry, col = "deepskyblue", lwd = .5, add = TRUE)
    plot(sf_rivers$sf_major_rivers$geometry, col = "dodgerblue3", lwd = 1, add = TRUE)
  }

  return(plot_fun)
}
