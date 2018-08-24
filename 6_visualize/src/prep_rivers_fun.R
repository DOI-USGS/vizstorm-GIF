
prep_rivers_fun <- function(river_ind){

  sf_rivers <- readRDS(sc_retrieve(river_ind))

  plot_fun <- function(){
    plot(st_geometry(sf_rivers$sf_gage_rivers), col = "deepskyblue", lwd = .5, add = TRUE)
    plot(st_geometry(sf_rivers$sf_major_rivers), col = "dodgerblue3", lwd = 1, add = TRUE)
    plot(st_geometry(sf_rivers$sf_waterbodies), col = "deepskyblue", add = TRUE)
  }

  return(plot_fun)
}
