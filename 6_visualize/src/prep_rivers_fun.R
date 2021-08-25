
prep_rivers_fun <- function(river_ind, rivers_cfg){

  sf_rivers <- readRDS(sc_retrieve(river_ind, remake_file = getOption("scipiper.remake_file")))
  #sf_rivers$sf_gage_rivers <- sf_rivers$sf_gage_rivers
  sf_rivers$sf_gage_rivers <- sf_rivers$sf_major_rivers
  sf_rivers$sf_waterbodies <- sf_rivers$sf_waterbodies
  marsh_fcodes <- c(46600, 44601, 44602)

  plot_fun <- function(){
    #plot(st_geometry(sf_rivers$sf_gage_rivers), col = rivers_cfg$gage_river_col, lwd = 0.4, add = TRUE)
    plot(st_geometry(sf_rivers$sf_major_rivers), col = rivers_cfg$major_river_col, lwd = 0.5, add = TRUE)
    plot(st_geometry(filter(sf_rivers$sf_waterbodies, fcode %in% marsh_fcodes)), col = rivers_cfg$marsh_col, border = NA, add = TRUE)
    plot(st_geometry(filter(sf_rivers$sf_waterbodies, !(fcode %in% marsh_fcodes))), col = rivers_cfg$lake_col, border = NA, add = TRUE)
  }

  return(plot_fun)
}
