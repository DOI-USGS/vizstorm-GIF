
prep_storm_line_fun <- function(ind_file, storm_line_ind_file){
  storm_line_sf <- readRDS(sc_retrieve(storm_line_ind_file))

  plot_fun <- function(){
    plot(storm_line_sf, add = TRUE, col = 'black', lwd = 2)
  }
  write_put_fun(plot_fun, ind_file)
}
