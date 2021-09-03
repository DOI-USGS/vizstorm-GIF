prep_basemap_fun <- function(focus_geoms_ind, secondary_geoms_ind = NULL, detail_geoms_ind = NULL){

  plot_fun <- function(){
    if (!is.null(secondary_geoms_ind)){
      secondary_geoms <- readRDS(sc_retrieve(secondary_geoms_ind, remake_file = getOption("scipiper.remake_file")))
      plot(secondary_geoms, add = TRUE, lwd = 0.3, col = 'grey80', border = 'grey75') # should style args be in a config?
    }

    if (!is.null(detail_geoms_ind)){
      detail_geoms <- readRDS(sc_retrieve(detail_geoms_ind, remake_file = getOption("scipiper.remake_file")))
      plot(detail_geoms, add = TRUE, lwd = 0.3, col = NA, border = 'grey95') # should style args be in a config?
    }

    focus_geoms <- readRDS(sc_retrieve(focus_geoms_ind, remake_file = getOption("scipiper.remake_file")))
    state_outlines <- focus_geoms %>% st_union()
    plot(focus_geoms, add = TRUE, lwd = 0.3, col = "white", border = 'grey75')
    plot(state_outlines, add = TRUE, lwd = 0.3, col = NA, border = 'grey35')
  }
  return(plot_fun)
}
