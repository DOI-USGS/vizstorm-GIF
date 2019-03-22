prep_basemap_fun <- function(focus_geoms_ind, secondary_geoms_ind = NULL, detail_geoms_ind = NULL, outline_only = FALSE){

  plot_fun <- function(){
    if (!is.null(secondary_geoms_ind)){
      secondary_geoms <- readRDS(sc_retrieve(secondary_geoms_ind))
      if(outline_only) {
        plot(secondary_geoms, add = TRUE, lwd = 0.3, col = NA, border = 'grey75')
      } else {
        plot(secondary_geoms, add = TRUE, lwd = 0.3, col = 'grey80', border = 'grey75') # should style args be in a config?
      }
    }

    if (!is.null(detail_geoms_ind)){
      detail_geoms <- readRDS(sc_retrieve(detail_geoms_ind))
      plot(detail_geoms, add = TRUE, lwd = 0.3, col = NA, border = 'grey95') # should style args be in a config?
    }

    focus_geoms <- readRDS(sc_retrieve(focus_geoms_ind))
    if(outline_only) {
      plot(focus_geoms, add = TRUE, col = NA, border = 'grey65')
    } else {
      plot(focus_geoms, add = TRUE, col = "grey85", border = 'grey65')
    }
  }
  return(plot_fun)
}
