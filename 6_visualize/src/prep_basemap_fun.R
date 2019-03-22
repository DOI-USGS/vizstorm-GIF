prep_basemap_fun <- function(focus_geoms_ind, bg_geom_cfg, secondary_geoms_ind = NULL, detail_geoms_ind = NULL, outline_only = FALSE){
  
  # fix potential NAs
  bg_geom_cfg <- lapply(bg_geom_cfg, function(d) { 
    d$col <- ifelse(d$col == "NA", NA, d$col)
    d$border <- ifelse(d$border == "NA", NA, d$border)
    return(d)
  })
  
  focus_cfg <- bg_geom_cfg$focus_geom
  secondary_cfg <- bg_geom_cfg$secondary_geom
  detail_cfg <- bg_geom_cfg$detail_geom
  
  rm(bg_geom_cfg)
  
  plot_fun <- function(){
    if (!is.null(secondary_geoms_ind)){
      secondary_geoms <- readRDS(sc_retrieve(secondary_geoms_ind))
      if(outline_only) {
        plot(secondary_geoms, add = TRUE, lwd = 0.3, col = NA, border = 'grey75')
      } else {
        plot(secondary_geoms, add = TRUE, lwd = 0.3, col = secondary_cfg$col, border = secondary_cfg$border) 
      }
    }

    if (!is.null(detail_geoms_ind)){
      detail_geoms <- readRDS(sc_retrieve(detail_geoms_ind))
      plot(detail_geoms, add = TRUE, lwd = 0.3, col = NA, border = detail_cfg$border) # should style args be in a config?
    }

    focus_geoms <- readRDS(sc_retrieve(focus_geoms_ind))
    if(outline_only) {
      plot(focus_geoms, add = TRUE, col = NA, border = focus_cfg$border)
    } else {
      plot(focus_geoms, add = TRUE, col = focus_cfg$col, border = focus_cfg$border)
    }
  }
  return(plot_fun)
}
