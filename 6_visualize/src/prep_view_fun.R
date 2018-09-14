
prep_view_fun <- function(view_polygon, view_cfg){
  plot_fun <- function(){
    par(omi = c(0,0,0,0), mai = c(0,0,0,0), bg = view_cfg$ocean_col)
    plot(view_polygon, col = NA, border = NA, xaxs = 'i', yaxs = 'i')
  }
  return(plot_fun)
}

prep_bbox_fun <- function(view_config){
  
  bbox <- bbox_to_polygon(bbox = unlist(view_config$bbox), return_crs = view_config$projection)

  plot_fun <- function(){
    plot(bbox, add = TRUE, border = 'green', col = NA, lty = 3, lwd = 3)
  }
  return(plot_fun)
}
