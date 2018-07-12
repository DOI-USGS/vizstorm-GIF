
prep_view_fun <- function(view_polygon){
  plot_fun <- function(){
    par(omi = c(0,0,0,0), mai = c(0,0,0,0))
    plot(view_polygon, col = NA, border = NA, xaxs = 'i', yaxs = 'i')
  }
  return(plot_fun)
}
