
prep_legend_fun <- function(){

  plot_fun <- function(){
    coord_space <- par()$usr
    text(coord_space[1], coord_space[3], labels = 'TESTING', pos = 4)
  }
  return(plot_fun)
}
