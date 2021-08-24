
prep_ocean_name_fun <- function(ocean_name_placement){

  plot_fun <- function(){
    # coordinate space (edges, width, height)
    coord_space <- par()$usr
    coord_space_bot <- coord_space[3]
    coord_space_left <- coord_space[1]
    coord_width <- diff(coord_space[c(1, 2)])
    coord_height <- diff(coord_space[c(3, 4)])

    ocean_name_w_frac <- 0.05 # fraction of the width of the entire figure
    ocean_name_w <- ocean_name_w_frac * coord_width # width of ocean name

    for (ocean in ocean_name_placement){
      x <- coord_space_left + coord_width * ocean$x1
      y <- coord_space_bot + coord_height * ocean$y1
      text(x = x, y = y, labels = ocean$label, cex = 1.2, pos = 4, col = 'grey', font = 3)
    }
  }
  return(plot_fun)
}
