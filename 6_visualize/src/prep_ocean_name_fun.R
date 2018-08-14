
prep_ocean_name_fun <- function(ocean_name_placement, ocean_name_labels){

  plot_fun <- function(){
    # coordinate space (edges, width, height)
    coord_space <- par()$usr
    coord_space_bot <- coord_space[3]
    coord_space_left <- coord_space[1]
    coord_width <- diff(coord_space[c(1, 2)])
    coord_height <- diff(coord_space[c(3, 4)])

    ocean_name_w_frac <- 0.1 # fraction of the width of the entire figure
    ocean_name_w <- ocean_name_w_frac * coord_width # width of ocean name
    for (ocean in names(ocean_name_labels)){
      x <- coord_space_left + coord_width * ocean_name_placement[[ocean]]$x1
      y <- coord_space_bot + coord_height * ocean_name_placement[[ocean]]$y1
      text(x = x, y = y, labels = ocean_name_labels[[ocean]], cex = 2, pos = 4, col = 'grey', font = 3)
    }
  }
  return(plot_fun)
}
