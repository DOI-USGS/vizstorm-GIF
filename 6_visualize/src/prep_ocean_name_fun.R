
prep_ocean_name_fun <- function(image_placement){

  plot_fun <- function(){
    # coordinate space (edges, width, height)
    coord_space <- par()$usr
    coord_space_bot <- coord_space[3]
    coord_space_left <- coord_space[1]
    coord_width <- diff(coord_space[c(1, 2)])
    coord_height <- diff(coord_space[c(3, 4)])

    ocean_name_w_frac <- 0.1 # fraction of the width of the entire figure
    ocean_name_w <- ocean_name_w_frac * coord_width # width of ocean name

    # ocean_name_alpha <- 0.4

    # ocean name placement
    atlantic_x1 <- coord_space_left + coord_width * image_placement$ocean_name$atlantic$x1
    atlantic_y1 <- coord_space_bot + coord_height * image_placement$ocean_name$atlatnic$y1
    g_of_mex_x1 <- coord_space_left + coord_width * image_placement$ocean_name$g_of_mex$x1
    g_of_mex_y1 <- coord_space_bot + coord_height * image_placement$ocean_name$g_of_mex$y1

    x1_coords <- c(atlantic_x1, g_of_mex_x1)
    y1_coords <- c(atlantic_y1, g_of_mex_y1)

    labs <- c('Atlantic Ocean', 'Gulf of Mexico')

    text(x = x1_coords, y = y1_coords, labels = labs, cex = 2, pos = 4, col = 'grey', font = 3)
  }
  return(plot_fun)
}
