
prep_datetime_fun <- function(datetime, component_placement, date_display_tz){
  
  datetime <- strftime(as.POSIXct(datetime, tz="UTC"), format = '%b %d %I:%M %p %Z', tz = date_display_tz)

  plot_fun <- function(){
    # coordinate space (edges, width, height)
    coord_space <- par()$usr
    coord_space_bot <- coord_space[3]
    coord_space_left <- coord_space[1]
    coord_width <- diff(coord_space[c(1, 2)])
    coord_height <- diff(coord_space[c(3, 4)])

    datetime_w_frac <- 0.1 # fraction of the width of the entire figure
    datetime_w <- datetime_w_frac * coord_width # width of datetime image

    # datetime_alpha <- 0.4 # transparency of datetime image

    # datetime placement
    x1 <- coord_space_left + coord_width * component_placement$x1
    y1 <- coord_space_bot + coord_height * component_placement$y1 # bump up from bottom of figure (trying to place above the USGS watermark)

    text(x = x1, y = y1, labels = datetime, cex = 2, pos = 4, col = 'grey40')
  }
  return(plot_fun)
}
