
prep_datetime_fun <- function(datetime){
  datetime <- strftime(as.POSIXct(datetime), format = '%b %d %I:%M %p')

  plot_fun <- function(){
    coord_space <- par()$usr
    datetime_frac <- 0.1 # fraction of the width of the figure
    datetime_bump_frace <- 0.01 #
    datetime_w <- bin_w_perc * diff(coord_space[c(1,2)])

    datetime_alpha <- 0.4

    # THIS IS ALL right justified for now...
    right_edge <- coord_space[2]
    xright <- right_edge
    ybottom <- coord_space[3]

    watermark_frac <- 0.2 # fraction of the width of the figure
    watermark_bump_frac <- 0.01
    coord_space <- par()$usr

    d <- png::readPNG(watermark_file)

    which_image <- d[,,4] != 0 # transparency
    d[which_image] <- watermark_alpha

    coord_width <- coord_space[2]-coord_space[1]
    coord_height <- coord_space[4]-coord_space[3]
    watermark_width <- dim(d)[2]
    img_scale <- coord_width*watermark_frac/watermark_width

    x1 <- coord_space[1]+coord_width*watermark_bump_frac
    y1 <- coord_space[3]+coord_height*watermark_bump_frac


    precip_txt_y <- ybottom+2*bin_h*0.8
    flood_y <- ybottom+3*bin_h
    normal_y <- ybottom+4*bin_h
    gage_caveat_y <- ybottom+5*bin_h*1.02
    hurricane_y <- ybottom+6*bin_h*1.05

    dot_x <- right_edge-bin_w/2
    dot_txt_x <- right_edge-bin_w*0.7

    text(right_edge, precip_txt_y, labels = 'NOAA total rainfall amount (inches)', pos = 2, cex = 1.5)
    points(dot_x, flood_y, pch = 21, bg = legend_styles$gage_norm_col, col = legend_styles$gage_flood_col, lwd = 4, cex = 2)
    text(dot_txt_x, flood_y, labels = 'Above flood stage', pos = 2, cex = 1.5)
    points(dot_x, normal_y, pch = 21, bg = legend_styles$gage_norm_col, col = NA, cex = 2)
    text(dot_txt_x, normal_y, labels = 'Below flood stage', pos = 2, cex = 1.5)
    text(right_edge, gage_caveat_y, labels = 'USGS Stream Gages (< 1% of U.S. total)', pos = 2, cex = 1.5)
    points(dot_x, hurricane_y, pch = 21, bg = legend_styles$hurricane_col, col = NA, cex = 3)
    text(dot_txt_x, hurricane_y, labels = legend_styles$storm_name, pos = 2, cex = 1.5)



    plot(datetime, add = TRUE, col = 'grey')
  }
  return(plot_fun)
}
