

prep_watermark_fun <- function(watermark_file, pos = 1){
  if (pos != 1){
    stop('the feature of positioning the watermark is not yet implemented')
  }
  plot_fun <- function(){
    watermark_frac <- 0.2 # fraction of the width of the figure
    watermark_bump_frac <- 0.01
    coord_space <- par()$usr

    watermark_alpha <- 0.4
    d <- png::readPNG(watermark_file)

    which_image <- d[,,4] != 0 # transparency
    d[which_image] <- watermark_alpha

    coord_width <- coord_space[2]-coord_space[1]
    coord_height <- coord_space[4]-coord_space[3]
    watermark_width <- dim(d)[2]
    img_scale <- coord_width*watermark_frac/watermark_width

    x1 <- coord_space[1]+coord_width*watermark_bump_frac
    y1 <- coord_space[3]+coord_height*watermark_bump_frac

    rasterImage(d, x1, y1, x1+ncol(d)*img_scale, y1+nrow(d)*img_scale)
  }
  return(plot_fun)
}
