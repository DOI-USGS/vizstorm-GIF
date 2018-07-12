
POSIX_from_filename <- function(filename){
  date_char <- gsub("\\[|\\]", "", regmatches(filename, gregexpr("\\[.*?\\]", filename))[[1]][1])
  posix_out <- as.POSIXct(date_char, format = '%Y%m%d-%H', tz = "UTC")
  if (is.na(posix_out)){
    stop('Parsing error with', filename, call. = FALSE)
  }
  return(posix_out)
}

write_put_fun <- function(plot_fun, ind_file){
  data_file <- as_data_file(ind_file)
  saveRDS(plot_fun, data_file)
  gd_put(ind_file, data_file)
}

prep_watermark_fun <- function(ind_file, watermark_file){
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
  write_put_fun(plot_fun, ind_file)
}
