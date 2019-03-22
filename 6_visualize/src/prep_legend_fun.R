
prep_legend_fun <- function(snow_cfg, legend_styles, timesteps_ind, DateTime=NA, x_pos = c('left', 'right'), y_pos = c('bottom','top'), legend_text_cfg){

  x_pos <- match.arg(x_pos)
  y_pos <- match.arg(y_pos)

  if(is.na(DateTime)) {
    timesteps <- readRDS(sc_retrieve(timesteps_ind))
    DateTime <- timesteps[1]
    rm(timesteps)
  }
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.

  plot_fun <- function(){

    # compute position info shared across multiple legend elements
    coord_space <- par()$usr
    bin_w_perc <- 0.11 # percentage of X domain
    bin_h_perc <- 0.03 # *also* percentage of X domain
    bin_w <- bin_w_perc * diff(coord_space[c(1,2)])
    bin_h <- bin_h_perc * diff(coord_space[c(1,2)])
    if (x_pos == 'left'){
      txt_pos = 4
      x_edge <- coord_space[1]
      shift_dir <- 1
    } else if (x_pos == 'right'){
      txt_pos = 2
      x_edge <- coord_space[2]
      shift_dir <- -1
    }
    ybottom <- coord_space[3]+bin_h*0.5
    dot_x <- x_edge+bin_w/2*shift_dir
    dot_txt_x <- x_edge+bin_w*0.7*shift_dir
    seg_x <- x_edge+bin_w/3*shift_dir
    center_to_txt_y <- strheight("A")/3 # height of character divided by three seems to do the trick

    # plot snow bins and snow label
    snow_txt_y <- ybottom+2*bin_h*0.8
    text(x_edge, snow_txt_y, labels = 'NOAA snow depth (???)', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    snow_cfg$snow_bins
    if (x_pos == 'left'){
      bin_j <- 1:length(snow_cfg$snow_bins)
      xright <- x_edge+bin_w*0.25 + bin_w
    } else if (x_pos == 'right'){
      bin_j <- length(snow_cfg$snow_bins):1
      xright <- x_edge-bin_w*0.25
    }
    for (j in bin_j){
      col <- as.character(snow_cfg$snow_cols[j])
      text_col <- ifelse(any(col2rgb(col) < 130), 'white','black')
      rect(xleft = xright-bin_w, ybottom = ybottom, xright = xright, ytop = ybottom+bin_h, col = col, border = NA)
      text_char <- as.character(snow_cfg$snow_bins[j]) %>% gsub(pattern = ',', replacement = '-') %>% gsub(pattern = '-Inf', replacement = '+') %>% gsub(pattern = "\\(|\\]", replacement = '')
      text(xright-bin_w/2, ybottom+bin_h/2, text_char, col = text_col, cex = 1.0)
      xright <- xright+bin_w*shift_dir
    }

    # plot gage points legend
    gage_caveat_y <- ybottom+5*bin_h*1.02
    text(x_edge, gage_caveat_y, labels = 'Selected USGS stream gages', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    normal_y <- ybottom+4*bin_h
    points(dot_x, normal_y+center_to_txt_y, pch = 21, bg = legend_styles$gage_norm_col, col = NA, cex = 2)
    text(dot_txt_x, normal_y, labels = 'Below flood stage', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    flood_y <- ybottom+3*bin_h
    points(dot_x, flood_y+center_to_txt_y, pch = 21, bg = legend_styles$gage_norm_col, col = legend_styles$gage_flood_col, lwd = 4, cex = 2)
    text(dot_txt_x, flood_y, labels = 'Above flood stage', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)

  }
  return(plot_fun)
}
