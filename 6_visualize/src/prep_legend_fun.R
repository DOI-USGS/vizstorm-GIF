
prep_legend_fun <- function(precip_bins, legend_styles, timesteps_ind, storm_points_sf, DateTime=NA, x_pos = c('left', 'right'), y_pos = c('bottom','top'), legend_text_cfg, intro = FALSE){

  x_pos <- match.arg(x_pos)
  y_pos <- match.arg(y_pos)

  if(is.na(DateTime)) {
    timesteps <- readRDS(sc_retrieve(timesteps_ind, , remake_file = getOption("scipiper.remake_file")))
    DateTime <- timesteps[1]
    rm(timesteps)
  }
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.

  hurricane_col <- NA
  has_storm_track <- !is.null(storm_points_sf)
  if(has_storm_track) {
    this_dot <- filter(storm_points_sf, DateTime == this_DateTime)
    if(nrow(this_dot) > 0) {
      hurricane_col <- legend_styles$hurricane_cols[(this_dot$SS + 1)]
      hurricane_cat <- legend_styles$hurricane_col_names[(this_dot$SS + 1)]
    }
  }

  rm(storm_points_sf)

  plot_fun <- function(){

    # compute position info shared across multiple legend elements
    coord_space <- par()$usr
    bin_w_perc <- 0.05 # percentage of X domain
    bin_h_perc <- 0.02 # *also* percentage of X domain
    bin_w <- bin_w_perc * diff(coord_space[c(1,2)])
    bin_h <- bin_h_perc * diff(coord_space[c(1,2)])

    if (x_pos == 'left'){
      txt_pos = 4
      x_edge <- coord_space[1]*1.05
      shift_dir <- 1
    } else if (x_pos == 'right'){
      txt_pos = 2
      x_edge <- coord_space[2]
      shift_dir <- -1
    }
    ybottom <- coord_space[3]*1.05
    gage_y <-  (coord_space[2]- coord_space[1])*0.66
    dot_x <- x_edge+bin_w/2*shift_dir
    dot_x_gage <- x_edge+bin_w/2*shift_dir + gage_y
    dot_txt_x <- x_edge+bin_w*0.7*shift_dir
    dot_txt_x_gage <- x_edge+bin_w*0.7*shift_dir + gage_y
    seg_x <- x_edge+bin_w/3*shift_dir
    center_to_txt_y <- strheight("A")/3 # height of character divided by three seems to do the trick

    # plot precip bins and precip label
    precip_txt_y <- ybottom+2*bin_h*0.9 ## just the text
    text(x_edge, precip_txt_y, labels = 'NOAA total rainfall (inches)', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)

    if (x_pos == 'left'){
      bin_j <- 1:nrow(precip_bins)
      xright <- x_edge+bin_w
    } else if (x_pos == 'right'){
      bin_j <- nrow(precip_bins):1
      xright <- x_edge
    }
    for (j in bin_j){
      col <- as.character(precip_bins$col[j])
      text_col <- ifelse(any(col2rgb(col) < 130), 'white','black')
      rect(xleft = xright-bin_w, ybottom = ybottom, xright = xright, ytop = ybottom+bin_h, col = col, border = NA)
      text_char <- as.character(precip_bins$break_factor[j]) %>% gsub(pattern = ',', replacement = '-') %>% gsub(pattern = '-Inf', replacement = '+') %>% gsub(pattern = "\\(|\\]", replacement = '')
      text(xright-bin_w/2, ybottom+bin_h/2, text_char, col = text_col, cex = 1.3)
      xright <- xright+bin_w*shift_dir
    }

    # plot gage points legend
    gage_caveat_y <- ybottom+5*bin_h

    #text(x_edge, gage_caveat_y, labels = 'Select USGS stream gages', pos = txt_pos,
    #     cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    normal_y <- coord_space[4]*0.9 #ybottom+4*bin_h
    if(intro == FALSE){
    points(dot_x_gage+bin_w*1.75, normal_y+center_to_txt_y, pch = 21, bg = legend_styles$gage_norm_col, col = NA, cex = 2)
    text(dot_txt_x_gage-bin_w, normal_y, labels = 'Flood stage:', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    text(dot_txt_x_gage+bin_w*1.75, normal_y, labels = 'Below', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    flood_y <- coord_space[4]*0.9
    points(dot_x_gage+bin_w*3.5, flood_y+center_to_txt_y, pch = 21, bg = legend_styles$gage_norm_col, col = legend_styles$gage_flood_col, lwd = 4, cex = 2)
    text(dot_txt_x_gage+bin_w*3.5, flood_y, labels = 'Above', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    }

    # plot storm legend
    hurricane_y <- coord_space[3]*1.3


    if(intro == FALSE){
      if(has_storm_track) {
        if(is.na(hurricane_col)) {
          # When the hurricane dot is no longer visible, switch to showing just the path
          text(dot_txt_x, hurricane_y, labels = paste("Path of", legend_styles$storm_name), pos = txt_pos,
               cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)

          segments(seg_x, hurricane_y+center_to_txt_y, dot_txt_x, lty = "dotted",
                   col = legend_styles$storm_line_col, lwd = 2)
        } else {
          text(dot_txt_x, hurricane_y, labels = sprintf(hurricane_cat, legend_styles$storm_name), pos = txt_pos,
               cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
          text(dot_txt_x, hurricane_y+coord_space[4]*0.95, labels = legend_styles$storm_name, pos = txt_pos,
               cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
          points(dot_x, hurricane_y+center_to_txt_y, pch = 21, bg = hurricane_col, col = NA, cex = 3)
        }
      }

    }

  }
  return(plot_fun)
}
