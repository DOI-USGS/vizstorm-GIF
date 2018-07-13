
prep_legend_fun <- function(precip_bins, legend_styles){

  plot_fun <- function(){
    coord_space <- par()$usr
    bin_w_perc <- 0.05 # percentage of X domain
    bin_h_perc <- 0.02 # *also* percentage of X domain
    bin_w <- bin_w_perc * diff(coord_space[c(1,2)])
    bin_h <- bin_h_perc * diff(coord_space[c(1,2)])

    # THIS IS ALL right justified for now...
    right_edge <- coord_space[2]
    xright <- right_edge
    ybottom <- coord_space[3]
    for (j in nrow(precip_bins):1){
      col <- as.character(precip_bins$col[j])
      text_col <- ifelse(any(col2rgb(col) < 130), 'white','black')
      rect(xleft = xright-bin_w, ybottom = ybottom, xright = xright, ytop = ybottom+bin_h, col = col, border = NA)
      text_char <- as.character(precip_bins$break_factor[j]) %>% gsub(pattern = ',', replacement = '-') %>% gsub(pattern = '-Inf', replacement = '+') %>% gsub(pattern = "\\(|\\]", replacement = '')
      text(xright-bin_w/2, ybottom+bin_h/2, text_char, col = text_col, cex = 1.3)

      xright <- xright-bin_w
    }
    precip_txt_y <- ybottom+2*bin_h*0.8
    flood_y <- ybottom+3*bin_h
    normal_y <- ybottom+4*bin_h
    gage_caveat_y <- ybottom+5*bin_h*1.02
    hurricane_y <- ybottom+6*bin_h*1.05
    title_y <- ybottom+7*bin_h*1.05

    dot_x <- right_edge-bin_w/2
    dot_txt_x <- right_edge-bin_w*0.7

    text(right_edge, precip_txt_y, labels = 'NOAA total rainfall amount (inches)', pos = 2, cex = 1.5)
    points(dot_x, flood_y, pch = 21, bg = legend_styles$gage_norm_col, col = legend_styles$gage_flood_col, lwd = 4, cex = 2)
    text(dot_txt_x, flood_y, labels = 'Above flood stage', pos = 2, cex = 1.5)
    points(dot_x, normal_y, pch = 21, bg = legend_styles$gage_norm_col, col = NA, cex = 2)
    text(dot_txt_x, normal_y, labels = 'Below flood stage', pos = 2, cex = 1.5)
    text(right_edge, gage_caveat_y, labels = 'USGS Stream Gages (< 1% of U.S. total)', pos = 2, cex = 1.5)
    text(right_edge, title_y, labels = paste(legend_styles$storm_name, "Storm Severity"), pos = 2, cex = 1.5)

    h_dot_x <- dot_x
    h_dot_size <- 3
    for(i in length(legend_styles$hurricane_cols):1) {
      points(h_dot_x, hurricane_y, pch = 21, bg = legend_styles$hurricane_cols[i], col = NA, cex = h_dot_size)
      h_dot_txt_x <- h_dot_x - (h_dot_size + bin_w * 0.1)
      text(h_dot_txt_x, hurricane_y, labels = legend_styles$hurricane_col_names[i], pos = 2, cex = 1.5)
      h_dot_x <- h_dot_x - (bin_w * 0.8)
    }
  }
  return(plot_fun)
}
