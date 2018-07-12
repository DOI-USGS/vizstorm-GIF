
prep_legend_fun <- function(precip_bins){

  plot_fun <- function(){
    coord_space <- par()$usr
    bin_w_perc <- 0.05 # percentage of X domain
    bin_h_perc <- 0.02 # *also* percentage of X domain
    bin_w <- bin_w_perc * diff(coord_space[c(1,2)])
    bin_h <- bin_h_perc * diff(coord_space[c(1,2)])
    xright <- coord_space[2]
    ybottom <- coord_space[3]
    for (j in nrow(precip_bins):1){
      col <- as.character(precip_bins$col[j])
      text_col <- ifelse(any(col2rgb(col) < 130), 'white','black')
      rect(xleft = xright-bin_w, ybottom = ybottom, xright = xright, ytop = ybottom+bin_h, col = col, border = NA)
      text(xright-bin_w/2, ybottom+bin_h/2, precip_bins$break_factor[j], col = text_col)

      xright <- xright-bin_w
    }
    text(coord_space[1], coord_space[3], labels = 'TESTING', pos = 2)
  }
  return(plot_fun)
}