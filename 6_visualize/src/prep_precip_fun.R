
prep_precip_fun <- function(precip_rasters, precip_bins, timestep){

  breaks <- unique(c(precip_bins$left_break,
                     precip_bins$right_break))
  breaks[which(breaks == Inf)] <- 100000 # Just quiets an error
  breaks <- c(0, 1, breaks[2:length(breaks)]) # Add a tiny break for alpha

  rgb_ramp <- col2rgb(precip_bins$col, alpha = TRUE)
  rgb_ramp <- cbind(rgb_ramp[,1],rgb_ramp)
  rgb_ramp["alpha", 1:4] <- c(0, 100, 180, 220) # fade in from 0
  rgb_ramp["alpha", ] <- rgb_ramp["alpha", ] * 0.8 # decrease all alpha a bit

  colors <- apply(rgb_ramp, 2,
                  function(x) {
                    rgb(x[1], x[2], x[3], x[4],
                        maxColorValue = 255)})

  plot_fun <- function(){
    plot(precip[[timestep]], add = TRUE, breaks = breaks, col = colors, legend = FALSE)
  }
  return(plot_fun)
}
