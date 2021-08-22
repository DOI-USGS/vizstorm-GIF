prep_major_cities_fun <- function(cities_ind){
  cities <- readRDS(sc_retrieve(cities_ind, , remake_file = getOption("scipiper.remake_file")))

  if(nrow(cities) > 0) {
    # check for extreme text_angles
    if(any(cities$text_angle < -360 | cities$text_angle > 360)) {
      stop("text_angle must be between -360 and 360")
    }

    # compute label coordinates and orientation
    cities <- cities %>%
      mutate(
        dot_x = sf::st_coordinates(geometry)[,'X'],
        dot_y = sf::st_coordinates(geometry)[,'Y']) %>%
      sf::st_set_geometry(NULL) %>% # avoids an error if nrow(cities)==0: "Evaluation error: no 'dimnames' attribute for array."
      mutate(
        text_angle = case_when(
          (text_angle >= -360 & text_angle < 0) ~ (360 + text_angle),
          TRUE ~ as.double(text_angle)),
        text_pos = case_when(
          text_angle > 225 & text_angle <= 315 ~ 1,
          text_angle > 135 & text_angle <= 225 ~ 2,
          text_angle > 45 & text_angle <= 135 ~ 3,
          text_angle > 315 | text_angle <= 45 ~ 4))
  }

  plot_fun <- function(){

    if(nrow(cities) == 0) return()

    # compute text coords from angle and distance as fraction of x range of plot
    xrange <- diff(par('usr')[c(1,2)])
    cities <- cities %>%
      mutate(
        text_x = dot_x + text_dist * xrange * cos(2*pi*(text_angle/360)),
        text_y = dot_y + text_dist * xrange * sin(2*pi*(text_angle/360)))

    # plot the city points
    points(x = cities$dot_x,
           y = cities$dot_y,
           pch = cities$dot_pch,
           bg = cities$dot_bg,
           col = cities$dot_col,
           cex = cities$dot_cex)

    # plot the city labels
    text(x = cities$text_x,
         y = cities$text_y,
         labels = cities$label,
         cex = cities$text_cex,
         pos = cities$text_pos,
         col = cities$text_col,
         font = cities$text_font,
         offset = 0) # use offset = 2 if adding callout lines

    # add callout lines between points and labels
    # segments(x0=cities$text_x, y0=cities$text_y, x1=cities$dot_x, y1=cities$dot_y, col=cities$dot_col)

  }
  return(plot_fun)
}



