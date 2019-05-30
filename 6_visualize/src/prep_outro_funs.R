prep_outro_rdgs_fun <- function(rdg_ind="1_fetch/out/rapid_dep_sites.rds.ind", gage_col_config, outro_placement, legend_text_cfg, opacity=1) {
  # project from lat/lon to plot coordinates. . this properly belongs in a 2_process step, but we're releasing tomorrow
  proj_str <- "+proj=lcc +lat_1=34.83333333333334 +lat_2=32.5 +lat_0=31.83333333333333 +lon_0=-81 +x_0=609600 +y_0=0 +ellps=GRS80 +units=m +no_defs "
  rdgs <- readRDS(sc_retrieve(rdg_ind))
  
  if(length(rdgs) == 0) {
    # Return empty plot function
    message("There are no rapid deployment gages.")
    plot_fun <- function() {
      return()
    }
  } else {

    rdgs <- rdgs %>%
      sf::st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
      sf::st_transform(crs = proj_str) %>%
      select(site_no, geometry)
    
    if(opacity != 1) stop("opacity other than 1 not yet supported")
    
    stn_pch <- 19
    stn_col <- gage_col_config$gage_stn_col
    stn_text_cex <- 1.2
    stn_title_cex <- 1.8
    
    plot_fun <- function(){
      font_add_google('Abel', "abel")
      font_add_google('Oswald', "Oswald")
      
      # plot the gage points
      plot(sf::st_geometry(rdgs$geometry), add = TRUE,
           pch = stn_pch, col = stn_col, cex = stn_text_cex*0.75)
      
      # plan text and legend coordinates
      user_coords <- par()$usr
      line_spacing_vert <- strheight("A", cex=legend_text_cfg$cex, family="Oswald")
      dot_spacing_vert <- line_spacing_vert / 3
      
      x_title <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
      x_text <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
      y_title <- user_coords[3] + outro_placement$ytop_stn * diff(user_coords[3:4])
      y_text <- user_coords[3] + (outro_placement$ytop_stn-0.08) * diff(user_coords[3:4])
      x_dot <- x_text + 0.101 * diff(user_coords[1:2])
      y_dot <- y_text - 0.052 * diff(user_coords[3:4])
      
      # plan bg rectangle
      bg_x_left <- user_coords[1] + (outro_placement$xleft-0.02) * diff(user_coords[1:2])
      bg_x_right <- user_coords[1] + (outro_placement$xright+0.005) * diff(user_coords[1:2])
      bg_y_top <- user_coords[3] + outro_placement$ytop_stn * diff(user_coords[3:4]) + line_spacing_vert * 0.5
      bg_y_bottom <- user_coords[3] + outro_placement$ytop_stn * diff(user_coords[3:4]) - line_spacing_vert * 5
      
      # plot bg rectangle
      rect(bg_x_left, bg_y_bottom, bg_x_right, bg_y_top, col = "#ffffffB3", border = "transparent")
      
      # plot text and legend
      text(x=x_title, y=y_title, labels="RAPID RESPONSE TO FLOODS", adj=c(0, 1),
           cex=stn_title_cex, col=legend_text_cfg$col, family = 'Oswald')
      text_chars <- "During floods, USGS rapidly deploys extra gages\n(orange dots   ) to help protect life and property."
      text(x=x_text, y=y_text, labels=text_chars, adj=c(0, 1),
           cex=stn_text_cex, col=legend_text_cfg$col, family = 'abel')
      points(x=x_dot, y=y_dot, pch = stn_pch, col = stn_col, cex = stn_text_cex)
    }
  }
  return(plot_fun)
}

prep_outro_allsites_fun <- function(allsites_ind="2_process/out/gage_sites_geom.rds.ind",
                                    gage_col_config, outro_placement, legend_text_cfg, opacity=1) {

  # gage_sites_geom.rds.ind is already projected for us
  allsites <- readRDS(sc_retrieve(allsites_ind))

  if(opacity != 1) stop("opacity other than 1 not yet supported")

  ltn_pch <- 19
  ltn_col <- gage_col_config$gage_ltn_col
  ltn_text_cex <- 1.2
  ltn_title_cex <- 1.8

  plot_fun <- function(){
    font_add_google('Abel', "abel")
    font_add_google('Oswald', "Oswald")
    
    # plot the gage points
    plot(sf::st_geometry(allsites$geometry), add = TRUE,
         pch = ltn_pch, col = ltn_col, cex = ltn_text_cex*0.5)

    # plan text and legend coordinates
    user_coords <- par()$usr
    line_spacing_vert <- strheight("A", cex=legend_text_cfg$cex, family="Oswald")
    x_title <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
    x_text <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
    y_title <- user_coords[3] + outro_placement$ytop_ltn * diff(user_coords[3:4])
    y_text <- user_coords[3] + (outro_placement$ytop_ltn-0.08) * diff(user_coords[3:4])
    x_dot <- x_text + 0.155 * diff(user_coords[1:2])
    y_dot <- y_text - 0.051 * diff(user_coords[3:4])
    
    # plan bg rectangle
    bg_x_left <- user_coords[1] + (outro_placement$xleft-0.02) * diff(user_coords[1:2])
    bg_x_right <- user_coords[1] + (outro_placement$xright+0.005) * diff(user_coords[1:2])
    bg_y_top <- user_coords[3] + outro_placement$ytop_ltn * diff(user_coords[3:4]) + line_spacing_vert * 0.5
    bg_y_bottom <- user_coords[3] + outro_placement$ytop_ltn * diff(user_coords[3:4]) - line_spacing_vert * 6
    
    # plot bg rectangle
    rect(bg_x_left, bg_y_bottom, bg_x_right, bg_y_top, col = "#ffffffB3", border = "transparent")
    
    # plot text and legend
    text(x=x_title, y=y_title, labels="NATIONAL SCALE OBSERVING NETWORK", adj=c(0, 1),
         cex=ltn_title_cex, col=legend_text_cfg$col, family = 'Oswald')
    text_chars <- "USGS monitors water levels at thousands of gages
nationally (blue dots   ). Long-term USGS gages across
the southeastern US are shown on the map."
    text(x=x_text, y=y_text, labels=text_chars, adj=c(0, 1),
         cex=ltn_text_cex, col=legend_text_cfg$col, family = 'abel')
    points(x=x_dot, y=y_dot, pch = ltn_pch, col = ltn_col, cex = ltn_text_cex)
  }
  return(plot_fun)
}

prep_outro_readmore_fun <- function(outro_placement, legend_text_cfg, opacity=1) {

  if(opacity != 1) stop("opacity other than 1 not yet supported")

  plot_fun <- function(){
    font_add_google('Abel', "abel")
    font_add_google('Oswald', "Oswald")
    
    # plan text coordinates
    user_coords <- par()$usr
    line_spacing_vert <- strheight("A", cex=legend_text_cfg$cex, family=legend_text_cfg$family)
    x_title <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
    x_text <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
    y_title <- user_coords[3] + outro_placement$ytop_more * diff(user_coords[3:4])
    y_text <- user_coords[3] + (outro_placement$ytop_more-0.08) * diff(user_coords[3:4])
    
    # plan bg rectangle
    bg_x_left <- user_coords[1] + (outro_placement$xleft-0.02) * diff(user_coords[1:2])
    bg_x_right <- user_coords[1] + (outro_placement$xright+0.005) * diff(user_coords[1:2])
    bg_y_top <- user_coords[3] + outro_placement$ytop_more * diff(user_coords[3:4]) + line_spacing_vert * 0.5
    bg_y_bottom <- user_coords[3] + (outro_placement$ytop_more-0.04) * diff(user_coords[3:4]) - line_spacing_vert * 5.5
    
    # plot bg rectangle
    rect(bg_x_left, bg_y_bottom, bg_x_right, bg_y_top, col = "#ffffffB3", border = "transparent")
    
    # plot text and legend
    text(x=x_title, y=y_title, labels="STAY SAFE DURING FLOODS", adj=c(0, 1),
         cex=2.5, col=legend_text_cfg$col, family = 'Oswald')
    text_chars <- "Learn more about USGS response to \nfloods at www.water.usgs.gov/floods"
    text(x=x_text, y=y_text, labels=text_chars, adj=c(0, 1),
         cex=1.8, col=legend_text_cfg$col, family = 'abel')
  }
  return(plot_fun)
}


prep_outro_pkqs_fun <- function(pkq_ind="1_fetch/out/pkq_sites.rds.ind", gage_col_config, outro_placement, legend_text_cfg, opacity=1) {
  
  pkqs <- readRDS(sc_retrieve(pkq_ind)) %>% select(site_no, geometry)
  
  if(opacity != 1) stop("opacity other than 1 not yet supported")
  
  pkq_pch <- 2
  pkq_col <- gage_col_config$gage_pkq_col
  pkq_text_cex <- 1.2
  pkq_title_cex <- 1.8
  
  plot_fun <- function(){
    font_add_google('Abel', "abel")
    font_add_google('Oswald', "Oswald")
    
    # plot the gage points
    plot(pkqs$geometry, add = TRUE,
         pch = pkq_pch, col = pkq_col, lwd = 2, cex = pkq_text_cex*0.75)
    
    # plan text and legend coordinates
    user_coords <- par()$usr
    line_spacing_vert <- strheight("A", cex=legend_text_cfg$cex, family="Oswald")
    dot_spacing_vert <- line_spacing_vert / 3
    
    x_title <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
    x_text <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
    y_title <- user_coords[3] + outro_placement$ytop_pkq * diff(user_coords[3:4])
    y_text <- user_coords[3] + (outro_placement$ytop_pkq-0.08) * diff(user_coords[3:4])
    x_dot <- x_text + 0.193 * diff(user_coords[1:2])
    y_dot <- y_text - 0.052 * diff(user_coords[3:4])
    
    # plan bg rectangle
    bg_x_left <- user_coords[1] + (outro_placement$xleft-0.02) * diff(user_coords[1:2])
    bg_x_right <- user_coords[1] + (outro_placement$xright+0.005) * diff(user_coords[1:2])
    bg_y_top <- user_coords[3] + outro_placement$ytop_pkq * diff(user_coords[3:4]) + line_spacing_vert * 0.5
    bg_y_bottom <- user_coords[3] + outro_placement$ytop_pkq * diff(user_coords[3:4]) - line_spacing_vert * 5
    
    # plot bg rectangle
    rect(bg_x_left, bg_y_bottom, bg_x_right, bg_y_top, col = "#ffffffB3", border = "transparent")
    
    # plot text and legend
    text(x=x_title, y=y_title, labels="RECORD PEAK FLOWS", adj=c(0, 1),
         cex=pkq_title_cex, col=legend_text_cfg$col, family = 'Oswald')
    text_chars <- sprintf("USGS recorded %s locations with record high\nstream flow (red triangles   ) during this event.",
                          nrow(pkqs))
    text(x=x_text, y=y_text, labels=text_chars, adj=c(0, 1),
         cex=pkq_text_cex, col=legend_text_cfg$col, family = 'abel')
    points(x=x_dot, y=y_dot, pch = pkq_pch, col = pkq_col, cex = pkq_text_cex, lwd = 2)
  }
  return(plot_fun)
}
