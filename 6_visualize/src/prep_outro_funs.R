prep_outro_rdgs_fun <- function(rdg_ind="1_fetch/out/rapid_dep_sites.rds.ind", gage_col_config, outro_placement, legend_text_cfg, opacity=1) {
  # project from lat/lon to plot coordinates. . this properly belongs in a 2_process step, but we're releasing tomorrow
  proj_str <- "+proj=lcc +lat_1=34.83333333333334 +lat_2=32.5 +lat_0=31.83333333333333 +lon_0=-81 +x_0=609600 +y_0=0 +ellps=GRS80 +units=m +no_defs "

  rdgs <- readRDS(sc_retrieve(rdg_ind, remake_file = getOption("scipiper.remake_file")))

  if(length(rdgs) ==  0) {
    plot_fun <- function(){ return() }
    return(plot_fun)
  }

  rdgs <- rdgs %>%
    sf::st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
    sf::st_transform(crs = proj_str) %>%
    select(site_no, geometry)

  if(opacity != 1) stop("opacity other than 1 not yet supported")

  stn_pch <- 19
  stn_col <- gage_col_config$gage_stn_col
  stn_cex <- 1.0
  font_add_google('Abel', "abel")
  font_add_google('Oswald', "Oswald")

  plot_fun <- function(){
    # plot the gage points
    plot(sf::st_geometry(rdgs$geometry), add = TRUE,
         pch = stn_pch, col = stn_col, cex = stn_cex*0.75)

    # plan text and legend coordinates
    user_coords <- par()$usr
    line_spacing_vert <- strheight("A", cex=legend_text_cfg$cex, family=legend_text_cfg$family)
    dot_spacing_vert <- line_spacing_vert / 3

    x_title <- user_coords[1] + 0.5 * diff(user_coords[1:2])
    x_text <- user_coords[1] + 0.5 * diff(user_coords[1:2])
    y_title <- user_coords[3] + 0.88 * diff(user_coords[3:4])
    y_text <- user_coords[3] + 0.82 * diff(user_coords[3:4])
    x_dot <- x_text + 0.104 * diff(user_coords[1:2])
    y_dot <- y_text - 0.061 * diff(user_coords[3:4])
    # plot text and legend
    text(x=x_title, y=y_title, labels="RAPID RESPONSE TO FLOODS", adj=c(0, 1),
         cex=2.2, col=legend_text_cfg$col, family = 'Oswald')
    text_chars <- "During floods, USGS rapidly deploys extra sensors\n(orange dots   ) to help protect life and property."
    text(x=x_text, y=y_text, labels=text_chars, adj=c(0, 1),
         cex=1.8, col=legend_text_cfg$col, family = 'abel')
    points(x=x_dot, y=y_dot, pch = stn_pch, col = stn_col, cex = stn_cex)
  }
  return(plot_fun)
}

prep_outro_allsites_fun <- function(allsites_ind="2_process/out/gage_sites_geom.rds.ind",
                                    gage_col_config, outro_placement, legend_text_cfg, opacity=1) {

  # gage_sites_geom.rds.ind is already projected for us
  allsites <- readRDS(sc_retrieve(allsites_ind, remake_file = getOption("scipiper.remake_file")))

  if(opacity != 1) stop("opacity other than 1 not yet supported")

  ltn_pch <- 19
  ltn_col <- gage_col_config$gage_ltn_col
  ltn_cex <- 1

  font_add_google('Abel', "abel")
  font_add_google('Oswald', "Oswald")

  plot_fun <- function(){

    # plot the gage points
    plot(sf::st_geometry(allsites$geometry), add = TRUE,
         pch = ltn_pch, col = ltn_col, cex = ltn_cex*0.5)

    # plan text and legend coordinates
    user_coords <- par()$usr
    x_title <- user_coords[1] + 0.535 * diff(user_coords[1:2])
    x_text <- user_coords[1] + 0.535 * diff(user_coords[1:2])
    y_title <- user_coords[3] + 0.49 * diff(user_coords[3:4])
    y_text <- user_coords[3] + 0.43 * diff(user_coords[3:4])
    x_dot <- x_text + 0.1635 * diff(user_coords[1:2])
    y_dot <- y_text - 0.061 * diff(user_coords[3:4])
    # plot text and legend
    text(x=x_title, y=y_title, labels="NATIONAL SCALE OBSERVING NETWORK", adj=c(0, 1),
         cex=2.2, col=legend_text_cfg$col, family = 'Oswald')
    text_chars <- "USGS monitors water levels at thousands of gages
nationally (blue dots   ). Long-term USGS gages across
the southeastern US are shown on the map."
    text(x=x_text, y=y_text, labels=text_chars, adj=c(0, 1),
         cex=1.8, col=legend_text_cfg$col, family = 'abel')

    points(x=x_dot, y=y_dot, pch = ltn_pch, col = ltn_col, cex = ltn_cex)
  }
  return(plot_fun)
}

prep_outro_readmore_fun <- function(outro_placement, legend_text_cfg, opacity=1) {

  if(opacity != 1) stop("opacity other than 1 not yet supported")

  plot_fun <- function(){

    # plan text coordinates
    user_coords <- par()$usr
    x_title <- user_coords[1] + 0.52 * diff(user_coords[1:2])
    x_text <- user_coords[1] + 0.43 * diff(user_coords[1:2])
    y_title <- user_coords[3] + 0.17 * diff(user_coords[3:4])
    y_text <- user_coords[3] + 0.11 * diff(user_coords[3:4])

    # plot text and legend
    text(x=x_title, y=y_title, labels="STAY SAFE DURING FLOODS", adj=c(0, 1),
         cex=2.2, col=legend_text_cfg$col, family = 'Oswald')
    text_chars <- "Learn more about the USGS response at www.usgs.gov/hurricanes"
    text(x=x_text, y=y_text, labels=text_chars, adj=c(0, 1),
         cex=1.5, col=legend_text_cfg$col, family = 'abel')
  }
  return(plot_fun)
}
