prep_outro_allsites_fun <- function(allsites_ind="2_process/out/gage_sites_geom.rds.ind",
                                    gage_col_config, outro_placement, legend_text_cfg, opacity=1) {
  
  # gage_sites_geom.rds.ind is already projected for us
  allsites <- readRDS(sc_retrieve(allsites_ind)) 
  
  if(opacity != 1) stop("opacity other than 1 not yet supported")
  
  ltn_pch <- 19
  ltn_col <- gage_col_config$gage_ltn_col
  ltn_cex <- 0.8
  
  plot_fun <- function(){
    # plot the gage points
    plot(sf::st_geometry(allsites$geometry), add = TRUE,
         pch = ltn_pch, col = ltn_col, cex = ltn_cex)
    
    # plan text and legend coordinates    
    user_coords <- par()$usr
    line_spacing_vert <- strheight("A", cex=legend_text_cfg$cex, family=legend_text_cfg$family)
    dot_spacing_vert <- line_spacing_vert / 3
    y_text <- user_coords[3] + outro_placement$ytop_ltn * diff(user_coords[3:4])
    y_legend_text <- y_text - line_spacing_vert * 5.5
    y_legend_dot <- y_legend_text - dot_spacing_vert
    xleft <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
    xleft_dot <- xleft + min(outro_placement$xleft, 1-outro_placement$xleft) * 0.05 * diff(user_coords[1:2])
    xleft_indent <- xleft + min(outro_placement$xleft, 1-outro_placement$xleft) * 0.1 * diff(user_coords[1:2])
    
    # plot text and legend
    text(x=xleft, y=y_text, labels="USGS monitors water levels\nat these and thousands of\nother gages nationally", adj=c(0, 1),
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    text(x=xleft_indent, y=y_legend_text, labels="Other long-term gages", adj=c(0, 1),
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    points(x=xleft_dot, y=y_legend_dot, pch = ltn_pch, col = ltn_col, cex = ltn_cex)
  }
  return(plot_fun)
}

prep_outro_rdgs_fun <- function(rdg_ind="1_fetch/out/rapid_dep_sites.rds.ind", gage_col_config, outro_placement, legend_text_cfg, opacity=1) {
  # project from lat/lon to plot coordinates. . this properly belongs in a 2_process step, but we're releasing tomorrow
  proj_str <- "+proj=lcc +lat_1=34.83333333333334 +lat_2=32.5 +lat_0=31.83333333333333 +lon_0=-81 +x_0=609600 +y_0=0 +ellps=GRS80 +units=m +no_defs "
  rdgs <- readRDS(sc_retrieve(rdg_ind)) %>%
    sf::st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
    sf::st_transform(crs = proj_str) %>%
    select(site_no, geometry)
  
  if(opacity != 1) stop("opacity other than 1 not yet supported")
  
  stn_pch <- 19
  stn_col <- gage_col_config$gage_stn_col
  stn_cex <- 0.8
  
  plot_fun <- function(){
    # plot the gage points
    plot(sf::st_geometry(rdgs$geometry), add = TRUE,
         pch = stn_pch, col = stn_col, cex = stn_cex)

    # plan text and legend coordinates    
    user_coords <- par()$usr
    line_spacing_vert <- strheight("A", cex=legend_text_cfg$cex, family=legend_text_cfg$family)
    dot_spacing_vert <- line_spacing_vert / 3
    y_text <- user_coords[3] + outro_placement$ytop_stn * diff(user_coords[3:4])
    y_legend_text <- y_text - line_spacing_vert * 4
    y_legend_dot <- y_legend_text - dot_spacing_vert
    xleft <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
    xleft_dot <- xleft + min(outro_placement$xleft, 1-outro_placement$xleft) * 0.05 * diff(user_coords[1:2])
    xleft_indent <- xleft + min(outro_placement$xleft, 1-outro_placement$xleft) * 0.1 * diff(user_coords[1:2])
    
    # plot text and legend
    text(x=xleft, y=y_text, labels="We also deployed extra\ngages just for this storm", adj=c(0, 1),
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    text(x=xleft_indent, y=y_legend_text, labels="Short-term gages", adj=c(0, 1),
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    points(x=xleft_dot, y=y_legend_dot, pch = stn_pch, col = stn_col, cex = stn_cex)
  }
  return(plot_fun)
}

prep_outro_readmore_fun <- function(outro_placement, legend_text_cfg, opacity=1) {
  
  if(opacity != 1) stop("opacity other than 1 not yet supported")
  
  plot_fun <- function(){
    
    # plan text coordinates    
    user_coords <- par()$usr
    line_spacing_vert <- strheight("A", cex=legend_text_cfg$cex, family=legend_text_cfg$family)
    y_text <- user_coords[3] + outro_placement$ytop_more * diff(user_coords[3:4])
    xleft <- user_coords[1] + outro_placement$xleft * diff(user_coords[1:2])
    
    # plot text
    text(x=xleft, y=y_text, labels="Learn more at\nhttps://waterdata.usgs.gov/nwis\nand stay safe!", adj=c(0, 1),
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
  }
  return(plot_fun)
}