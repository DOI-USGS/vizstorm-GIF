prep_storm_sites_initial <- function(storm_sites_ind_file, viz_config_yaml, DateTime){
  storm_sites <- readRDS(sc_retrieve(storm_sites_ind_file))
  viz_config <- yaml::yaml.load_file(viz_config_yaml)

  plot_fun <- function(){

    plot(sf::st_geometry(storm_sites$geometry), add = TRUE,
         pch = 21, bg = viz_config$gage_norm_col, col = NA)

  }
  return(plot_fun)
}

prep_storm_sites_fun <- function(storm_data, viz_config_yaml, DateTime){
  viz_config <- yaml::yaml.load_file(viz_config_yaml)
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.
  this_storm_data <- filter(storm_data, dateTime == this_DateTime)

  norm_data_sites <- filter(this_storm_data, stage_normalized < flood_stage_normalized)
  flood_data_sites <- filter(this_storm_data, stage_normalized >= flood_stage_normalized)

  plot_fun <- function(){

    plot(sf::st_geometry(norm_data_sites$geometry), add = TRUE,
         pch = 21, bg = viz_config$gage_norm_col, col = NA)
    plot(sf::st_geometry(flood_data_sites$geometry),
         pch = 21, bg = viz_config$gage_norm_col, add = TRUE,
         col = viz_config$gage_flood_col, lwd = 1)

  }
  return(plot_fun)
}
