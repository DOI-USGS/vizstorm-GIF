prep_storm_sites_fun <- function(storm_data, gage_col_config, DateTime){
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.
  this_storm_data <- filter(storm_data, dateTime == this_DateTime)

  norm_data_sites <- filter(this_storm_data, stage_normalized < flood_stage_normalized)
  flood_data_sites <- filter(this_storm_data, stage_normalized >= flood_stage_normalized)
  missing_data_sites <- filter(this_storm_data, is.na(stage_normalized))

  plot_fun <- function(){

    plot(sf::st_geometry(norm_data_sites$geometry), add = TRUE,
         pch = 21, bg = gage_col_config$gage_norm_col, col = NA, cex = 1.3)
    plot(sf::st_geometry(flood_data_sites$geometry),
         pch = 21, bg = gage_col_config$gage_norm_col, add = TRUE,
         col = gage_col_config$gage_flood_col, lwd = 2, cex = 1.3)
    plot(sf::st_geometry(missing_data_sites$geometry),
         pch = 4, bg = gage_col_config$gage_norm_col, add = TRUE,
         col = gage_col_config$gage_out_col, lwd = 4, cex = 1.3)

  }
  return(plot_fun)
}
