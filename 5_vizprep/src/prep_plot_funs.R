
prep_basemap_fun <- function(ind_file, focus_geoms_ind, secondary_geoms_ind = NULL, detail_geoms_ind = NULL){

  plot_fun <- function(){
    if (!is.null(secondary_geoms_ind)){
      secondary_geoms <- readRDS(sc_retrieve(secondary_geoms_ind))
      plot(secondary_geoms, add = TRUE, lwd = 0.3, col = 'grey80') # should style args be in a config?
    }

    if (!is.null(detail_geoms_ind)){
      detail_geoms <- readRDS(sc_retrieve(detail_geoms_ind))
      plot(detail_geoms, add = TRUE, lwd = 0.3, col = NA, border = 'grey95') # should style args be in a config?
    }

    focus_geoms <- readRDS(sc_retrieve(focus_geoms_ind))
    plot(focus_geoms, add = TRUE, col = NA, border = 'grey40')
  }
  write_put_fun(plot_fun, ind_file)
}

prep_view_fun <- function(ind_file, view_polygon){
  plot_fun <- function(){
    par(omi = c(0,0,0,0), mai = c(0,0,0,0))
    plot(view_polygon, col = NA, border = NA, xaxs = 'i', yaxs = 'i')
  }
  write_put_fun(plot_fun, ind_file)
}

POSIX_from_filename <- function(filename){
  date_char <- gsub("\\[|\\]", "", regmatches(filename, gregexpr("\\[.*?\\]", filename))[[1]][1])
  posix_out <- as.POSIXct(date_char, format = '%Y%m%d-%H', tz = "UTC")
  if (is.na(posix_out)){
    stop('Parsing error with', filename, call. = FALSE)
  }
  return(posix_out)
}

prep_storm_point_fun <- function(ind_file, storm_points_ind_file){
  storm_points_sf <- readRDS(sc_retrieve(storm_points_ind_file))
  # parse date from ind_file
  this_DateTime <- POSIX_from_filename(ind_file)
  this_dot <- filter(storm_points_sf, DateTime == this_DateTime)
  plot_fun <- function(){
    plot(this_dot, add = TRUE, col = 'red', pch = 20, cex = 2) # should style args be in a config?
  }
  write_put_fun(plot_fun, ind_file)
}

prep_storm_line_fun <- function(ind_file, storm_line_ind_file){
  storm_line_sf <- readRDS(sc_retrieve(storm_line_ind_file))

  plot_fun <- function(){
    plot(storm_line_sf, add = TRUE, col = 'black', lwd = 2)
  }
  write_put_fun(plot_fun, ind_file)
}
write_put_fun <- function(plot_fun, ind_file){
  data_file <- as_data_file(ind_file)
  saveRDS(plot_fun, data_file)
  gd_put(ind_file, data_file)
}
