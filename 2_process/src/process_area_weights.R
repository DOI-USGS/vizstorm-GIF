process_area_weights <- function(ind_file, nc_ind, nc_var, viz_polys_ind) {

  target_polygons <- readRDS(sc_retrieve(viz_polys_ind))

  nc <- RNetCDF::open.nc(sc_retrieve(nc_ind))

  nc_coord_vars <- nc_coord_var(nc, nc_var)
  nc_prj <- nc_gm_to_prj(nc_grid_mapping_atts(nc))

  x <- var.get.nc(nc, nc_coord_vars$X, unpack = TRUE)
  y <- var.get.nc(nc, nc_coord_vars$Y, unpack = TRUE)

  data_source_cells <- create_cell_geometry(X_coords = x,
                                            Y_coords = y,
                                            prj = nc_prj,
                                            geom = target_polygons,
                                            buffer_dist = 0)

  # id here may need to change when we use specific polygons.
  target_polygons <- st_sf(id = c(1:length(target_polygons)), target_polygons)
  st_agr(data_source_cells) <- "constant"
  st_agr(target_polygons) <- "constant"

  area_weights <-
    calculate_area_intersection_weights(st_sf(select(data_source_cells, grid_ids)),
                                        target_polygons)

  data_file <- as_data_file(ind_file)
  saveRDS(list(area_weights = area_weights,
               data_source_cells = data_source_cells,
               nc_coord_vars = nc_coord_vars),
          file = data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')

}

process_intersectr <- function(ind_file, nc_ind, nc_var, area_weights_ind,
                               dates) {
  area_weights <- readRDS(sc_retrieve(area_weights_ind))

  data_source_cells <- area_weights$data_source_cells
  nc_coord_vars <- area_weights$nc_coord_vars
  area_weights <- area_weights$area_weights

  start_date <- as.POSIXct(dates$start, tz = 'UTC')
  end_date <- as.POSIXct(dates$end, tz = 'UTC')

  intersected <- execute_intersection(nc_file = sc_retrieve(nc_ind),
                                      variable_name = nc_var,
                                      intersection_weights = area_weights,
                                      cell_geometry = data_source_cells,
                                      x_var = nc_coord_vars$X,
                                      y_var = nc_coord_vars$Y,
                                      t_var = nc_coord_vars$T,
                                      start_datetime = start_date,
                                      end_datetime = end_date)

  data_file <- as_data_file(ind_file)
  saveRDS(intersected, file = data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}
