#' @param ind_file scipiper indicator file
#' @param gridmet_file file name to download from gridmet
fetch_gridmet_data <- function(ind_file, gridmet_file) {
  nc_file <- as_data_file(ind_file)

  gridmet_url <- "https://www.northwestknowledge.net/metdata/data/"

  download.file(paste0(gridmet_url, gridmet_file), nc_file)

  gd_put(remote_ind=ind_file, local_source=nc_file, mock_get='none')
}

#' @param ind_file scipiper indicator file
#' @param view_polygon
fetch_viz_cells <- function(ind_file, view_polygon, viz_cell_size_meters) {

  viz_cells <- sf::st_make_grid(view_polygon, viz_cell_size_meters, square = FALSE)

  data_file <- as_data_file(ind_file)
  saveRDS(viz_cells, file = data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}
