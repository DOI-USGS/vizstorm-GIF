
fetch_read <- function(ind_file) readRDS(sc_retrieve(ind_file))

format_start_date <- function(timestep_ind) {
  timesteps <- fetch_read(timestep_ind)
  return(timesteps[1])
}
