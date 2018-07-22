
fetch_read <- function(ind_file) readRDS(sc_retrieve(ind_file))

format_start_date <- function(timestep_ind) {
  timesteps <- fetch_read(timestep_ind)
  return(timesteps[1])
}

existing_png_tasks <- function(path='6_visualize/tmp', pattern='a_[[:digit:]]{8}_[[:digit:]]{2}') {
  files <- dir(path, pattern=sprintf('%s.*\\.png', pattern))
  tasks <- regmatches(files, regexpr(sprintf("(%s)", pattern), files))
  return(tasks)
}
