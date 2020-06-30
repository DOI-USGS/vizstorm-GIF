
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

timesteps_to_build <- function(timestep_ind, frame_step = 1) {

  all_timestep <- readRDS(sc_retrieve(timestep_ind))

  timestep <- all_timestep[seq(1, by = frame_step, to = length(all_timestep))] %>%
    # ** skip the first frame! it is empty for sparks and causes a nasty blink between intro and storm frames:
    tail(-1L)

}
