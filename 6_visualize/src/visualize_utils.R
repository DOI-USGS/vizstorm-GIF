
fetch_read <- function(ind_file) readRDS(sc_retrieve(ind_file, remake_file = getOption("scipiper.remake_file")))

get_date_limits <- function(timestep_ind) {
  timesteps_utc <- fetch_read(timestep_ind)
  return(range(timesteps_utc))
}

convert_to_local_tz <- function(date, local_tz = "") {
  # Convert to the correct timezone (must change to string to do that)
  date_str <- strftime(date, format = '%Y-%m-%d %H:%M:%S %Z', tz = local_tz)
  # Convert string back into POSIXct using local tz
  date_local <- as.POSIXct(date_str, tz = local_tz)
  return(date_local)
}

existing_png_tasks <- function(path='6_visualize/tmp', pattern='a_[[:digit:]]{8}_[[:digit:]]{2}') {
  files <- dir(path, pattern=sprintf('%s.*\\.png', pattern))
  tasks <- regmatches(files, regexpr(sprintf("(%s)", pattern), files))
  return(tasks)
}

timesteps_to_build <- function(timestep_ind, frame_step = 1) {

  all_timestep <- readRDS(sc_retrieve(timestep_ind, remake_file = getOption("scipiper.remake_file")))

  timestep <- all_timestep[seq(1, by = frame_step, to = length(all_timestep))] %>%
    # ** skip the first frame! it is empty for sparks and causes a nasty blink between intro and storm frames:
    tail(-1L)

}
