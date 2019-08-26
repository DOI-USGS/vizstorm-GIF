combine_animation_frames <- function(gif_file, animation_cfg, frame_ind_intro=NULL, frame_ind_storm=NULL, frame_ind_outro=NULL, intro_config) {

  # run imageMagick convert to build a gif

  # TO DO: situation where any of the frame inds are NULL

  # Gather appropriate file names (not just whatever is in the directory)
  png_files_intro <- extract_filenames_from_ind(frame_ind_intro)
  png_files_storm <- extract_filenames_from_ind(frame_ind_storm)
  png_files_outro <- extract_filenames_from_ind(frame_ind_outro)
  png_files <- c(png_files_intro, png_files_storm, png_files_outro)
  png_files_string <- paste(png_files, collapse=' ')

  tmp_dir <- './6_visualize/tmp/magick'
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)
  magick_command <- sprintf(
    'convert -define registry:temporary-path=%s -limit memory 24GiB -delay %d -loop 0 %s %s',
    tmp_dir, animation_cfg$frame_delay_cs, png_files_string, gif_file)
  if(Sys.info()[['sysname']] == "Windows") {
    magick_command <- sprintf('magick %s', magick_command)
  }
  system(magick_command)

  # simplify the gif with gifsicle - cuts size by about 2/3

  # how many intro frames? how many storm frames? how many outro frames?
  total_frames <- length(png_files)
  n_intro <- length(png_files_intro)
  n_outro <- length(png_files_outro)

  intro_delay <- intro_config$frame_delay_cs
  storm_delay <- animation_cfg$frame_delay_cs
  outro_delay <- 200
  final_delay <- 700
  freeze_delay <- 150
  # **trash code for now:
  calc_delays <- function(delay, start_frame, end_frame){
    paste(paste(sprintf('-d%s "#', delay), seq(start_frame-1, end_frame-1), sep = '') %>%
            paste('"', sep = ''), collapse = " ")
  }
  intro_delays <- calc_delays(intro_delay, 1, n_intro)
  storm_delays <- calc_delays(storm_delay, n_intro+1, total_frames-n_outro-1)
  # freeze the last storm frame too for as long as we are showing each outro frame:
  last_storm_delay <- calc_delays(freeze_delay, total_frames-n_outro, total_frames-n_outro)
  outro_delays <- calc_delays(outro_delay, total_frames-n_outro+1, total_frames-1)
  final_delay <- calc_delays(final_delay, total_frames, total_frames)

  gifsicle_command <- sprintf('gifsicle -b -O3 %s %s %s %s %s %s --colors 256', gif_file, intro_delays, storm_delays, last_storm_delay, outro_delays, final_delay)
  system(gifsicle_command)
}

extract_filenames_from_ind <- function(ind_file) {
  filename_hash_list <- readLines(ind_file)
  only_names <- lapply(filename_hash_list, function(fn) {
    head(unlist(strsplit(fn, split = ":")), 1)
  })
  return(unlist(only_names))
}
