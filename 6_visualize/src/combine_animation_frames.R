combine_animation_frames <- function(gif_file, animation_cfg, frame_file_inds=NULL, frame_file_makefiles=NULL, intro_config, n_outro) {
  # run imageMagick convert to build a gif
  if(is.null(task_names)) {
    task_names <- '*'
    png_files <- paste(sprintf('6_visualize/tmp/gif_frame_%s.png', task_names), collapse=' ')
  } else {
    #this only works for length 1 right now
      # Check to make sure that makefiles matchs inds
      # stopifnot(length(frame_file_makefiles) %in% c(1, length(frame_file_inds)))
    png_files <- readRDS(sc_retrieve(frame_file_inds, remake_file = frame_file_makefiles))
  }
  tmp_dir <- './6_visualize/tmp/magick'
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)
  magick_command <- sprintf(
    'convert -define registry:temporary-path=%s -limit memory 24GiB -delay %d -loop 0 %s %s',
    tmp_dir, animation_cfg$frame_delay_cs, png_files, gif_file)
  if(Sys.info()[['sysname']] == "Windows") {
    magick_command <- sprintf('magick %s', magick_command)
  }
  system(magick_command)

  # simplify the gif with gifsicle - cuts size by about 2/3
  stopifnot(task_names == '*')

  png_dir <- dirname(png_files)
  png_patt <- basename(png_files) %>% tools::file_path_sans_ext()
  total_frames <- grepl(dir(png_dir), pattern = png_patt) %>% sum()
  # how many intro frames? how many storm frames? how many outro frames?
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
  intro_delays <- calc_delays(intro_delay, 1, intro_config$n_frames)
  storm_delays <- calc_delays(storm_delay, intro_config$n_frames+1, total_frames-n_outro-1)
  # freeze the last storm frame too for as long as we are showing each outro frame:
  last_storm_delay <- calc_delays(freeze_delay, total_frames-n_outro, total_frames-n_outro)
  outro_delays <- calc_delays(outro_delay, total_frames-n_outro+1, total_frames-1)
  final_delay <- calc_delays(final_delay, total_frames, total_frames)

  gifsicle_command <- sprintf('gifsicle -b -O3 %s %s %s %s %s %s --colors 256', gif_file, intro_delays, storm_delays, last_storm_delay, outro_delays, final_delay)
  system(gifsicle_command)
}


combine_frames_into_list <- function(ind_file, ...){

  png_files <- c(...)

  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(png_files, data_file)
  gd_put(ind_file, data_file)
}
