combine_animation_frames_video <- function(out_file, animation_cfg, intro_config, frame_ind_intro=NULL, frame_ind_storm=NULL, frame_ind_outro=NULL) {

  # Use ffmpeg to build a video

  # Gather appropriate file names (not just whatever is in the directory)
  png_files_intro <- extract_filenames_from_ind(frame_ind_intro)
  png_files_storm <- extract_filenames_from_ind(frame_ind_storm)
  png_files_outro <- extract_filenames_from_ind(frame_ind_outro)

  # Create separate videos for intro, storm, and outro
  make_video <- function(video_out, png_files, frame_delay) {

    # FFMPEG only works on files numbered sequentially, so rename temporarily
    file_name_df <- tibble(origName = png_files,
                           countFormatted = zeroPad(1:length(png_files), padTo = 3),
                           newName = file.path("6_visualize/tmp", paste0("frame_", countFormatted, ".png")))
    file.rename(from = file_name_df$origName, to = file_name_df$newName)

    fps <- 100/frame_delay # turn time per frame into frame per second (fps)

    shell_command <- sprintf(
      "ffmpeg -y -framerate %s -i 6_visualize/tmp/frame_%%03d.png -r %s -pix_fmt yuv420p -vcodec libx264 -crf 27 %s",
      fps, fps, video_out)
    system(shell_command)

    file.rename(from = file_name_df$newName, to = file_name_df$origName)
    return(video_out)
  }

  video_intro <- make_video("6_visualize/tmp/intro.mp4", png_files_intro, intro_config$frame_delay_cs)
  video_storm <- make_video("6_visualize/tmp/storm.mp4", png_files_storm, animation_cfg$frame_delay_cs)
  video_outro <- make_video("6_visualize/tmp/outro.mp4", png_files_outro, 200)

  # Stitch videos together
  files_to_cat_fn <- "6_visualize/tmp/videos_to_concat.txt"
  writeLines(sprintf("file '%s'", c(basename(video_intro), basename(video_storm), basename(video_outro))), files_to_cat_fn)

  system(sprintf(
    'ffmpeg -y -safe 0 -f concat -i %s -c copy %s',
    files_to_cat_fn,
    out_file
  ))

  return(out_file)
}

combine_animation_frames_gif <- function(gif_file, animation_cfg, intro_config, frame_ind_intro=NULL, frame_ind_storm=NULL, frame_ind_outro=NULL) {

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
  sprintf(
    'convert -define registry:temporary-path=%s -limit memory 24GiB -delay %d -loop 0 %s %s',
    tmp_dir, animation_cfg$frame_delay_cs, png_files_string, gif_file) %>%
    run_magick_command()

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

  calc_delays <- function(delay, start_frame, end_frame){
    paste(paste(sprintf('-d%s "#', delay), seq(start_frame-1, end_frame-1), sep = '') %>%
            paste('"', sep = ''), collapse = " ")
  }

  intro_delay_str <- calc_delays(intro_delay, 1, n_intro)
  storm_delay_str <- calc_delays(storm_delay, n_intro+1, total_frames-n_outro-1)
  # freeze the last storm frame too for as long as we are showing each outro frame:
  last_storm_delay_str <- calc_delays(freeze_delay, total_frames-n_outro, total_frames-n_outro)
  outro_delay_str <- calc_delays(outro_delay, total_frames-n_outro+1, total_frames-1)
  final_delay_str <- calc_delays(final_delay, total_frames, total_frames)

  gifsicle_command <- sprintf('gifsicle -b -O3 %s %s %s %s %s %s --colors 256', gif_file,
                              intro_delay_str, storm_delay_str, last_storm_delay_str,
                              outro_delay_str, final_delay_str)
  system(gifsicle_command)
}

extract_filenames_from_ind <- function(ind_file) {
  filename_hash_list <- readLines(ind_file)
  only_names <- unlist(lapply(strsplit(filename_hash_list, ":"), `[`, 1))
  only_real_files <- na.omit(only_names) # sometimes there is an empty line at the end of the file that is read and turns into an NA
  return(only_real_files)
}
