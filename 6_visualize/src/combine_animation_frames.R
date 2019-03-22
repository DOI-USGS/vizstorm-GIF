combine_animation_frames <- function(out_file, animation_cfg, task_names=NULL, intro_config, n_outro) {

  ## I still want to use a better way than just assuming what is in tmp will be used
  
  # use ffmpeg to build a videos
  png_frames <- list.files('6_visualize/tmp', full.names = TRUE)
  png_frames_right <- png_frames[grep('gif_frame_a_', png_frames)]
  tmp_dir_video <- '6_visualize/tmp/video'
  if(dir.exists(tmp_dir_video)) {
    existing_files <- list.files(tmp_dir_video, full.names = TRUE)
    if(length(existing_files) > 0) file.remove(existing_files)
  } else { 
    dir.create('6_visualize/tmp/video') 
  }
  file_name_df <- tibble(origName = png_frames,
                         countFormatted = zeroPad(1:length(png_frames), padTo = 3),
                         newName = file.path("6_visualize/tmp/video", paste0("gif_frame_", countFormatted, ".png")))
  file.copy(from = file_name_df$origName, to = file_name_df$newName)
  shell_command <- sprintf(
    "ffmpeg -y -framerate %s -i 6_visualize/tmp/video/gif_frame_%%03d.png -r %s -pix_fmt yuv420p %s",
    animation_cfg$video_cfg$frame_rate, animation_cfg$video_cfg$frame_rate, out_file)
  
  system(shell_command)
  file.remove(file_name_df$newName)
}
