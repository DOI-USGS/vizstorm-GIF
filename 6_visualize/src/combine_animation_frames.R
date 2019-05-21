combine_animation_frames <- function(out_file, animation_cfg, gif_tasks_ind) {

  # use ffmpeg to build a video
  
  # Use ind from 6_storm_gif_tasks.yml to see what frames should be in 
  # the animation, rather than assume it's any frame in 6_visualize/tmp
  png_frames <- names(yaml.load_file(gif_tasks_ind))
  
  # Create or clear tmp directory if it's not already
  tmp_dir_video <- '6_visualize/tmp/video'
  if(dir.exists(tmp_dir_video)) {
    existing_files <- list.files(tmp_dir_video, full.names = TRUE)
    if(length(existing_files) > 0) file.remove(existing_files)
  } else { 
    dir.create('6_visualize/tmp/video') 
  }
  
  # Copy pngs into a tmp directory to rename so they can be used in the ffmpeg statement
  file_name_df <- tibble(origName = png_frames,
                         countFormatted = zeroPad(1:length(png_frames), padTo = 3),
                         newName = file.path("6_visualize/tmp/video", paste0("gif_frame_", countFormatted, ".png")))
  file.copy(from = file_name_df$origName, to = file_name_df$newName)
  
  # Create and execute the ffmpeg command
  shell_command <- sprintf(
    "ffmpeg -y -framerate %s -i 6_visualize/tmp/video/gif_frame_%%03d.png -r %s -pix_fmt yuv420p %s",
    animation_cfg$video_cfg$frame_rate, animation_cfg$video_cfg$output_frame_rate, out_file)
  system(shell_command)
  
  # reset tmp directory
  file.remove(file_name_df$newName)
}
