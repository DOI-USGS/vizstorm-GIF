combine_storm_frames <- function(gif_file, task_names) {

  # run imageMagick convert to build a gif
  png_files <- paste(sprintf('6_visualize/tmp/gif_frame_%s.png', task_names), collapse=',')
  command <- sprintf('convert -delay 8 -loop 0 %s %s', png_files, gif_file)
  system(command)

  # simplify the gif with gifsicle
  #[coming]
}
