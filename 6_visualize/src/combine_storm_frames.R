combine_animation_frames <- function(gif_file, animation_cfg, task_names=NULL) {

  # run imageMagick convert to build a gif
  if(is.null(task_names)) task_names <- '*'
  png_files <- paste(sprintf('6_visualize/tmp/gif_frame_%s.png', task_names), collapse=',')
  magick_command <- sprintf('magick convert -delay %d -loop 0 %s %s',
                            animation_cfg$frame_delay_cs, '6_visualize/tmp/gif_frame_*.png', gif_file)
  system(magick_command)

  # simplify the gif with gifsicle - cuts size by about 2/3
  gifsicle_command <- sprintf('gifsicle -b -O3 %s --colors 256', gif_file)
  system(gifsicle_command)
}
