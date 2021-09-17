prep_logo_overlay <- function(out_file, in_file, viz_config_dim) {

  # Get rid of logo background, change to grey, then resize and make a gif to overlay
  sprintf(
    'convert %s -transparent white -fill "#808080" -opaque black -adaptive-resize %sx %s',
    in_file,
    viz_config_dim$width*0.10, # Make logo 10% of frame width
    out_file
  ) %>% run_magick_command()

  return(out_file)

}

apply_logo_to_gif <- function(out_file, in_file, logo_file) {

  logo_prepped_file <- "6_visualize/tmp/usgs_logo.gif"
  sprintf(
    'convert %s %s',
    logo_file,
    logo_prepped_file
  ) %>% run_magick_command()

  sprintf(
    'convert %s -coalesce -gravity NorthWest -geometry +5+5 null: %s -layers composite -layers optimize %s',
    in_file,
    logo_prepped_file,
    out_file
  ) %>% run_magick_command()

  return(out_file)
}

apply_logo_to_video <- function(out_file, in_file, logo_file, viz_config_dim) {

  # Overlay the logo onto the video
  system(sprintf(
    'ffmpeg -y -i %s -i %s -filter_complex "overlay=%s:%s" -c:v libx264  %s',
    in_file,
    logo_file,
    viz_config_dim$width*0.005, # Place just slightly in from the left
    viz_config_dim$height*0.005, # Place just slightly down from the top
    out_file))

  return(out_file)
}

apply_visid_banner <- function(out_file, in_file, viz_config_dim, visid_overlay_file) {

  # Identify intermediate files
  video_scaled_for_visid_file <- "6_visualize/tmp/video_scaled_for_visid.mp4"
  visid_overlay_file_scaled <- '6_visualize/tmp/visid_overlay_scaled.png'

  # Declare how tall the black bar should be (for now, adding in an extra 10% of the image height)
  height_black_bar <- viz_config_dim$height*0.10

  # Pad the existing video to include a black bar on the bottom
  system(sprintf(
    'ffmpeg -y -i %s -vf "pad=%s:%s:(ow-iw)/2:color=black" %s',
    in_file,
    viz_config_dim$width,
    viz_config_dim$height+height_black_bar,
    video_scaled_for_visid_file
  ))

  # Scale the logo to fit in the black bar
  height_logo <- height_black_bar*0.95
  sprintf(
    'convert %s -resize x%s %s',
    visid_overlay_file,
    height_logo,
    visid_overlay_file_scaled
  ) %>% run_magick_command()

  # Overlay the logo onto the video
  system(sprintf(
    'ffmpeg -y -i %s -i %s -filter_complex "overlay=%s:(H-%s)" -c:v libx264  %s',
    video_scaled_for_visid_file,
    visid_overlay_file_scaled,
    viz_config_dim$width*0.005, # Place just slightly in from the left
    height_black_bar - (height_black_bar - height_logo)/2, # Center the logo in the black bar
    out_file))

  return(out_file)
}
