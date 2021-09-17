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
