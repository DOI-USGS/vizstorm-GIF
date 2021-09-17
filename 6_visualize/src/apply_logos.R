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
