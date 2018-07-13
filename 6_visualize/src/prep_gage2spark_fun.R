# animate gage points so they move vertically, round a corner, and then move
# horizontally until they stop at their respective sparkline start locations
prep_gage2spark_fun <- function(n_timesteps, timestep) {

  # will be arguments or extracted from arguments
  n_sites <- 5
  x1 <- seq(400, 600, length.out=n_sites) # gage location for each site
  y1 <- rep(200, n_sites)
  x2 <- 900 # beginning of spark line
  y2 <- seq(400, 200, length.out=n_sites)
  r <- 30 # radius of the curvature at the elbow of the dot travel path, in pixels
  t_start <- 1:n_sites # timestep at which the site dot first leaves its lat/lon location, vector of 1 value per site
  t_end <- (20-n_sites+1):20 # timestep at which the site dot arrives at its sparkline location, vector of 1 value per site

  # calculate additional variables. naming convention for d and t: lowercase =
  # position in space/time, uppercase = length/duration
  T_move <- t_end - t_start # length of the move in number of timesteps. probably the same for all sites
  D_rise <- abs(y2 - r - y1) # length of the purely vertical segment of the travel path
  D_round <- pi*r/2 # length of the segment of the travel path that rounds the corner from vertical to horizontal
  D_run <- abs(x2 - r - x1) # length of the purely horizontal segment of the travel path
  D <- D_rise + D_round + D_run # length of the full travel path

  # define phases by their first timestep (different for each site=row)
  phases <- data_frame(
    prelude = rep(-Inf, length(x1)),
    rise = t_start,
    round = t_start + T_move * D_rise/D,
    run = t_start + T_move * (D_rise+D_round)/D,
    epilog = t_end,
    end = Inf) %>%
    as.matrix()

  phase <- sapply(seq_len(n_sites), function(site) {
    cut(timestep, breaks=phases[site,], labels=colnames(phases)[-6], right=FALSE)
  })
  phase_pos <- (timestep

  plot_fun <- function(){
    # empty placeholder for now
  }
  return(plot_fun)
}
