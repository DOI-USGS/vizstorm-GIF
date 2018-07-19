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
  r_revised <- pmin(r, x2-x1, y2-y1)
  r_rise <- r_revised * sign(y2 - y1)
  r_run <- r_revised * sign(x2 - x1)
  T_move <- t_end - t_start # length of the move in number of timesteps. probably the same for all sites
  D_rise <- y2 - r_rise - y1 # length of the purely vertical segment of the travel path
  D_round <- pi*r/2 # length of the segment of the travel path that rounds the corner from vertical to horizontal
  D_run <- x2 - r_run - x1 # length of the purely horizontal segment of the travel path
  D <- abs(D_rise) + abs(D_round) + abs(D_run) # length of the full travel path

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
    breaks_all <- phases[site,]
    empty_bins <- breaks_all[diff(breaks_all)==0]
    breaks_nonempty <- breaks_all[setdiff(names(breaks_all), names(empty_bins))]
    cut(timestep, breaks=breaks_nonempty, labels=setdiff(names(breaks_nonempty), 'end'), right=FALSE) %>%
      as.character()
  }) %>% ordered(levels=colnames(phases))
  # calculate phase_pos, the fraction of the way each site is through its current phase
  phase_start <- phases[cbind(seq_len(n_sites), phase)]
  phase_end <- phases[cbind(seq_len(n_sites), as.numeric(phase)+1)]
  phase_pos <- (timestep - phase_start)/(phase_end - phase_start)
  theta <- phase_pos*pi/2 # angle as fraction of 90 degrees (pi/2 radians); calculated for all, applies only to round phase

  # calculate coordinates for this timestep for each site
  too_many_coords <- list(
    prelude = data_frame(x=x1, y=y1),
    rise = data_frame(x=x1, y=y1 + phase_pos*D_rise),
    round = data_frame(x=x1 + r_run*(1-cos(theta)), y=y2 - r_rise*(1 - sin(theta))),
    run = data_frame(x=x1 + r_run + phase_pos*D_run, y=y2),
    epilog = data_frame(x=x2, y=y2))
  coords <- bind_rows(lapply(seq_len(n_sites), function(site) {
    too_many_coords[[as.character(phase[site])]][site,]
  }))

  # make a sterile environment with nothing except what we need in the closure,
  # then make the function
  clean_env <- function(coords) {
    function(){
      plot(x=pull(coords, x), y=pull(coords, y), add=TRUE)
    }
  }
  plot_fun <- clean_env(coords)
  return(plot_fun)
}
