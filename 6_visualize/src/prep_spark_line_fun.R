
prep_spark_line_fun <- function(storm_data_ind_file, viz_config_yaml, DateTime){
  storm_data <- readRDS(sc_retrieve(storm_data_ind_file))
  viz_config <- yaml::yaml.load_file(viz_config_yaml)

  sites <- unique(storm_data$site_no)
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.
  this_spark <- filter(storm_data, dateTime <= this_DateTime) # keep all data up until this timestep

  plot_fun <- function(){
    orig_par <- par()

    # Set spacing configurations
    x_coords <- c(viz_config$sparks$xleft, viz_config$sparks$xright)
    y_start <- viz_config$sparks$ybottom
    y_space <- viz_config$sparks$ytop-y_start
    vertical_spacing <- y_space/length(sites)

    y_pos <- y_start # initialize position of first spark
    for(i in sites) {

      # Filter data to just one site & create polygon out of it
      storm_data_i <- filter(this_spark, site_no == i) %>% arrange(dateTime)
      storm_data_site_all <- filter(storm_data, site_no == i)

      if(nrow(storm_data_i) == 0) { next } # skip if there's no data at or before this timestep

      flood_stage_va <- as.numeric(unique(storm_data_i$flood_stage_normalized))
      lowest_stage <- min(storm_data_site_all$stage_normalized, na.rm = TRUE)
      highest_stage <- max(storm_data_site_all$stage_normalized, na.rm = TRUE)

      full_polygon <- data.frame(dateTime = head(storm_data_i$dateTime, 1),
                                 stage_normalized = min(storm_data_i$stage_normalized,1)) %>%
        bind_rows(select(storm_data_i, dateTime, stage_normalized)) %>%
        bind_rows(data.frame(dateTime = tail(storm_data_i$dateTime, 1),
                             stage_normalized = min(storm_data_i$stage_normalized)))

      # Fill values lower than flood stage with the stage & then create a polygon out of it
      above_flood_data <- mutate(storm_data_i, stage_normalized = ifelse(stage_normalized < flood_stage_va,
                                                                         flood_stage_va,
                                                                         stage_normalized))
      above_flood_polygon <- data.frame(dateTime = head(above_flood_data$dateTime, 1),
                                        stage_normalized = head(above_flood_data$stage_normalized, 1)) %>%
        bind_rows(select(above_flood_data, dateTime, stage_normalized)) %>%
        bind_rows(data.frame(dateTime = tail(above_flood_data$dateTime, 1),
                             stage_normalized = min(above_flood_data$stage_normalized)))

      y_coords <- c(y_pos, y_pos + vertical_spacing) # y coords relative to plot box
      fig.new <- c(grconvertX(x_coords, from="npc", to="ndc"), grconvertY(y_coords, from="npc", to="ndc"))

      op <- par(fig=fig.new, cex=2, new=TRUE, mar=rep(0, 4)) # setup parameters
      # setup a plotting device
      plot(x=NA, y=NA, type='n', axes=FALSE, xlab="", ylab="",
           xlim = c(as.POSIXct(viz_config$dates$start, tz = "UTC"), as.POSIXct(viz_config$dates$end, tz = "UTC")),
           ylim = c(lowest_stage, highest_stage))
      # put stage polygons on plot
      polygon(full_polygon$dateTime, full_polygon$stage_normalized, col = "#4BA3C3", border=NA)
      polygon(above_flood_polygon$dateTime, above_flood_polygon$stage_normalized, col = "#175676", border=NA)
      points(full_polygon$dateTime[-nrow(full_polygon)], full_polygon$stage_normalized[-nrow(full_polygon)],
             col = "#175676", type='l', lwd=2.5)
      par(op) # reset plot parameters

      y_pos <- y_pos + vertical_spacing # increment spacing for next plot
    }

    # Reset so next plot item is using full plot parameters
    par(orig_par)
  }

  return(plot_fun)
}
