
prep_spark_line_fun <- function(storm_data, viz_config_yaml, DateTime){
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

      if(nrow(storm_data_i) == 0) { next } # skip if there's no data at or before this timestep

      flood_stage_va <- unique(storm_data_i$flood_stage_normalized)

      full_polygon <- data.frame(dateTime = head(storm_data_i$dateTime, 1),
                                 stage_normalized = 0) %>%
        bind_rows(select(storm_data_i, dateTime, stage_normalized)) %>%
        bind_rows(data.frame(dateTime = tail(storm_data_i$dateTime, 1),
                             stage_normalized = 0))

      # Fill values lower than flood stage with the stage & then create a polygon out of it
      above_flood_data <- mutate(storm_data_i, stage_normalized = pmax(stage_normalized, flood_stage_va))
      above_flood_polygon <- data.frame(dateTime = head(above_flood_data$dateTime, 1),
                                        stage_normalized = flood_stage_va) %>%
        bind_rows(select(above_flood_data, dateTime, stage_normalized)) %>%
        bind_rows(data.frame(dateTime = tail(above_flood_data$dateTime, 1),
                             stage_normalized = min(above_flood_data$stage_normalized)))

      y_coords <- c(y_pos, y_pos + vertical_spacing) # y coords relative to plot box
      fig.new <- c(grconvertX(x_coords, from="npc", to="ndc"), grconvertY(y_coords, from="npc", to="ndc"))

      op <- par(fig=fig.new, cex=2, new=TRUE, mar=rep(0, 4)) # setup parameters
      # setup a plotting device
      plot(x=NA, y=NA, type='n', axes=FALSE, xlab="", ylab="",
           xlim = c(as.POSIXct(viz_config$dates$start, tz = "UTC"), as.POSIXct(viz_config$dates$end, tz = "UTC")),
           ylim = c(0, 1)) # we assume "normalized" stage is between 0 and 1
      # put stage polygons on plot
      polygon(full_polygon$dateTime, full_polygon$stage_normalized, col = viz_config$gage_norm_col, border=NA)
      polygon(above_flood_polygon$dateTime, above_flood_polygon$stage_normalized, col = viz_config$gage_flood_col, border=NA)
      points(full_polygon$dateTime[-nrow(full_polygon)], full_polygon$stage_normalized[-nrow(full_polygon)],
             col = viz_config$gage_flood_col, type='l', lwd=2.5)
      par(op) # reset plot parameters

      y_pos <- y_pos + vertical_spacing # increment spacing for next plot
    }

    # Reset so next plot item is using full plot parameters
    par(orig_par)
  }

  return(plot_fun)
}
