# normalize stream stage for sparklines

normalize_streamdata <- function(ind_file, raw_ind_file, sites_ind_file, timesteps_ind_file, stage_gap_threshold=3){

  streamdata <- readRDS(sc_retrieve(raw_ind_file, remake_file = getOption("scipiper.remake_file"))) # NWIS raw data
  gage_sites <- readRDS(sc_retrieve(sites_ind_file, remake_file = getOption("scipiper.remake_file")))
  timesteps <- readRDS(sc_retrieve(timesteps_ind_file, remake_file = getOption("scipiper.remake_file")))

  # remove duplicated columns
  if(any(duplicated(names(streamdata)))) {
    streamdata <- streamdata[,-which(duplicated(names(streamdata)))]
  }

  # make sure every site's timeseries has at least the standard timesteps
  complete_streamdata <- streamdata %>%
    expand(site_no, dateTime = timesteps) %>%
    full_join(streamdata, by=c('site_no','dateTime'))

  # add site geometry and flood stage info
  site_stage_data <- left_join(complete_streamdata, gage_sites, by='site_no')

  # rename, complete, mutate, and normalize the stage data. we assume
  # "normalized" stage is between 0 and 1 if that changes, see
  # 6_visualize/src/prep_spark_line_fun.R
  norm_stream <- site_stage_data %>%
    rename(stage = X_00065_00000, stage_cd = X_00065_00000_cd) %>%
    select(site_no, dateTime, stage, flood_stage, geometry) %>%
    group_by(site_no) %>%
    # normalize stage and flood_stage to min & max ignoring missing data
    mutate(stage_normalized = (stage - min(stage, na.rm = T)) / (max(stage, na.rm = T) - min(stage, na.rm = T))) %>%
    mutate(flood_stage_normalized = (as.numeric(flood_stage) - min(stage, na.rm = T)) / (max(stage, na.rm = T) - min(stage, na.rm = T))) %>%
    ungroup() %>%
    # make sure dateTimes are ascending
    arrange(site_no, dateTime)

  # turn the data back into an sf object
  norm_stream_sf <- sf::st_as_sf(norm_stream)

  # fill gaps by linear interpolation as requested
  for(site in unique(norm_stream_sf$site_no)) {
    site_rows <- which(norm_stream_sf$site_no == site)
    if(any(diff(site_rows) != 1)) stop("need each site's stage data to be contiguous")
    if(any(diff(site_rows) < 0)) stop("need each site's stage data to be sorted")
    storm_data_i <- norm_stream_sf[site_rows,]
    gap_runs <- rle(is.na(storm_data_i$stage_normalized)) %>% {
      data_frame(
        is_gap=.$values,
        end=cumsum(.$lengths),
        start=end - .$lengths + 1,
        before=ifelse(start-1==0, NA, start-1),
        after=ifelse(end+1>nrow(storm_data_i), NA, end+1),
        duration_hr=as.numeric(storm_data_i$dateTime[end] - storm_data_i$dateTime[start], units='hours'))
    }
    small_gaps <- filter(gap_runs, is_gap, duration_hr < stage_gap_threshold)
    if(nrow(small_gaps) > 0) {
      storm_data_i <- mutate(storm_data_i, dateTimeNumeric = as.numeric(storm_data_i$dateTime))
      for(i in seq_len(nrow(small_gaps))) {
        small_gap <- small_gaps[i,]
        gap_i <- small_gap$start : small_gap$end
        storm_data_i[gap_i, 'stage_normalized'] <- approx(
          x=storm_data_i$dateTimeNumeric,
          y=storm_data_i$stage_normalized,
          xout=storm_data_i$dateTimeNumeric[gap_i],
          rule=1)$y
      }
    }
    # put the gapfilled data back into norm_stream_sf
    norm_stream_sf[site_rows,'stage_normalized'] <- storm_data_i$stage_normalized
  }

  data_file <- as_data_file(ind_file)
  saveRDS(norm_stream_sf, data_file)
  gd_put(ind_file, data_file)
}
