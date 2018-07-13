# normalize stream stage for sparklines

normalize_streamdata <- function(ind_file, raw_ind_file, sites_ind_file){

  streamdata <- readRDS(sc_retrieve(raw_ind_file)) # NWIS raw data
  storm_sites <- readRDS(sc_retrieve(sites_ind_file))
  storm_data <- left_join(streamdata, storm_sites)

  # we assume "normalized" stage is between 0 and 1
  # if that changes, see 6_visualize/src/prep_spark_line_fun.R
  norm_stream <- storm_data %>%
    rename(stage = X_00065_00000, stage_cd = X_00065_00000_cd) %>%
    select(site_no, dateTime, stage, flood_stage, geometry) %>%
    group_by(site_no) %>%
    # normalizing stage and flood_stage to min & max ignoring missing data
    mutate(stage_normalized = (stage - min(stage, na.rm = T)) / (max(stage, na.rm = T) - min(stage, na.rm = T))) %>%
    mutate(flood_stage_normalized = (as.numeric(flood_stage) - min(stage, na.rm = T)) / (max(stage, na.rm = T) - min(stage, na.rm = T))) %>%
    ungroup()

  data_file <- as_data_file(ind_file)
  saveRDS(norm_stream, data_file)
  gd_put(ind_file, data_file)
}
