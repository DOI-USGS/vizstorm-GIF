# normalize stream stage for sparklines

normalize_streamdata <- function(ind_file, raw_ind_file, gd_config){

  norm_stream <- readRDS(sc_retrieve(raw_ind_file)) %>% # NWIS raw data
    rename(stage = X_00065_00000, stage_cd = X_00065_00000_cd) %>%
    select(site_no, dateTime, stage, stage_cd) %>%
    group_by(site_no) %>%
    mutate(stage_normalized = (stage - min(stage, na.rm = T)) / (max(stage, na.rm = T) - min(stage, na.rm = T))) %>% # normalizing to min & max
    ungroup()

  data_file <- as_data_file(ind_file)
  saveRDS(norm_stream, data_file)
  gd_put(ind_file, data_file)
}
