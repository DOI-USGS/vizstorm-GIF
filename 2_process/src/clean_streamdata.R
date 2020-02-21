# this function contains hard-coded, ~manual removal of specific points that we don't believe
clean_streamdata <- function(ind_file, normalized_ind_file = '2_process/out/normalized_streamdata.rds.ind',
                             gage_temp_ind = '2_process/out/gage_air_temp.rds.ind') {

  normalized <- readRDS(sc_retrieve(normalized_ind_file))
  gage_air_temp <- readRDS(sc_retrieve(gage_temp_ind))
  assertthat::assert_that(attr(normalized$dateTime, 'tzone') == attr(gage_air_temp$dateTime, 'tzone'))
  cleaned <- normalized %>% left_join(gage_air_temp, by = c("site_no", "dateTime"))
  # cleaned <- mutate(
  #   normalized,
  #   stage_normalized = ifelse(
  #     (site_no == '02091500' & dateTime >= as.POSIXct('2018-09-15 08:00:00', tz='UTC')) |
  #       (site_no == '02092500' & dateTime >= as.POSIXct('2018-09-15 17:00:00', tz='UTC')),
  #     NA,
  #     stage_normalized)
  # )

  # save and push to Drive
  saveRDS(cleaned, as_data_file(ind_file))
  gd_put(ind_file)
}
