#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind, dates) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind))

  # ### filter sites_df here ###
  # set.seed(303)
  #
  # # Get sites in desired states
  # sites_state_specific <- dataRetrieval::readNWISsite(sites_df$site_no) %>%
  #   filter(state_cd %in% stateCdLookup(c("AL", "FL"), "id")) %>%
  #   pull(site_no)
  #
  # # # Filter to sites whose dv was at or above flood stage during this time period
  # # sites_above_flood <-
  # #   dataRetrieval::readNWISdv(
  # #     sites_state_specific,
  # #     startDate = as.Date(dates$start),
  # #     endDate = as.Date(dates$end),
  # #     parameterCd = "00065") %>%
  # #   renameNWISColumns() %>%
  # #   group_by(site_no) %>%
  # #   summarize(GH_max = max(GH, na.rm = TRUE)) %>%
  # #   ungroup() %>%
  # #   left_join(st_drop_geometry(sites_df)) %>%
  # #   filter(GH_max >= as.numeric(flood_stage)) %>%
  # #   pull(site_no)
  # #
  # # # Don't allow tidal gages
  # # tidal_gages <- c("02244040", "02244440", "02246459", "02246500")
  # #
  # # sites_info_to_choose_from <- sites_df %>%
  # #   filter(site_no %in% sites_above_flood) %>%
  # #   filter(!site_no %in% tidal_gages)
  # sites_info_to_choose_from <- sites_df %>%
  #     filter(site_no %in% sites_state_specific)
  #
  # # Randomly pick at most 6 gages
  # sites_info_subset <- sites_info_to_choose_from %>%
  #   sample_n(ifelse(nrow(sites_info_to_choose_from) >= 6, 6, nrow(sites_info_to_choose_from)))

  sites_info_subset <- filter(sites_df, site_no %in%
                                c('02378780','02378790','02362000','02374250', '02363000', '02370000',
                                  '02365470', '02359000', '02329600', '02327100', '02365200', '02363000', '02371500',
                                  '02351890'))

  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_info_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
