#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind, remake_file = getOption("scipiper.remake_file")))

  ### filter sites_df here ###
  set.seed(100)
  sites_info_subset <- sites_df %>%
    filter(site_no %in%
             c(
               '07374581', '02491500', '07381324', '301104089253400', '07380200', '07375280', '07377000',  '07375500', '07369500', # LA
               '07374525', # Belle Chasse, LA gage that reversed flow direction
               '02492360', '07291000',  '02481660',"02481880","02439400",# MS
               '03574500', '02466030',# AL
               '02377570',
               '07026040', '03603000','03431599',# TN
               # 'LACAM04361', 'LATAN23291', 'LASTB00004', 'LASTT23298', 'LAJEF06563', 'LALAF27100', 'LAJEF27112', 'LAJEF27136', 'LASTM27175', 'LASTT27187', # RDGs,
              # '01613525', # MD
              # '03053500','03200500', #WV
              # '01451500', '01556000', '01573600', '01536000', '01474500',# PA
              # '01403900', '01379500 ', # NJ
               #'01302020', # NY
               #'01110500','01183500', # MA
              # '01188090', '01127000' # CT

             ))


  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_info_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
