#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind, remake_file = getOption("scipiper.remake_file")))

  ### filter sites_df here ###

  site_list <- c('02030103', '01311500', '01376800','01310500','01350355','01371500','01350180','01421000','01434000','01303000', # NY
                 '01409280', '01398500', '01474500','01392500','01445000','01393450','01386000','01350355','01364500','01327750','01304000','01381900', '01401750', '01383500',# NJ
                 '01117350', '01117000','01111300','01109403', #RI
                 '01192883' ,'01195100','01199000', '01209700', '01199050', '01188090', '01191000', '01184100',#CT
                 '01152500', '01334000', #VT
                 '01092000 ', '01085000','01332500','01161000', #NH
                 '01105600' ,'01197500', '01163200','01109070','01105730','01198000','01169000','01094500', '01105000','01096000', '01183500','01401750','01184100',  #MA
                 '01536000', '01452000','01576000','01428500','01442500','01427510', #PA
                 '01483700', '01429500'  #DE
                 )
  sites_info_subset <- sites_df %>%
    filter(site_no %in% site_list) %>%
    filter(!(site_no %in% c('01199000','01474500','01327750','01398500','01109070','01085000','01105000','01434000','01096000','01197500','01364500','01334000','01536000','1332500','01428500','01169000','01188090','01427510','01188090')))




  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_info_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
