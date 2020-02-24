#just testing for now

library(jsonlite)
library(dplyr)
base_url <- "https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-%s/local"
sites <- readRDS('2_process/out/gage_sites_custom.rds')[['site_no']]
all_sites_data <- tibble()
for(site in sites) {
  nldi_url <- sprintf(base_url, site)
  this_site_data <- fromJSON(nldi_url)[['characteristics']] %>%
    filter(grepl(x = characteristic_id, pattern = "NLCD")) %>%
    mutate(site = site)
  all_sites_data <- bind_rows(all_sites_data, this_site_data)
}
developed_total <- all_sites_data %>%
  mutate(characteristic_value = as.numeric(characteristic_value)) %>%
  filter(grepl(pattern = "NLCD11_2", x= characteristic_id)) %>%
  group_by(site) %>% summarise(sum_val = sum(characteristic_value))

