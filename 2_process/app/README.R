# Step 1: use 2_process/app/get_raw_data.R to create the files shiny_sites and
# shiny_flow.rds within 2_process/in. Source that file then edit the args below
# and run the fetch_sites_from_states() function.
source('2_process/app/get_raw_data.R')
fetch_sites_from_states(
  state_cds = c("FL","GA","AL","SC","NC","TN","VA","WV","MD","DC","DE","NJ","PA"),
  dates = list(start = "2018-10-09 12:00:00"),
  path_to_save = "2_process/in",
  pCodes = c("00065"))
# push the flow data to Drive
library(scipiper)
gd_put(as_ind_file('2_process/in/shiny_flow.rds'))

# Step 2: use the Shiny site-selection app in 2_process/app
library(shiny)
# you need these libraries installed (though only shiny actually needs to be loaded):
# library(shinydashboard)
# library(shinycssloaders)
# library(DT)
# library(leaflet)
shiny::runApp("2_process/app", launch.browser = TRUE)
# select the sites you want, Download the file, and copy the downloaded file
# from Downloads to 2_process/in/shiny_sites.rds

# Step 3: push the manually selected site info to Drive
library(scipiper)
gd_put(as_ind_file('2_process/in/shiny_sites.rds'))

# Step 4: From here, anybody should be able to pull and use shiny_sites.rds (and
# the picked_sites column) to apply in filter_sites_custom
sc_retrieve('2_process/in/shiny_sites.rds.ind')
sc_retrieve('2_process/in/shiny_flow.rds.ind')
# then go to step 5

# Step 5 = 1+2+3: Now anybody should be able to redo steps 2-3 (and update with
# step 1 if needed), git commit and PR, and share those results with everyone
# else
