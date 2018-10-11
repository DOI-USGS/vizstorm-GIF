# Step 1: someone needs to go into the Shiny site-selection app at
# https://github.com/USGS-VIZLAB/viz-scratch/tree/master/vizstorm_sites, create
# the files all_sites and all_flow.rds, rename them to shiny_sites and
# shiny_flow, and move them into 2_process/in.

# Step 2: the following manual posting step needs to be completed by that first
# someone.
library(scipiper)
gd_put(as_ind_file('2_process/in/shiny_sites.rds'))
gd_put(as_ind_file('2_process/in/shiny_flow.rds'))

# Step 3: From here, anybody should be able to pull and use all_sites.rds (and
# the picked_sites column)  to apply in filter_sites_custom

# Step 4 = 1+2: And anybody should be able to redo steps 1 and 2, git commit and
# PR, and share those results with everyone else
