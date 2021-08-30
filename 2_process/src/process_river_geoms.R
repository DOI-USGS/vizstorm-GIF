process_river_geoms <- function(ind_file,
                                major_river_geoms_ind,
                                waterbody_geoms_ind,
                                river_geom_config) {

  coastal_threshold_sqkm <- river_geom_config$coastal_threshold_sqkm
  inland_threshold_sqkm <- river_geom_config$inland_threshold_sqkm
  simplification_tolerance_m <- river_geom_config$simplification_tolerance_m

  sf_major_rivers <- readRDS(sc_retrieve(major_river_geoms_ind, remake_file = getOption("scipiper.remake_file")))
  #sf_gage_rivers <- readRDS(sc_retrieve(gage_river_geoms_ind, remake_file = getOption("scipiper.remake_file")))
  sf_waterbodies <- readRDS(sc_retrieve(waterbody_geoms_ind, remake_file = getOption("scipiper.remake_file")))

  outlets <- filter(sf_major_rivers,
                    (terminalfl == 1 &
                       divdasqkm > coastal_threshold_sqkm))$levelpathi

  inland <- filter(sf_major_rivers,
                   divdasqkm > inland_threshold_sqkm)$levelpathi

  sf_major_rivers <- filter(sf_major_rivers,
                            levelpathi %in% c(outlets, inland)) %>%
    group_by(levelpathi) %>%
    summarise() %>%
    st_cast("MULTILINESTRING") %>%
    ungroup() %>%
    st_zm() %>%
    st_line_merge() %>%
    st_cast("LINESTRING") %>%
    st_simplify(dTolerance = simplification_tolerance_m)

 # sf_gage_rivers <- sf_gage_rivers %>%
 #   group_by(site_id, up_down) %>%
 #   summarise() %>%
 #   st_cast("MULTILINESTRING") %>%
 #   ungroup() %>%
 #   st_line_merge() %>%
 #   st_cast("LINESTRING") %>%
 #   st_simplify(dTolerance = simplification_tolerance_m)

  sf_waterbodies <- st_simplify(sf_waterbodies,
                                dTolerance = simplification_tolerance_m)

  data_file <- as_data_file(ind_file)
  saveRDS(list(#sf_gage_rivers = sf_gage_rivers,
               sf_major_rivers = sf_major_rivers,
               sf_waterbodies = sf_waterbodies),
          data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}
