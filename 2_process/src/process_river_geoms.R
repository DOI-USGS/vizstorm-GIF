process_river_geoms <- function(ind_file,
                                view_polygon,
                                river_geoms_ind,
                                coastal_threshold_sqkm,
                                inland_threshold_sqkm,
                                simplification_tolerance_m) {

  sf_rivers <- readRDS(sc_retrieve(river_geoms_ind))

  sf_major_rivers <- sf_rivers$sf_major_rivers

  outlets <- filter(sf_major_rivers,
                    (terminalfl == 1 &
                       totdasqkm > coastal_threshold_sqkm))$levelpathi

  inland <- filter(sf_major_rivers,
                   totdasqkm > inland_threshold_sqkm)$levelpathi

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

  sf_gage_rivers <- sf_rivers$sf_gage_rivers %>%
    group_by(site_id, up_down) %>%
    summarise() %>%
    st_cast("MULTILINESTRING") %>%
    ungroup() %>%
    st_line_merge() %>%
    st_cast("LINESTRING") %>%
    st_simplify(dTolerance = simplification_tolerance_m)

  data_file <- as_data_file(ind_file)
  saveRDS(list(sf_gage_rivers = sf_gage_rivers,
               sf_major_rivers = sf_major_rivers),
          data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}
