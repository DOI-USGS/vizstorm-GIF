shiny::shinyServer(function(input, output,session) {

  observe({
    if (input$close > 0) shiny::stopApp()
  })

  # One way to keep track of values:
  siteDF <- reactiveValues(fileName = "Choose file",
                           stream_data = data.frame(),
                           lat_lon = data.frame(),
                           picked_sites = NULL,
                           clicked_map_site = NULL,
                           clicked_table_site = NULL,
                           site_that_flooded = NULL)

  observeEvent(input$site_data,{
    path <- file.path(input$site_data$datapath)

    if(all(tools::file_ext(input$site_data$name) == "rds")){

      x_1 <- readRDS(input$site_data$datapath[1])
      x_2 <- readRDS(input$site_data$datapath[2])

      if(ncol(x_1) > 8){
        siteDF[["stream_data"]] <- x_1
        siteDF[["lat_lon"]] <- x_2
      } else {
        siteDF[["stream_data"]] <- x_2
        siteDF[["lat_lon"]] <- x_1
      }
    }

    stream_data <- siteDF[["stream_data"]]
    site_data <- siteDF[["lat_lon"]]

    site_data$flood_stage <- as.numeric(site_data$flood_stage)
    stream_data <- left_join(stream_data, select(site_data, site_no, flood_stage), by="site_no")
    stream_data$flooded <- stream_data$X_00065_00000 > stream_data$flood_stage
    siteDF[["stream_data"]] <- stream_data
    siteDF[["site_that_flooded"]] <- unique(stream_data$site_no[stream_data$flooded])
    siteDF[["site_that_flooded"]] <- siteDF[["site_that_flooded"]][!is.na(siteDF[["site_that_flooded"]])]

    site_data$has_flooded <- site_data$site_no %in% siteDF[["site_that_flooded"]]
    siteDF[["lat_lon"]] <- site_data

    if("picked_sites" %in% names(siteDF[["lat_lon"]])){
      siteDF[["picked_sites"]] <- site_data$picked_sites
    } else {
      flooded_sites <- which(site_data$site_no %in% siteDF[["site_that_flooded"]])
      siteDF[["picked_sites"]] <- site_data$site_no[flooded_sites]
      siteDF[["lat_lon"]][["picked_sites"]] <- site_data$site_no %in% siteDF[["site_that_flooded"]]
    }

    mean_lat <- mean(site_data$dec_lat_va, na.rm = TRUE)
    mean_lon <- mean(site_data$dec_long_va, na.rm = TRUE)
    map <- leaflet::leafletProxy("mymap") %>%
      leaflet::setView(lng = mean_lon, lat = mean_lat, zoom=6)

  })

  output$mymap <- leaflet::renderLeaflet({
    isolate({
      map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::setView(lng = -82.3, lat = 34.25, zoom=6)
    })
  })

  observeEvent(input$sitesDT_rows_selected, {

    rows_DT <- input$sitesDT_rows_selected
    if(is.null(rows_DT)){
      return()
    }

    sites <- isolate(siteDF[["lat_lon"]][["site_no"]])

    siteDF[["clicked_table_site"]] <-  sites[rows_DT]

    new_picks <- unique(c(siteDF[["clicked_table_site"]],siteDF[["clicked_map_site"]]))
    siteDF[["picked_sites"]] <- new_picks
    siteDF[["lat_lon"]][["picked_sites"]] <- siteDF[["lat_lon"]][["site_no"]] %in% new_picks
  })

  observeEvent(input$mymap_marker_click, {
    clicked_site <- input$mymap_marker_click

    if(is.null(clicked_site)){
      return()
    }

    if(clicked_site$id %in% siteDF[["picked_sites"]]){
      siteDF[["picked_sites"]] <- siteDF[["picked_sites"]][!(siteDF[["picked_sites"]] %in% clicked_site$id)]
    } else {
      siteDF[["picked_sites"]] <- unique(c(siteDF[["picked_sites"]], clicked_site$id))
    }
    siteDF[["lat_lon"]][["picked_sites"]] <- siteDF[["lat_lon"]][["site_no"]] %in% siteDF[["picked_sites"]]
    proxy %>% selectRows(which(siteDF[["lat_lon"]][["picked_sites"]]))

  })

  plot_sparks <- reactive({

    validate(
      need(nrow(siteDF[["stream_data"]]) > 0, "Please select a data set")
    )
    # Next step....
    # Turn this facetted ggplot2 into:
    # https://leonawicz.github.io/HtmlWidgetExamples/ex_dt_sparkline.html
    # Then...Let that table *also* click on/off sites.

    x <- siteDF[["stream_data"]]

    sites_to_show <- siteDF[["picked_sites"]]

    x <- filter(x, site_no %in% sites_to_show)

    x <- left_join(x, select(siteDF[["lat_lon"]], station_nm, site_no), by="site_no")
    x$name_num <- paste(x$station_nm, x$site_no, sep = "\n")

    sparklines <- ggplot(data = x) +
      geom_line(aes(x=dateTime, y=X_00065_00000),size = 1) +
      geom_point(data = filter(x, flooded), aes(x=dateTime, y=X_00065_00000),size = 3, color = "blue") +
      facet_grid(name_num ~ ., scales = "free") +
      theme_minimal() +
      theme(axis.title =  element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            strip.text.y = element_text(angle = 0),
            panel.spacing = unit(0.0001, "lines"))

    return(sparklines)
  })

  output$sparks <- renderPlot({

    validate(
      need(nrow(siteDF[["stream_data"]]) > 0, "Please select a data set")
    )
    plot_sparks()

  })

  observe({

    mapData <- siteDF[["lat_lon"]]

    validate(
      need(nrow(mapData) > 0, "Please select a data set")
    )

    mapData <- mapData

    pal <- leaflet::colorNumeric(c("red", "blue"), c(0,1))

    popup_labels <- paste0("<div style='font-size:12px'><b>",mapData$station_nm,"</b><br/>",
                            mapData$site_no,"<br/>",
                            "<table>",
                            "<tr><td>Begin Date</td><td>",mapData$begin_date,'</td></tr>',
                            "<tr><td>End Date</td><td>",mapData$end_date,'</td></tr>',
                            "<tr><td>Drainage Area</td><td>",mapData$drain_area_va,'</td></tr>',
                            "<tr><td>Number of Samples: </td><td>",mapData$count_nu,'</td></tr>',
                            '</table></div>')

    mapData$labels <- lapply(popup_labels, function(x) {htmltools::HTML(x)})

    mapData$drain_area_va[is.na(mapData$drain_area_va)] <- min(mapData$drain_area_va, na.rm = TRUE)

    mapData$da_perc <- sapply(mapData$drain_area_va, function(x){
      ecdf(mapData$drain_area_va)(x)
    })
    # Need the points to be clickable:
    mapData$da_perc[mapData$da_perc < 0.25] <- 0.25
    mapData$count_perc <- sapply(mapData$count_nu, function(x){
      ecdf(mapData$count_nu)(x)
    })

    #Can't see the short PORs:
    mapData$count_perc <- 1.25*(0.25 + mapData$count_perc/2)

    map <- leaflet::leafletProxy("mymap", data=mapData) %>%
      leaflet::clearMarkers() %>%
      leaflet::addCircleMarkers(lat = ~dec_lat_va,
                                lng = ~dec_long_va, layerId = ~site_no,
                                fillColor = ~pal(picked_sites),
                                label = ~labels,
                                radius = ~da_perc*7,
                                labelOptions = leaflet::labelOptions(textOnly = TRUE,
                                                                     style=list(
                                                                       'background'='rgba(255,255,255,0.75)',
                                                                       'border-color' = 'rgba(0,0,0,1)',
                                                                       'border-radius' = '2px',
                                                                       'border-style' = 'solid',
                                                                       'border-width' = '2px')),
                                fillOpacity = ~count_perc+0.01,
                                opacity = 0.8,
                                stroke=FALSE)
  })

  proxy = dataTableProxy('sitesDT')

  output$sitesDT <- DT::renderDataTable({

    sites_dt <- siteDF[["lat_lon"]]

    validate(
      need(nrow(sites_dt) > 0, "Please select a data set")
    )

    sites_dt <- dplyr::select(sites_dt, site_no, station_nm, drain_area_va, has_flooded, picked_sites)
    picked_index <- which(sites_dt$picked_sites)
    DT::datatable(dplyr::select(sites_dt, -picked_sites),
                  rownames = FALSE,
                  selection = list(selected = picked_index))

  })

  output$downloadSites <- downloadHandler(

    filename = "shiny_sites.rds",

    content = function(file) {
      x <- siteDF[["lat_lon"]]
      # sites_to_show <- siteDF[["picked_sites"]]
      # x <- filter(x, site_no %in% sites_to_show)
      saveRDS(file = file, object = x)
    }
  )
})
