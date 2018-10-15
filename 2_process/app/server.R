
shiny::shinyServer(function(input, output,session) {

  observe({
    if (input$close > 0) shiny::stopApp()
  })

  # One way to keep track of values:
  siteDF <- reactiveValues(fileName = "Choose file",
                           stream_data = data.frame(),
                           lat_lon = data.frame(),
                           insta_flow = data.frame(),
                           clickableTable = data.frame(),
                           picked_sites = NULL,
                           clicked_map_site = NULL,
                           site_that_flooded = NULL)

  observeEvent(input$site_data,{
    path <- file.path(input$site_data$datapath)

    if(all(tools::file_ext(input$site_data$name) == "rds")){

      x_1 <- readRDS(input$site_data$datapath[1])
      x_2 <- readRDS(input$site_data$datapath[2])

      if(isTRUE(all(c("station_nm","dec_lat_va","dec_long_va") %in% names(x_1)))){
        siteDF[["stream_data"]] <- x_2
        siteDF[["lat_lon"]] <- x_1
      } else {
        siteDF[["stream_data"]] <- x_1
        siteDF[["lat_lon"]] <- x_2
      }
    }

    stream_data <- siteDF[["stream_data"]]
    site_data <- siteDF[["lat_lon"]]

    site_data$flood_stage <- as.numeric(site_data$flood_stage)
    stream_data <- left_join(stream_data, select(site_data, site_no, flood_stage), by="site_no")
    stream_data$flooded <- stream_data$X_00065_00000 > stream_data$flood_stage
    siteDF[["stream_data"]] <- stream_data %>%
      select(site_no, dateTime, X_00065_00000, flooded)

    siteDF[["site_that_flooded"]] <- unique(stream_data$site_no[stream_data$flooded])
    siteDF[["site_that_flooded"]] <- siteDF[["site_that_flooded"]][!is.na(siteDF[["site_that_flooded"]])]

    site_data$has_flooded <- site_data$site_no %in% siteDF[["site_that_flooded"]]
    siteDF[["lat_lon"]] <- site_data

    if("picked_sites" %in% names(siteDF[["lat_lon"]])){
      siteDF[["picked_sites"]] <- site_data$site_no[site_data$picked_sites]
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

  })

  observeEvent(input$mymap_marker_mouseover,{

    hovered_site <- input$mymap_marker_mouseover
    if(is.null(hovered_site)){
      return()
    }

    stream_data <- isolate(siteDF[["stream_data"]]) %>%
      filter(site_no == hovered_site$id)

    siteDF[["insta_flow"]] <- stream_data

  })

  observeEvent(input$mymap_marker_mouseout,{

    empty <- data.frame(site_no = NA,
                        dateTime = NA,
                        X_00065_00000 = NA,
                        flooded = NA,
                        station_nm = NA)

    siteDF[["insta_flow"]] <- na.omit(empty)

  })

  plot_sparks <- reactive({

    stream_data <- siteDF[["stream_data"]]

    validate(
      need(nrow(stream_data) > 0, "Please select a data set")
    )

    sites_to_show <- siteDF[["picked_sites"]]

    stream_data <- filter(stream_data, site_no %in% sites_to_show)

    stream_data <- left_join(stream_data, select(siteDF[["lat_lon"]], station_nm, site_no, dec_lat_va), by="site_no")
    stream_data$name_num <- paste(stream_data$station_nm, stream_data$site_no, sep = "\n")

    ordered_names <- unique(stream_data$name_num[order(stream_data$dec_lat_va, decreasing = TRUE)])

    stream_data$name_num <- factor(stream_data$name_num, levels = ordered_names)

    sparklines <- ggplot(data = stream_data) +
      geom_line(aes(x=dateTime, y=X_00065_00000)) +
      geom_point(data = filter(stream_data, flooded), aes(x=dateTime, y=X_00065_00000), color = "blue") +
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

  output$insta_flow <- renderPlot({
    flow <- siteDF[["insta_flow"]]
    validate(
      need(nrow(flow) > 0, "Hover over a site")
    )

    station_nm <- select(isolate(siteDF[["lat_lon"]]), station_nm, site_no) %>%
      filter(site_no == flow$site_no[1]) %>%
      pull(station_nm)

    ggplot(data = flow) +
      geom_line(aes(x=dateTime, y=X_00065_00000)) +
      geom_point(data = filter(flow, flooded),
                 aes(x=dateTime, y=X_00065_00000), color = "blue") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank()
      ) +
      ggtitle(station_nm)

  })

  proxy_sparky = dataTableProxy('sparkTable')

  output$sparkTable <- DT::renderDataTable({

    # https://leonawicz.github.io/HtmlWidgetExamples/ex_dt_sparkline.html

    stream_data <- siteDF[["stream_data"]]

    validate(
      need(nrow(stream_data) > 0, "Please select a data set")
    )

    sites_to_show <- siteDF[["picked_sites"]]
    site_df <- siteDF[["lat_lon"]]

    stream_data <- stream_data %>%
      filter(site_no %in% sites_to_show) %>%
      left_join(select(site_df, station_nm, site_no, dec_lat_va), by="site_no")

    max_stream <- stream_data %>%
      group_by(site_no) %>%
      summarize(max_h = max(X_00065_00000, na.rm = TRUE),
                min_h = min(X_00065_00000, na.rm = TRUE)) %>%
      ungroup()

    stream_data_norm <-  stream_data %>%
      left_join(max_stream, by="site_no") %>%
      mutate(normalized_height = (X_00065_00000 - min_h)/(max_h - min_h))

    js <- "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"

    x <- "function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { "
    line_string <- "type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange', height: '15px', width: '250px'"
    cb_line <- JS(paste0(x, line_string, ", chartRangeMin: ", 0, ", chartRangeMax: ",
                         1, " }); }"), collapse = "")
    # targets is the column+1 to make the sparkline:
    colDefs1 <- list(list(targets = c(1),
                          render = JS(js)))

    dat_t <- stream_data_norm %>%
      group_by(station_nm, dec_lat_va, site_no) %>%
      summarise(norm_gage = paste(normalized_height, collapse = ",")) %>%
      ungroup() %>%
      arrange(-dec_lat_va) %>%
      select(-dec_lat_va) %>%
      data.frame()

    rownames(dat_t) <- dat_t$site_no
    dat_t <- select(dat_t, -site_no)

    siteDF[["clickableTable"]] <- dat_t

    d1 <- DT::datatable(dat_t, rownames = FALSE,
                        options = list(pageLength = nrow(dat_t),
                                       columnDefs = colDefs1,
                                       fnDrawCallback = cb_line,
                                       dom = 't'))
    d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency("sparkline"))
    d1

  })

  observeEvent(input$sparkTable_rows_selected, {

    rows_DT <- input$sparkTable_rows_selected
    if(is.null(rows_DT)){
      return()
    }

    sites_to_show <- siteDF[["picked_sites"]]
    dat_t <- siteDF[["clickableTable"]]

    site_to_remove <- row.names(dat_t)[rows_DT]
    siteDF[["picked_sites"]] <- sites_to_show[!(sites_to_show %in% site_to_remove)]
    siteDF[["lat_lon"]][["picked_sites"]] <- siteDF[["lat_lon"]][["site_no"]] %in% siteDF[["picked_sites"]]
  })

  observe({

    mapData <- siteDF[["lat_lon"]]

    validate(
      need(nrow(mapData) > 0, "Please select a data set")
    )

    mapData <- mapData

    pal <- leaflet::colorNumeric(c("red", "blue"), c(0,1))

    popup_labels <- paste0("<div style='font-size:12px'>",
                           "<b>",mapData$station_nm,"</b><br/>",
                           mapData$site_no,
                           '</div>')

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

  output$downloadSites <- downloadHandler(

    filename = "all_sites.rds",

    content = function(file) {
      x <- siteDF[["lat_lon"]]
      saveRDS(file = file, object = x)
    }
  )
})
