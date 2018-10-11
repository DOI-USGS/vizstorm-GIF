library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

header <- dashboardHeader(title = "Pick sites",
                          tags$li(class = "dropdown",
                                  div(style="text-align:center;
                                      font-size: 20px;
                                      height: 50px;
                                      font-weight: 300;
                                      margin-right:25px;
                                      font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif;
                                      line-height: 50px;
                                      color: #fff;")),
                          tags$li(class = "dropdown", tags$button(
                            id = 'close',
                            type = "button",
                            class = "btn action-button",
                            style='color: #000000;
                            margin-right:13px;margin-top:7px;margin-bottom:7px',
                            onclick = "setTimeout(function(){window.close();},500);",  # close browser
                            "Stop App"
                          )))

body <- dashboardBody(
  fluidRow(
    column(6, fileInput("site_data", "Load 2_process/in/shiny_flow.rds and 2_process/in/shiny_sites.rds", multiple = TRUE)),
    column(6, downloadButton('downloadSites', 'Download RDS'))
  ),

  fluidRow(
    column(6,
           h4("Size relates to drainage area"),
           h4("Opacity relates to period of record"),
           shinycssloaders::withSpinner(leaflet::leafletOutput("mymap",height = "700px"))),
    column(6,
           tabBox(width = 12, id="mainOut",
                  tabPanel(title = tagList(title = "Table", shiny::icon("bars")), value = "table",
                           shinycssloaders::withSpinner(DT::dataTableOutput('sitesDT'))),
                  tabPanel(title = tagList(title = "Sparklines"), value = "sparks",
                           plotOutput("sparks",height = 1000,width = 600))
           ))
  )
)

dashboardPage(header = header,
              body = body,
              sidebar =  dashboardSidebar(collapsed = TRUE))
