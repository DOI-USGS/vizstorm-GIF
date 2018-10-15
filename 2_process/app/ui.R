library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(sparkline)

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
    column(6,
           plotOutput("insta_flow",height = 100),
           shinycssloaders::withSpinner(leaflet::leafletOutput("mymap",height = "400px")),
           h5("Size relates to drainage area"),
           h5("Opacity relates to period of record")),
    column(6,
           tabBox(width = 12, id="mainOut",
                  tabPanel(title = tagList(title = "Sparklines ggplot"),
                           plotOutput("sparks",height = 500,width = 500)),
                  tabPanel(title = tagList(title = "Sparklines Table"),
                           shinycssloaders::withSpinner(DT::dataTableOutput('sparkTable')))
           )
    )
  )

)

side <- dashboardSidebar(
  fileInput("site_data", "Load all_flow and all_sites (RDS)",multiple = TRUE),
  downloadButton('downloadSites', 'Download RDS')

)

dashboardPage(header = header,
              body = body,
              sidebar =  side)
