#' Geo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import DT
#' @import leaflet
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_wider
#' @importFrom htmltools HTML
#' @importClassesFrom tibble tbl_df
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na
mod_Geo_ui <- function(id){

  ns<-NS(id)

  tabPanel("Geographic Data",
           tabsetPanel(
             tabPanel("Map", div(class="outer",
                                 tags$link(rel="stylesheet", type="test/css", href="www/styles.css"), # Includes Custom CSS

                                 leafletOutput(ns("map"), width="100%", height="90%"),

                                 absolutePanel(id = ns("controls"), class = "panel panel-default", fixed = TRUE,
                                               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                               width = 330, height = "auto",

                                               actionButton(ns("Geo_Button"), "Get Zip Data"),

                                               selectInput(inputId = ns("Area"),
                                                           label = "Select Area Designation",
                                                           choices = c('ZIP', 'COUNTY'), selected ='COUNTY'),

                                               sliderInput(inputId = ns("Scat_Slide"), label = "Select Minimum Population Voters",
                                                           min= 0, max= 10000, value = 0 , step = 50),

                                               selectizeInput(inputId = ns("Zips"),
                                                              label = "Get Demographics for Specific Areas",
                                                              choices = c(c('A','B')), selected='NONE',
                                                              multiple=TRUE),

                                               plotOutput(ns("demo_geo"))

                                 ), #absolutePanel "controls"
             ), # div "class=outer"
             ), #  tabPanel "Map"
             tabPanel("Table",DTOutput(ns('table4')))
           ) # tabsetPanel
  ) # tabPanel
} # Geo_UI

#' Geo Server Functions
#'
#' @noRd
mod_Geo_server<-function(id, r, Geo_Report)
{
  moduleServer(id,
               function(input, output, session)
               {

                 Min_Pop<-reactive({input$Scat_Slide})
                 Area<-reactive({input$Area})
                 Zips<-reactive({input$Zips})


                 observe(
                   updateSelectInput(session, "Zips",
                                     label = paste("Get Breakdown for Specific Area"),
                                     choices = arrange(unique(STATE_SAMPLE %>% select(.data[[Area()]])), .data[[Area()]]))
                 )



                 observeEvent(input$Geo_Button,
                              {

                                ########################
                                # MODULE FUNCTION CALLS
                                ########################
                                Zip_Tab<-Zip_Codes(isolate(r$pop), isolate(r$issueA), isolate(r$issueB),
                                                   isolate(r$Level_1) , isolate(r$Level_2), 1, Area())

                                Mappy<-Generate_Map(fl_counties_2, Zip_Tab, isolate(r$pop))
                                GeoPlot<-Demo(isolate(r$pop), Zips(),Area())

                                ###############
                                # Updating UI
                                ###############
                                updateSliderInput(session, "Scat_Slide", min = min(Zip_Tab[2]), max=max(Zip_Tab[2]))


                                ######################
                                # Making Data-Tables
                                #######################
                                Zip_Tab_Filter<- Zip_Tab %>% filter(Total_Voters > Min_Pop())
                                Formatted_GEO<-Format_Geo(Zip_Tab)


                                #########################
                                # OUTPUTTING TO THE UI
                                #########################

                                output$table4<-renderDT(Formatted_GEO)
                                output$demo_geo<-renderPlot(GeoPlot)
                                output$map<-renderLeaflet(Mappy)


                                ##############################
                                # SAVING TO THE REPORT LIST
                                ##############################
                                Geo_Report$DT<-Formatted_GEO
                                Geo_Report$PIE<-GeoPlot
                                Geo_Report$Mappy<-Mappy


                              }) # observeEvent
               } # function
  ) # moduleServer
} # Geo_server

## To be copied in the UI
# mod_Geo_ui("Geo_1")

## To be copied in the server
# mod_Geo_server("Geo_1")
