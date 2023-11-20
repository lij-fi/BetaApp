#' Area UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import DT
#' @import ggsankey
#' @import ggplot2
#' @import RColorBrewer
#' @importFrom tidyr replace_na
mod_Area_ui <- function(id){
  ns<-NS(id)
  tabPanel("Area Analysis", sidebarPanel(selectInput(inputId = ns("Area"),
                                                     label = "Select Area",
                                                     choices = c('ZIP', 'COUNTY'), selected ='ZIP'),

                                         selectizeInput(inputId = ns("Area_Select"),
                                                        label = "Get Demographics for Specific Areas",
                                                        choices = c(c('A','B')), selected='NONE',
                                                        multiple=TRUE),
                                         actionButton(ns("Button"), "Generate"),

                                         "There are:", textOutput(ns('total')), "Voters", inline=TRUE),

           mainPanel(plotOutput(ns('sankey')),plotOutput(ns("barstack")), DTOutput(ns("byparty")))

  )

}

#' Area Server Functions
#'
#' @noRd
mod_Area_server <- function(id,r,Area_Report)
{
  moduleServer(id,
               function(input,output,session)
               {
                 ######################
                 # MAKING REACTIVE VALUES
                 #######################s
                 Area<-reactive({input$Area})
                 Area_Select<-reactive({input$Area_Select})


                 ######################
                 # REACTIVE UI UPDATES
                 ######################

                 observe({updateSelectInput(session, "Area_Select", label = paste("Select one or more Areas"),
                                            choices = arrange(unique(STATE_SAMPLE %>% select(.data[[Area()]])), .data[[Area()]]))
                 })



                 observeEvent(input$Button,
                              {
                                #######################
                                # FILTERING POPULATION
                                #######################

                                population<-STATE %>% filter(.data[[Area()]] %in% Area_Select())

                                #########################
                                ## MODULE FUNCTION CALLS
                                #########################

                                sankey<-Make_Sankey(population)
                                Stats_Saver<-Basic_Stats(population)
                                Pie_Saver<-Demographic_Pie(population)
                                Stack_Saver<-Bar_Stack(population)

                                ######################
                                # Making Data-Tables
                                #######################

                                ## Turns the data frame of the population
                                ## broken down by party into a DataTable
                                byparty<-Make_DT_Basic(as.data.frame(Stats_Saver[2]))

                                #########################
                                # OUTPUTTING TO THE UI
                                #########################

                                output$sankey<-renderPlot(sankey)
                                output$barstack<-renderPlot(Stack_Saver)
                                output$total<-renderText(as.character(Stats_Saver[1]))
                                output$byparty<-renderDT(byparty)

                                ##############################
                                # SAVING TO THE REPORT LIST
                                ##############################

                                Area_Report$sankey<-sankey
                                Area_Report$barstack<-Stack_Saver
                                Area_Report$total<-Stats_Saver[1]
                                Area_Report$byparty<-byparty
                                Area_Report$Areas<-isolate(Area_Select())

                              }) # observeEvent
               } # function
  ) # moduleServer
} # Area_server

## To be copied in the UI
# mod_Area_ui("Area_1")

## To be copied in the server
# mod_Area_server("Area_1")
