#' Group_Comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import DT
#' @import dplyr
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom purrr map_df
mod_Group_Comp_ui <- function(id){

  ns<-NS(id)


  tabPanel("Group Comparisions", fluidRow(column(width=5, offset=1, h5("Group One"),

                                                 selectizeInput(inputId = ns("G1_Race"),
                                                                label = "Ethnicity",
                                                                choices = Race_Levels,
                                                                selected='NONE',
                                                                multiple=TRUE),

                                                 sliderInput(inputId = ns("G1_Age"),
                                                             label = "Age",
                                                             min=18, max=100,
                                                             value = c(18,100), step = 1),

                                                 selectInput(inputId = ns("G1_Gender"),
                                                             label = "Gender",
                                                             choices = Gender_Levels,
                                                             selected = 'NONE'),
                                                 selectizeInput(inputId = ns("G1_Elections"),
                                                                label = "Included Elections",
                                                                choices = c("All","GENERAL_2022","GENERAL_2020","GENERAL_2018", "GENERAL_2016", "GENERAL_2014", "GENERAL_2012"), # ADD ELECTION OPTIONS
                                                                selected='All',
                                                                multiple=TRUE),
                                                 selectizeInput(inputId = ns("G1_No_Elections"),
                                                                label = "Excluded Elections",
                                                                choices = c("All","GENERAL_2022","GENERAL_2020","GENERAL_2018", "GENERAL_2016", "GENERAL_2014", "GENERAL_2012"), # ADD ELECTION OPTIONS
                                                                selected='All',
                                                                multiple=TRUE),

                                                 selectizeInput(inputId = ns("G1_Income_Range"),
                                                                label = "Income Range",
                                                                choices = Income_Levels, # ADD Income OPTIONS
                                                                selected='All',
                                                                multiple=TRUE),

                                                 actionButton(ns("Group_One_Trigger"), "Generate")),

                                          column(width=5, offset=1, h5("Group Two"),

                                                 selectizeInput(inputId = ns("G2_Race"),
                                                                label = "Ethnicity",
                                                                choices = Race_Levels,
                                                                selected='NONE',
                                                                multiple=TRUE),

                                                 sliderInput(inputId = ns("G2_Age"),
                                                             label = "Age",
                                                             min=18, max=100,
                                                             value = c(18,100), step = 1),

                                                 selectInput(inputId = ns("G2_Gender"),
                                                             label = "Gender",
                                                             choices = Gender_Levels,
                                                             selected = 'NONE'),

                                                 selectizeInput(inputId = ns("G2_Elections"),
                                                                label = "Included Elections",
                                                                choices = c("All","GENERAL_2022","GENERAL_2020","GENERAL_2018", "GENERAL_2016", "GENERAL_2014", "GENERAL_2012"), # ADD ELECTION OPTIONS
                                                                selected='All',
                                                                multiple=TRUE),
                                                 selectizeInput(inputId = ns("G2_No_Elections"),
                                                                label = "Excluded Elections",
                                                                choices = c("All","GENERAL_2022","GENERAL_2020","GENERAL_2018", "GENERAL_2016", "GENERAL_2014", "GENERAL_2012"), # ADD ELECTION OPTIONS
                                                                selected='All',
                                                                multiple=TRUE),

                                                 selectizeInput(inputId = ns("G2_Income_Range"),
                                                                label = "Income Range",
                                                                choices = Income_Levels, # ADD Income OPTIONS
                                                                selected='All',
                                                                multiple=TRUE))
  ),


  fluidRow(column(5, offset=1, h3('Most Common G-One'), DTOutput(ns('G1_MC'))),
           column(5, offset = 1, h3("Most Common G-Two"), DTOutput(ns('G2_MC')))),

  fluidRow(column(5, offset=1, h3('Partisanship'), DTOutput(ns('G1_Part'))),
           column(5, offset = 1, h3("Partisanship"), DTOutput(ns('G2_Part')))),

  fluidRow(column(5, offset=1, h2("Most Agreed") ,DTOutput(ns("Chi_MS"))),
           column(5, offset=1,h2("Least Agreed"), DTOutput(ns("Chi_LS"))))
  )
}

#' Group_Comp Server Functions
#'
#' @noRd
mod_Group_Comp_server <- function(id, Group_Report)
{
  moduleServer(id,

               function(input,output,session){


                 ######################
                 # MAKE REACTIVE VALUES
                 #######################

                 G1_Age<-reactive({input$G1_Age})
                 G2_Age<-reactive({input$G2_Age})

                 G1_Race<-reactive({input$G1_Race})
                 G2_Race<-reactive({input$G2_Race})

                 G1_Gender<-reactive({input$G1_Gender})
                 G2_Gender<-reactive({input$G2_Gender})

                 G1_Income_Range<-reactive({input$G1_Income_Range})
                 G2_Income_Range<-reactive({input$G2_Income_Range})

                 G1_Elections<-reactive({input$G1_Elections})
                 G2_Elections<-reactive({input$G2_Elections})

                 G1_No_Elections<-reactive({input$G1_No_Elections})
                 G2_No_Elections<-reactive({input$G2_No_Elections})

                 observeEvent(input$Group_One_Trigger,
                              {

                                ## Calls Pop_Comp which filters down the STATE data frame to just the voters we want.
                                pop_one<-Pop_Comp(STATE, G1_Race(), G1_Age(), G1_Gender(), G1_Income_Range(), G1_Elections(), G1_No_Elections())
                                pop_two<-Pop_Comp(STATE, G2_Race(), G2_Age(), G2_Gender(), G2_Income_Range(), G2_Elections(), G2_No_Elections())

                                #######################
                                # MODULE FUNCTION CALLS
                                ########################

                                # Gets the counts of all flags for group one and two and makes a data table out of them
                                G1_MC<-Make_DT_Basic(Count_Flags(pop_one, haystack_issues))
                                G2_MC<-Make_DT_Basic(Count_Flags(pop_two, haystack_issues))

                                # Gets the counts of all flags for group one and two and makes a data table out of them
                                G_TML<-Make_DT_Basic(TML_2(pop_one,pop_two))


                                ## Using the map_df function from the purr package
                                ## we calculate the Chi Squared values between the
                                ## Two populations for each issue

                                Chi_Dt<-map_df(haystack_issues, Chi_Sq, pop_one, pop_two) %>%
                                  arrange(Chi_SQ) %>%
                                  filter(!Issue %in% c("VAX_PRO_ANTI", "RIGHT_WING_MILITAS"))

                                #####################
                                # Making Data-Tables
                                #####################

                                Chi_MS<-Make_DT_Basic(head(Chi_Dt, n=10))
                                Chi_LS<-Make_DT_Basic(tail(Chi_Dt, n=10) %>% arrange(desc(Chi_SQ)))

                                G1_Part<-Make_DT_Basic(Part_Overall(pop_one))
                                G2_Part<-Make_DT_Basic(Part_Overall(pop_two))

                                #######################
                                # OUTPUTTING TO THE UI
                                #######################

                                output$G1_MC<-renderDT(G1_MC)
                                output$G2_MC<-renderDT(G2_MC)
                                output$G_TML<-renderDT(G_TML)
                                output$G1_Part<-renderDT(G1_Part)
                                output$G2_Part<-renderDT(G2_Part)
                                output$Chi_MS<-renderDT(Chi_MS)
                                output$Chi_LS<-renderDT(Chi_LS)


                                ############################
                                # SAVING TO THE REPORT LIST
                                ############################
                                Group_Report$G2_MC<-G2_MC
                                Group_Report$G1_MC<-G1_MC
                                Group_Report$G1_Part<-G1_Part
                                Group_Report$G2_Part<-G2_Part
                                Group_Report$Chi_MS<-Chi_MS
                                Group_Report$Chi_LS<-Chi_LS

                                Group_Report$G1_Age<-isolate(input$G1_Age)
                                Group_Report$G2_Age<-isolate(input$G2_Age)

                                Group_Report$G1_Race<-isolate(input$G1_Race)
                                Group_Report$G2_Race<-isolate(input$G2_Race)

                                Group_Report$G1_Gender<-isolate(input$G1_Gender)
                                Group_Report$G2_Gender<-isolate(input$G2_Gender)

                                Group_Report$G1_Income_Range<-isolate(input$G1_Income_Range)
                                Group_Report$G2_Income_Range<-isolate(input$G2_Income_Range)

                                Group_Report$G1_Elections<-isolate(input$G1_Elections)
                                Group_Report$G2_Elections<-isolate(input$G2_Elections)

                                Group_Report$G1_No_Elections<-isolate(input$G1_No_Elections)
                                Group_Report$G2_No_Elections<-isolate(input$G2_No_Elections)

                              }) # observeEvent
               } # function
  ) # moduleServer
} # Group_server



## To be copied in the UI
# mod_Group_Comp_ui("Group_Comp_1")

## To be copied in the server
# mod_Group_Comp_server("Group_Comp_1")
