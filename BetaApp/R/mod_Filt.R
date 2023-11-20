#' Filt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom tidyr pivot_wider
#' @import DT
#' @import dplyr
mod_Filt_ui <- function(id){
  ns<-NS(id)

  tabPanel("Filters and Report Handler",

           # Sidebar panel for inputs ----

           fluidRow(column(11, offset=1, h1("Filters and Flags"))),

           fluidRow(
             column(5, offset=1,


                    h4("Demographic Filters"),


                    selectizeInput(inputId = ns("Race"),
                                   label = "Ethnicity Filter",
                                   choices = Race_Levels,
                                   selected='NONE',
                                   multiple=TRUE),

                    sliderInput(inputId = ns("Age"),
                                label = "Age Filter",
                                min=18, max=100,
                                value = c(18,100), step = 1),

                    selectInput(inputId = ns("Gender"),
                                label = "Gender Filter",
                                choices = Gender_Levels,
                                selected = 'NONE'),

                    actionButton(ns("Run"), "Apply Filters")),

             column(5, offset=1,
                    h4("Flag Selection"),
                    selectInput(inputId = ns("flag"),
                                label = "Choose a Issue:",
                                choices = haystack_issues,
                                selected ="BIDEN_APPROVAL"),

                    selectInput(inputId = ns("Level_1"),
                                label="Choose a Flag",
                                choices = c('Approve','Disapprove', "Unknown"),
                                selected="Approve"),

                    selectInput(inputId = ns("flag2"),
                                label = "Choose another Issue:",
                                selected="DESANTIS_APPROVAL",
                                choices = haystack_issues),

                    selectInput(inputId = ns("Level_2"),
                                label="Choose a Flag",
                                choices = c('Approve','Disapprove', "Unknown"),
                                selected="Disapprove")),
           ), # fluidRow

           fluidRow(column(11, offset=1, h2("Report Options"))),
           fluidRow(
             column(5, offset = 1, h4("Which Panels to Output"),

                    checkboxInput(inputId = ns("ST_Eval"), "Spread Tables", value = TRUE),
                    checkboxInput(inputId = ns("Analy_Eval"), "Analytics", value = TRUE),
                    checkboxInput(inputId = ns("Geo_Eval"), "Geographic", value = TRUE),
                    checkboxInput(inputId = ns("Area_Eval"), "Area Analysis", value = TRUE),
                    checkboxInput(inputId = ns("Group_Eval"), "Group Comp", value = TRUE),

                    downloadButton(ns("report"), "Generate report"),
                    textInput(ns("FN"), label="Report Name",value = "Enter Report Name", width = '400px')
             )
           )
  ) # Tab Panel
} #Filt Ui Function

#' Filt Server Functions
#'
#' @noRd
mod_Filt_server<-function(id, r, Filt_Report, ST_Report, Area_Report, Geo_Report, Analy_Report, Group_Report)
{
  moduleServer(id,
               function(input,output,session){

                 ## Creates the "global database"
                 ## of user inputs that are passed to
                 ## the ST, Analy and Geo Modules.
                 observe(
                   {
                     r$Gender<-input$Gender
                     r$Race<-input$Race
                     r$Age<-input$Age
                     r$issueB<-input$flag2
                     r$issueA<-input$flag
                     r$Level_1<-input$Level_1
                     r$Level_2<-input$Level_2
                     r$Run<-input$Run
                   })

                 ## Accesses the input issues for the updating of
                 ## The flag options
                 issueB2<-reactive({input$flag2})
                 issueA2<-reactive({input$flag})

                 ## Updates which options to display
                 ## Depending on which issue has been selected
                 observe({
                   updateSelectInput(session, "Level_2",
                                     label = paste("Select Flag"),
                                     choices = levels(STATE_SAMPLE %>% pull(.data[[issueB2()]])))

                   updateSelectInput(session, "Level_1",
                                     label = paste("Select Flag"),
                                     choices = levels(STATE_SAMPLE %>% pull(.data[[issueA2()]])))
                 })


                 ## Runs the Filters on the STATE file
                 ## and outputs it to the whole application
                 observeEvent(input$Run,
                              {
                                AT<-(!identical(isolate(r$Age),c(18,100)))
                                RT<-(!identical(isolate(r$Race),c("NONE")))
                                GT<-(isolate(r$Gender)!="NONE")

                                if( AT || GT || RT)
                                { r$pop<-Pop_Cut(STATE, isolate(r$Race), isolate(r$Age), isolate(r$Gender), AT, RT, GT)}

                                else
                                {r$pop<-sample_n(STATE, size = 100000)}

                              })



                 ## Saves the Filters, Flags and Levels
                 ## so they can be outputted to the report
                 observeEvent(input$Run,
                              {
                                Filt_Report$Gender<-input$Gender
                                Filt_Report$Race<-input$Race
                                Filt_Report$Age<-input$Age
                                Filt_Report$issueB<-input$flag2
                                Filt_Report$issueA<-input$flag
                                Filt_Report$Level_1<-input$Level_1
                                Filt_Report$Level_2<-input$Level_2
                              })

                 ## Updates which pages are to be
                 ## outputted to the report
                 observe({
                   Filt_Report$Geo_Eval<-input$Geo_Eval
                   Filt_Report$Group_Eval<-input$Group_Eval
                   Filt_Report$Analy_Eval<-input$Analy_Eval
                   Filt_Report$ST_Eval<-input$ST_Eval
                   Filt_Report$Area_Eval<-input$Area_EVal
                 })


                 ## Generates the Rmarkdown report. Don't ask
                 ## Me how it works because I don't exacly Know

                 output$report<-downloadHandler(filename = function(){
                   repname <- "filtered_report.html"
                   if (isTruthy(input$FN))
                   {
                     if (input$FN != "Enter Report Name")
                     {
                       repname <- paste(input$FN, Sys.Date(),".html", sep="-")
                     }
                   }
                   repname
                 },

                 content = function(file){

                   #tempReport <- file.path(tempdir(), "GenRep.Rmd")
                   #file.copy("GenRep.Rmd", tempReport, overwrite = TRUE)
                   # Set up parameters to pass to Rmd document
                   params<-list(Filt=Filt_Report, ST=ST_Report, Area=Area_Report,
                                Geo=Geo_Report, Analy=Analy_Report, Group=Group_Report)

                   # Knit the document, passing in the `params` list, and eval it in a
                   # child of the global environment (this isolates the code in the document
                   # from the code in this app).
                   rmarkdown::render(app_sys("GenRep.Rmd"), output_file = file, params = params, envir = new.env(parent = globalenv()))
                 }
                 )




                 # Download Handler
               } # function
  ) # moduleServer
} # Filt_server

## To be copied in the UI
# mod_Filt_ui("Filt_1")

## To be copied in the server
# mod_Filt_server("Filt_1")
