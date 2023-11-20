#' ST UI Function
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
#' @importFrom tidyr pivot_wider replace_na spread
mod_ST_ui <- function(id){
  ns<-NS(id)

  tabPanel( "Spread Tables",

            fluidRow(column(10,offset=1, h1("Spread Tables"),DTOutput(ns("table")))),
            fluidRow(column(10, offset=1, h2("Race"), DTOutput(ns("race_table")))),
            fluidRow(column(10, offset=1, h2("Age"), DTOutput(ns("age_table")))),

  )

}

#' ST Server Functions
#'
#' @noRd
mod_ST_server <- function(id,r,ST_Report){
  moduleServer(id,
               function(input,output,session)
               {
                 observeEvent(r$Run,
                              {

                                ##########################
                                # MODULE FUNCTION CALLS
                                #########################
                                # puts the issues into their own data frame.
                                # forgot why I did it this way initially but don't feel
                                # like changing it atm.
                                issues<-as.data.frame(c(isolate(r$issueA),isolate(r$issueB)))

                                # Does the basic first couple calculations.
                                Forjac_H<-Get_Counts(issues, isolate(r$pop), haystack_issues)
                                Forjac<-as.data.frame(Forjac_H[2])
                                colnames(Forjac)<-gsub(".", " ",colnames(Forjac), fixed=TRUE)
                                ALL_Probs<-as.data.frame(Forjac_H[1])

                                # Generates the first spread table for flag combinations
                                # and then formats it using the format ST function.
                                DT<-D_Jac(Forjac, issues, ALL_Probs)


                                # Calls the spread tables function and then
                                # assigns the returned list into two data frames
                                ST<-Spread_Tables(issues,isolate(r$pop))
                                age_tableo<-as.data.frame(ST[1])
                                race_tableo<-as.data.frame(ST[2])

                                print("ST_MOD_CALLS")
                                ########################
                                # CREATING DATA-TABLES
                                ########################
                                colnames(age_tableo)<-gsub(".", " ",colnames(age_tableo), fixed=TRUE)
                                colnames(age_tableo)<-gsub("X", " ",colnames(age_tableo), fixed=TRUE)
                                colnames(race_tableo)<-gsub(".", " ",colnames(race_tableo), fixed=TRUE)
                                DT2<-Format_ST(DT)
                                age_tableo<-Make_DT_Basic((age_tableo))
                                race_tableo<-Make_DT_Basic((race_tableo))


                                #######################
                                # OUTPUTTING TO THE UI
                                ########################
                                output$table <- renderDT(DT2)
                                output$race_table <- renderDT(race_tableo)
                                output$age_table <- renderDT(age_tableo)


                                ###########################
                                # SAVING TO THE REPORT LIST
                                ############################
                                ST_Report$Partisan<-DT2
                                ST_Report$Race<-race_tableo
                                ST_Report$Age<-age_tableo

                              }) # observeEvent
               } # function
  ) # moduleServer
} # ST_Module

## To be copied in the UI
# mod_ST_ui("ST_1")

## To be copied in the server
# mod_ST_server("ST_1")
