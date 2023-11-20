#' Analytics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @importClassesFrom tibble tbl_df
#' @importFrom tibble as_tibble
#' @import VennDiagram
#' @import DT
#' @importFrom tidyr replace_na
#' @importFrom grid grid.newpage grid.draw
mod_Analytics_ui <- function(id){

  ns<-NS(id)

  tabPanel("Analytics",



           fluidRow(column(10, offset=1,h2('Conditional Probabilities'),
                           DTOutput(ns("Prob_BA")), DTOutput(ns("Prob_AB")),plotOutput(ns("venn")), h2('Commonly Shared Flags'),h3("Shared With Both"),
                           DTOutput(ns("MostShared"))
           )),
           fluidRow(column(5, offset=1, h3('Shared With ', textOutput(ns('AN_F1'), inline = TRUE)), DTOutput(ns('First_Flag'))),
                    column(5, offset = 1, h3("Shared With", textOutput(ns('AN_F2'), inline = TRUE)), DTOutput(ns('Second_Flag'))))





  )



}

#' Analytics Server Functions
#'
#' @noRd
mod_Analytics_server<-function(id, r, Analy_Report)
{
  moduleServer(id,
               function(input, output, session)
               {
                 observeEvent(r$Run,
                              {

                                # Puts the issues into their own data frame.
                                # Forgot why I did it this way initially but don't feel
                                # Like changing it atm.

                                issues<-as.data.frame(c(isolate(r$issueA),isolate(r$issueB)))


                                #########################
                                # MODULE FUNCTION CALLS
                                #########################

                                ## Calls Get_Counts which returns a list of two data frames
                                ## The counts of each combination of Flags spread by party
                                ## A list of the probability of every flag.
                                ##
                                ## Then Assigns the Flag combination to the dataframe named
                                ## Forjac.
                                ##
                                ## Also removes the . from the column names of ForJac

                                Forjac_H<-Get_Counts(issues, isolate(r$pop), haystack_issues)
                                Forjac<-as.data.frame(Forjac_H[2])
                                colnames(Forjac)<-gsub(".", " ",colnames(Forjac), fixed=TRUE)

                                ## Calls Con_Probs which Generates the Conditional Probabilities
                                ## of each Flag combination.
                                ##
                                ## Then Assigns them to data frames.
                                ##
                                ## Then Calls Times_More_Likely which calculates how much more or less likely
                                ## The conditional probability is than the general probability

                                DT2<-Con_Probs(Forjac, isolate(r$pop), issues)

                                Prob_BA<-as.data.frame(DT2[1])
                                Prob_AB<-as.data.frame(DT2[2])

                                Prob_BA <-Times_More_Likely(as_tibble(Prob_BA),isolate(r$issueB), isolate(r$pop))
                                Prob_AB <-Times_More_Likely(as_tibble(Prob_AB),isolate(r$issueA), isolate(r$pop))

                                # Calls the Make_Venn function which returns the grid.object venn diagram
                                VENN_SAVE<-Make_Venn(isolate(r$issueA), isolate(r$issueB), isolate(r$Level_1), isolate(r$Level_2), isolate(r$pop))

                                # Calls the module function most_shared which returns a list of 3 data frames
                                Shared_List<-Most_Shared(isolate(r$pop), isolate(r$issueA),isolate(r$issueB),isolate(r$Level_1), isolate(r$Level_2))


                                ########################
                                # CREATING DATA-TABLES
                                ########################

                                Prob_BA_Formatted<-Format_DT(Prob_BA)
                                Prob_AB_Formatted<-Format_DT(Prob_AB)
                                Both_Flags<-Make_DT_Basic(as.data.frame(Shared_List[1]))
                                First_Flag<-Make_DT_Basic(as.data.frame(Shared_List[2]))
                                Second_Flag<-Make_DT_Basic(as.data.frame(Shared_List[3]))


                                ########################
                                # OUTPUTTING TO THE UI
                                ########################

                                ## Outputs the Results of the Module Functions to the UI
                                ## The first two outputs send the flags to the titles of the Data Tables
                                output$AN_F1<-renderText(c(gsub("_", " ",isolate(r$flag), fixed=TRUE),'-', isolate(r$Level_1)))
                                output$AN_F2<-renderText(c(gsub("_", " ",isolate(r$flag2), fixed=TRUE),'-', isolate(r$Level_2)))


                                output$MostShared<-renderDT(Both_Flags)
                                output$First_Flag<-renderDT(First_Flag)
                                output$Second_Flag<-renderDT(Second_Flag)
                                output$Prob_BA <- renderDT(Prob_BA_Formatted)
                                output$Prob_AB<- renderDT(Prob_AB_Formatted)

                                # The Venn has to be output in a specific way so Display_Venn_2
                                # must be called inside the render plot Function
                                output$venn <- renderPlot({Display_Venn(VENN_SAVE)})




                                #############################
                                # SAVING TO THE REPORT LIST
                                #############################

                                ## Saves All of the Vales in this Tab so
                                ## They can be sent to the Report Function
                                Analy_Report$Most_Shared<-Both_Flags
                                Analy_Report$First_Flag<-First_Flag
                                Analy_Report$Second_Flag<-Second_Flag
                                Analy_Report$Prob_BA<-Prob_BA_Formatted
                                Analy_Report$Prob_AB<-Prob_AB_Formatted
                                Analy_Report$VENN_SAVE<-VENN_SAVE

                              }) # observeEvent
               } # function
  ) # moduleServer
} # Analy_Server

## To be copied in the UI
# mod_Analytics_ui("Analytics_1")

## To be copied in the server
# mod_Analytics_server("Analytics_1")
