#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # We are now calling the module server functions
  # on a given id that matches the one from the UI

  r<-reactiveValues()
  Filt_Report<-reactiveValues()
  ST_Report<-reactiveValues()
  Area_Report<-reactiveValues()
  Geo_Report<-reactiveValues()
  Analy_Report<-reactiveValues()
  Group_Report<-reactiveValues()


  mod_Filt_server(id="Filt_2", r, Filt_Report, ST_Report, Area_Report, Geo_Report, Analy_Report, Group_Report)
  mod_ST_server(id="ST", r, ST_Report)
  mod_Geo_server(id="Geo", r, Geo_Report)
  mod_Area_server(id="Area", r, Area_Report)
  mod_Analytics_server(id="Analy", r, Analy_Report)
  mod_Group_Comp_server(id = "Group", Group_Report)
}
