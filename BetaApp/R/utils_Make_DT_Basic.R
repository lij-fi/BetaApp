#' Make_DT_Basic
#'
#' @description Takes a DataFrame or Tibble and turns it into a Data table wit buttons to download and export it.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
Make_DT_Basic<-function(DT){



  Formatted2<-datatable(DT, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons =
                                                                     list('copy', 'print', list(
                                                                       extend = 'collection',
                                                                       buttons = c('csv', 'excel', 'pdf'),
                                                                       text = 'Download'
                                                                     )), initComplete = JS(
                                                                       "function(settings, json) {",
                                                                       "$('body').css({'font-family': 'Times New Roman'});",
                                                                       "}"
                                                                     )))




  return(Formatted2)

}
