Format_Geo<-function(GDT)
{


  brks <- seq(0, .75, .05)
  clr <- brewer.pal(10,"RdBu")
  clr<-c(clr[10:6], "#FFFFFF", clr[5:1])
  clrs <- colorRampPalette(clr)(length(brks) + 1)



  Formatted<-datatable(GDT, rownames = FALSE, filter= 'bottom',
                       extensions = 'Buttons',
                       options = list(dom = 'Bfrtip',
                                      initComplete = JS(
                                        "function(settings, json) {",
                                        "$('body').css({'font-family': 'Times New Roman'});",
                                        "}"
                                      ),buttons = list(list(
                                        extend = 'collection',
                                        buttons = c('csv', 'excel', 'pdf', 'copy', 'print'),
                                        text = 'Export',
                                        exportOptions = list(
                                          modifier = list(page = "all"))
                                      ))


                       ))%>%

    formatStyle('Concentration', target = 'row',  backgroundColor = styleInterval(brks,clrs),
                color = styleInterval(c(.2,2.5),c('white','black','white')))%>% formatPercentage(columns =4 )

  return(Formatted)

}
