Format_DT<-function(DFT)
{

  brks <- seq(0, 3, .05)
  clr <- brewer.pal(10,"RdBu")
  clr<-c(clr[10:6], "#FFFFFF", clr[5:1])
  clrs <- colorRampPalette(clr)(length(brks) + 1)

  A<-ceiling(ncol(DFT)/2)

  Formatted<-datatable(DFT, rownames = FALSE,
                       extensions = 'Buttons',
                       options = list(dom = 'Bfrtip',
                                      initComplete = JS(
                                        "function(settings, json) {",
                                        "$('body').css({'font-family': 'Times New Roman'});",
                                        "}"
                                      ),buttons = list(list(extend = 'colvis', columns = c(0:(ncol(DFT)-1)))),

                                      columnDefs = list(list(visible=FALSE, targets=c(A:(ncol(DFT)-1))))
                       ))%>%

    formatStyle(c(colnames(DFT[,2:A])), valueColumns = c((A+1):ncol(DFT)),  backgroundColor = styleInterval(brks,clrs),
                color = styleInterval(c(.2,2.5),c('white','black','white')))%>%

    formatRound(columns = c(2:ncol(DFT)),digits=3) %>% formatPercentage(columns = c((A+1):ncol(DFT)))

  return(Formatted)

}
