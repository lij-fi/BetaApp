Format_ST<-function(ST)
{
  ST2<-ST[1:(nrow(ST)),3:5]

  MAXVAL<-colnames(ST2)[apply(ST2,1,which.max)]

  ST3<-bind_cols(ST[1:(nrow(ST)),],(as.data.frame(MAXVAL)))

  #print(ST3)

  colnames(ST3)<-c(colnames(ST), "MAXVAL")
  Formatted2<-datatable(ST3, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons =
                                                                      list('copy', 'print', list(
                                                                        extend = 'collection',
                                                                        buttons = c('csv', 'excel', 'pdf'),
                                                                        text = 'Download'
                                                                      )), initComplete = JS(
                                                                        "function(settings, json) {",
                                                                        "$('body').css({'font-family': 'Times New Roman'});",
                                                                        "}"
                                                                      ), columnDefs = list(list(visible=FALSE, targets=8))))%>%

    formatStyle('MAXVAL', target='row', backgroundColor = styleEqual(c('Republican','Non.Partisan', 'Democratic'), c('#FF3333','#FF66FF','skyblue')))


  return(Formatted2)

}
