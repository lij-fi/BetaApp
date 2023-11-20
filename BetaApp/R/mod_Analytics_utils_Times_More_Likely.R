Times_More_Likely<-function(cpout, issue, population)
{
  Selected_Issue<-population %>% select(.data[[issue[1]]]) %>% group_by(.data[[issue[1]]]) %>% count()
  Probs<-t((Selected_Issue[,2]/nrow(population)))
  moreless<<-as.matrix(cpout[1:nrow(cpout),2:ncol(cpout)])
  moreless2<-sweep(as.matrix(moreless), 2, Probs, '/')
  return(cbind(cpout,moreless2))
}
