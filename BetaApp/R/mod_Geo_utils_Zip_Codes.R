Zip_Codes<-function(population_internal, issue1, issue2, Lev1, Lev2, P, Area)
{
  Area2<-Area



  Zip_Count<-population_internal %>%  filter(.data[[issue1]]=={{Lev1}} & .data[[issue2]]=={{Lev2}}) %>% count(.data[[Area2]]) %>% rename(Selected_Voters=n)

  Zip_Total<- population_internal %>% count(.data[[Area2]]) %>% rename(Total_Voters=n)
  Zip_End<-merge(Zip_Total, Zip_Count, by=1, all=T) %>%
    mutate(Concentration=Selected_Voters/Total_Voters)




  return(Zip_End)

}
