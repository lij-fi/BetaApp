Basic_Stats<-function(population_internal)
{
  Total_Voters<-nrow(population_internal)
  Voter_Table <- population_internal %>% group_by(POLITICAL_PARTY) %>% count(POLITICAL_PARTY)
  Voter_List<-list(Total_Voters,Voter_Table)



  return(Voter_List)


}
