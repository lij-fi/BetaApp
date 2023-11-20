Con_Probs<-function(N_COUNT, population_2 ,issues_internal){

  # Initalizes the two data frames
  Prob_AB<-data.frame()
  Prob_BA<-data.frame()


  #Prob_AB_Internal<-data.frame()

  #
  #A <- population_2 %>% count(.data[[(issues_internal[1,1])]])
  #B <- population_2 %>% count(.data[[(issues_internal[2,1])]])


  # These two group the population by the issues (group_by doesn't change the datta it only changes how it interacts with tally())
  # Then it tallies the occurrences of each combination using tally and spread.
  # Then it replaces all Na's with 0's
  Prob_AB<-population_2 %>% group_by(.data[[(issues_internal[1,1])]],.data[[(issues_internal[2,1])]]) %>%
    tally() %>% spread(.data[[(issues_internal[1,1])]], n) %>% mutate_if(is.numeric, ~replace_na(., 0)) %>% mutate_if(is.numeric, ~(./sum(.)))

  Prob_BA<-population_2 %>% group_by(.data[[(issues_internal[2,1])]],.data[[(issues_internal[1,1])]]) %>%
    tally() %>% spread(.data[[(issues_internal[2,1])]], n) %>% mutate_if(is.numeric, ~replace_na(., 0)) %>% mutate_if(is.numeric, ~(./sum(.)))


  Prob_List<-list(Prob_BA,Prob_AB)
  return(Prob_List)

}
