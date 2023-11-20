Pop_Comp<-function(population_internal, Race, Age, Gender, Income, Elections, Elections_No)
{

  # Access and Assigns the low age and high age to Age1 and Age2. Needed because dplyr doesn't like
  # when they are accessed like AGE[1] internally
  Age1<-Age[1]
  Age2<-Age[2]


  # creates the population_internal_2 dataframe
  population_internal_2<-data.frame()


  #colnames(population_internal_2)<-colnames(population_internal)

  # Checks is we want to filter on race: If so filters rows by the selected criteria. If not just assigns
  # population to population_internal_2 and moves on.

  RN<-!identical(Race, c("NONE"))

  if(RN)
  {
    population_internal_2<-population_internal %>% filter(ETHNIC_GROUP %in% Race)
  }
  else
  {
    population_internal_2<-population_internal

  }

  # Checks if we want to filter by age: If so it does it.
  AN<-!identical(Age,c(18,100))
  if (AN)
  {
    population_internal_2 <- population_internal_2 %>% filter(AGE>{{Age1}} & AGE<{{Age2}})
  }

  # Checks if we want to filter by Gender. If so does it:
  GN<-!identical(Gender, c("NONE"))
  if (GN)
  {
    population_internal_2 <- population_internal_2 %>% filter(GENDER=={{Gender}})

  }
  IN<-!identical(Income,c("All"))
  if(IN)
  {
    population_internal_2 <- population_internal_2 %>% filter(STATE_INCOME_DECILE %in% Income)
  }
  EN<-!identical(Elections,c("All"))
  if (EN)
  {
    population_internal_2 <- population_internal_2 %>% mutate(across(all_of(Elections), as.numeric)) %>%
      filter(if_all(Elections, ~ . ==1))
  }
  ENN<-!identical(Elections_No,c("All"))
  if (ENN)
  {
    population_internal_2 <- population_internal_2 %>% mutate(across(all_of(Elections_No), as.numeric)) %>%
      filter(if_all(Elections_No, ~ . ==2))
  }

  return(population_internal_2)
}
