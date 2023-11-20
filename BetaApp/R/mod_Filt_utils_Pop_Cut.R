Pop_Cut<-function(population_internal, Race, Age, Gender, AT, RT, GT)
{

  # Access and Assigns the low age and high age to Age1 and Age2. Needed because dplyr doesn't like
  # when they are accessed like AGE[1] internally
  Age1<-Age[1]
  Age2<-Age[2]

  # creates the population_internal_2 dataframe
  population_internal_2<-data.frame()
  population_internal_2[1,1:ncol(population_internal)]<-1
  # Fills the first row with ones since the rbind function won't work if the data frame has no dimensions
  #population_internal_2[1,(1:ncol(population_internal))]<-population_internal

  # Gives population internal 2 the same column names as the OG population internal. Also needed for rbind() to work

  colnames(population_internal_2)<-colnames(population_internal)

  # Checks is we want to filter on race: If so filters rows by the selected criteria. If not just assigns
  # population to population_internal_2 and moves on.

  if(RT)
  {
    population_internal_2<-population_internal %>% filter(ETHNIC_GROUP %in% Race)
  }
  else
  {
    population_internal_2<-population_internal

  }

  # Checks if we want to filter by age: If so it does it.
  if (AT)
  {
    population_internal_2 <- population_internal_2 %>% filter(AGE>{{Age1}} & AGE<{{Age2}})
  }

  # Checks if we want to filter by Gender. If so does it:
  if (GT)
  {
    population_internal_2 <- population_internal_2 %>% filter(GENDER=={{Gender}})

  }

  return(population_internal_2)
}
