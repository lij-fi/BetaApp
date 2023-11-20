Spread_Tables<-function(input_names, population_internal_st){
  issue1<-input_names[1,1]
  issue2<-input_names[2,1]

  AGE_TAB<-population_internal_st %>% group_by(AGE_RANGE) %>% count(.data[[issue1]],.data[[issue2]]) %>% rename("TOTAL"="n") %>%
    pivot_wider(names_from = AGE_RANGE, values_from = TOTAL) %>%
    select(!any_of(c("Unknown"))) %>%
    mutate(Total=rowSums(across(where(is.numeric)))) %>%
    mutate_if(is.numeric, ~replace_na(., 0))

  # This function works exactly the same as the last one.

  RACE_TAB<-population_internal_st %>% group_by(ETHNIC_GROUP) %>% count(.data[[issue1]],.data[[issue2]]) %>% rename("TOTAL"="n") %>%
    pivot_wider(names_from = ETHNIC_GROUP, values_from = TOTAL) %>%
    select(!any_of(c("Unknown"))) %>%
    mutate(Total=rowSums(across(where(is.numeric)))) %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    relocate()

  # This line reorders the AGE_TAB data frame from youngest to oldest
  #AGE_TAB<-AGE_TAB %>% relocate(any_of(c("NONE",  "18 to 24", "30 to 39", "40 to 49", "50 to 64", "65 and over")), .before= Total)
  AGE_TAB<-AGE_TAB %>% relocate(any_of(c(levels(population_internal_st$AGE_RANGE))), .before= Total)


  # shiny apps only allow one return so this places the two data frames in a list and returns that list.
  Spread_Tab<-list(AGE_TAB, RACE_TAB)

  return(Spread_Tab)
}
