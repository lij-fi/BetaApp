Generate_Label<-function(Population)
{

  COUNTY_DATA<-Population %>% group_by(COUNTY) %>% count(POLITICAL_PARTY) %>%
    filter(POLITICAL_PARTY %in% c("Democratic", "Republican", "Non-Partisan")) %>%
    pivot_wider(names_from = POLITICAL_PARTY, values_from = n)

  COUNTY_LOST_VOTERS<- Population %>% group_by(COUNTY, POLITICAL_PARTY) %>% count(GENERAL_2022, GENERAL_2020) %>%
    filter(GENERAL_2022==0, GENERAL_2020==1) %>%
    pivot_wider(names_from = POLITICAL_PARTY, values_from = n) %>%
    select('Democratic', 'Republican', 'Non-Partisan') %>%
    rename('Lost Dems'= Democratic, 'Lost NP'= `Non-Partisan`, 'Lost Rep'= Republican)

  LAB_DATA<-merge(COUNTY_LOST_VOTERS,COUNTY_DATA, by=1)

  labels2 <- sprintf(
    "<strong>%s</strong><br/>Democrats: %s<br/>Republicans %s<br/>Non-Partisans %s <br/>
    <br/>Lost Dems: %s<br/>Lost Rep: %s<br/>Lost NP %s",
    LAB_DATA$COUNTY, LAB_DATA$Democratic, LAB_DATA$Republican, LAB_DATA$`Non-Partisan`,
    LAB_DATA$`Lost Dems`, LAB_DATA$`Lost NP`, LAB_DATA$`Lost Rep`
  ) %>% lapply(htmltools::HTML)



  return(labels2)

}
