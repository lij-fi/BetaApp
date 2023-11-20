Part_Overall<-function(G1)
{
  G1_Part<-G1 %>%
    count(PARTISANSHIP_OVERALL) %>%
    mutate(Percent=n/sum(n)) %>%
    rename(Total=n) %>%
    rename(Partisanship=PARTISANSHIP_OVERALL) %>%
    filter(Partisanship!="Unknown") %>%
    arrange(desc(Percent))
  return(G1_Part)
}
