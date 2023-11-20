Bar_Stack<-function(population_internal)
{

  ggdf<-population_internal %>% group_by(AGE_RANGE) %>%
    count(ETHNIC_GROUP) %>%
    arrange(desc(ETHNIC_GROUP), .by_group=TRUE) %>%
    filter(!ETHNIC_GROUP %in% c("East and South Asian", "Other", "Unknown", AGE_RANGE=c("Unknown"))) %>%
    mutate(ylab=cumsum(n)-(.5*n)) %>%
    rename(Total=n)

  print(ggdf)

  return(ggplot(ggdf, aes(x=AGE_RANGE, y=Total, fill=ETHNIC_GROUP)) +
           geom_bar(stat="identity")+
           geom_text(aes(y=ylab, label=Total), color="white", size=3.5)+
           theme_classic())

}
