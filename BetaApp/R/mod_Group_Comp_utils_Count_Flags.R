Count_Flags<-function(Population, haystack_issues_internal)
{
  Holer<- Population %>% group_by(.data[[haystack_issues_internal[1]]]) %>% count() %>% rename(Flag=CHURCH_ATTENDANCE)
  Holer[,3]<-haystack_issues[1]
  Holer<-Holer %>% rename(Issue='...3')
  for (i in 2:length(haystack_issues))
  {
    tst<-haystack_issues[i]
    Holer_2<- Population %>% group_by(.data[[haystack_issues[i]]]) %>% count() %>% rename(Flag={{tst}})
    Holer_2[,3]<-haystack_issues[i]
    Holer_2<-Holer_2 %>% rename(Issue='...3')
    Holer<-rbind(Holer,Holer_2)
  }
  ALL_Probs_Internal<-Holer
  ALL_Probs_Internal<-ALL_Probs_Internal %>%
    filter(!Flag=="Unknown") %>%
    relocate(Issue, .before=Flag) %>%
    arrange(desc(n)) %>%
    rename(Total=n)

  return(ALL_Probs_Internal)
}
