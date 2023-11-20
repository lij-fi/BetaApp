#' Get_Counts
#'
#' @description #This Function takes a List of Issue Combinations we want to find Jacards distance between
#' and a data frame of the population and returns a data frame with 5 columns
#' Flag, Flag, Count, Issue, Issue.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
Get_Counts<-function(issues_Internal, Population, haystack_issues_internal){
  #print(issues_Internal)
  issues1<-issues_Internal[1,1]
  issues2<-issues_Internal[2,1]

  #N_COUNT<-Population %>% group_by(.data[[issues1]], .data[[issues2]]) %>% summarize(Total=n())

  N_COUNT<- Population %>% group_by(.data[[issues1]], .data[[issues2]], POLITICAL_PARTY) %>%
    summarize(Total=n()) %>%
    filter(POLITICAL_PARTY %in% c("Democratic", "Republican", "Non-Partisan")) %>%
    pivot_wider(names_from = POLITICAL_PARTY, values_from = Total) %>% mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate(Total=sum(c(Democratic, Republican, `Non-Partisan`)))



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





  return(list(ALL_Probs_Internal, N_COUNT))
}
