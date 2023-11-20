Chi_Sq<-function(issue_1,G1,G2){



  G1_C1<- G1 %>% filter(.data[[issue_1]]!="Unknown") %>%
    {if(nrow(.)>10000) sample_n(.,size = 10000) else .} %>%
    count(.data[[issue_1]]) %>%
    rename(TotalG1=n)

  G2_C1<- G2 %>% filter(.data[[issue_1]]!="Unknown") %>%
    {if(nrow(.)>10000) sample_n(.,size = 10000) else .} %>%
    count(.data[[issue_1]]) %>%
    rename(TotalG2=n)



  tst14<-merge(G2_C1, G1_C1, by=issue_1)
  tst15<-chisq.test(tst14[,2:3])
  ret_list<-list(issue_1, tst15$statistic)
  ret_list[[2]]<-round(ret_list[[2]], digits=1)




  ret_list_2<-setNames(ret_list, c("Issue", "Chi_SQ"))

  return(ret_list_2)


}
