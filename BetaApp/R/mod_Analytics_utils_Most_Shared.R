Most_Shared<-function(population_internal, issue1, issue2, Lev1, Lev2)
{
  With_Flags<-population_internal %>%  filter(.data[[issue1]]=={{Lev1}} & .data[[issue2]]=={{Lev2}})
  With_One<-population_internal %>%  filter(.data[[issue1]]=={{Lev1}})
  With_Two<- population_internal %>%  filter(.data[[issue2]]=={{Lev2}})

  With_Flags<-as_tibble(With_Flags)
  With_One<-as_tibble(With_One)
  With_Two<-as_tibble(With_Two)

  # These three Count the First issue in the data Frame to initialize the
  # data frame so rind can be used later.

  Shared <-With_Flags %>%
    count(CHURCH_ATTENDANCE) %>%
    rename(Flag="CHURCH_ATTENDANCE") %>%
    mutate(Issue="CHURCH_ATTENDANCE")

  Shared_Two <-With_Two %>%
    count(CHURCH_ATTENDANCE) %>%
    rename(Flag="CHURCH_ATTENDANCE") %>%
    mutate(Issue="CHURCH_ATTENDANCE")

  Shared_One <-With_One %>%
    count(CHURCH_ATTENDANCE) %>%
    rename(Flag="CHURCH_ATTENDANCE") %>%
    mutate(Issue="CHURCH_ATTENDANCE")


  for (BB in unique(haystack_issues))
  {

    Shared<-rbind(Shared,(With_Flags %>%
                            count(.data[[BB]]) %>%
                            mutate(Issue={{BB}}) %>%
                            rename(Flag={{BB}})))

    Shared_One<-rbind(Shared_One,(With_One %>%
                                    count(.data[[BB]]) %>%
                                    mutate(Issue={{BB}}) %>%
                                    rename(Flag={{BB}})))

    Shared_Two<-rbind(Shared_Two,(With_Two %>%
                                    count(.data[[BB]])%>%
                                    mutate(Issue={{BB}}) %>%
                                    rename(Flag={{BB}})))

  }




  Shared<-Shared %>% filter(Flag != "Unknown", Issue!={{issue1}}, Issue!={{issue2}}) %>%
    mutate(Overlap=n/nrow(With_Flags)) %>%
    rename(Total=n) %>%
    arrange(desc(as.numeric(Total)))
  #Shared<-Shared %>% arrange(desc(as.numeric(n)))

  Shared_One<-Shared_One %>%
    filter(Flag != "Unknown", Issue!={{issue1}}) %>%
    mutate(Overlap=n/nrow(With_One)) %>%
    rename(Total=n) %>%
    arrange(desc(as.numeric(Total)))
  #Shared_One<-Shared_One %>% arrange(desc(as.numeric(n)))

  Shared_Two<-Shared_Two %>%
    filter(Flag != "Unknown", Issue!={{issue2}}) %>%
    mutate(Overlap=n/nrow(With_Two)) %>%
    rename(Total=n) %>%
    arrange(desc(as.numeric(Total)))

  Shared_List<-list(Shared,Shared_One,Shared_Two)

  return(Shared_List)
}
