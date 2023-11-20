TML_2<-function(G1, G2)
{
  pp<-data.frame()
  pp<-colnames(c("Flag", "Issue","G-One Ratio" ,"G-One Count", "G-Two Ratio", "G-Two Count", "G1 Unknown", "G2 Unknown"))
  for (var in names(G1[,40:102]))
  {
    g1hldr<-G1 %>%
      count(.data[[var]]) %>%
      mutate(`G1 Unknown`=n[.data[[var]]=="Unknown"]/sum(n)) %>%
      filter(!.data[[var]]=="Unknown") %>%
      mutate(`G-One Ratio`=n/(sum(n)-n))  %>%
      mutate(Issue={{var}}) %>%
      rename(Flag={{var}}) %>%
      rename(`G-One Count`="n") %>%
      relocate(Issue, .before = Flag)

    g2hldr<-G2 %>%
      count(.data[[var]]) %>%
      mutate(`G2 Unknown`=n[.data[[var]]=="Unknown"]/sum(n)) %>%
      filter(!.data[[var]]=="Unknown") %>%
      mutate(`G-Two Ratio`=n/(sum(n)-n))  %>%
      mutate(Issue={{var}}) %>%
      rename(Flag={{var}}) %>%
      rename(`G-Two Count`="n") %>%
      relocate(Issue, .before = Flag)

    g1g2hldr<-merge(g1hldr, g2hldr)

    pp<-rbind(pp,g1g2hldr)

  }

  PP<-pp %>% filter(!Issue %in% c("VAX_PRO_ANTI", "RIGHT_WING_MILITAS", "PARTISANSHIP_OVERALL"))

  return(pp)
}
