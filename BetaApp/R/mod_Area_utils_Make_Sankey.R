Make_Sankey<-function(population_internal)
{
  VT<-population_internal %>%
    select(GENERAL_2022, GENERAL_2020, GENERAL_2018, GENERAL_2016, POLITICAL_PARTY) %>%
    mutate(VOTER_TYPE=apply(.,1,FUN=Voter_Type))

  Sa_DT<-cbind(population_internal,VT[6])

  Sa_DT_2<-Sa_DT %>% select(ETHNIC_GROUP, POLITICAL_PARTY, VOTER_TYPE) %>%
    filter(POLITICAL_PARTY %in% c("Republican", "Democratic", "Non-Partisan"), ETHNIC_GROUP %in% c("Likely African-American", "Hispanic and Portuguese", "European", "East and South Asian") ,
           !VOTER_TYPE %in% c("Other"))


  part_color<-colorRampPalette(c("red", "darkslategray4","blue"))
  nodecol<-part_color(1000)

  coldf<-population_internal %>% group_by(ETHNIC_GROUP) %>%
    count(POLITICAL_PARTY)

  colvec<-vector(length = 22)
  ethlev<-c("Likely African-American", "Hispanic and Portuguese", "European", "East and South Asian")

  for(i in 1:4)
  {
    etL<-ethlev[i]
    coldfsv<-coldf %>% filter(ETHNIC_GROUP=={{etL}})
    tt<-(coldfsv$n/sum(coldfsv$n))
    colval<-((tt[1]*500)-(tt[3]*500))+500
    colvec[i]<-nodecol[colval]
  }

  colvec[5:22]<-c("blue", "red", "darkslategray4", # First node whole party
                  "blue4","red4", "darkslategrey", # Lost VOters
                  "grey", "grey", "grey",           # Non VOters
                  "blue", "red", "darkslategray4", #Reliable Voters
                  "blue2", "red2", "darkslategray3", # Semi Reliable
                  "blue3","red3","darkslategrey")    # Other
  colvec<-setNames(colvec,c("Likely African-American", "Hispanic and Portuguese", "European", "East and South Asian",
                            "Democratic", "Republican", "Non-Partisan",
                            "Lost Dem", "Lost Republican", "Lost Non-Partisans",
                            "Non-Voting Dem", "Non-Voting Rep", "Non-Voting NP",
                            "Reliable Dem", "Reliable Rep", "Reliable NP",
                            "Semi-Reliable Dem", "Semi-Reliable NP", "Semi-Reliable Rep",
                            "Other Dem", "Other Rep","Other NP"))
  print(colvec)



  print("make long started")
  df <-  Sa_DT_2 %>% make_long(POLITICAL_PARTY, ETHNIC_GROUP, VOTER_TYPE)
  print("make long done")
  dagg <- df %>%
    dplyr::group_by(node)%>%
    tally()

  df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)
  print("merge done")



  pl <- ggplot(df2, aes(x = x
                        , next_x = next_x
                        , node = node
                        , next_node = next_node
                        , fill = node

                        , label = paste0(node," ", round((n/nrow(population_internal)*100), digits=2),"%")
  )
  )


  pl <- pl + geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)
  pl<- pl+ scale_fill_manual(values = colvec)
  pl <- pl + geom_sankey_label(size = 3, color = "white", fill= "gray40", hjust = -0.2)

  pl <- pl +  theme_bw()
  pl <- pl + theme(legend.position = "none")
  pl <- pl +  theme(axis.title = element_blank()
                    , axis.text.y = element_blank()
                    , axis.ticks = element_blank()
                    , panel.grid = element_blank())


  return(pl)

}
