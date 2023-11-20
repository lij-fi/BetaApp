Demo<-function(population_internal, Zips, Area)
{

  population_internal_2<-data.frame()
  population_internal_2[1,1:103]<-1
  colnames(population_internal_2)<-colnames(population_internal)

  print("is it here")
  population_internal_2<-population_internal %>% filter(.data[[Area]] %in% Zips)


  GGZIP<-ggplot(population_internal_2, aes(x='', fill=ETHNIC_GROUP))+
    geom_bar(position = "stack")+coord_polar("y", start=0) + theme(axis.text = element_text(colour = NA),panel.background = element_rect(fill = "white"))+
    labs(title = "Demographics Of Selected Area", x = NULL, y = NULL)+ guides(fill=guide_legend(title=NULL))


  return(GGZIP)


}
