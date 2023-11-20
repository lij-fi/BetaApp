Demographic_Pie<-function(population_internal)
{

  GGZIP<-ggplot(population_internal, aes(x='', fill=ETHNIC_GROUP))+geom_bar(position = "stack")+coord_polar("y", start=0) + theme(axis.text = element_text(colour = NA),
                                                                                                                                  panel.background = element_rect(fill = "white")) +labs(title = "Demographics Of Selected Area",x = NULL, y = NULL)+ guides(fill=guide_legend(title=NULL))

  return(GGZIP)


}
