Generate_Map<-function(counties, Zip_Tab, population)
{
  # sets the color pallet for the map
  pal <- colorNumeric("viridis", NULL)

  Labels2<-Generate_Label(population)


  # creates a temporary dat aframe in which to access the json data
  ct_new_data<- as_tibble(counties@data)

  # merges our geo data with the other fiels data
  ct_new_data<-merge(ct_new_data,Zip_Tab, by.x='TP_NAME', by.y='COUNTY')

  # re-inseters the data into the data frame with json data and orders both data frames so
  # labels line up with correct polygons
  Labels2<-Labels2[order(as.numeric(ct_new_data$rn))]
  counties@data<-ct_new_data[order(as.numeric(ct_new_data$rn)),]

  # resets the rows names of the json data frame so the counties line up with the
  # polygons
  row.names(counties@data)<-(1:nrow(counties@data))

  # creates the map
  Mappy<-leaflet(counties) %>%
    # addTiles() %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .75,
                fillColor = ~pal(log10(Concentration)),
                label = Labels2, popup = paste0(counties$Concentration)) %>%

    addLegend("bottomright", pal = pal, values = ~Concentration,
              title = "Percent of Voters with Selected Flag",
              opacity = 1)

  return(Mappy)

}
