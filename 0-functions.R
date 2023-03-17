# Run this script first to define functions used later

#This default label syntax means that the first column will be used if no label 
#is specified at the function call
make_leaflet <- function(df, label = df[[1]]){
  df %>% 
    leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(fillColor = 'red', 
                fillOpacity = 0.1, color = 'red', 
                weight = 1, smoothFactor = 0.5, 
                popup = ~label, 
                label = ~label
                ) 
}


# If using with Shiny, follow option 2 in this answer:
# https://gis.stackexchange.com/questions/283658/add-layers-with-leaflet-from-different-spatial-data-frames-in-r
make_leaflet_2 <- function(df,
                           label = df[[1]],
                           group1='layer_1',
                           df2,
                           label2 = df2[[1]],
                           group2='layer_2'){
    leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(data=df, fillColor = 'red', 
                fillOpacity = 0.1, color = 'red', 
                weight = 1, smoothFactor = 0.5, 
                popup = ~label, 
                label = ~label,
                group = group1
    ) %>% 
    addPolygons(data=df2, fillColor = 'blue', 
                fillOpacity = 0.1, color = 'blue', 
                weight = 1, smoothFactor = 0.5, 
                popup = ~label2, 
                label = ~label2,
                group = group2
    ) %>% 
    addLayersControl(
      overlayGroups = c(group1, group2),
    )
}

make_leaflet_3 <- function(df,
                           label = df[[1]],
                           group1='layer_1',
                           df2,
                           label2 = df2[[1]],
                           group2='layer_2',
                           df3,
                           group3='layer_3'){
  leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(data=df, fillColor = 'red', 
                fillOpacity = 0.1, color = 'red', 
                weight = 1, smoothFactor = 0.5, 
                popup = ~label, 
                label = ~label,
                group = group1
    ) %>% 
    addPolygons(data=df2, fillColor = 'blue', 
                fillOpacity = 0.1, color = 'blue', 
                weight = 1, smoothFactor = 0.5, 
                popup = ~label2, 
                label = ~label2,
                group = group2
    ) %>%
    addCircleMarkers(data=df3,
                     radius = 1,
                     group = group3
    ) %>%
    addLayersControl(
      overlayGroups = c(group1, group2, group3),
    )
}
  
  
  
