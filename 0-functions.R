


# May need to sort out quasinotation to be able to pass in multiple argument 
#(eg. popup/label here): https://adv-r.hadley.nz/quasiquotation.html

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
