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

###
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

###
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

### THIS IS THE MAIN LEAFLET USED IN THE REPORT ################################
make_leaflet_compare2 <- function(df,
                                  label = df[[1]],
                                  varDesc=names(df)[2],
                                  df2,
                                  label2 = df2[[1]],
                                  varDesc2=names(df)[2],
                                  df_nh = nhood_data,
                                  perc_or_num_or_cur='num'){
  # TODO refactor this repetition
  if (perc_or_num_or_cur == 'num'){
    pal <- colorNumeric(palette = "YlOrRd", domain = df[[2]], n = 5, reverse = FALSE)
    pal2 <- colorNumeric(palette = "YlOrRd", domain = df2[[2]], n = 5, reverse = FALSE)
    popup1 <- paste0("<strong>CT: </strong>",df[[1]],
                     "<br><strong>",names(df)[2]," value: </strong>",df[[2]])
    popup2 <- paste0("<strong>CT: </strong>",df2[[1]],
                     "<br><strong>",names(df2)[2]," value: </strong>",df2[[2]])
  } 
  if (perc_or_num_or_cur == 'perc'){
    pal <- colorQuantile(palette = "YlOrRd", domain = df[[2]], n = 5, reverse = FALSE)
    pal2 <- colorQuantile(palette = "YlOrRd", domain = df2[[2]], n = 5, reverse = FALSE)
    popup1 <- paste0("<strong>CT: </strong>",df[[1]],
                     "<br><strong>",names(df)[2]," value: </strong>",df[[2]],'%')
    popup2 <- paste0("<strong>CT: </strong>",df2[[1]],
                     "<br><strong>",names(df2)[2]," value: </strong>",df2[[2]],'%')
  }
  if (perc_or_num_or_cur == 'cur'){
    pal <- colorNumeric(palette = "YlOrRd", domain = df[[2]], n = 8, reverse = FALSE)
    pal2 <- colorNumeric(palette = "YlOrRd", domain = df2[[2]], n = 5, reverse = FALSE)
    popup1 <- paste0("<strong>CT: </strong>",df[[1]],
                     "<br><strong>",names(df)[2]," value: </strong>",'$',df[[2]])
    popup2 <- paste0("<strong>CT: </strong>",df2[[1]],
                     "<br><strong>",names(df2)[2]," value: </strong>",'$',df2[[2]])
  } 


  # Setting width here helps with mobile html formatting (from RMD knit)
  # Makes it look worse within RStudio however
  leaflet(width = "100%") %>%
    addSearchOSM() %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data=df_nh, 
                fillOpacity = 0, 
                color = 'grey', 
                weight = 0.5, 
                smoothFactor = 0.5, 
                popup = ~df_nh$AREA_NAME, 
                label = ~df_nh$AREA_NAME,
                group = 'Neighbourhoods'
    ) %>% 
    addPolygons(data=df, 
                fillColor = ~pal(df[[2]]),
                fillOpacity = 0.8, 
                color = 'grey', 
                weight = 0.5, 
                smoothFactor = 0.5, 
                # popup = ~df[[1]],
                popup = ~popup1,
                label = ~df[[1]],
                group = varDesc
    ) %>%
    addPolygons(data=df2, 
                fillColor = ~pal2(df2[[2]]),
                fillOpacity = 0.8, 
                color = 'grey', 
                weight = 0.5, 
                smoothFactor = 0.5, 
                popup = ~popup2, 
                label = ~df2[[1]],
                group = varDesc2
    ) %>% 
    addLayersControl(
      overlayGroups = c(varDesc, varDesc2,'Neighbourhoods'),
    ) %>% 
    addLegend(pal = pal, values = df[[2]], opacity = 0.7, title = names(df)[2],
              position = "bottomleft", group = varDesc) %>%
    addLegend(pal = pal2, values = df2[[2]], opacity = 0.7, title = names(df2)[2],
              position = "bottomleft", group = varDesc2) %>% 
    hideGroup(varDesc2)
}

####### FUNCTION FOR INTEREACTIVE SLIDER PLOTLY in report appendix ####################
make_plotly_sortedbar <- function(df, axisStr, varVect, varStr) {
  input_df <- df %>%
    select(name_concat, varStr)
  cols <- colnames(input_df)
  input_df$name_concat <- factor(input_df$name_concat, levels = unique(input_df$name_concat)[order(varVect)])
  fig <- plot_ly(input_df, x = ~name_concat, y = as.numeric(unlist((input_df[,cols[2]])), 
                 type = 'bar', orientation='h'
  ))
  
  # m <- list(
  #   l = 10,
  #   r = 10,
  #   b = 50,
  #   t = 50,
  #   pad = 20
  # )
  
  fig <- fig %>% layout(
    title = axisStr,
    # margin = m,
    # yaxis = list(title = axisStr),
    xaxis = list(
      rangeslider = list(),
      title = '')
  )
}
##############################################################################
  
