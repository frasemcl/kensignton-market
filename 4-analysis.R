# TODO dict of variable names. eg. v_CA21_572
# Why the repetition? Answer: Total, male, female
library(cancensus)
library(plotly)
library(tidyverse)
library(vtable)
library(leaflet)

# WORK IN PROGRESS

# This is a nice feature, TODO get the VAR details
# Use this to look at what may be comparable in prev years
list_census_vectors('CA21')


# fig <- plot_ly(ggplot2::diamonds, x = ~cut, y = ~price, color = ~clarity, type = "box")
# fig <- fig %>% layout(boxmode = "group")
# fig


test <- tor_census_df %>% 
  select(c(1:3,17:26))

# TODO How to fill NAs? 
test <- drop_na(test)

# This if reasonable to add zeros
# temp2[is.na(temp2)] <- 0

# Subsetting while testing
# test <- test[1:10,]

# How to summarize?

test2 <- summary(test)

# A quick sumtable() is a nice look:
# https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html
st(test)


##### Leaflet choropleth



# Try plotly choropleth?



# Helpful for ideas: https://walker-data.com/census-r/exploring-us-census-data-with-visualization.html
# For eg. dot and whisker cleaner than boxplots? 
# population pyramids? 





########## Plotly histogram for all variables 
# Select the variables of interest
var_names <- colnames(test)
# Edit this when I start working with the true DF
var_names <- tail(var_names, -3)
var_names
# Check, to delete later:
var_names[1]
var_names[length(var_names)]

# This function creates the dropdown options
create_dropdown <- function(df, vars) {
  lapply(
    vars,
    FUN = function(var_name, df) {
      button <- list(
        method = 'restyle',
        args = list('x', list(df[, var_name])),
        label = var_name
      )
    },
    df
  )
  
}

# Subset based on variable column name (testing123)....delete later
# test[, var_names[1]]

# making the histogram(s)
fig <- plot_ly(x = test[, var_names[1]], type = "histogram", 
               showlegend=FALSE,
               name='Histogram',
               marker = list(line = list(color = 'rgb(8,48,107)',
                                         width = 1.5))) %>% 
  layout(title='test', xaxis = list(title = var_names[1]),
         yaxis = list(title = 'n census tracts'),
         updatemenus = list(
           list(
             y = 0.7,
             buttons = create_dropdown(test, var_names)
           )
          )
  )
fig




#################### Keeper #######
# Markers in plotly
# https://plotly-r.com/working-with-symbols.html

# Version with one var, and all individual markers:
# TODO Dream is to combine this version with the version above
fig2 <- plot_ly(x = test[, var_names[1]], type = "histogram", 
               showlegend=FALSE,
               name='Histogram',
               marker = list(line = list(color = 'rgb(8,48,107)',
                                         width = 1.5))) %>% 
  layout(title='Where each names census tract falls on the hist....', xaxis = list(title = var_names[1]),
         yaxis = list(title = 'n census tracts')
  )
for (row in 1:nrow(test)) {
 xval <- test[, var_names[1]][row]
 fig2 <- fig2 %>% add_markers(x = xval, y=1, opacity = 0.7,
                            marker= list(symbol = 'diamond-tall',
                                                        size = 25,
                                         line = list(
                                           color = 'white',
                                           width = 1
                                         )),
                             showlegend=TRUE,
                             name=test$name_concat[row])
}
fig2

################################################


############# Archive 
#  buttons were made manually before using a function:
fig <- plot_ly(x = test[, var_names[1]], type = "histogram", 
               showlegend=FALSE,
               name='Histogram',
               marker = list(line = list(color = 'rgb(8,48,107)',
                                         width = 1.5))) %>% 
  layout(title='test', xaxis = list(title = var_names[1]),
         yaxis = list(title = 'n census tracts'),
         updatemenus = list(
           list(
             y = 0.7,
             buttons = list(
               list(method = "restyle",
                    args = list("x", list(test[, var_names[1]]),
                                list(yaxis = list(title = var_names[1])),
                    ),
                    label = var_names[1]),
               list(method = "restyle",
                    args = list("x", list(test[, var_names[2]]),
                                list(yaxis = list(title = var_names[2]))),
                    label = var_names[2])
             )))
  )
fig

# Helpful but I'm not quite there yet (combining the two histograms...)
# https://stackoverflow.com/questions/40024029/plotly-updating-data-with-dropdown-selection
# https://gist.github.com/AliciaSchep/beca4cd1145b8caf7c8136e5979039b4
# https://plotly.com/r/custom-buttons/#restyle-buttons


## THIS part is comented out below (where it was initially working)
# trying to make it a function called within each dropdown 'update'

# How to remove them first? Kind of like:
# https://community.plotly.com/t/remove-all-traces/13469
# fig2$x$attrs[2] onwards have to be cleared first
# Not working yet, HOW TO RUN A FUNCTION within 'restyle' or 'update'?
# Probably worth giving up on this dream....
update_traces <- function(fig, df, var){
  # This line clears all the traces (except the histogram which is [1])
  fig$x$attrs <- fig$x$attrs[1]
  
  for (row in 1:nrow(df)) {
    xval <- df[, var][row]
    fig <- fig %>% add_markers(x = xval, y=1, opacity = 0.7,
                               marker= list(symbol = 'diamond-tall',
                                            size = 25,
                                            line = list(
                                              color = 'white',
                                              width = 1
                                            )),
                               showlegend=TRUE,
                               name=test$name_concat[row])
  }
}

for (row in 1:nrow(test)) {
  xval <- test[, var_names[1]][row]
  fig <- fig %>% add_markers(x = xval, y=1, opacity = 0.7,
                             marker= list(symbol = 'diamond-tall',
                                          size = 25,
                                          line = list(
                                            color = 'white',
                                            width = 1
                                          )),
                             showlegend=TRUE,
                             name=test$name_concat[row])
}



