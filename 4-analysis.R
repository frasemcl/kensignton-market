# TODO dict of variable names. eg. v_CA21_572
# Why the repetition? Answer: Total, male, female
library(cancensus)
library(plotly)
library(tidyverse)
library(vtable)
library(leaflet)


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


# Rough idea, something like this:
# https://stackoverflow.com/questions/73213963/add-vertical-line-to-plotly-express-charts-to-show-last-observations




fig <- plot_ly(x = ~test$`v_CA21_1: Population, 2021`, type = "histogram", 
               showlegend=FALSE,
               name='Histogram',
               marker = list(line = list(color = 'rgb(8,48,107)',
                                         width = 1.5)))
  for (row in 1:nrow(test)) {
   xval <- test$`v_CA21_1: Population, 2021`[row]
   fig <- fig %>% add_markers(x = xval, y=0, opacity = 0.7,
                              marker= list(symbol = 'diamond-tall', 
                                                          size = 25,
                                           line = list(
                                             color = 'white',
                                             width = 1
                                           )),
                               showlegend=TRUE,
                               name=test$name_concat[row])
  }
fig



# Markers in plotly
# https://plotly-r.com/working-with-symbols.html


# Delete when working above

fig <- plot_ly(x = ~test$`v_CA21_1: Population, 2021`, type = "histogram", 
               showlegend=FALSE,
               name='Histogram')
for (row in 1:nrow(test)) {
  xval <- test$`v_CA21_1: Population, 2021`[row]
  fig <- fig %>% add_segments(x = xval, xend = xval, y = 0, yend = 10,
                              showlegend=TRUE,
                              name=test$name_concat[row])
}
fig
