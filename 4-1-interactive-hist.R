library(plotly)
library(tidyverse)

# Relies on '4-analysis.R' being run first

########## Plotly histogram for all variables
# THIS IS A WORK IN PROGRESS NOT USED IN REPORT, but useful to look at distribution of all 
# variables that I was considering looking at in more detail.
# Idea is a 
# Select the variables of interest
var_names <- colnames(tor_census_df_na_999)

# The first 16 variables don't contain census data
# TODO make this selection less manual
var_names <- tail(var_names, -16)
var_names

# This function creates the dropdown options
# Logic to change title: https://community.plotly.com/t/changing-the-xy-axis-titles-with-dropdown-menu/5421/2
# And: https://plotly.com/r/dropdowns/
# Revisit:
# https://plotly.com/r/custom-buttons/?_gl=1*1ovco31*_ga*NDc0MDczMDYuMTY3NjA0MDU4MQ..*_ga_6G7EE0JNSC*MTY3OTM0MzUzNy4yMC4xLjE2NzkzNDM3MDIuMC4wLjA.
create_dropdown <- function(df, vars) {
  lapply(
    vars,
    FUN = function(var_name, df) {
      button <- list(
        method = 'restyle',
        args = list('x', list( df[, var_name])),
        # args = list(
        #     list(list(x =list(df[, var_name])), list(xaxis = list(title = "test")))
        #     ),
        label = var_name
      )
    },
    df
  )
}

# making the histogram(s)
interactive_histogram <- plot_ly(x = tor_census_df_na_999[, var_names[1]], type = "histogram", 
               showlegend=FALSE,
               name='Histogram',
               marker = list(line = list(color = 'rgb(8,48,107)',
                                         width = 1.5))) %>%   
  layout(xaxis = list(title = 'census var'),
         yaxis = list(title = 'n census tracts'),
         updatemenus = list(
           list(
             # y = 0.7,
             buttons = create_dropdown(tor_census_df_na_999, var_names)
           )
         )
  )
interactive_histogram
