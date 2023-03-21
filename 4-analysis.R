library(cancensus)
library(plotly)
library(tidyverse)
library(vtable)
library(leaflet)
library(reactable)

# WORK IN PROGRESS

# This is a nice feature, TODO get the VAR details
# Use this to look at what may be comparable in prev years
census_vectors_21 <- list_census_vectors('CA21')
# Subset by the ones I chose to look at ('1.cancensus.R')
census_var_lookup <- census_vectors_21 %>% 
  filter(vector %in% VECTOR_IDS) %>%
  select(-parent_vector, -aggregation) %>% 
  rename(census_variable = vector)
# Make a searchable reactable:
table_var_lookup<- reactable(census_var_lookup, searchable = TRUE, minRows = 10)
table_var_lookup

# Shorten colnames (will build a lookup table with better info on each code)
colnames(tor_census_df) <- c(str_split(colnames(tor_census_df), ":") %>% map_chr(`[`, 1))


# TODO How to fill NAs more appropirately? For now using -999
# tor_census_df <- drop_na(tor_census_df)
tor_census_df[is.na(tor_census_df)] <- -999
# Subsetting while testing
# tor_census_df <- tor_census_df[1:10,]




# How to summarize?
summary_tor_census_df <- summary(tor_census_df)
# A quick sumtable() is a nice look:
# https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html
# Hmm this worked the first time....
st(tor_census_df)


##### Leaflet choropleth


#### Try plotly choropleth?


#### Ranked Bar graph. Sortable? Paginated?
# Or just vars I'm most interested in? Accept that Shiny is way better for responsive...
# TODO Make it a function where I pass in the var of interest

plot_df <- tor_census_df %>%  
  select(name_concat, v_CA21_4317) %>% 
  arrange(desc(v_CA21_4317))

plot_df$name_concat <- factor(plot_df$name_concat, levels = unique(plot_df$name_concat)[order(plot_df$v_CA21_4317)])


plot <- plot_ly(plot_df,x = ~v_CA21_4317, y = ~name_concat, type = 'bar', orientation = 'h')

plot



# Helpful for ideas: https://walker-data.com/census-r/exploring-us-census-data-with-visualization.html
# For eg. dot and whisker cleaner than boxplots? 
# population pyramids? 



########## Plotly histogram for all variables 
# Select the variables of interest
var_names <- colnames(tor_census_df)


# The first 16 variables don't contain census data
# TODO make this selection less manual
var_names <- tail(var_names, -16)
var_names
# Check, to delete later:
var_names[1]
var_names[length(var_names)]

# This function creates the dropdown options
# Logic to change title: https://community.plotly.com/t/changing-the-xy-axis-titles-with-dropdown-menu/5421/2
# And: https://plotly.com/r/dropdowns/
# Still not working, put on pause
# This should get me there! :
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


# TODO shorten dropdown labels, update x axis title 

# Subset based on variable column name (testing123)....delete later
# test[, var_names[1]]

# making the histogram(s)
fig <- plot_ly(x = tor_census_df[, var_names[1]], type = "histogram", 
               showlegend=FALSE,
               name='Histogram',
               marker = list(line = list(color = 'rgb(8,48,107)',
                                         width = 1.5))) %>%   
  layout(xaxis = list(title = 'census var'),
         yaxis = list(title = 'n census tracts'),
         updatemenus = list(
           list(
             # y = 0.7,
             buttons = create_dropdown(tor_census_df, var_names)
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



