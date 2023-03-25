library(cancensus)
library(plotly)
library(tidyverse)
library(vtable)
library(leaflet)
library(reactable)

# WORK IN PROGRESS

# Info on interpretation:
# https://www12.statcan.gc.ca/census-recensement/2021/ref/98-26-0006/982600062021001-eng.cfm
# Anything useful?
# https://open.canada.ca/data/en/dataset/85ca6dcc-c694-441e-afff-9ea7eeb265d8

# This is a nice feature, TODO get the VAR details
# Use this to look at what may be comparable in prev years
census_vectors_21 <- list_census_vectors('CA21')
# Subset by the ones I chose to look at ('1.cancensus.R')
census_var_lookup <- census_vectors_21 %>% 
  filter(vector %in% VECTOR_IDS) %>%
  # select(-parent_vector, -aggregation) %>% 
  rename(census_variable = vector)
# Make a searchable reactable:
table_var_lookup<- reactable(census_var_lookup, searchable = TRUE, minRows = 10)
table_var_lookup
table_var_lookup_all<- reactable(census_vectors_21, searchable = TRUE, minRows = 10)
table_var_lookup_all
# Searching 'shelter' in this one is interesting (and confusing)
# TODO my understanding is that to get % of housholds with high shelter costs,
# I need to calculate it based on v_CA21_4288 and v_CA21_4290, right?
# Nevermind, v_CA21_4307 gives me that...wait just for owner...not all...

# v_CA21_4288 Total - Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio
# v_CA21_4290 (households) Spending 30% or more of income on shelter costs
# v_CA21_4302 Total - Owner and tenant households with household total income greater than zero and shelter-cost-to-income ratio less than 100%, in non-farm, non-reserve private dwellings
# v_CA21_4307 % of owner households spending 30% or more of its income on shelter costs
# v_CA21_4309 Median monthly shelter costs for owned dwellings ($)
# v_CA21_4310 Average monthly shelter costs for owned dwellings ($)
# v_CA21_4315 % of tenant households spending 30% or more of its income on shelter costs
# v_CA21_4317 Median monthly shelter costs for rented dwellings ($)
# v_CA21_4318 Average monthly shelter costs for rented dwellings ($)

# Shorten colnames (will build a lookup table with better info on each code)
colnames(tor_census_df) <- c(str_split(colnames(tor_census_df), ":") %>% map_chr(`[`, 1))


# TODO How to fill NAs more appropirately? For now using -999
# TODO explain that tor_census_df is
# tor_census_df <- drop_na(tor_census_df)
# Doing it two ways for now
tor_census_df_backup <- tor_census_df
tor_census_df_na_zero <- tor_census_df
tor_census_df_na_zero[is.na(tor_census_df_na_zero)] <- 0
# NA as -999 on the main df
tor_census_df[is.na(tor_census_df)] <- -999
# Subsetting while testing
# tor_census_df <- tor_census_df[1:10,]


# Lookup table for select vars
# The first three vars used for now as an example, but in the output I'll choose carefully and define each above/below
tor_census_df_sel <- tor_census_df %>% 
  select(name_concat, v_CA21_1, v_CA21_2, v_CA21_3)

# Use the drop geom version here
table_census_df_lookup1<- reactable(tor_census_df_sel, searchable = TRUE, minRows = 10)
table_census_df_lookup1


##### Leaflet choropleth

#Filter the df by the var of interest
df <- tor_census_df_backup %>% 
  select(name_concat, v_CA21_1)

df2 <- nhood_data

# remove NAs
# Trying first with the NAs as a test
# pal <- colorQuantile(palette = "YlOrRd", domain = df[[2]], n = 5, reverse = FALSE)
pal <- colorNumeric(palette = "YlOrRd", domain = df[[2]], n = 5, reverse = FALSE)



leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=df2, 
              fillOpacity = 0, 
              color = 'grey', 
              weight = 0.5, 
              smoothFactor = 0.5, 
              popup = ~df2$AREA_NAME, 
              label = ~df2$AREA_NAME,
              group = 'Neighbourhoods'
  ) %>% 
  addPolygons(data=df, 
              fillColor = ~pal(df[[2]]),
              fillOpacity = 0.8, 
              color = 'grey', 
              weight = 0.5, 
              smoothFactor = 0.5, 
              popup = ~df[[1]], 
              label = ~df[[1]],
              group = 'Census Tracts'
  ) %>% 
  addLayersControl(
    overlayGroups = c('Census Tracts', 'Neighbourhoods'),
  ) %>% 
  addLegend(pal = pal, values = df[[2]], opacity = 0.7, title = names(df)[2],
                position = "bottomleft")






#### Ranked Bar graph. Sortable? Paginated? ########## DELETE THIS SECTION AFTER PULLING ANYTHING USEFUL
# Or just vars I'm most interested in? Accept that Shiny is way better for responsive...
# TODO Make it a function where I pass in the var of interest

plot_df <- tor_census_df %>%  
  select(name_concat, AREA_NAME, v_CA21_4317) %>% 
  arrange(desc(v_CA21_4317))


# Appendix or delete. Here are two ways to arrange:
plot_df$name_concat <- factor(plot_df$name_concat, levels = unique(plot_df$name_concat)[order(plot_df$v_CA21_4317)])
plot <- plot_ly(plot_df,x = ~v_CA21_4317, y = ~name_concat, type = 'bar', orientation = 'h')
plot
# Same thing a different way, if works I could add dropdown to select var! Maybe, but might be same problem as before...
test <-plot_ly(
  data = plot_df,
  y = ~name_concat,
  x = ~v_CA21_4317,
  type = "bar",
  orientation='h'
) %>% 
  layout(yaxis = list(categoryorder = "total ascending"))
test



################## Make prev known as'coolOne' into a function

# FUNCTION FOR PLOTLY 1####################
make_plotly_sortedbar_filter <- function(df, axisStr, varVect, varStr) {
  # TODO make matching reactable function using just this first part (but maybe pass in more vars?)
  # TODO could this be cleaner (eg. one less input) using the column name from cols obj instead?
  input_df <- df %>%  
    select(name_concat, AREA_NAME, varStr) 
  # %>% 
  #   arrange(desc(varVect))
  input_df$name_concat <- factor(input_df$name_concat, levels = unique(input_df$name_concat)[order(varVect)])

  temp_df <- spread(input_df, key = AREA_NAME, value = colnames(input_df)[3])
  temp_df[is.na(temp_df)] <- 0
  cols <- colnames(temp_df)
  fig <- plot_ly(temp_df, y = ~name_concat, x = as.numeric(unlist(temp_df[,cols[3]])), type = 'bar', name = cols[3],
                      orientation='h')
  i <- 2
  while ( i <= length(cols)){
    fig <- fig %>% add_trace(x = as.numeric(unlist(temp_df[,cols[i]])), name = cols[i])
    i <- i + 1
  }
  fig <- fig %>% layout(xaxis = list(title = axisStr), 
                                 yaxis = list(title = '')
  )

}

############################ USING THIS FUNCTION
# Graph 1 of this type
avg_montly_rent_plotly <- make_plotly_sortedbar_filter(df = tor_census_df,
                             axisStr = 'v_CA21_4317: Average Monthly Rent ($CAD)',
                             varVect = tor_census_df$v_CA21_4317,
                             varStr = 'v_CA21_4317'
                             )
avg_montly_rent_plotly
# Graph 2
v_CA21_4290_plotly <- make_plotly_sortedbar_filter(
  df = tor_census_df,
  axisStr = "Households spending 30% or more of income on shelter costs",
  varVect = tor_census_df$v_CA21_4290,
  varStr = 'v_CA21_4290'
)
v_CA21_4290_plotly 
#####################################################

# TODO make a function for the version without filtering? As a more responsive quick look?
# TODO add a mean and median line for this next graph

# FUNCTION FOR PLOTLY 2 ####################
make_plotly_sortedbar <- function(df, axisStr, varVect, varStr) {
  input_df <- df %>%
    select(name_concat, varStr)
  cols <- colnames(input_df)
  input_df$name_concat <- factor(input_df$name_concat, levels = unique(input_df$name_concat)[order(varVect)])
  fig <- plot_ly(input_df, x = ~name_concat, y = as.numeric(unlist((input_df[,cols[2]])), type = 'bar'
                                                            , orientation='h'
                                                            ))

  
  m <- list(
    l = 10,
    r = 10,
    b = 50,
    t = 50,
    pad = 20
  )
  
  fig <- fig %>% layout(
    title = axisStr,
    margin = m,
    # yaxis = list(title = axisStr),
    xaxis = list(
      rangeslider = list(),
      title = '')
  )

}
############################ USING THIS FUNCTION
# v_CA21_4307 % of owner households spending 30% or more of its income on shelter costs
# v_CA21_4309 Median monthly shelter costs for owned dwellings ($)
# v_CA21_4310 Average monthly shelter costs for owned dwellings ($)
# v_CA21_4315 % of tenant households spending 30% or more of its income on shelter costs
# v_CA21_4317 Median monthly shelter costs for rented dwellings ($)
# v_CA21_4318 Average monthly shelter costs for rented dwellings ($)
###
v_CA21_4290_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                              axisStr = "Households spending 30% or more of income on shelter costs",
                              varVect = tor_census_df_na_zero$v_CA21_4290,
                              varStr = 'v_CA21_4290')

v_CA21_4290_plotly_sorted_bar
###
v_CA21_4310_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                                                       axisStr = "Average monthly shelter costs for owned dwellings ($)",
                                                       varVect = tor_census_df_na_zero$v_CA21_4310,
                                                       varStr = 'v_CA21_4310')

v_CA21_4310_plotly_sorted_bar
###
v_CA21_4310_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                                                       axisStr = "Average monthly shelter costs for owned dwellings ($)",
                                                       varVect = tor_census_df_na_zero$v_CA21_4310,
                                                       varStr = 'v_CA21_4310')

v_CA21_4310_plotly_sorted_bar
###
v_CA21_4307_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                                                       axisStr = "% of owner households spending 30% or more of its income on shelter costs",
                                                       varVect = tor_census_df_na_zero$v_CA21_4307,
                                                       varStr = 'v_CA21_4307')

v_CA21_4307_plotly_sorted_bar
###
v_CA21_4318_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                                                       axisStr = "Average monthly shelter costs for rented dwellings ($)",
                                                       varVect = tor_census_df_na_zero$v_CA21_4318,
                                                       varStr = 'v_CA21_4318')

v_CA21_4310_plotly_sorted_bar
###
v_CA21_4315_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                                                       axisStr = "% of tenant households spending 30% or more of its income on shelter costs",
                                                       varVect = tor_census_df_na_zero$v_CA21_4315,
                                                       varStr = 'v_CA21_4315')

v_CA21_4315_plotly_sorted_bar
###

# TODO make a reactable with all these vars, and a few others (population, income etc). Make it searchable






# 3? versions of this plotly ... Working test above
# 1) Legend grouped by neighbourhood, filter by multiple vars, no sort, no axis lab
# I think this means making df wide instead of long first, so all vars would be huge. Set instead?

# 2) Legend grouped by neighbourhood, one var, sorted, nice axis
# 3) Legend neighbourhood, set of vars, sorted, nice axis


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

# make this a function, and pair it in the markdown doc with the lookup table





#################### Keeper 1#######
# TODO make it a function
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


############# Archive, to delete after a read thru

# How to summarize?
summary_tor_census_df <- summary(tor_census_df)
# A quick sumtable() is a nice look:
# https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html
# Hmm this worked the first time....
st(tor_census_df)


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



