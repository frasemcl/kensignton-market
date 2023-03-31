library(cancensus)
library(plotly)
library(tidyverse)
library(vtable)
library(leaflet)
library(reactable)
library(leaflet.extras)

# Info on interpretation:
# https://www12.statcan.gc.ca/census-recensement/2021/ref/98-26-0006/982600062021001-eng.cfm
# There may be complimentary data here, or use to validate my findings:
# https://open.canada.ca/data/en/dataset/85ca6dcc-c694-441e-afff-9ea7eeb265d8


# Shorten colnames for the two dataframes from script 3. Details will be in a lookup table instead
colnames(tor_census_df) <- c(str_split(colnames(tor_census_df), ":") %>% map_chr(`[`, 1))
colnames(tor_census_df_no_geom) <- c(str_split(colnames(tor_census_df_no_geom), ":") %>% map_chr(`[`, 1))

# Dealing with NAs in different ways at this point to keep options open
# A couple backups for the dev process
tor_census_df_backup <- tor_census_df
tor_census_df_no_geom_backup <- tor_census_df_no_geom
# TODO return to this once settling on consistent way to treat NA
tor_census_df_na_zero <- tor_census_df_no_geom
tor_census_df_na_zero[is.na(tor_census_df_na_zero)] <- 0
tor_census_df_na_999 <- tor_census_df_no_geom
tor_census_df_na_999[is.na(tor_census_df_no_geom)] <- -999
# In some cases, I also choose to pass in the df with NAs then drop_na()


############# LOOKUP TABLES ####################################################
# TABLE General variables of interest for this analaysis:
tor_census_df_sel <- tor_census_df_no_geom %>% 
  select(name_concat, v_CA21_1, v_CA21_3, v_CA21_386, v_CA21_560, v_CA21_906)
# Table shown in report, for some general stats:
# v_CA21_1 Total Population, 2021
# v_CA21_3 Total Population percentage change, 2016 to 2021 
# v_CA21_386 Total Average age
# v_CA21_560 Total Median total income in 2020 among recipients ($)
# v_CA21_906 Total Median total income of household in 2020 ($)
table_background_info<- reactable(tor_census_df_sel, searchable = TRUE, defaultPageSize=5)
table_background_info
###############################################
# TABLE amount of housing per CT
tor_census_df_sel_2 <- tor_census_df_no_geom %>% 
  select(name_concat, v_CA21_434, v_CA21_4288, v_CA21_4302, v_CA21_452)
# Table shown in report:
# v_CA21_434 Total Occupied private dwellings by structural type of dwelling data
# v_CA21_4288 Total - Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio
# v_CA21_4302 Total - Owner and tenant households with household total income greater than zero and shelter-cost-to-income ratio less than 100%, in non-farm, non-reserve private dwellings
# v_CA21_452 Total Average household size
table_housing_amt<- reactable(tor_census_df_sel_2, searchable = TRUE, defaultPageSize=5)
table_housing_amt
###############################################
# TABLE amount of high expense housing per CT
tor_census_df_sel_3 <- tor_census_df_no_geom %>% 
  select(name_concat, v_CA21_4290, v_CA21_4307, v_CA21_4315)
# Table shown in report:
# v_CA21_4290 (households) Spending 30% or more of income on shelter costs
# v_CA21_4307 % of owner households spending 30% or more of its income on shelter costs
# v_CA21_4315 % of tenant households spending 30% or more of its income on shelter costs
table_expensive <- reactable(tor_census_df_sel_3, searchable = TRUE, defaultPageSize=5)
table_expensive
###############################################
# TABLE cost of housing per CT
tor_census_df_sel_4 <- tor_census_df_no_geom %>% 
  select(name_concat, v_CA21_4309, v_CA21_4310, v_CA21_4317, v_CA21_4318)
# Table shown in report:
# v_CA21_4309 Median monthly shelter costs for owned dwellings ($)
# v_CA21_4310 Average monthly shelter costs for owned dwellings ($)
# v_CA21_4317 Median monthly shelter costs for rented dwellings ($)
# v_CA21_4318 Average monthly shelter costs for rented dwellings ($)
table_costs <- reactable(tor_census_df_sel_4, searchable = TRUE, defaultPageSize=5)
table_costs


# Lookup table for the variables (cancensus vectors) ###########################
# View(variables21)
table_var_lookup_all <- reactable(variables21, searchable = TRUE, defaultPageSize=5)
table_var_lookup_all
vars21 <- variables21 %>% 
  select(-parent_vector, -aggregation)
table_var_lookup <- reactable(vars21, searchable = TRUE, defaultPageSize=5,
                              columns = list(
                                details = colDef(minWidth = 200)  # overrides the default
                              ))
table_var_lookup

##### LEAFLET CHOROPLETH ##################################################

#Filter the df by the var of interest. Function takes two dataframes
# vars of interst to be filtered selected like this first
df <- tor_census_df %>% 
  select(name_concat, v_CA21_4307) %>% 
  drop_na()
df2 <- tor_census_df %>% 
  select(name_concat, v_CA21_4315) %>% 
  drop_na()

####### Calling function from script0 ###### KEEPER #######
map_leaflet_perc30 <- make_leaflet_compare2(df=df,
                      varDesc='v_CA21_4307: % of owner households spending 30% or more of its income on shelter costs',
                      df2=df2,
                      varDesc2 = 'v_CA21_4315: % of tenant households spending 30% or more of its income on shelter costs',
                      perc_or_num = 'perc')

map_leaflet_perc30

########################################################################




# IN REPORT APPENDIX
####### CALLING FUNCTION FOR INTEREACTIVE SLIDER PLOTLY ####################

# v_CA21_4307 % of owner households spending 30% or more of its income on shelter costs
# v_CA21_4309 Median monthly shelter costs for owned dwellings ($)
# v_CA21_4310 Average monthly shelter costs for owned dwellings ($)
# v_CA21_4315 % of tenant households spending 30% or more of its income on shelter costs
# v_CA21_4317 Median monthly shelter costs for rented dwellings ($)
# v_CA21_4318 Average monthly shelter costs for rented dwellings ($)
###

# TODO it will look better on mobile to put the title in the markdown (or html append?) instead of from plotly
v_CA21_4290_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                              axisStr = "v_CA21_4290: Number of households spending +30% of income on shelter",
                              varVect = tor_census_df_na_zero$v_CA21_4290,
                              varStr = 'v_CA21_4290')

v_CA21_4310_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                                                       axisStr = "v_CA21_4310: Average monthly shelter costs for owned dwellings ($)",
                                                       varVect = tor_census_df_na_zero$v_CA21_4310,
                                                       varStr = 'v_CA21_4310')

v_CA21_4307_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                                                       axisStr = "v_CA21_4307: % of owner households spending 30% or more of its income on shelter costs",
                                                       varVect = tor_census_df_na_zero$v_CA21_4307,
                                                       varStr = 'v_CA21_4307')

v_CA21_4318_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                                                       axisStr = "v_CA21_4318: Average monthly shelter costs for rented dwellings ($)",
                                                       varVect = tor_census_df_na_zero$v_CA21_4318,
                                                       varStr = 'v_CA21_4318')

v_CA21_4315_plotly_sorted_bar <- make_plotly_sortedbar(df = tor_census_df_na_zero,
                                                       axisStr = "v_CA21_4315: % of tenant households spending 30% or more of its income on shelter costs",
                                                       varVect = tor_census_df_na_zero$v_CA21_4315,
                                                       varStr = 'v_CA21_4315')

# Results of these function calls
v_CA21_4307_plotly_sorted_bar
v_CA21_4310_plotly_sorted_bar
v_CA21_4318_plotly_sorted_bar
# Showing these three in appendix
v_CA21_4290_plotly_sorted_bar
v_CA21_4315_plotly_sorted_bar
################################################################################


############# Appendix (not in report)
########## FUNCTION Plotly bar chart with filterable traces ####################
# Interesting result but NOT INCLUDING in report since output is huge and slow
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
########## USING Plotly bar chart with filterable traces ####################
# Graph 1 of this type
avg_montly_rent_plotly <- make_plotly_sortedbar_filter(
  df = tor_census_df_no_geom,
  axisStr = 'v_CA21_4317: Average Monthly Rent ($CAD)',
  varVect = tor_census_df_no_geom$v_CA21_4317,
  varStr = 'v_CA21_4317'
)
avg_montly_rent_plotly
# Graph 2
v_CA21_4290_plotly <- make_plotly_sortedbar_filter(
  df = tor_census_df_no_geom,
  axisStr = "Households spending 30% or more of income on shelter costs",
  varVect = tor_census_df_no_geom$v_CA21_4290,
  varStr = 'v_CA21_4290'
)
###############################################################################




