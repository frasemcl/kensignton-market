library(tidyverse)
library(sf)
library(leaflet)
library(data.table)


#This script is used to name Census Tract based on neighbourhoods that they fall in. 
# Next I may aggregate results to neighbourhood level, but for now this is interesting!


###RESOURCES
# https://geocompr.robinlovelace.net/spatial-operations.html
# https://r-spatial.github.io/sf/articles/
# https://r-spatial.github.io/sf/reference/geos_binary_ops.html
# Potentially: https://tmieno2.github.io/R-as-GIS-for-Economists/spatial-intersection-transformative-join.html


# Create geographic centre points (centroids) of all CTs around Toronto
ct_centroids <- st_centroid(census_data) %>%
  select(name, geometry)

# check these on the map and determine if any centroids won't fall in a neighbourhood
# make_leaflet_3(df=census_data,
#                label=census_data$name,
#                group1='Census Tracts',
#                df2=nhood_data,
#                label2=nhood_data$AREA_NAME,
#                group2='Neighbourhoods',
#                df3=ct_centroids,
#                group3='CT Centroids')

# Visiual inspection of the map above shows the following fall outside any neighbourhood (in the water)
# Correction points were established using google (just inland from the anomalies)
new_point_0003.00 <- st_point(c(-79.453501, 43.638184)) %>% 
  st_sfc(crs = 4283)
new_point_0210.04 <- st_point(c(-79.477144, 43.626379)) %>%
  st_sfc(crs = 4283)
new_point_0200.01 <- st_point(c(-79.485422, 43.617952)) %>% 
  st_sfc(crs = 4283)
ct_centroids$geometry[ct_centroids$name=='0003.00']=new_point_0003.00
ct_centroids$geometry[ct_centroids$name=='0210.04']=new_point_0210.04
ct_centroids$geometry[ct_centroids$name=='0200.01']=new_point_0200.01

# Return the intersection of these centroids and GTA neighbourhoods
ct_centroids_named <- st_intersection(ct_centroids, nhood_data) %>% 
  select(name, AREA_NAME)

# This table could be useful to share as a CSV on github
ct_centroids_no_geom <- ct_centroids_named %>% 
  st_drop_geometry()

# Merge these names to the input census tracts, after filtering
tor_census_df <- census_data %>% 
  filter(name %in% ct_centroids_no_geom$name)
tor_census_df <- merge(tor_census_df, ct_centroids_no_geom, all.y=TRUE)

# Reformat this a little
tor_census_df <- tor_census_df %>%
  mutate(name_concat = paste0(name, '_', AREA_NAME)) %>% 
  select(name, AREA_NAME, name_concat, everything())


# map the results then results and results from previous script
# make_leaflet(tor_census_df, tor_census_df$name_concat)

make_leaflet_2(df=tor_census_df, 
               label=tor_census_df$name_concat,
               group1='Census Tracts',
               df2=nhood_data, 
               label2=nhood_data$AREA_NAME,
               group2='Neighbourhoods')

make_leaflet_3(df=tor_census_df, 
                label=tor_census_df$name_concat,
                group1='Census Tracts',
                df2=nhood_data, 
                label2=nhood_data$AREA_NAME,
                group2='Neighbourhoods',
                df3=ct_centroids_named,
                group3='CT Centroids')


# The following is to export CSVs to KMCLT
# Not required for upcoming data visualizations
# SELECT kensington CT only, transpose it for legibility, rename the column
# Flipped from wide to long
kensingtonCensusData <- st_drop_geometry(tor_census_df %>% 
  filter(name_concat=='0038.00_Kensington-Chinatown'))
kensingtonCensusData <- as.data.frame(t(kensingtonCensusData))
colnames(kensingtonCensusData) <- c('KensingtonMarket_CensusTract_Select2021Data_forKMCLT')
### Couldn't get this working without bringing in data.table library
kensingtonCensusData <- setDT(kensingtonCensusData, keep.rownames = TRUE)[]


#Export Kensington data, and the large dataframe containing all Toronto Census Tracts (and a few outside Toronto)
write_csv(kensingtonCensusData, './output/kensington21CensusData.csv')

#last little cleanup
tor_census_df_no_geom <- st_drop_geometry(tor_census_df) %>% 
  select(!c("Shape Area", "Quality Flags"))

write_csv(tor_census_df_no_geom, './output/toronto21CensusTractData.csv')





