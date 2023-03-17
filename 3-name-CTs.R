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


# Fix or delete
make_leaflet_3(df=nhood_data, df2=test2, group='test', group2='test2', label1 = nhood_data$AREA_NAME)








# map the results then results and results from previous script
make_leaflet(tor_census_df, tor_census_df$name_concat)

# TODO clean up bug where sunnyside gets no nhood. Or is there just nothing there?

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
tor_census_df <- st_drop_geometry(tor_census_df) %>% 
  select(!c("Shape Area", "Quality Flags"))

write_csv(tor_census_df, './output/toronto21CensusTractData.csv')





