library(tidyverse)
library(sf)
library(leaflet)
library(data.table)


#This script is used to name Census Tract based on neighbourhoods that they fall in. If there is a better way to do this, please let me know:)

###RESOURCES
# https://geocompr.robinlovelace.net/spatial-operations.html
# https://r-spatial.github.io/sf/articles/
# https://r-spatial.github.io/sf/reference/geos_binary_ops.html

# Run this once, it takes a while
ct_nh_inters <- st_intersection(census_data, nhood_data)

nh_names_to_merge<- st_drop_geometry(ct_nh_inters %>% 
  group_by(name) %>% 
  mutate(ct_nhoods=paste0(AREA_NAME, sep = ', ', collapse = '')) %>% 
  select(name, ct_nhoods))
#get rid of pesky commas
nh_names_to_merge$ct_nhoods <- substr(nh_names_to_merge$ct_nhoods,1,nchar(nh_names_to_merge$ct_nhoods)-2)
#make the df distinct rows only
nh_names_to_merge <- distinct(nh_names_to_merge)

#Moving on, we will only be working with the Census Tracts that interected with Toronto Neighbourhoods
toronto_census_df <- merge(census_data, nh_names_to_merge, all.y=TRUE)

#### NAME OUR CENSUS TRACT OF INTEREST ####
# Results are imperfect becuase I believe any neighbourhood that a tract borders
# (or of course overlaps) will be included in the new custom label
toronto_census_df <- within(toronto_census_df, ct_nhoods[name == '0038.00'] <- 'KENSINGTON MARKET')
# Reformat this one a litt
toronto_census_df <- toronto_census_df %>%
  select(ct_nhoods, everything()) %>% 
  rename(Census_Tract_neighbourhoods = ct_nhoods) %>%
  select(name, everything()) %>% 
  rename(CENSUS_TRACT_NAME = name)

#map the results
# make_leaflet(toronto_census_df, toronto_census_df$ct_nhoods)

# SELECT kensington CT only, transpose it for legibility, rename the column
# Can someone please tell me how to flip this from wide to long in a nicer way?
kensingtonCensusData <- st_drop_geometry(toronto_census_df %>% 
  filter(neighbourhoods_touching_this_census_tract=='KENSINGTON MARKET'))
kensingtonCensusData <- as.data.frame(t(kensingtonCensusData))
colnames(kensingtonCensusData) <- c('KensingtonMarket_CensusTract_Select2021Data_forKMCLT')
### Couldn't get this working without bringing in data.table library
kensingtonCensusData <- setDT(kensingtonCensusData, keep.rownames = TRUE)[]



#Export Kensington data, and the large dataframe containing all Toronto Census Tracts (and a few outside Toronto)
write_csv(kensingtonCensusData, './output/kensington21CensusData.csv')

#last little cleanup
toronto_census_df <- st_drop_geometry(toronto_census_df) %>% 
  select(!c("Shape Area", "Quality Flags"))

write_csv(toronto_census_df, './output/toronto21CensusTractData.csv')
