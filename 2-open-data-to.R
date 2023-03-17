library(opendatatoronto)
library(tidyverse)
library(sf)
library(leaflet)

###RESOURCES
# https://open.toronto.ca/dataset/neighbourhood-profiles/
# https://www.toronto.ca/city-government/data-research-maps/neighbourhoods-communities/neighbourhood-profiles/find-your-neighbourhood/neighbourhood-profile-detail/?id=NeighbourhoodProfiles-CityofToronto/Snapshot110&title=Neighbourhood%20Profile%20Data#type=filtered&filter=Select+a+Neighbourhood&value=Kensington-Chinatown%20(78)
# https://www.toronto.ca/wp-content/uploads/2022/04/973a-Neighbourhood-Profiles-2016-Overview.pdf
# https://onlinelibrary.wiley.com/doi/epdf/10.1111/cag.12467
###USED HERE
#https://open.toronto.ca/dataset/neighbourhoods/

# get package
package <- show_package("neighbourhoods")
package

# get all resources for this package
resources <- list_package_resources("neighbourhoods")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
nhood_data <- filter(datastore_resources, row_number()==1) %>% get_resource()

#test map them
mapNHs <- make_leaflet(nhood_data, label=nhood_data$AREA_NAME)





