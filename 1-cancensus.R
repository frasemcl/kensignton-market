library(tidyverse)
library(sf)
library(leaflet)
library(viridis)
library(cancensus)


# api_key saved in .Rprofile. You will need to download your own as outlined in 'cancensus' docs
api_key <- Sys.getenv("CM_API_KEY")

# Setup credentials for cancensus 
options(cancensus.api_key = api_key)
options(cancensus.cache_path = "./cache")

#Query for avail datasets
availDatasets <- list_census_datasets()

# To view available named regions at different levels of Census hierarchy for the 2021 Census in this case
regions21 <- list_census_regions("CA21")

# To view available Census variables for the 2021 Census
variables21 <- list_census_vectors("CA21")

# Long list of census vars used for this analysis carefully picked out using the tool here: https://censusmapper.ca/api#api_overview
VECTOR_IDS <- c("v_CA21_1","v_CA21_2","v_CA21_3","v_CA21_4","v_CA21_5","v_CA21_6","v_CA21_7","v_CA21_8","v_CA21_386","v_CA21_389","v_CA21_434","v_CA21_435","v_CA21_436","v_CA21_437","v_CA21_438","v_CA21_439","v_CA21_440","v_CA21_441","v_CA21_442","v_CA21_443","v_CA21_449","v_CA21_452","v_CA21_513","v_CA21_531","v_CA21_543","v_CA21_554","v_CA21_557","v_CA21_560","v_CA21_561","v_CA21_562","v_CA21_563","v_CA21_564","v_CA21_565","v_CA21_567","v_CA21_568","v_CA21_571","v_CA21_574","v_CA21_570","v_CA21_573","v_CA21_569","v_CA21_572","v_CA21_577","v_CA21_580","v_CA21_576","v_CA21_579","v_CA21_575","v_CA21_578","v_CA21_583","v_CA21_586","v_CA21_582","v_CA21_585","v_CA21_581","v_CA21_584","v_CA21_589","v_CA21_592","v_CA21_588","v_CA21_591","v_CA21_587","v_CA21_590","v_CA21_595","v_CA21_598","v_CA21_594","v_CA21_597","v_CA21_593","v_CA21_596","v_CA21_905","v_CA21_906","v_CA21_907","v_CA21_923","v_CA21_924","v_CA21_925","v_CA21_926","v_CA21_927","v_CA21_928","v_CA21_929","v_CA21_930","v_CA21_931","v_CA21_932","v_CA21_933","v_CA21_934","v_CA21_935","v_CA21_936","v_CA21_937","v_CA21_938","v_CA21_939","v_CA21_964","v_CA21_965","v_CA21_966","v_CA21_967","v_CA21_1012","v_CA21_1011","v_CA21_1010","v_CA21_1057","v_CA21_1056","v_CA21_1055","v_CA21_1139","v_CA21_1140","v_CA21_1141","v_CA21_1142","v_CA21_1143","v_CA21_4237","v_CA21_4238","v_CA21_4239","v_CA21_4240","v_CA21_4241","v_CA21_4242","v_CA21_4243","v_CA21_4257","v_CA21_4258","v_CA21_4259","v_CA21_4260","v_CA21_4261","v_CA21_4262","v_CA21_4263","v_CA21_4264","v_CA21_4265","v_CA21_4266","v_CA21_4267","v_CA21_4268","v_CA21_4269","v_CA21_4270","v_CA21_4271","v_CA21_4272","v_CA21_4273","v_CA21_4274","v_CA21_4275","v_CA21_4276","v_CA21_4277","v_CA21_4278","v_CA21_4288","v_CA21_4289","v_CA21_4290","v_CA21_4292","v_CA21_4293","v_CA21_4294","v_CA21_4295","v_CA21_4296","v_CA21_4297","v_CA21_4298","v_CA21_4299","v_CA21_4300","v_CA21_4301","v_CA21_4302","v_CA21_4303","v_CA21_4304","v_CA21_4305","v_CA21_4306","v_CA21_4307","v_CA21_4308","v_CA21_4309","v_CA21_4310","v_CA21_4311","v_CA21_4312","v_CA21_4313","v_CA21_4314","v_CA21_4315","v_CA21_4316","v_CA21_4317","v_CA21_4318","v_CA21_4319","v_CA21_4320","v_CA21_4321","v_CA21_4322")


# Returns data and geography as an sf-class data frame for all Census Tracts in Toronto Census Metropolitan Area
# Careful, this is a pretty big API call but it works. Won't work if you go too big it seems
census_data <- get_census(
  dataset='CA21', 
  regions=list(CMA="35535"), 
  vectors=VECTOR_IDS, 
  labels="detailed",
  geo_format='sf', 
  level='CT'
  )

# map CTs (not needed, just a check):
# map_CTs <- make_leaflet(census_data, label=census_data$name)
# map_CTs

