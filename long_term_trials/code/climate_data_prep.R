source("code/libraries.R")

load("data/d.carbon.RData")
load("data/d.yield.RData")

d.yield <- d
rm(d)

d.yield %>%
  group_by(Paper, Study_name, begin_obs, end_obs) %>%
  count() %>%
  select(Paper, Study_name, begin_obs, end_obs) -> yield.climate

d.carbon %>%
  group_by(Paper, Study_name, Year_of_observation) %>%
  count() %>%
  select(Paper, Study_name, Year_of_observation) -> carbon.climate

# Need to clean up location names to searchable formats; default to city, country
# with more specificity if available

yield.climate <- yield.climate %>%
  mutate(Location = Study_name) %>%
  mutate(Location = str_replace_all(Location, 
                                c("Makoka Agricultural Research Station" = "Thondwe, Malawi",
                                  "Bulbertson" = "Culbertson",
                                  "Pietranera farm, Universita degli Studi di Palermo" = "Pietranera farm, Agrigento, Sicily, Italy",
                                  "Research Station of the Riograndense Rice Institute \\(IRGA\\), Cachoeirinha City, Rio Grande do Sul State, Southern Brazil" = "Cachoeirinha City, Rio Grande do Sul State, Brazil",
                                  "Purdue University Agronomy Center for Research and Education \\(ACRE\\), West Lafayette, Indiana" = "West Lafayette, IN",
                                  "La Canaleja \\(INIA\\), " = "",
                                  "USDA-ARS Conservation and Production Research Laboratory, " = "",
                                  "USDA-ARTS Central Great Plains Research Station, " = "",
                                  "Loess Plateau, Chenghuang village, " = "",
                                  "University Farm, Institue for Sustainable Agro-ecosystem Services, " = "",
                                  "Southern Illinois University Belleville Research Center, " = "",
                                  "Mississippi Agricultural and Forestry Experiment Station at Holly Springs" = "Holly Springs, MS",
                                  "Macdonald Campus Research Farm, McGill University, " = "",
                                  "Southeast Norway, morainic loam soil" = "Hedmark, Norway",
                                  "USDA-ARS J. Phil Campbell Sr. Natural Resource Conservation Center, " = "",
                                  "No name" = "Clay Center, NE", # Based on statement in paper that manure was transported from nearby US Meat Animal Research Center
                                  "La Chimenea field Station, " = "",
                                  "Columbia Basin Agricultural Research Center, " = "",
                                  "Yadkin Valley Vineyard, North Carolina, USA" = "New Hope, NC",
                                  "Hayathnagar Research Farm, Central Research Institute for Dryland Agriculture, " = "",
                                  "Macdonald Research Farm of McGill University, Ste-Anne de Beelevue, " = "Ste-Anne-de-Bellevue, ",
                                  "Hyndevad, Jutland, Denmark \\s+Odum, Jutland, Denmark" = "Jutland, Denmark",
                                  "Shang Tuhe village, " = "",
                                  "Instituto Madrileno de Investigacion y Desarollo Rural Agrario y Alimentario Experimental Station, " = "",
                                  "Farm of Southwest Agricultural University, " = "",
                                  "Beiqiu, " = "",
                                  "Research farm, Rajendra Agricultural University, " = "",
                                  "Haycreek Township, Goodhue Country, Minnesota, USA" = "Red Wing, MN",
                                  "Clemson University Research and Education Center, " = "",
                                  "Iowa State University \\(ISU\\) Agronomy/Ag Engineering Research and Education Center, Boone County, Iowa, USA" = "Boone County, IA",
                                  "Research farm, International Institute of Tropical Agriculture \\(IITA\\), Ihadan, " = "Ibadan, ",
                                  "Tidewater Research Station, " = "",
                                  "International Center for Agricultural Research in the Dry Areas \\(ICARDA\\), Tel Hadya, " = "",
                                  "Oklahoma Pandhandle Research and Extension Center \\(OPREC\\), " = "",
                                  "Northwestern Agricultural Research Station, " = "",
                                  "Northwestern Branch, " = "",
                                  "Ohio Agricultural Research and Development Center, " = "",
                                  "Waterman Farm, " = "",
                                  "Site = Agramunt" = "Agramunt, Catalonia, Spain",
                                  "Site = El Canos" = "El Canos, Catalonia, Spain",
                                  "Site = Selvanera" = "Selvanera, Catalonia, Spain",
                                  "University of Minnesota Agricultural Experiment Station, " = "",
                                  "Palouse Conservation Field Station, " = "",
                                  "Shiwang Village, " = "",
                                  "Enrico Avanzi Interdepartmental Center for Agro-Environmental Research, " = "",
                                  "Agriculture and Agri-Food Canada Research Centre, " = "",
                                  "Cantuar, " = "Swift Current, ",
                                  "Horticultural Experiment Station, " = "",
                                  "L'Acadie Research Station, Agriculture and Agri-Food Canada" = "St-Jean-sur-Richelieu, Quebec",
                                  "Swedish University of Agricultural Sciences, " = "",
                                  "Eastern South Dakota Soil and Water Research Farm, " = "",
                                  "Rasmussen dryland farm site, " = "",
                                  "Regional Agricultural Research Station, Bhairhawa, " = "Bhairahawa, ",
                                  "Baden-Wurttemberg site" = "Forchtenberg, Germany",
                                  "Fire" = "Forchtenberg, Germany",
                                  "Forchtenberg site" = "Forchtenberg, Germany",
                                  "Stagnic Luvisol" = "Forchtenberg, Germany",
                                  "Haplic Luvisol" = "Forchtenberg, Germany",
                                  "Fort Ellis Research and Extension Center, Montana State University, " = "",
                                  "Hisar" = "Hisar, India",
                                  "Solapur" = "Solapur, India",
                                  "Eastern site" = "Horse Heaven, WA",
                                  "Western site" = "Horse Heaven, WA",
                                  "University of Nebraska Agricultural Research and Development Center, " = "",
                                  "Tennessee Valley Research and Extension Center, Alabama Agriculture Experiment Station, " = "",
                                  "Taihu Lake region, China" = "Taihu Lake, China",
                                  "Asasa research stations, south-eastern highlands, Ethiopia" = "Asassa, Ethiopia",
                                  "Kulumsa research stations, south-eastern highlands, Ethiopia" = "Kulumsa, Ethiopia",
                                  "Shenmu erosion and environmental testing station, Institute of Soil and Water Conservation, Shen Mu county, " = "",
                                  "Chilato village, Zimuto Communal Area \\(Chikato\\), " = "",
                                  "Henderson Research Station, " = "",
                                  "Hereford Farm, " = "",
                                  "National Agriculture Research Center, " = "",
                                  "ADAS Bridgets, " = "",
                                  "ADAS Drayton, " = "",
                                  "ADAS High Mowthorpe, " = "",
                                  "ADAS Terrington, " = "",
                                  "Campo Experimental Norman E. Borlaug, " = "",
                                  "University of California Coachella Valley Agricultural Research Station, " = "",
                                  "Sand Mountain Research and Extension Center, Appalachian Plateau region, " = "",
                                  "Southern Loess Plateau" = "Zhouzhi, Xi'an, Shaanxi, China",
                                  "L'Acadie Experimental Farm of Agriculture and Agri-Food Canada, " = "St-Jean-sur-Richelieu, ")))

carbon.climate <- carbon.climate %>%
  mutate(Location = Study_name) %>%
  mutate(Location = str_replace_all(Location,
                                   c("Oklahoma Pandhandle Research and Extension Center \\(OPREC\\), " = "",
                                     "Southeast Norway, morainic loam soil" = "Hedmark, Norway",
                                     "La Chimenea field Station, " = "",
                                     "Columbia Basin Agricultural Research Center, " = "",
                                     "Shang Tuhe village, " = "",
                                     "Farm of Southwest Agricultural University, " = "",
                                     "Beiqiu, " = "",
                                     "Research farm, Rajendra Agricultural University, " = "",
                                     "Clemson University Research and Education Center, " = "",
                                     "Northwestern Agricultural Research Station, " = "",
                                     "Ohio Agricultural Research and Development Center, " = "",
                                     "Waterman Farm, " = "",
                                     "Pietranera farm, Universita degli Studi di Palermo" = "Pietranera farm, Agrigento, Sicily, Italy",
                                     "Enrico Avanzi Interdepartmental Center for Agro-Environmental Research, " = "",
                                     "L'Acadie Research Station, Agriculture and Agri-Food Canada" = "St-Jean-sur-Richelieu, Quebec",
                                     "Eastern South Dakota Soil and Water Research Farm, " = "",
                                     "Regional Agricultural Research Station, Bhairhawa, " = "Bhairahawa, ",
                                     "University of Nebraska Agricultural Research and Development Center, " = "",
                                     "Shenmu erosion and environmental testing station, Institute of Soil and Water Conservation, Shen Mu county, " = "",
                                     "Chilato village, Zimuto Communal Area \\(Chikato\\), " = "",
                                     "Henderson Research Station, " = "",
                                     "Hereford Farm, " = "",
                                     "ADAS Bridgets, " = "",
                                     "ADAS Drayton, " = "",
                                     "ADAS High Mowthorpe, " = "",
                                     "ADAS Terrington, " = "",
                                     "Southern Loess Plateau" = "Zhouzhi, Xi'an, Shaanxi, China"
                                     )))
  
  
yield.climate %>%
  group_by(Paper, Study_name, Location) %>%
  select(Paper, Study_name, Location) %>%
  distinct(Paper, Study_name, Location) -> yield.locations


yield.locations$lat <- NA
yield.locations$lon <- NA


get_geo <- function(study_name){
  result <- geocode_OSM(study_name, return.first.only = TRUE, as.data.frame = TRUE)
  return(result)
}

# Takes awhile

for (i in 1:nrow(yield.locations)){
  geo <- try(get_geo(yield.locations$Location[i]))
  if(inherits(geo, "try-error"))
  {
    next
  }
  else
  {
    yield.locations$lat[i] <- geo["lat"]
    yield.locations$lon[i] <- geo["lon"]
  }
}

yield.locations$lat[which(yield.locations$Location=="Cangwu Agri-ecological Station of the Loess Plateau, China")] <- 35.200
yield.locations$lon[which(yield.locations$Location=="Cangwu Agri-ecological Station of the Loess Plateau, China")] <- 107.667

yield.locations$lat <- as.numeric(as.character(yield.locations$lat))
yield.locations$lon <- as.numeric(as.character(yield.locations$lon))

# new data

d.yield.new <- read.xlsx("data/AgEvidence_Oldfield_selected.xlsx", sheet = "yield")

d.yield.new <- d.yield.new %>%
  filter(Paper != "Campbell et al. 2007")

new.locations <- d.yield.new %>%
  select(Paper, lat, lon) %>%
  unique()

new.locations$Study_name <- NA
new.locations$Location <- NA

yield.locations <- yield.locations %>%
  bind_rows(new.locations)

yield.locations %>%
  group_by(Paper, Study_name, Location, lat, lon) %>%
  unique() -> yield.locations



## Agro-ecological zones
## Data from http://www.fao.org/geonetwork/srv/en/metadata.show?id=30589&currTab=simple

library(rgdal)
library(data.table)

GDALinfo("data/thcli/thcli/hdr.adf")

aez <- readGDAL("data/thcli/thcli/hdr.adf")

image(aez)

coordinates(aez)[1:5,]
aez[1:5,]
 
aez.test1 <- as.matrix(aez) 
which(!is.na(aez@data))

# Code adapted from 
# https://stackoverflow.com/questions/18073861/r-matching-coordinates-from-one-large-data-frame-into-grid-cells-from-another

  # Match locations to SGDF first by lon/x

grid <- data.table(coordinates(aez))
grid$grid.id <- row.names(grid)
grid <- grid[, grid.x := x]
grid <- grid[, grid.y := y]
setkey(grid, x)

points <- data.table(yield.locations)
points <- points[, lat.og := lat]
points <- points[, lon.og := lon]
setkey(points, lon)


intermediate <- grid[points, roll = Inf][, list(Paper, Study_name, Location, grid.x, lat, lon.og, lat.og)]

  # Then match by lat/y

setkey(intermediate, grid.x, lat)
setkey(grid, x, y)

grid.id.df <- grid[intermediate, roll = "nearest"][, list(Paper, Study_name, Location, x, grid.y, lon.og, lat.og, grid.id)]

  # Get data for each location based on grid id

grid.id.df$grid.id <- as.numeric(grid.id.df$grid.id)
grid.id.df %>%
  mutate(AEZ.no = aez@data$band1[grid.id]) -> grid.id.df

  # Update names of AEZ based on grid number (derived from legend and general deduction from "image(aez)")

grid.id.df %>%
  mutate(AEZ = case_when(AEZ.no == 1 ~ "Tropics",
                         AEZ.no == 2 ~ "Subtropics (summer rainfall)",
                         AEZ.no == 3 ~ "Subtropics (winter rainfall)",
                         AEZ.no == 4 ~ "Temperate (oceanic)",
                         AEZ.no == 5 ~ "Temperate (sub-continental)",
                         AEZ.no == 6 ~ "Temperate (continental)",
                         Location == "Prince Edward Island" ~ "Temperate (sub-continental)")) -> grid.id.df

  # Clean up locations dataframe

names(grid.id.df)[6:7] <- c("lon", "lat")
yield.locations <- left_join(yield.locations, grid.id.df, by = c("Paper", "Study_name", "Location", "lat", "lon"))
yield.locations <- yield.locations[,-c(6:9)]

save(yield.locations, file = "data/locations.RData")


