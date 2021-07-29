library(tidyverse)
library(readxl)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(geobr)
library(cancensus)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world_cities <- read_csv("worldcities.csv",
                         col_types="ccddcccccdd")
world_cities
world_cities <- world_cities %>%
  st_as_sf(coords=c("lng",
                    "lat"))

#brazilian municipalities that may be missing

estados <- c("PR","BA","SP","RJ","MT","RS","RN","AM","PA","RR","RO","PE","PB","AC","AL","AP","AM","CE","DF","ES",
             "GO","MA","MS","MG","PI","SC","SE","TO")

BR_municipalities <- estados %>%
  map_df(read_municipality, 2020)

BR_municipalities <- BR_municipalities %>%
  filter(str_detect(name_muni, "San |São|Santo|Santa" )) %>%
  rename("city" = "name_muni",
         "geometry"="geom",
         "admin_name"="name_state") %>%
  mutate(country="Brazil")

BR_municipalities_crs <-
BR_municipalities %>% st_transform(4326) %>%
  st_as_sf(coords=c("lon","lat"))

#opening Canadian data

set_api_key('CensusMapper_ca1b48734e6d7631234d322430518e1b', install = T)

census_data <- get_census(dataset='CA16', regions=list(CMA="59933"),
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
                          level='CSD', use_cache = FALSE, geo_format = 'sf')

test <-get_census(dataset = 'CA16',regions = list(CMA=c("59933","35535")),
                  level = "CSD",use_cache = F,geo_format = "sf")
  
test<-  
list_census_regions('CA16') %>% 
  filter(level == "CSD")

census_data <- get_census(dataset='CA16', regions=list(CMA=c("59933","35535")),
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
                          level='CSD', use_cache = FALSE)

#opening US data
library(tigris)

urb <- urban_areas(2020)

urb<-
urb %>%   filter(str_detect(NAME10, "San |St. |Saint|São|Santo|Santa")) %>%
  rename("city" = "NAME10") %>%
  mutate(country="United States")

#uruguay
library(geouy)

ciud <- 
load_geouy("Municipios")

ciud <-ciud %>%
  mutate(city = as.logical(NOMBRE))

ciud<-
ciud %>%
  filter(str_detect(NOMBRE, "SAN |SANTO |SANTA ")) %>%
  rename("city"="NOMBRE",
         "geometry"="SHAPE") %>%
  mutate(country="Uruguay")

ciud <-
ciud %>%
  mutate(city = case_when(city == "SANTA ROSA"~"Santa Rosa",
                          city == "SANTA LUCÍA"~"Santa Lucia",
                          city =="SAN RAMÓN"~"San Ramon",
                          city =="SAN JACINTO"~"San Jacinto",
                          city =="SAN BAUTISTA"~"San Bautista",
                          city =="SAN ANTONIO"~"San Antonio",
                          city =="SAN JAVIER"~"San Javier",
                          city =="PUEBLO SAN ANTONIO"~"Pueblo San Antonio",
                          city =="SAN GREGORIO DE POLANCO"~"San Gregorio de Polanco",
                          city =="SANTA CLARA"~"Santa Clara",
                          city =="SAN CARLOS"~"San Carlos"))

uruguay_crs <-
  ciud %>% st_transform(4326) %>%
  st_as_sf(coords=c("lon","lat"))

#creating a columns for continent in world cities
world_cities <- world_cities %>%
  mutate(continent=case_when(country=="Canada"~"America",
                             country=="United States"~"America",
                             country=="Mexico"~"America",
                             country=="Costa Rica"~"America",
                             country=="Nicaragua"~"America",
                             country=="El Salvador"~"America",
                             country=="Cuba"~"America",
                             country=="Belize"~"America",
                             country=="Honduras"~"America",
                             country=="Guatemala"~"America",
                             country=="Panama"~"America",
                             country=="Haiti"~"America",
                             country=="Dominican Republic"~"America",
                             country=="Jamaica"~"America",
                             country=="Puerto Rico"~"America",
                             country=="Cayman Islands"~"America",
                             country=="Aruba"~"America",
                             country=="Guadeloupe"~"America",
                             country=="Martinique"~"America",
                             country=="Colombia"~"America",
                             country=="Brazil"~"America",
                             country=="Argentina"~"America",
                             country=="Venezuela"~"America",
                             country=="Bolivia"~"America",
                             country=="Chile" ~"America",
                             country=="Uruguay"~"America",
                             country=="Paraguay"~"America",
                             country=="Equador"~"America",
                             country=="Guyana"~"America",
                             country=="Suriname"~"America",
                             country=="Peru"~"America"))
#filtering for cities starting with Saint names in the american continent
america_st_cities <- world_cities %>%
  filter(str_detect(city, "San |St. |Saint|São|Santo|Santa" ) &
           continent == "America")

#establishing the origin
america_st_cities <- america_st_cities %>%
  mutate(origin=case_when(str_detect(city,"St. ")~"English",
                          str_detect(city,"San")~"Spanish",
                          str_detect(city,"Saint")~"French",
                          str_detect(city,"São")~"Portuguese",
                          str_detect(city,"Santo")~"Spanish",
                          str_detect(city, "Santa")~"Spanish"))

america_st_cities <- america_st_cities %>%
  mutate(origin=case_when(city %in% c("Santo André",
                                      "Santos",
                                      "Santo Agostinho",
                                      "Santo Antônio de Jesus",
                                      "Santo Ângelo",
                                      "Santo Antônio de Posse",
                                      "Santo Anastácio",
                                      "Santo Antônio do Pinhal",
                                      "Santo Antônio da Alegria",
                                      "Santo Antônio do Jardim",
                                      "Feira de Santana",
                                      "Santa Maria",
                                      "Santarém",
                                      "Santa Bárbara d’Oeste",
                                      "Santana de Parnaíba",
                                      "Santa Cruz do Sul",
                                      "Santana",
                                      "Santa Inês",
                                      "Santa Rosa",
                                      "Santa Vitória do Palmar",
                                      "Santa Maria da Vitória",
                                      "Santana do Livramento",
                                      "Santa Cruz do Rio Pardo",
                                      "Santa Cruz das Palmeiras",
                                      "Santa Fé do Sul",
                                      "Santa Rita do Passa Quatro",
                                      "Santa Rosa de Viterbo",
                                      "Santa Gertrudes",
                                      "Santa Cruz Cabrália",
                                      "Santa Adélia",
                                      "Santa Branca",
                                      "Santa Lúcia",
                                      "Santa Albertina",
                                      "Santa Bárbara do Rio Pardo",
                                      "Santa Maria da Serra",
                                      "Santa Ernestina")~"Portuguese",
                          TRUE~origin))

america_st_cities <- america_st_cities %>%
  mutate(origin=case_when(city == "Santa Cruz" &
                            country == "Brazil" ~ "Portuguese",
                          TRUE~origin))

america_st_cities <- america_st_cities %>%
  mutate(origin=case_when(city %in% c("Santa Rosa",
                                      "Santa Maria") &
                            country == "United States" ~ "Spanish",
                          city == "Santa Rosa" &
                            country == "Argentina" ~ "Spanish",
                          TRUE~origin))

#america_st_cities %>%
  #ggplot() +
#  geom_sf(aes(color=origin),
   #       size=0.5)

america_st_cities_crs <- america_st_cities %>% st_set_crs(4326)

BR_municipalities_crs <- BR_municipalities_crs %>%
  select("city","country", "geometry","admin_name")

uruguay_crs <-  uruguay_crs %>% select("city", "geometry")

america_st_cities_crs <- bind_rows(america_st_cities_crs,BR_municipalities_crs, uruguay_crs)

america_st_cities_crs <-
america_st_cities_crs %>%
  replace_na(list(origin="Portuguese"))

america_st_cities_crs <- 
  america_st_cities_crs %>%
  mutate(origin=case_when(city == "Santa Rosa" ~ "Spanish",
                          city == "Santa Lucia" ~ "Spanish",
                          city == "San Ramon" ~ "Spanish",
                          city == "San Jacinto" ~ "Spanish",
                          city == "San Bautista" ~ "Spanish",
                          city == "San Antonio" ~ "Spanish",
                          city == "San Javier" ~ "Spanish",
                          city == "Pueblo San Antonio" ~ "Spanish",
                          city == "San Gregorio de Polanco" ~ "Spanish",
                          city == "Santa Clara" ~ "Spanish",
                          city == "San Carlos"~ "Spanish",
                            TRUE~origin))

st_cities_americas <-
ggplot(data = world) +
    geom_sf()+
  geom_sf(data = america_st_cities_crs,
            aes(color=origin),
          size=0.5) +
  coord_sf(xlim=c(-168.750000,-27.070313),
           ylim=c(-57.326521,72.711903),
           expand = F)+
    ggtitle("Cities in the American Continent named after Saints") +
    theme_minimal()

st_cities_americas
