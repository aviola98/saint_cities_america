library(tidyverse)
library(readxl)
library(sf)

world_cities <- read_csv("worldcities.csv",
                         col_types="ccddcccccdd")
world_cities
world_cities <- world_cities %>%
  st_as_sf(coords=c("lng",
                    "lat"))

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

america_st_cities <- world_cities %>%
  filter(str_detect(city, "San |St. |Saint|São|Santo|Santa" ) &
           continent == "America")

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

america_st_cities %>%
  ggplot() +
  geom_sf(aes(color=origin),
          size=0.5)

world <- st_read("4a7d27e1-84a3-4d6a-b4c2-6b6919f3cf4b202034-1-2zg7ul.ht5ut.shp")


world %>%
  st_transform(4326) %>%
  filter(CONTINENT %in% c("North America","South America")) %>%
  ggplot() +
  geom_sf(colour="#756bb1") +
  geom_sf(data = america_st_cities %>%
            st_transform(4326),
          color="origin",
          size=2)