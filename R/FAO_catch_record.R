library(tidyverse)

# create a plot for historical catch record from NPFC convention areas by countries

# download data from FAO

# read the data
FAO_catch <- read.csv("data/FAO/CAPTURE_QUANTITY.csv", header = T)

# read the species data
species_data <- read.csv("data/FAO/CL_FI_SPECIES_GROUPS.csv", header = T)
species_code <- species_data %>% 
  filter(Name_En %in% c("Slender armorhead",
                        "Pelagic armourhead",
                        "Longfin armorhead",
                        "Splendid alfonsino")) %>% 
  select(X3A_Code)

# read the country data
country_data <- read.csv("data/FAO/CL_FI_COUNTRY_GROUPS.csv", header = T) 
country_code <- country_data %>% 
  filter(ISO2_Code %in% c("JP")) %>% 
  select(UN_Code)


# filter by species (NPA and SA)
FAO_catch <- read.csv("data/FAO/CAPTURE_QUANTITY.csv", header = T) %>% 
  filter(SPECIES.ALPHA_3_CODE %in% unlist(species_code)) %>% 
  filter(COUNTRY.UN_CODE %in% unlist(country_code)) %>% 
  pivot_wider(names_from = 2, values_from = 6)
head(FAO_catch)
FAO_catch_Russia <- FAO_catch %>% 
  filter(COUNTRY.UN_CODE == "643")
FAO_catch_Soviet <- FAO_catch %>% 
  filter(COUNTRY.UN_CODE == "810")
FAO_catch_Japan <- FAO_catch %>% 
  filter(COUNTRY.UN_CODE == "392") %>% 
  arrange(PERIOD)

ggplot(data = FAO_catch_NPA_SA %>% filter(SPECIES.ALPHA_3_CODE == "EDJ"),
       aes(PERIOD, VALUE, fill = COUNTRY.UN_CODE)) +
  geom_bar(stat = "identity")

# filter by countries (Russia)

# check the fishing grounds

# filter by regions