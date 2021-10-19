library(tidyverse)

# create a plot for historical catch record from NPFC convention areas by countries

# download data from FAO

# read the data
FAO_catch <- read.csv("data/FAO/CAPTURE_QUANTITY.csv", header = T)

# read the species data
species_data <- read.csv("data/FAO/CL_FI_SPECIES_GROUPS.csv", header = T)
species_data_NPA_SA <- species_data %>% 
  filter(Name_En %in% c("Slender armorhead",
                        "Pelagic armourhead",
                        "Longfin armorhead",
                        "Splendid alfonsino",
                        "Alfonsino")) %>%  # get the species data for NPA and SA including their synonyms
  select(X3A_Code, Scientific_Name, Name_En)

# read the country data
country_data <- read.csv("data/FAO/CL_FI_COUNTRY_GROUPS.csv", header = T) 
country_data_NPFC <- country_data %>% 
  filter(ISO2_Code %in% c("SU", "RU", "JP", "KR")) %>%  # get the country data for NPFC members 
  select(UN_Code, ISO2_Code)

# filter by species (NPA and SA)
FAO_catch <- read.csv("data/FAO/CAPTURE_QUANTITY.csv", header = T) %>% 
  left_join(species_data_NPA_SA, by = c("SPECIES.ALPHA_3_CODE" = "X3A_Code")) %>% 
  left_join(country_data_NPFC, by = c("COUNTRY.UN_CODE" = "UN_Code")) %>% 
  select(- COUNTRY.UN_CODE)
FAO_catch_NPFC_area <- FAO_catch %>% 
  filter(AREA.CODE == "61") %>%  # filter by NPFC convention area (area code = 61)
  filter(SPECIES.ALPHA_3_CODE %in% species_data_NPA_SA$X3A_Code) %>% # filter by NPA and SA
  filter(ISO2_Code %in% country_data_NPFC$ISO2_Code) # filter by NPFC members

# merge the data from Russia and Soviet
NPA_catch <- FAO_catch_NPFC_area %>% 
  filter(SPECIES.ALPHA_3_CODE == "EDJ") %>% 
  pivot_wider(names_from = ISO2_Code, values_from = VALUE) %>% 
  mutate_all(., ~replace(., is.na(.), 0)) %>% 
  mutate(Russia = RU + SU) %>% 
  rename(Japan = JP)
SA_catch <- FAO_catch_NPFC_area %>% 
  filter(SPECIES.ALPHA_3_CODE == "BXD") %>% 
  pivot_wider(names_from = ISO2_Code, values_from = VALUE) %>% 
  mutate_all(., ~replace(., is.na(.), 0)) %>% 
  mutate(Russia = RU + SU) %>% 
  rename(Japan = JP)
head(sum_species)
head(FAO_catch_NPFC_area)

FAO_catch_Russia <- FAO_catch_NPFC_area %>% 
  filter(COUNTRY.UN_CODE == "643") %>% 
  filter(SPECIES.ALPHA_3_CODE %in% c("BXD", "BYS", "ALF"))

FAO_catch_Soviet <- FAO_catch_NPFC_area %>% 
  filter(COUNTRY.UN_CODE == "810") %>% 
  filter(SPECIES.ALPHA_3_CODE %in% c("BXD", "BYS", "ALF"))

FAO_catch_Japan <- FAO_catch_NPFC_area %>% 
  filter(COUNTRY.UN_CODE == "392") %>% 
  filter(SPECIES.ALPHA_3_CODE %in% c("BXD", "BYS", "ALF"))
  arrange(PERIOD)

ggplot(data = FAO_catch_NPA_SA %>% filter(SPECIES.ALPHA_3_CODE == "EDJ"),
       aes(PERIOD, VALUE, fill = COUNTRY.UN_CODE)) +
  geom_bar(stat = "identity")

# filter by countries (Russia)

# check the fishing grounds

# filter by regions