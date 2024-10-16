# load libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(fasterize)
sf::sf_use_s2(FALSE)

# loadlayer after 1st processing on Arc
eco <- st_read("outputs/arcpro/post-processing_1/Provinces_P5S2.shp") %>% 
  mutate(type = case_when(MERGE_SRC == "provinces_p5s1" ~ "coastal",
                          MERGE_SRC == "p4s2" ~ "abyssal",
                          MERGE_SRC == "GOODSprovinces_bathyal_Rfix_irregularities" ~ "bathyal"),
         prov_n = PROVINCE,
         prov_id = PROV_CODE,
         prov_id = ifelse(type %in% c("abyssal", "bathyal"), PROVINCE, prov_id),
         prov_n = ifelse(type %in% c("abyssal","bathyal"), Name, prov_n),
         eco_n = ECOREGION,
         eco_id = ECO_CODE_X,
         eco_id = ifelse(eco_id ==0, NA, eco_id),
         rlm_id = RLM_CODE,
         rlm_n = REALM) %>% 
  dplyr::select(ID, type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id)

st_write(eco, dsn = "outputs/bpow/bpow_p5s4.shp")
