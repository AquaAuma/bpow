rm(list = ls())

### Libraries
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(RColorBrewer)
library(raster)
library(geosphere)
library(fasterize)
library(smoothie)
library(exactextractr)
library(Morpho)
library(rgl)
library(units)

### Finalize shapefile
load("outputs/holes/seafloor_meow_deepsea_filled_1_to_5.RData")
seafloor_meow_deepsea_final <- seafloor_meow_deepsea_filled
load("outputs/holes/seafloor_meow_deepsea_filled_6_to_11.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_11_to_200.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_201_to_400.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_401_to_600.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_601_to_800.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_801_to_1000.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_1001_to_1200.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_1201_to_1400.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_1401_to_1600.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_1601_to_1800.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_1801_to_2000.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_2001_to_2232.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)


seafloor_meow_deepsea_final <- seafloor_meow_deepsea_final %>% 
  group_by(ID, type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id,
           had_n, had_id) %>%
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()

xx <- st_is_valid(seafloor_meow_deepsea_final) # all true so shapefile valid!

st_write(obj = seafloor_meow_deepsea_final, dsn = "outputs/holes/seafloor_meow_deepsea_final.shp")

