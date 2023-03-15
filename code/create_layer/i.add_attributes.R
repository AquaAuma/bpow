# load libraries
library(sf)
library(tidyverse)
library(ggplot2)


bpow <- st_read(dsn = "outputs/bpow_p10_attributes_clip2.shp") %>% 
  dplyr::select(ID) %>% 
  st_zm(drop = TRUE, what = "ZM")
attributes <- st_read("outputs/bpow/bpow_p8s5_abyssal_corr.shp") %>% 
  st_drop_geometry() %>% 
  mutate(depth_r = ifelse(type == "coastal", "0-800m", NA_character_),
         depth_r = ifelse(type == "bathyal", "800-3,500m", depth_r),
         depth_r = ifelse(type == "abyssal", "3,500-6,500m", depth_r),
         depth_r = ifelse(type == "hadal", "6,500m+", depth_r))

bpow <- left_join(bpow, attributes, by = "ID")

st_write(bpow, dsn = "outputs/bpow/bpow_p10_final.shp")
