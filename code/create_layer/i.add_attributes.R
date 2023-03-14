# load libraries
library(sf)
library(tidyverse)
library(ggplot2)


bpow <- st_read(dsn = "/Users/auroremaureaud/Documents/bpow_p10.shp") 
attributes <- st_read("outputs/bpow/bpow_p8s5_abyssal_corr.shp") %>% 
  st_drop_geometry() %>% 
  mutate(depth_r = ifelse(type = "coastal", "0-800m", NA_character_),
         depth_r = ifelse(type = "bathyal", "800-3,500m", depth_r),
         depth_r = ifelse(type == "abyssal", "3,500-6,500m", depth_r),
         depth_r = ifelse(type == "hadal", "6,500m+", depth_r))

bpow <- left_join(bpow, attributes, by = "ID")

st_write(bpow, dsn = "outputs/bpow/bpow_p10_attributes.shp",
         append=F)
