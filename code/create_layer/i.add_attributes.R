# load libraries
library(sf)
library(tidyverse)
library(ggplot2)


bpow <- st_read(dsn = "/Users/auroremaureaud/Documents/bpow_p10.shp") 
attributes <- st_read("outputs/bpow/bpow_p8s5_abyssal_corr.shp") %>% 
  st_drop_geometry()

bpow <- left_join(bpow, attributes, by = "ID")

st_write(bpow, dsn = "outputs/bpow/bpow_p10_attributes.shp",
         append=F)
