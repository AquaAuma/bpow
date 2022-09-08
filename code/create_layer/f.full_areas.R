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
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")


### load holes transformed into polygons with regions from e. ran on the HPC
load("outputs/holes/seafloor_meow_deepsea_filled_1_to_5.RData")
seafloor_meow_deepsea_final <- seafloor_meow_deepsea_filled
load("outputs/holes/seafloor_meow_deepsea_filled_6_to_10.RData")
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

# re-run
load("outputs/holes/seafloor_meow_deepsea_filled_limit_5500_1_to_10.RData")
seafloor_meow_deepsea_final <- seafloor_meow_deepsea_filled
load("outputs/holes/seafloor_meow_deepsea_filled_limit_5500_11_to_200.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_limit_5500_201_to_400.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_limit_5500_401_to_600.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_limit_5500_601_to_800.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_limit_5500_801_to_1200.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_limit_5500_1201_to_1600.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_limit_5500_1601_to_2000.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)
load("outputs/holes/seafloor_meow_deepsea_filled_limit_5500_2001_to_2232.RData")
seafloor_meow_deepsea_final <- rbind(seafloor_meow_deepsea_final, seafloor_meow_deepsea_filled)


# load provinces_p7s3
seafloor_meow_deepsea <- st_read("outputs/hadal/provinces_p7s3_limit_5500.shp") %>% 
  mutate(prov_id = ifelse(type == "hadal", had_id, prov_id),
         prov_n = ifelse(type == "hadal", had_n, prov_n),
         type_id = paste0(type,"-",prov_id)) %>% 
  dplyr::select(-had_id, -had_n, -eco_id, -eco_n, -rlm_n, -rlm_id, -ID)

seafloor_meow_deepsea_final <- seafloor_meow_deepsea_final  %>% 
  mutate(prov_id = ifelse(type == "hadal", had_id, prov_id),
         prov_n = ifelse(type == "hadal", had_n, prov_n),
         type_id = paste0(type,"-",prov_id)) %>% 
  dplyr::select(-had_id, -had_n, -eco_id, -eco_n, -rlm_n, -rlm_id, -ID)

# creation of the bpow shapefile
bpow <- rbind(seafloor_meow_deepsea, seafloor_meow_deepsea_final) %>% 
  mutate(source = case_when(type == "coastal" ~ "Spalding et al., (2007)",
                            type %in% c("bathyal","abyssal") ~ "Watling et al., (2013)",
                            type == "hadal" ~ "adapted from Belyaev (1989)")) %>% 
  group_by(type, type_id, prov_n, prov_id, source) %>%
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()
bpow$ID <- 1:100

xx <- st_is_valid(bpow) # all true so shapefile valid!

st_write(obj = bpow, dsn = "outputs/bpow/bpow_p8s5_limit_5500.shp")


### check and plot all regions
for(i in 1:100){
  png(filename = paste0("figures/all_regions/",i,"_",bpow$type[i],".png"),
  width = 16*200, height = 10*200, res = 200)
  print(ggplot(world) + geom_sf(fill = "grey", color = NA) +
    geom_sf(data = bpow[i,], fill = "blue", color = "black") +
    ggtitle(bpow$prov_n[i]))
  dev.off()
}

### problem with 20 and 24
new_1 <- bpow[20,] %>% 
  st_cast(geometry, to = "POLYGON")
pb_1 <- bpow[20,] %>% 
  st_cast(geometry, to = "POLYGON") %>% 
  st_centroid() %>% 
  st_coordinates()
new_1 <- cbind(new_1, pb_1) %>% 
  filter(!(X<0 & X>(-90))) %>% 
  dplyr::select(-X, -Y) %>% 
  group_by(type, type_id, prov_n, prov_id, source, ID) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()
st_geometry(bpow[20,]) <- st_geometry(new_1)

new_2 <- bpow[24,] %>% 
  st_cast(geometry, to = "POLYGON")
pb_2 <- bpow[24,] %>% 
  st_cast(geometry, to = "POLYGON") %>% 
  st_centroid() %>% 
  st_coordinates()
new_2 <- cbind(new_2, pb_2) %>% 
  filter(Y<0) %>% 
  dplyr::select(-X, -Y) %>% 
  group_by(type, type_id, prov_n, prov_id, source, ID) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()
st_geometry(bpow[24,]) <- st_geometry(new_2)

st_write(obj = bpow, dsn = "outputs/bpow/bpow_p8s5_limit_5500_corr.shp")
