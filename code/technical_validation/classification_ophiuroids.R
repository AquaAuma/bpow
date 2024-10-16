################################################################################
#### Coding and data processing: Aurore Maureaud
#### Test of the layer for a global dataset of ophiuroids
#### Data from Victorero et al., 2023 Ecography
#### October 2024
################################################################################

rm(list = ls())

# load libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(raster)
sf::sf_use_s2(FALSE)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")


################################################################################
#### 1. Load Datasets ----
################################################################################

# load shapefile layer
eco <- st_read("~/Documents/Yale University/Marine Biogeography/writing/layer manuscript/submission/Supplementary File 1/bpow_p10_final.shp") %>% 
  arrange(ID)

# load occurrence data


################################################################################
#### 2. Test layer ----
################################################################################


