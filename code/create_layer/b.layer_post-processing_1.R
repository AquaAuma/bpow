################################################################################
#### Re-format layer
#### Coding and data processing: Aurore Maureaud
#### July 2022
################################################################################

# load libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(fasterize)
sf::sf_use_s2(FALSE)

# loadlayer after 1st processing on Arc
eco <- st_read("outputs/arcpro/post-processing_1/Provinces_P5S2.shp")