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
library(stars)

# load last layer available
bpow <- st_read("outputs/bpow/bpow_p8s5_limit_5500_corr.shp")

# rasterize layer

# mol grid
grid_2880 <- st_read("E:/Yale data/Grids/Grids v2/2880x912_grid_v2/2880x912global_v2/2880x912global_v2.shp")
# rasterize the grid
rast_2880 <- raster(grid_2880, nrow=912, ncol=2880)
r_id <- fasterize(grid_2880, rast_2880, background=NA, field="ID_1440")
rast_288_stars <- st_as_stars(rast_2880)

bpow_r <- fasterize(sf = bpow, 
                    raster = rast_2880,
                    background = NA,
                    field = "ID")

bpow_r <- st_rasterize(bpow,
                       template = rast_2880_stars)

bpow_r <- raster(bpow, nrow = 912, ncol = 2880,
                 field = "ID")



r <- raster(ncol=2880, nrow=912)
extent(r) <- extent(bpow)
rp <- rasterize(bpow, r, 'ID')


