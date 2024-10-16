# load libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(raster)
library(exactextractr)
library(rgdal)
library(fasterize)
sf::sf_use_s2(FALSE)
library(polyclip)
library(tiff)
library(raster)
library(geosphere)
library(fasterize)
library(smoothie)
library(exactextractr)

rm(list = ls())

# load new bathyal shapefile
bathyal <- st_read("outputs/deep_sea/GOODSprovinces_bathyal_Rfix.shp")

# fix florida region irregularities - object 17014 - will fix problems with
# coast intersection
# ggplot(bathyal) + geom_sf() +
#   coord_sf(xlim = c(-80,-78), ylim = c(28,31))
# 
# ggplot(bathyal[17014,]) + geom_sf()
# bbox_one <- st_bbox(bathyal[17014,])
# load depth raster
depth_n90_s0_w90_e0 <- raster("data/gebco_2020_ascii/gebco_2020_n90.0_s0.0_w-90.0_e0.0.asc")
depth <- crop(depth_n90_s0_w90_e0, y = extent(-80,-78.3,27,30.4))

depth[depth>-765] <- NA
plot(depth)

# extract necessary polygons
depth_spdf <- as(depth, "SpatialPixelsDataFrame")
depth_df <- as.data.frame(depth_spdf)
colnames(depth_df) <- c("depth", "x", "y")

# select polygons from shapefile and depth raster
depth_pts <- data.frame(rasterToPoints(depth))
depth_pts_sf <- st_as_sf(depth_pts, coords = c("x","y"), crs = st_crs(bathyal))
select_polygons <- st_intersects(depth_pts_sf, bathyal)
select_polygons <- unique(unlist(select_polygons))
select_polygons <- bathyal[select_polygons,]

ggplot(select_polygons) + geom_sf() +
  coord_sf(xlim = c(-80,-78), ylim = c(27,31)) +
  geom_tile(data = depth_df, aes(x = x, y = y, fill = depth), alpha = 0.5, color = NA)

new_poly <- rasterToPolygons(aggregate(depth, fact = 2, fun = mean))
new_poly <- st_as_sf(new_poly, crs = st_crs(bathyal))
new_poly <- new_poly %>% 
  mutate(fix = "fix") %>% 
  group_by(fix) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  mutate(ID = bathyal[17014,]$ID,
         Province = bathyal[17014,]$Province,
         Name = bathyal[17014,]$Name) %>% 
  dplyr::select(-fix)

ggplot(new_poly) + geom_sf(fill = "red", alpha= 0.5) +
  geom_sf(data = select_polygons, fill = "blue") +
  coord_sf(xlim = c(-80,-78), ylim = c(27,31))

new_poly <- rbind(new_poly, select_polygons) %>% 
  group_by(Province,Name) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  mutate(ID = 17014)

ggplot(new_poly) + geom_sf(fill = 'red') +
  #geom_sf(data = select_polygons, fill = "blue") +
  coord_sf(xlim = c(-80,-78), ylim = c(27,31))

# save the new polygon
bathyal <- bathyal %>% 
  filter(!ID %in% select_polygons$ID)
bathyal_fixed <- rbind(bathyal, new_poly)

# check validity
bathyal_validation <- st_is_valid(bathyal_fixed, reason=TRUE, NA_on_exception = FALSE)
unique(bathyal_validation) # 341 bathyal with self-ring intersection

bathyal_corrected <- st_make_valid(bathyal_fixed)

# save new bathyal shapefile
st_write(bathyal_corrected, dsn = "outputs/deep_sea/GOODSprovinces_bathyal_Rfix_irregularities.shp",
         append = T)
