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


### Abyssal & Bathyal provinces from Watling et al., 2013
### Related publication: https://www.sciencedirect.com/science/article/abs/pii/S0079661112001693?via%3Dihub
### Access to shapefiles given by the author
abyssal <- st_read(dsn = "data/goods_provinces", layer = "GOODSprovinces_abyssal")
bathyal <- st_read(dsn = "data/goods_provinces", layer = "GOODSprovinces_bathyal")


### A. Work on abyssal self intersections
abyssal_validation <- st_is_valid(abyssal, reason=TRUE, NA_on_exception = FALSE)
View(abyssal_validation) # 189 abyssal with self-ring intersection

abyssal_corrected <- st_make_valid(abyssal)

abyssal_validation2 <- st_is_valid(abyssal_corrected, reason=TRUE, NA_on_exception = FALSE)
unique(abyssal_validation2) # all valid geometries


### B. Work on bathyal self intersections
bathyal_validation <- st_is_valid(bathyal, reason=TRUE, NA_on_exception = FALSE)
View(bathyal_validation) # 341 bathyal with self-ring intersection

bathyal_corrected <- st_make_valid(bathyal)

bathyal_validation2 <- st_is_valid(bathyal_corrected, reason=TRUE, NA_on_exception = FALSE)
unique(bathyal_validation2) # all valid geometries


### C. Save new files
st_write(abyssal_corrected, dsn = "outputs/deep_sea/GOODSprovinces_abyssal_Rfix.shp")
st_write(bathyal_corrected, dsn = "outputs/deep_sea/GOODSprovinces_bathyal_Rfix.shp")

