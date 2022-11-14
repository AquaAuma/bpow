# load libraries
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)
library(ggplot2)

### where to download the last MOL land layer: https://drive.google.com/drive/u/1/folders/1bcy06CafcC-G9rTO5JU7mQoNCyjuTqjW
land <- st_read(dsn = "data/MOL_Land_Layer_Countries_Fixed/MOL_Land_Layer_Countries_Fixed.gpkg")

land_validation <- st_is_valid(land, reason=TRUE, NA_on_exception = FALSE)
View(land_validation) # all seems 100% valid geometries
