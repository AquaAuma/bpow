# load libraries
library(sf)
library(tidyverse)
library(ggplot2)

bpow <- st_read(dsn = "~/Documents/bpow/outputs/drive-download-20221216T155202Z-001", 
                layer = "Marine_Layer_Unclipped_12162022")

### A. Work on abyssal self intersections
bpow_validation <- st_is_valid(bpow, reason=TRUE, NA_on_exception = FALSE)
View(bpow_validation) # 189 abyssal with self-ring intersection

bpow_corrected <- st_make_valid(bpow)
# 
# bpow_validation2 <- st_is_valid(bpow_corrected, reason=TRUE, NA_on_exception = FALSE)
# unique(bpow_validation2) # all valid geometries
# 
# bpow_corrected2 <- st_make_valid(bpow_corrected)
# 
# bpow_validation3 <- st_is_valid(bpow_corrected2, reason=TRUE, NA_on_exception = FALSE)
# unique(bpow_validation3) # all valid geometries
# 
# bpow_corrected3 <- st_make_valid(bpow_corrected2)
# bpow_validation4 <- st_is_valid(bpow_corrected3, reason = TRUE, NA_on_exception = FALSE)

st_write(bpow_corrected3, dsn = "outputs/bpow/bpow_p9_abyssal_corr.shp",
         append=F)

