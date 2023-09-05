rm(list = ls())

# load libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(ggtern)
library(raster)
library(rgdal)
sf::sf_use_s2(FALSE)
library(rnaturalearth)
library(rnaturalearthdata)
library(tricolore)
world <- ne_countries(scale = "medium", returnclass = "sf")
library(tiff)
library(RColorBrewer)

# load layer
eco <- st_read("outputs/bpow_p10_attributes_clip2.shp") %>% 
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90)) %>% 
  st_transform(crs = "+proj=moll") 


################################################################################
#### Make Figure 3
################################################################################

# define colors by bathy type
col_aby <- colorRampPalette(brewer.pal(12, "Paired"))(length(eco[eco$type=="abyssal",]$prov_n))
col_bat <- colorRampPalette(brewer.pal(12, "Paired"))(length(eco[eco$type=="bathyal",]$prov_n))
col_coast <- colorRampPalette(brewer.pal(12, "Paired"))(length(eco[eco$type=="coastal",]$prov_n))
col_had <- colorRampPalette(brewer.pal(12, "Paired"))(length(eco[eco$type=="hadal",]$prov_n))

# map for abyssal
eco_abyssal <- eco %>% filter(type == "abyssal")
map_abyssal <- ggplot() + 
  geom_sf(data = eco[eco$type!="abyssal",], fill = "lightgrey", color = NA) +
  geom_sf(data = eco_abyssal, aes(fill = prov_n), color = NA) + theme_minimal() +
  geom_sf(data = world, color = NA, fill = "white") +
  scale_fill_manual(values = sample(col_aby)) +
  labs(fill = "") + theme(legend.position = "none")

# map for bathyal
map_bathyal <- ggplot() +
  geom_sf(data = eco[eco$type!="bathyal",], fill = "lightgrey", color = NA) +
  geom_sf(data = eco[eco$type == "bathyal",],aes(fill = prov_n), color = NA) + theme_minimal() +
  geom_sf(data = world, color = NA, fill = "white") +
  scale_fill_manual(values = sample(col_bat)) +
  labs(fill = "") + theme(legend.position = "none")

# map for coastal provinces
map_coastal <- ggplot() +
  geom_sf(data = eco[eco$type != "coastal",], fill = "lightgrey", color = NA) +
  geom_sf(data = eco[eco$type == "coastal",],aes(fill = prov_n), color = NA) + theme_minimal() +
  geom_sf(data = world, color=NA, fill = "white") +
  scale_fill_manual(values = sample(col_coast)) +
  labs(fill = "") + theme(legend.position = "none")

# map for hadal
map_had <- ggplot() +
  geom_sf(data = eco[eco$type == "hadal",], aes(fill = prov_n), color = NA) + 
  geom_sf(data = eco[eco$type!="hadal",], fill = "lightgrey", color = NA) +
  geom_sf(data = world, color=NA, fill = "white") +
  theme_minimal() +
  scale_fill_manual(values = sample(col_had)) +
  labs(fill = "") + theme(legend.position = "none") 

# map for all provinces
col_aby <- colorRampPalette(brewer.pal(9, "Blues")[7:9])(length(eco[eco$type=="abyssal",]$prov_n))
col_bat <- colorRampPalette(brewer.pal(9, "Blues")[4:6])(length(eco[eco$type=="bathyal",]$prov_n))
col_coast <- colorRampPalette(brewer.pal(9, "YlGnBu"))(length(eco[eco$type=="coastal",]$prov_n))
col_had <- colorRampPalette(brewer.pal(9, "Blues"))(length(eco[eco$type=="hadal",]$prov_n))
col_prov <- c(col_aby, col_bat, col_coast, col_had)
col_prov <- sample(colorRampPalette(brewer.pal(12, "Paired"))(length(eco$prov_n)))
eco2 <- cbind(eco, col_prov)

map_all <- ggplot() +
  geom_sf(data = eco2, aes(fill = as.factor(ID)), color = NA) + 
  geom_sf(data = world, color=NA, fill = "white") +
  theme_minimal() +
  scale_fill_manual(values = eco2$col_prov) +
  labs(fill = "") + theme(legend.position = "none")

map_aby <- ggplot() +
  geom_sf(data = eco2, aes(fill = as.factor(ID)), color = NA) + 
  geom_sf(data = world, color=NA, fill = "white") +
  theme_minimal() +
  scale_fill_manual(values = eco2$col_prov) +
  labs(fill = "") + theme(legend.position = "none") +
  geom_sf(data = eco[eco$type %in% c("bathyal","coastal","hadal"),], fill = "lightgrey", color = NA)

map_had <- ggplot() +
  geom_sf(data = eco2, aes(fill = as.factor(ID)), color = NA) + 
  geom_sf(data = world, color=NA, fill = "white") +
  theme_minimal() +
  scale_fill_manual(values = eco2$col_prov) +
  labs(fill = "") + theme(legend.position = "none") +
  geom_sf(data = eco[eco$type %in% c("bathyal","coastal","abyssal"),], fill = "lightgrey", color = NA)

map_coast <- ggplot() +
  geom_sf(data = eco2, aes(fill = as.factor(ID)), color = NA) + 
  geom_sf(data = world, color=NA, fill = "white") +
  theme_minimal() +
  scale_fill_manual(values = eco2$col_prov) +
  labs(fill = "") + theme(legend.position = "none") +
  geom_sf(data = eco[eco$type %in% c("bathyal","hadal","abyssal"),], fill = "lightgrey", color = NA)

map_bat <- ggplot() +
  geom_sf(data = eco2, aes(fill = as.factor(ID)), color = NA) + 
  geom_sf(data = world, color=NA, fill = "white") +
  theme_minimal() +
  scale_fill_manual(values = eco2$col_prov) +
  labs(fill = "") + theme(legend.position = "none") +
  geom_sf(data = eco[eco$type %in% c("coastal","hadal","abyssal"),], fill = "lightgrey", color = NA)

# Map one by one for high resolution
png(filename = "figures/figure2_coastal.png",
    width = 16*200, height = 10*200, res = 200)
print(map_coast)
dev.off()

png(filename = "figures/figure2_bathyal.png",
    width = 16*200, height = 10*200, res = 200)
print(map_bat)
dev.off()

png(filename = "figures/figure2_abyssal.png",
    width = 16*200, height = 10*200, res = 200)
print(map_aby)
dev.off()

png(filename = "figures/figure2_hadal.png",
    width = 16*200, height = 10*200, res = 200)
print(map_had)
dev.off()

pdf(filename = "figures/figure2_all.png",
    width = 16*200, height = 10*200, res = 200)
print(map_all)
dev.off()



################################################################################
#### Make Make map of all provinces
################################################################################
col_all <- colorRampPalette(brewer.pal(12, "Paired"))(length(eco$prov_n))
map_all <- ggplot() +
  geom_sf(data = eco,aes(fill = prov_n), color = NA) + theme_minimal() +
  geom_sf(data = world, color=NA, fill = "white") +
  scale_fill_manual(values = sample(col_all)) +
  labs(fill = "") + theme(legend.position = "none")

png(filename = "figures/figure_3/figure_layer.png",
    width = 16*200, height = 10*200, res = 200)
print(map_all)
dev.off()


################################################################################
#### Make map of DSP and Holes
################################################################################

col_dsp <- colorRampPalette(brewer.pal(12, "Paired"))(length(eco$prov_n))
map_dsp <- ggplot() + 
  geom_sf(data = eco[eco$type %in% c("abyssal","bathyal"),], aes(fill = as.factor(ID)), color = NA) + theme_minimal() +
  geom_sf(data = world, color = NA, fill = "white") +
  coord_sf(crs = '+proj=moll') +
  scale_fill_manual(values = sample(col_pro)) +
  labs(fill = "") + theme(legend.position = "none")

png(filename = "figures/figures_MS/figure_dsp.png",
    width = 16*200, height = 10*200, res = 200)
print(map_dsp)
dev.off()

# load holes
holes <- st_read(dsn = "~/Yale University/Marine Biogeography/outputs/ClippedAndMergedFiles/MissingAreas.shp")
holes <- holes %>% 
  st_cast("POLYGON") %>% 
  mutate(ID = c(1:nrow(holes)),
         Shape_Area = drop_units(st_area(geometry)),
         Shape_Leng = drop_units(st_length(geometry))) %>% 
  dplyr::select(-Id) %>% 
  filter(!ID %in% c(1713,2210))

map_holes <- ggplot() + 
  geom_sf(data = eco, fill = "grey", color = NA) +
  geom_sf(data = holes, color = NA, fill = "red") + theme_minimal() +
  geom_sf(data = world, color = NA, fill = "white") +
  coord_sf(crs = '+proj=moll') +
  scale_fill_manual(values = sample(col_pro)) +
  labs(fill = "") + theme(legend.position = "none")

png(filename = "figures/figures_MS/figure_holes.png",
    width = 16*200, height = 10*200, res = 200)
print(map_holes)
dev.off()


# land map
map_land <- ggplot() + 
  geom_sf(data = world, color = NA, fill = "brown") + theme_minimal() +
  coord_sf(crs = '+proj=moll') +
  labs(fill = "") + theme(legend.position = "none")

png(filename = "figures/figures_MS/figure_land.png",
    width = 16*200, height = 10*200, res = 200)
print(map_land)
dev.off()
