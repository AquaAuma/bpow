#### Make map figure with panels by depth compartment
#### October 2024

rm(list = ls())

# load libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(raster)
library(exactextractr)
library(fasterize)
sf::sf_use_s2(FALSE)
library(units)
library(ggtern)
library(rnaturalearth)
library(rnaturalearthdata)
library(tricolore)
library(ggtern)
world <- ne_countries(scale = "medium", returnclass = "sf")
library(tiff)
library(RColorBrewer)
library(egg)
library(viridis)
library(ggmap)
library(gridExtra)


# load layer
eco <- st_read("~/Documents/Yale University/Marine Biogeography/writing/layer manuscript/submission/Supplementary File 1/bpow_p10_final.shp")
eco <- eco %>% 
  arrange(ID)

################################################################################
#### Make Figure 1
################################################################################
col_prov <- sample(colorRampPalette(brewer.pal(12, "Paired"))(length(eco$ID)))

# map types
map_types <- ggplot(eco[eco$type == "hadal",]) + geom_sf(fill = "black", color = NA) +
  geom_sf(data = eco[eco$type!="hadal",], aes(fill = type), color = NA) + theme_minimal() +
  geom_sf(data = world, fill = "white", color=NA) +
  #coord_sf(crs = '+proj=moll') + 
  scale_fill_manual(values = c(brewer.pal(9,"Blues")[9],brewer.pal(9, "YlGnBu")[c(6,4)]), 
                    labels = c("Abyssal","Bathyal","Coastal")) +
  labs(fill = "") + theme(legend.position = "none")

# map for abyssal
map_abyssal <- ggplot() + 
  geom_sf(data = eco, aes(fill = as.factor(ID)), color = NA) + theme_minimal() +
  geom_sf(data = eco[eco$type!="abyssal",], fill = "lightgrey", color = NA) +
  geom_sf(data = world, color = NA, fill = "white") +
  #coord_sf(crs = '+proj=moll') +
  scale_fill_manual(values = col_prov) +
  labs(fill = "") + theme(legend.position = "none")

# map for bathyal
map_bathyal <- ggplot() +
  geom_sf(data = eco,aes(fill = as.factor(ID)), color = NA) + theme_minimal() +
  geom_sf(data = eco[eco$type!="bathyal",], fill = "lightgrey", color = NA) +
  geom_sf(data = world, color = NA, fill = "white") +
  #coord_sf(crs = '+proj=moll') +
  scale_fill_manual(values = col_prov) +
  labs(fill = "") + theme(legend.position = "none")

# map for coastal
map_coastal <- ggplot() +
  geom_sf(data = eco,aes(fill = as.factor(ID)), color = NA) + theme_minimal() +
  geom_sf(data = eco[eco$type != "coastal",], fill = "lightgrey", color = NA) +
  geom_sf(data = world, color=NA, fill = "white") +
  #coord_sf(crs = '+proj=moll') +
  scale_fill_manual(values = col_prov) +
  labs(fill = "") + theme(legend.position = "none")

# map for hadal
map_had <- ggplot() +
  geom_sf(data = eco, aes(fill = as.factor(ID)), color = NA) + 
  geom_sf(data = eco[eco$type!="hadal",], fill = "lightgrey", color = NA) +
  geom_sf(data = world, color=NA, fill = "white") +
  #coord_sf(crs = '+proj=moll') +
  theme_minimal() +
  scale_fill_manual(values = col_prov) +
  labs(fill = "") + theme(legend.position = "none") 

# map for all
map_all <- eco %>% 
  ggplot() +
  geom_sf(data = eco, aes(fill = as.factor(ID)), color = NA) + 
  geom_sf(data = world, color=NA, fill = "white") +
  #coord_sf(crs = '+proj=moll') +
  theme_minimal() +
  scale_fill_manual(values = col_prov) +
  labs(fill = "") + theme(legend.position = "none") 

# map for all
  ggplot() +
  geom_sf(data = eco[1,], aes(fill = as.factor(ID)), color = NA) + 
  geom_sf(data = world, color=NA, fill = "white") +
  theme_minimal() +
  scale_fill_manual(values = col_prov) +
  labs(fill = "") + theme(legend.position = "none") 

# Map one by one for high resolution
png(filename = "figures/figure2_coastal.png",
    width = 16*200, height = 10*200, res = 200)
print(map_coastal)
dev.off()

png(filename = "figures/figure2_bathyal.png",
    width = 16*200, height = 10*200, res = 200)
print(map_bathyal)
dev.off()

png(filename = "figures/figure2_abyssal.png",
    width = 16*200, height = 10*200, res = 200)
print(map_abyssal)
dev.off()

png(filename = "figures/figure2_hadal.png",
    width = 16*200, height = 10*200, res = 200)
print(map_had)
dev.off()

png(filename = "figures/figure2_types.png",
    width = 16*200, height = 10*200, res = 200)
print(map_types)
dev.off()

png(filename = "figures/figure2_all.png",
    width = 16*200, height = 10*200, res = 200)
print(map_all)
dev.off()


################################################################################
#### Make Make map of all provinces
################################################################################
col_pro <- colorRampPalette(brewer.pal(12, "Paired"))(length(eco$prov_n))
map_pro <- ggplot() + 
  geom_sf(data = eco[eco$type == "hadal",], aes(fill = as.factor(ID)), color = NA) + theme_minimal() +
  geom_sf(data = eco[eco$type != "hadal",], aes(fill = as.factor(ID)), color = NA) + theme_minimal() +
  geom_sf(data = world, color = NA, fill = "white") +
  coord_sf(crs = '+proj=moll') +
  scale_fill_manual(values = sample(col_pro)) +
  labs(fill = "") + theme(legend.position = "none")

png(filename = "figures/figures_MS/figure_layer.png",
    width = 16*200, height = 10*200, res = 200)
print(map_pro)
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
