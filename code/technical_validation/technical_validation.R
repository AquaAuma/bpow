################################################################################
#### Coding and data processing: Aurore Maureaud
#### Technical validation of the layer product
#### March 2022
################################################################################
rm(list = ls())

# load libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(raster)
library(exactextractr)
library(rgdal)
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
library(viridis)
library(data.table)
library(occCite)
library(ggnewscale)
library(ggpmisc)


################################################################################
### 1. Load data
################################################################################

# load biogeographic layer
eco <- st_read("/Users/auroremaureaud/Documents/bpow_p9_post_op.shp")

# load species data
spp <- c("Actinostola callosa","Pseudoliparis swirei","Callinectes sapidus","Centroscymnus coelolepis",
         "Chionoecetes opilio","Coryphaenoides armatus","Patiria pectinifera","Plesiobatis daviesi",
         "Tridacna gigas")

# load GBIF
gbif <- fread(
  file.path("data/gbif_spp/occurrence.txt"),
  na.strings = c("", NA),
  quote = "",
  select = c(
    "decimalLatitude",
    "decimalLongitude",
    "coordinateUncertaintyInMeters",
    "year",
    "month",
    "day",
    "depth",
    "genus",
    "species",
    "specificEpithet",
    "infraspecificEpithet",
    "occurrenceStatus")) %>% 
  filter(!is.na(decimalLatitude), # remove records with lat/long
         !is.na(decimalLongitude),
         occurrenceStatus == "PRESENT") %>% # remove absence records
  distinct() %>% 
  dplyr::select(year, month, day, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters,
                species, depth) %>% 
  rename(latitude = decimalLatitude,
         longitude = decimalLongitude,
         coord_error = coordinateUncertaintyInMeters) %>% 
  mutate(source = "GBIF")

# load OBIS
obis <- read_csv("data/obis_spp/Occurrence.csv") %>% 
  dplyr::select(
    "decimallatitude",
    "decimallongitude",
    "coordinateuncertaintyinmeters",
    "year",
    "month",
    "day",
    "maximumdepthinmeters",
    "minimumdepthinmeters",
    "genus",
    "species",
    "specificepithet",
    "infraspecificepithet",
    "occurrencestatus") %>% 
  filter(!is.na(decimallatitude),
         !is.na(decimallongitude),
         occurrencestatus %in% c("present","Present","P","Presente")) %>% 
  distinct() %>% 
  rename(latitude = decimallatitude,
         longitude = decimallongitude,
         coord_error = coordinateuncertaintyinmeters,
         depth = maximumdepthinmeters) %>% 
  dplyr::select(year, month, day, latitude, longitude, coord_error,
         species, depth) %>% 
  mutate(source = "OBIS")

# merge and deduplicate
occ <- rbind(obis, gbif) %>% 
  #filter(coord_error < 5000) %>% 
  group_by(year, month, day, latitude, longitude, species) %>% 
  distinct()


################################################################################
### 2. Intersection with layer
################################################################################

occ_sf <- occ %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(eco))
occ_sf_layer <- st_join(occ_sf, eco, left=TRUE, largest=FALSE)

occ_sf_land <- occ_sf_layer %>% 
  st_drop_geometry() %>% 
  filter(is.na(ID)) %>% 
  group_by(species) %>% 
  summarize(nb = length(species))

occ_sf_eco <- occ_sf_layer %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ID)) %>% 
  group_by(species, type) %>% 
  summarize(nb = length(species))


################################################################################
### 3. Map of regions per species
################################################################################
spp <- sort(unique(occ$species))

for(i in 1:length(spp)){
  spp_xx <- occ_sf_layer %>%
    st_drop_geometry() %>% 
    filter(species == spp[i]) %>% 
    group_by(ID) %>% 
    summarize(nb = length(ID))
  
  eco_xx <- left_join(eco, spp_xx, by = "ID") %>% 
    filter(!is.na(nb))
  
  png(filename = paste0("figures/figure_4/technical_validation_",spp[i],".png"),
      width = 16*200, height = 10*200, res = 200)
  print(ggplot(eco_xx) + geom_sf(aes(fill = nb), color = NA) +
          geom_sf(data = occ_sf_layer[occ_sf_layer$species == spp[i],], col = "black", shape = 3, size = 4, lwd = 3) +
          geom_sf(data = world, fill = "grey", color=NA) +  coord_sf(crs = '+proj=moll') + 
          theme_minimal() +
          scale_fill_gradient(low= brewer.pal(9,"OrRd")[2], high = brewer.pal(9,"OrRd")[8]) +
          theme(legend.title = element_blank(),
                legend.key.size = unit(1, 'cm'),
                legend.key.height = unit(1, 'cm'),
                legend.key.width = unit(1, 'cm'),
                legend.text = element_text(size=20)))
  dev.off()
}


################################################################################
### 4. Map of blue crab
################################################################################

i = 3
spp_xx <- gbif_sf_layer %>%
  st_drop_geometry() %>% 
  filter(species == spp[i]) %>% 
  group_by(ID) %>% 
  summarize(nb = length(ID))

eco_xx <- left_join(eco, spp_xx, by = "ID") %>% 
  filter(!is.na(nb))

png(filename = paste0("figures/figures_MS_data/technical_validation_2_",spp[i],".png"),
    width = 16*200, height = 10*200, res = 200)
print(ggplot(eco_xx) + geom_sf(aes(fill = nb), color = NA) +
        geom_sf(data = gbif_sf_layer[gbif_sf_layer$species == spp[i],], col = "black", shape = 3, size = 4, lwd = 3) +
        geom_sf(data = world, fill = "grey", color=NA) +  coord_sf(crs = '+proj=moll') + 
        theme_minimal() +
        scale_fill_gradient(low= brewer.pal(9,"OrRd")[2], high = brewer.pal(9,"OrRd")[8],
                            trans = "log10") +
        theme(legend.title = element_blank(),
              legend.key.size = unit(1, 'cm'),
              legend.key.height = unit(1, 'cm'),
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=20)))
dev.off()

png(filename = paste0("figures/figures_MS_data/technical_validation_3_",spp[i],".png"),
    width = 16*200, height = 10*200, res = 200)
print(ggplot(eco_xx) + geom_sf(aes(fill = nb), color = NA) +
        geom_sf(data = eco_xx[eco_xx$type=="MEOW",], fill = "black", alpha = 0.3, color = "black") +
        #geom_sf(data = gbif_sf_layer[gbif_sf_layer$species == spp[i],], col = "black", shape = 3, size = 4, lwd = 3) +
        geom_sf(data = world, fill = "grey", color=NA) +  coord_sf(crs = '+proj=moll') + 
        theme_minimal() +
        scale_fill_gradient(low= brewer.pal(9,"OrRd")[2], high = brewer.pal(9,"OrRd")[8],
                            trans = "log10") +
        theme(legend.title = element_blank(),
              legend.key.size = unit(1, 'cm'),
              legend.key.height = unit(1, 'cm'),
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=20)))
dev.off()


################################################################################
### 5. Separate "true" versus "false" presences
################################################################################
spp <- sort(unique(occ$species))

occ_sf_layer <- occ_sf_layer %>% 
  mutate(type_tf = ifelse(species %in% c(spp[1:4],spp[6:7],spp[9]) & type == "MEOW", "TRUE", "FALSE"),
         type_tf = ifelse(species %in% c(spp[1],spp[3:5]) & type == "bathyal", "TRUE", type_tf),
         type_tf = ifelse(species %in% c(spp[3],spp[5],spp[8]) & type == "abyssal", "TRUE", type_tf),
         type_tf = ifelse(species==spp[8] & type == "hadal", "TRUE", type_tf))

summary_tab <- data.frame()

for(i in 1:length(spp)){
  spp_xx <- occ_sf_layer %>%
    st_drop_geometry() %>% 
    filter(species == spp[i]) %>% 
    group_by(ID, type, type_tf) %>% 
    summarize(nb = length(ID)) %>% 
    filter(!is.na(ID))
  
  spp_xx <- spp_xx %>% 
    mutate(prop = nb/sum(spp_xx$nb)*100)
  
  eco_xx <- left_join(eco, spp_xx, by = "ID") %>% 
    filter(!is.na(nb))
  
  # map map
  png(filename = paste0("figures/figure_4/true_false",spp[i],".png"),
      width = 16*200, height = 10*200, res = 200)
  print(ggplot(eco_xx[eco_xx$type_tf == "TRUE" ,]) + geom_sf(aes(fill = prop), color = NA) +
    scale_fill_gradient(limits = c(0,100), low= brewer.pal(9,"OrRd")[2], high = brewer.pal(9,"OrRd")[8]) +
    new_scale_fill() +
    geom_sf(data = eco_xx[eco_xx$type_tf == "FALSE" ,], aes(fill = prop), color = NA) +
    scale_fill_gradient(limits = c(0,100), low= brewer.pal(9,"BuPu")[2], high = brewer.pal(9,"BuPu")[8]) +
    theme_minimal() +
    geom_sf(data = world, fill = "grey", color=NA) +  coord_sf(crs = '+proj=moll') +
    theme(legend.title = element_blank(),
          legend.key.size = unit(1, 'cm'),
          legend.key.height = unit(1, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.text = element_text(size=20)))
  dev.off()
  
  #make map with table
  spp_tab <- data.frame(variable = c("total","true","false"),
                        value = c(round(sum(spp_xx$nb)),
                                  round(sum(spp_xx[spp_xx$type_tf==TRUE,]$prop, na.rm=T),2),
                                  round(sum(spp_xx[spp_xx$type_tf==FALSE,]$prop, na.rm=T),2)),
                        species = rep(spp[i], times = 3))
  summary_tab <- rbind(summary_tab, spp_tab)
}

write.csv(summary_tab, file = "figures/figure_4/summary_tab.csv", row.names = FALSE)
