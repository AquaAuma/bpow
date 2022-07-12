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
library(occCite)
library(data.table)


################################################################################
### 1. Load data
################################################################################

# load biogeographic layer
eco <- st_read("~/Yale University/Marine Biogeography/outputs/remove_hadal/seafloor_meow_deepsea_w_hadal_correctedgeom_10202021.shp")

# load species data
spp <- c("Actinostola callosa","Pseudoliparis swirei","Callinectes sapidus","Centroscymnus coelolepis",
         "Chionoecetes opilio","Coryphaenoides armatus","Patiria pectinifera","Plesiobatis daviesi",
         "Tridacna gigas")

gbif <- data.frame()

for(i in 1:length(spp)){
  
  xx <- fread(
    file.path(paste0("E:/Yale data/GBIF/biogeography_technical_validation/GBIF ",spp[i],"/occurrence.txt")) ,
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
      "occurrenceStatus"
    )
  ) %>% 
    filter(!is.na(decimalLatitude), # remove records with lat/long
           !is.na(decimalLongitude),
           occurrenceStatus == "PRESENT") %>% # remove absence records
    distinct() %>% 
    dplyr::select(year, month, day, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters,
                  species, depth) %>% 
    rename(latitude = decimalLatitude,
           longitude = decimalLongitude,
           coord_error = coordinateUncertaintyInMeters)
  
  gbif <- rbind(gbif, xx)
  rm(xx)
  
}


################################################################################
### 2. Intersection with layer
################################################################################

gbif_sf <- gbif%>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(eco))
gbif_sf_layer <- st_join(gbif_sf, eco, left=TRUE, largest=FALSE)

gbif_sf_land <- gbif_sf_layer %>% 
  st_drop_geometry() %>% 
  filter(is.na(ID)) %>% 
  group_by(species) %>% 
  summarize(nb = length(species))

gbif_sf_eco <- gbif_sf_layer %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ID)) %>% 
  group_by(species, type) %>% 
  summarize(nb = length(species))


################################################################################
### 3. Map of regions per species
################################################################################


for(i in 1:length(spp)){
  spp_xx <- gbif_sf_layer %>%
    st_drop_geometry() %>% 
    filter(species == spp[i]) %>% 
    group_by(ID) %>% 
    summarize(nb = length(ID))
  
  eco_xx <- left_join(eco, spp_xx, by = "ID") %>% 
    filter(!is.na(nb))
  
  png(filename = paste0("figures/figures_MS_data/technical_validation_",spp[i],".png"),
      width = 16*200, height = 10*200, res = 200)
  print(ggplot(eco_xx) + geom_sf(aes(fill = nb), color = NA) +
          geom_sf(data = gbif_sf_layer[gbif_sf_layer$species == spp[i],], col = "black", shape = 3, size = 4, lwd = 3) +
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

