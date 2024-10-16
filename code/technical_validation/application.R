################################################################################
#### Coding and data processing: Aurore Maureaud
#### Technical validation of the layer product
#### March 2023
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
library(gridExtra) 
library(robis)


################################################################################
### 1. Load data
################################################################################

# load biogeographic layer
eco <- st_read("outputs/bpow_p10_attributes.shp")

# load species data
spp <- c("Actinostola callosa","Pseudoliparis swirei","Centroscymnus coelolepis",
         "Chionoecetes opilio","Patiria pectinifera","Plesiobatis daviesi",
         "Tridacna gigas", "Psychropotes depressa")

# load GBIF
gbif <- fread(
  file.path("data/gbif_spp/0081481-230224095556074/occurrence.txt"),
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
# obis <- read_csv("data/obis_spp/Occurrence.csv") %>%
#   dplyr::select(
#     "decimallatitude",
#     "decimallongitude",
#     "coordinateuncertaintyinmeters",
#     "year",
#     "month",
#     "day",
#     "maximumdepthinmeters",
#     "minimumdepthinmeters",
#     "genus",
#     "species",
#     "specificepithet",
#     "infraspecificepithet",
#     "occurrencestatus") %>%
#   filter(!is.na(decimallatitude),
#          !is.na(decimallongitude),
#          occurrencestatus %in% c("present","Present","P","Presente")) %>%
#   distinct() %>%
#   rename(latitude = decimallatitude,
#          longitude = decimallongitude,
#          coord_error = coordinateuncertaintyinmeters,
#          depth = maximumdepthinmeters) %>%
#   dplyr::select(year, month, day, latitude, longitude, coord_error,
#          species, depth) %>%
#   mutate(source = "OBIS")


sppdat <- data.frame(matrix(ncol = 21, nrow = 0))
names(sppdat) <- c('date_year','scientificName','decimalLatitude','identificationQualifier',
                   'decimalLongitude','maximumDepthInMeters','eventDate',
                   'coordinateUncertaintyInMeters','marine','minimumDepthInMeters',
                   'datasetID','flags','depth','shoredistance','datasetName','habitat','geodeticDatum','dataset_id','country',
                   'month', 'day')
sppdat <- sppdat[,order(names(sppdat))]

for (i in 1:length(spp)){

    sppoc <- occurrence(scientificname = spp[i])
    if(nrow(sppoc)>0){
      sppoc <- sppoc %>%
        dplyr::select(one_of('date_year','scientificName','decimalLatitude','decimalLongitude','identificationQualifier',
                      'maximumDepthInMeters','eventDate','coordinateUncertaintyInMeters','marine','minimumDepthInMeters',
                      'datasetID','flags','depth','shoredistance','datasetName','habitat','geodeticDatum','dataset_id','country',
                      "day","month")) %>%
        mutate(scientificName = spp[i])
      missing <- setdiff(names(sppdat), names(sppoc))

      if(length(missing)>0){
        sppoc <- cbind(sppoc, matrix(ncol=length(missing), nrow=nrow(sppoc)))
        names(sppoc)[(ncol(sppdat)+1-length(missing)):ncol(sppdat)] <- missing
        sppoc <- sppoc[,order(names(sppoc))]
        sppdat <- rbind(sppdat, sppoc)
      }
      if(length(missing)==0){
        sppoc <- sppoc[,order(names(sppoc))]
        sppdat <- rbind(sppdat, sppoc)
      }
    }
    rm(sppoc)
}

sppdat <- sppdat %>%
  rename(year = date_year,
         latitude = decimalLatitude,
         longitude = decimalLongitude,
         coord_error = coordinateUncertaintyInMeters,
         species = scientificName
         ) %>%
  mutate(source = "OBIS") %>%
  distinct() %>%
  dplyr::select(names(gbif))

write.csv(sppdat, 'data/obis_spp/application_obis_March14.csv', row.names = F)


# merge and deduplicate
occ <- rbind(obis, gbif) %>% 
  #filter(coord_error < 5000) %>% 
  group_by(year, month, day, latitude, longitude, species) %>% 
  distinct()

# load FAO fishing areas
fao <- st_read("/Users/auroremaureaud/Documents/Yale University/Marine Biogeography/regions_base/fao/World_Fao_Zones.shp")


################################################################################
### 2. Intersection with layer
################################################################################

occ_sf <- occ %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(eco))
occ_sf_layer <- st_join(occ_sf, eco, left=TRUE, largest=FALSE)
occ_sf_layer <- st_join(occ_sf_layer, fao, left = TRUE, largest = TRUE)

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
### 3. Separate "true" versus "false" presences for BATHYMETRY
################################################################################
spp <- sort(unique(occ$species))

occ_sf_layer <- occ_sf_layer %>% 
  mutate(type_tf = ifelse(species %in% c(spp[1:4],spp[6:7],spp[10]) & type == "coastal", "TRUE", "FALSE"),
         type_tf = ifelse(species %in% c(spp[1],spp[3:5],spp[9]) & type == "bathyal", "TRUE", type_tf),
         type_tf = ifelse(species %in% c(spp[3],spp[5],spp[8:9]) & type == "abyssal", "TRUE", type_tf),
         type_tf = ifelse(species==spp[8] & type == "hadal", "TRUE", type_tf),
         type_tf_fao = ifelse(species == spp[1] & zone %in% c(18,21,27,31),
                              "TRUE", "FALSE"),
         type_tf_fao = ifelse(species == spp[3] & zone %in% c(18,21,27,31,34,37,41,47,51,57,58,61,71,81),
                              "TRUE", type_tf_fao),
         type_tf_fao = ifelse(species == spp[4] & zone %in% c(18,21,27,61,67),
                              "TRUE", type_tf_fao),
         type_tf_fao = ifelse(species == spp[5] & zone %in% c(21,27,31,34,41,47,48,57,58,61,67,77,81,87),
                              "TRUE", type_tf_fao),
         # type_tf_fao = ifelse(species == spp[6] & zone %in% c(),
         #                      "TRUE", "FALSE"),
         type_tf_fao = ifelse(species == spp[7] & zone %in% c(51,57,61,71,77,81),
                              "TRUE", type_tf_fao),
         # type_tf_fao = ifelse(species == spp[8] & zone %in% c(),
         #                      "TRUE", "FALSE"),
         type_tf_fao = ifelse(species == spp[9] & zone %in% c(31),
                              "TRUE", type_tf_fao),
         type_tf_fao = ifelse(species == spp[10] & zone %in% c(51,57,61,71,77),
                              "TRUE", type_tf_fao))

summary_tab <- data.frame()

pdf(file = "figures/appendix/true_false_bathymetry.pdf",
    width = 14)
for(i in 1:length(spp)){
  if(spp[i] != "Callinectes sapidus"){

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
  print(ggplot(eco_xx[eco_xx$type_tf == "TRUE" ,]) + geom_sf(aes(fill = prop), color = NA) +
    scale_fill_gradient(limits = c(0,100), low= brewer.pal(9,"OrRd")[2], high = brewer.pal(9,"OrRd")[8]) +
    new_scale_fill() +
    geom_sf(data = eco_xx[eco_xx$type_tf == "FALSE" ,], aes(fill = prop), color = NA) +
    scale_fill_gradient(limits = c(0,100), low= brewer.pal(9,"BuPu")[2], high = brewer.pal(9,"BuPu")[8]) +
    theme_minimal() +
    geom_sf(data = world, fill = "grey", color=NA) +
    # coord_sf(crs = '+proj=moll') +
    theme(legend.title = element_blank(),
          legend.key.size = unit(0.5, 'cm'),
          legend.key.height = unit(0.5, 'cm'),
          legend.key.width = unit(0.5, 'cm'),
          legend.text = element_text(size=10),
          legend.position = "bottom") +
    ggtitle(paste0(spp[i])))

  #make map with table
  spp_tab <- data.frame(variable = c("total","true","false"),
                        value = c(round(sum(spp_xx$nb)),
                                  round(sum(spp_xx[spp_xx$type_tf==TRUE,]$prop, na.rm=T),2),
                                  round(sum(spp_xx[spp_xx$type_tf==FALSE,]$prop, na.rm=T),2)),
                        species = rep(spp[i], times = 3))
  summary_tab <- rbind(summary_tab, spp_tab)
  }
}
dev.off()


write.csv(summary_tab, file = "outputs/application/summary_tab.csv", row.names = FALSE)


################################################################################
### 4. Separate "true" versus "false" presences for FAO FISHING AREAS
################################################################################

summary_tab <- data.frame()
pdf(file = "figures/appendix/true_false_fao.pdf",
    width = 14)
for(i in 1:length(spp)){
  if(!spp[i] %in% c("Callinectes sapidus","Patiria pectinifera",
                    "Pseudoliparis swirei")){
    spp_xx <- occ_sf_layer %>%
      st_drop_geometry() %>% 
      filter(species == spp[i]) %>% 
      group_by(ID, type_tf_fao, zone) %>% 
      summarize(nb = length(ID)) %>% 
      filter(!is.na(ID))
    
    spp_xx <- spp_xx %>% 
      mutate(prop = nb/sum(spp_xx$nb)*100)
    
    eco_xx <- left_join(eco, spp_xx, by = "ID") %>% 
      filter(!is.na(nb))
    
    # map map
    print(ggplot(eco_xx[eco_xx$type_tf_fao == "TRUE",]) + geom_sf(aes(fill = prop), color = NA) +
            scale_fill_gradient(limits = c(0,100), low= brewer.pal(9,"OrRd")[2], high = brewer.pal(9,"OrRd")[8]) +
            new_scale_fill() +
            geom_sf(data = eco_xx[eco_xx$type_tf_fao == "FALSE",], aes(fill = prop), color = NA) +
            scale_fill_gradient(limits = c(0,100), low= brewer.pal(9,"BuPu")[2], high = brewer.pal(9,"BuPu")[8]) +
            theme_minimal() +
            geom_sf(data = world, fill = "grey", color=NA) +
            # coord_sf(crs = '+proj=moll') +
            theme(legend.title = element_blank(),
                  legend.key.size = unit(0.5, 'cm'),
                  legend.key.height = unit(0.5, 'cm'),
                  legend.key.width = unit(0.5, 'cm'),
                  legend.text = element_text(size=10),
                  legend.position = "bottom") +
            ggtitle(paste0(spp[i])))
    
    #make map with table
    spp_tab <- data.frame(variable = c("total","true","false"),
                          value = c(round(sum(spp_xx$nb)),
                                    round(sum(spp_xx[spp_xx$type_tf_fao==TRUE,]$prop, na.rm=T),2),
                                    round(sum(spp_xx[spp_xx$type_tf_fao==FALSE,]$prop, na.rm=T),2)),
                          species = rep(spp[i], times = 3))
    summary_tab <- rbind(summary_tab, spp_tab)
  }
}
dev.off()

write.csv(summary_tab, file = "outputs/application/summary_tab_fao.csv", row.names = FALSE)


################################################################################
### 5. MAPS OF BOth CRITERIA
################################################################################

summary_tab <- data.frame()
pdf(file = "figures/appendix/true_false_both.pdf",
    width = 14)
for(i in 1:length(spp)){
  if(!spp[i] %in% c("Callinectes sapidus","Patiria pectinifera",
                    "Pseudoliparis swirei")){
    spp_xx <- occ_sf_layer %>%
      st_drop_geometry() %>% 
      filter(species == spp[i]) %>% 
      group_by(ID, type_tf_fao, type_tf,) %>% 
      summarize(nb = length(ID)) %>% 
      filter(!is.na(ID))
    
    ids <- sort(unique(spp_xx$ID))
    spp_xx_2 <- data.frame()
    for(j in 1:length(ids)){
      y <- spp_xx %>% 
        filter(ID == ids[j])
      if(nrow(y)>1){
        y <- y %>% 
          mutate(type_tf_fao = "TRUE") %>% 
          group_by(ID, type_tf_fao, type_tf) %>% 
          summarize(nb = sum(nb))
        if(j==1){spp_xx_2 <- y} else {spp_xx_2[nrow(spp_xx_2)+1,] <- y}
      } else {
        if(j==1){spp_xx_2 <- y} else {spp_xx_2[nrow(spp_xx_2)+1,] <- y}
      }
    }
    
    spp_xx_2 <- spp_xx_2 %>% 
      mutate(prop = nb/sum(spp_xx_2$nb)*100,
             flag = ifelse(type_tf==TRUE & type_tf_fao == TRUE, "TRUE (bathy) & TRUE (fao)", "FALSE"),
             flag = ifelse(type_tf==FALSE & type_tf_fao == TRUE, "FALSE (bathy) & TRUE (fao)", flag),
             flag = ifelse(type_tf==TRUE & type_tf_fao == FALSE, "TRUE (bathy) & FALSE (fao)", flag),
             flag = ifelse(type_tf==FALSE & type_tf_fao == FALSE, "FALSE (bathy) & FALSE (fao)", flag))
    
    eco_xx <- left_join(eco, spp_xx_2, by = "ID") %>% 
      filter(!is.na(nb))
    
    palette_tf <- c("purple","lightsteelblue2","plum3","orange")
    names_tf <- c("FALSE (bathy) & FALSE (fao)", "FALSE (bathy) & TRUE (fao)", 
                  "TRUE (bathy) & FALSE (fao)","TRUE (bathy) & TRUE (fao)")
    names(palette_tf) <- names_tf
    
    # map
    print(ggplot(eco_xx) + geom_sf(aes(fill = flag), color = NA) +
            scale_fill_manual(values = palette_tf) +
            theme_minimal() +
            geom_sf(data = world, fill = "grey", color=NA) +
            theme(legend.title = element_blank(),
                  legend.key.size = unit(0.5, 'cm'),
                  legend.key.height = unit(0.5, 'cm'),
                  legend.key.width = unit(0.5, 'cm'),
                  legend.text = element_text(size=10),
                  legend.position = "bottom") +
            ggtitle(paste0(spp[i])))
    
    #make map with table
    spp_tab <- data.frame(variable = c("total",
                                       "true (bathy) & true (fao)",
                                       "true (bathy) & false (fao)",
                                       "false (bathy) & true (fao)",
                                       "false (bathy) & false (fao)"),
                          value = c(round(sum(spp_xx_2$nb)),
                                    round(sum(spp_xx_2[spp_xx_2$flag=="TRUE (bathy) & TRUE (fao)",]$prop, na.rm=T),2),
                                    round(sum(spp_xx_2[spp_xx_2$flag=="TRUE (bathy) & FALSE (fao)",]$prop, na.rm=T),2),
                                    round(sum(spp_xx_2[spp_xx_2$flag=="FALSE (bathy) & TRUE (fao)",]$prop, na.rm=T),2),
                                    round(sum(spp_xx_2[spp_xx_2$flag=="FALSE (bathy) & FALSE (fao)",]$prop, na.rm=T),2)),
                          species = rep(spp[i], times = 5))
    summary_tab <- rbind(summary_tab, spp_tab)
  }
}
dev.off()

write.csv(summary_tab, file = "outputs/application/summary_tab_both.csv", row.names = FALSE)

