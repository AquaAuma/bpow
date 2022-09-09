
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
library(egg)

### Libraries
library(raster)
library(geosphere)
library(fasterize)
library(smoothie)
library(exactextractr)


# load biogeography layer
eco <- st_read("outputs/bpow/bpow_p5s4.shp")

holes <- st_read(dsn = "outputs/arcpro/post-processing_1", layer = "holes_p6s2_correct")
eco <- st_transform(eco, crs = st_crs(holes))
rm(holes)

# Load all depth shapefiles
depth_n0_s90_w180_e90 <- raster("data/gebco_2020_ascii/gebco_2020_n0.0_s-90.0_w-180.0_e-90.0.asc")
depth_n0_s90_w90_e0 <- raster("data/gebco_2020_ascii/gebco_2020_n0.0_s-90.0_w-90.0_e0.0.asc")
depth_n0_s90_w0_e90 <- raster("data/gebco_2020_ascii/gebco_2020_n0.0_s-90.0_w0.0_e90.0.asc")
depth_n0_s90_w90_e180 <- raster("data/gebco_2020_ascii/gebco_2020_n0.0_s-90.0_w90.0_e180.0.asc")
depth_n90_s0_w180_e90 <- raster("data/gebco_2020_ascii/gebco_2020_n90.0_s0.0_w-180.0_e-90.0.asc")
depth_n90_s0_w90_e0 <- raster("data/gebco_2020_ascii/gebco_2020_n90.0_s0.0_w-90.0_e0.0.asc")
depth_n90_s0_w0_e90 <- raster("data/gebco_2020_ascii/gebco_2020_n90.0_s0.0_w0.0_e90.0.asc")
depth_n90_s0_w90_e180 <- raster("data/gebco_2020_ascii/gebco_2020_n90.0_s0.0_w90.0_e180.0.asc")

obj = c('depth_n90_s0_w180_e90', 'depth_n90_s0_w90_e0',
        'depth_n90_s0_w0_e90', 'depth_n90_s0_w90_e180',
        "depth_n0_s90_w180_e90", "depth_n0_s90_w90_e0", 
        'depth_n0_s90_w0_e90', 'depth_n0_s90_w90_e180')
x.min = c(-180, -90, 0, 90, -180, -90, 0, 90)
x.max = c(-90, 0, 90, 180, -90, 0, 90, 180)
y.min = c(0, 0, 0, 0, -90, -90, -90, -90)
y.max = c(90, 90, 90, 90, 0, 0, 0, 0)

depth_files <- data.frame(obj) %>% 
  mutate(xmin = x.min, 
         xmax = x.max, 
         ymin = y.min, 
         ymax = y.max)
select_files_r <- raster(nrows = 2, ncols = 4, xmn = -180, xmx = 180,
                         ymn = -90, ymx = 90) %>% 
  rasterToPolygons() %>% 
  st_as_sf()


################################################################################
#### Remove hadal regions
################################################################################
eco_no_hadal <- eco

hadal_ecoregions <- c(46,47,48,51,53,54,122, #hadal 1
                      121,127,128,129, #hadal 2
                      123,125, #hadal 3
                      130,134,135,136,148, #hadal 4
                      146,157,195,196, #hadal 5
                      171,175,176,177,178, #hadal 6
                      111,119,131,132,120, #hadal 7
                      63,64,65,68, #hadal 8
                      60,166,167,168, #hadal 9
                      219,220) #hadal 10

# adapted from Belyaev 1989 & Jamieson, 2010
had_n <- data.frame(c("Aleutian-Japan",
                      "Philippine",
                      "Mariana",
                      "Bougainville-New Hebrides",
                      "Tonga-Kermadec",
                      "Peru-Chile",
                      "Java",
                      "Puerto Rico",
                      "Middle America",
                      "Southern Antilles"))

hadal <- data.frame(had_n) %>%
  mutate(type = "hadal",
         ID = c(180463:180472),
         prov_n = NA_character_,
         prov_id = NA,
         eco_n = NA_character_,
         eco_id = NA,
         rlm = NA_character_,
         rlm_id = NA,
         had_id = c(1:nrow(had_n))) %>%
  rename(had_n = `c..Aleutian.Japan....Philippine....Mariana....Bougainville.New.Hebrides...`)
hadal_poly <- data.frame()

abyssal <- data.frame(unique(eco[eco$type=="abyssal",]$prov_n)) %>% 
  mutate(type = "abyssal",
         ID = c(180473:180486),
         prov_id = unique(eco[eco$type =="abyssal",]$prov_id),
         eco_n = NA_character_,
         eco_id = NA,
         rlm = NA_character_,
         rlm_id = NA,
         had_id = NA,
         had_n = NA_character_) %>% 
  rename( prov_n = `unique.eco.eco.type.....abyssal.....prov_n.`)
abyssal_poly <- data.frame()


for(h in 1:length(hadal_ecoregions)){
  
  print(h)
  hadal_one <- eco[which(eco$eco_id == hadal_ecoregions[h]),]
  
  if(st_is_valid(hadal_one)==FALSE){
    hadal_one <- st_make_valid(hadal_one)
  }
  
  # extract lat/long boundaries
  bbox_one <- st_bbox(hadal_one)
  
  bbox_r <- raster(nrow=1, ncol=1, xmn = bbox_one[1], xmx = bbox_one[3],
                   ymn = bbox_one[2], ymx = bbox_one[4])
  
  overlap_rr <- coverage_fraction(bbox_r, select_files_r)
  select_depth <- c()
  for(r in 1:8){
    if(values(overlap_rr[[r]])==1){select_depth[length(select_depth)+1] <- depth_files$obj[r]}
    if(values(overlap_rr[[r]])>0 && values(overlap_rr[[r]]<1)){select_depth[length(select_depth)+1] <- depth_files$obj[r]}
  }
  
  if(length(select_depth)>1){
    for(k in 1:length(select_depth)){
      depth_k <- crop(get(select_depth[k]), y = extent(bbox_one[1],bbox_one[3],bbox_one[2],bbox_one[4]))
      if(k==1){depth <- depth_k}
      else{depth <- merge(depth, depth_k)}
      rm(depth_k)
    }
  }
  if(length(select_depth)==1){
    depth <- crop(get(select_depth), y = extent(bbox_one[1],bbox_one[3],bbox_one[2],bbox_one[4]))
  }
  
  depth_poly <- exact_extract(depth, hadal_one, include_xy = T)
  
  # save the hadal polygon
  new_r <- aggregate(depth, fact = 2, fun = min)
  new_r[new_r>(-6500)] <- NA
  
  overlap_ri <- coverage_fraction(new_r, hadal_one)[[1]]
  overlap_ri[overlap_ri==0] <- NA
  overlap_ri[overlap_ri>0] <- 1
  new_ri <- new_r*overlap_ri
  if(length(unique(new_ri))!=0){
    new_poly <- rasterToPolygons(new_ri)
    new_poly <- st_as_sf(new_poly, crs = st_crs(eco)) %>%
      mutate(ID = 1) %>%
      group_by(ID) %>%
      summarize(geometry = st_union(geometry))
    
    # ggplot(hadal_one) + geom_sf() + theme_bw() +
    #   geom_sf(data = new_poly, fill = "red", alpha = 0.5)
    # ggplot(new_poly) + geom_sf()
    
    if(st_is_valid(new_poly)==FALSE){
      new_poly <- st_make_valid(new_poly)
    }
    corr_poly <- st_difference(hadal_one, new_poly)
    
    # modify coastal ecoregion
    st_geometry(eco_no_hadal[which(eco_no_hadal$eco_id == hadal_ecoregions[h]),]) <- st_geometry(corr_poly)
    
    if (hadal_ecoregions[h] %in% c(46,47,48,51,53,54,122)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[1,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)
    } else if (hadal_ecoregions[h] %in% c(121,127,128,129)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[2,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)
    } else if (hadal_ecoregions[h] %in% c(123,125)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[3,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)
    } else if (hadal_ecoregions[h] %in% c(130,134,135,136,148)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[4,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)
    } else if (hadal_ecoregions[h] %in% c(146,157,195,196)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[5,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)
    } else if (hadal_ecoregions[h] %in% c(171,175,176,177,178)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[6,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)
    } else if (hadal_ecoregions[h] %in% c(111,119,131,132,120)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[7,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)
    } else if (hadal_ecoregions[h] %in% c(63,64,65,68)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[8,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)
    } else if (hadal_ecoregions[h] %in% c(60,166,167,168)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[9,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)
    } else if (hadal_ecoregions[h] %in% c(219,220)){
      new_poly <- new_poly %>%
        dplyr::select(-ID)
      new_poly <- st_as_sf(cbind(hadal[10,],new_poly))
      hadal_poly <- rbind(hadal_poly, new_poly)}
    
  }
  
  # remove abyssal from coastal
  new_r2 <- aggregate(depth, fact = 2, fun = min)
  new_r2[new_r2>(-3500)] <- NA
  new_r2[new_r2<(-6500)] <- NA
  
  overlap_ri2 <- coverage_fraction(new_r2, hadal_one)[[1]]
  overlap_ri2[overlap_ri2==0] <- NA
  overlap_ri2[overlap_ri2>0] <- 1
  new_ri2 <- new_r2*overlap_ri2
  
  if(length(unique(new_ri2))!=0 & length(unique(new_ri))!=0){
    new_poly2 <- rasterToPolygons(new_ri2)
    new_poly2 <- st_as_sf(new_poly2, crs = st_crs(eco)) %>%
      mutate(ID = 1) %>%
      group_by(ID) %>%
      summarize(geometry = st_union(geometry))
    
    if(st_is_valid(new_poly2)==FALSE){
      new_poly2 <- st_make_valid(new_poly2)
    }
    corr_poly2 <- st_difference(corr_poly, new_poly2)
    
    # modify coastal ecoregion
    st_geometry(eco_no_hadal[which(eco_no_hadal$eco_id == hadal_ecoregions[h]),]) <- st_geometry(corr_poly2)
    
    if (hadal_ecoregions[h] %in% c(46,47,48,51,53,54)){
      new_poly2 <- new_poly2 %>%
        dplyr::select(-ID)
      new_poly2 <- st_as_sf(cbind(abyssal[2,],new_poly2))
      abyssal_poly <- rbind(abyssal_poly, new_poly2)
      # two abyssal regions for 135
    } else if (hadal_ecoregions[h] %in% c(121,122,123,125,127,129,130)){
      new_poly2 <- new_poly2 %>%
        dplyr::select(-ID)
      new_poly2 <- st_as_sf(cbind(abyssal[3,],new_poly2))
      abyssal_poly <- rbind(abyssal_poly, new_poly2)
    } else if (hadal_ecoregions[h] %in% c(128,131)){
      new_poly2 <- new_poly2 %>%
        dplyr::select(-ID)
      new_poly2 <- st_as_sf(cbind(abyssal[4,],new_poly2))
      abyssal_poly <- rbind(abyssal_poly, new_poly2)
    } else if (hadal_ecoregions[h] %in% c(134,136,146,148,157,195,196)){
      new_poly2 <- new_poly2 %>%
        dplyr::select(-ID)
      new_poly2 <- st_as_sf(cbind(abyssal[6,],new_poly2))
      abyssal_poly <- rbind(abyssal_poly, new_poly2)
    } else if (hadal_ecoregions[h] %in% c(60,166,167,168,171,175,176,177,178,111)){
      new_poly2 <- new_poly2 %>%
        dplyr::select(-ID)
      new_poly2 <- st_as_sf(cbind(abyssal[14,],new_poly2))
      abyssal_poly <- rbind(abyssal_poly, new_poly2)
    } else if (hadal_ecoregions[h] %in% c(119,120,132)){
      new_poly2 <- new_poly2 %>%
        dplyr::select(-ID)
      new_poly2 <- st_as_sf(cbind(abyssal[7,],new_poly2))
      abyssal_poly <- rbind(abyssal_poly, new_poly2)
    } else if (hadal_ecoregions[h] %in% c(63,64,65,68)){
      new_poly2 <- new_poly2 %>%
        dplyr::select(-ID)
      new_poly2 <- st_as_sf(cbind(abyssal[12,],new_poly2))
      abyssal_poly <- rbind(abyssal_poly, new_poly2)
    } else if (hadal_ecoregions[h] %in% c(219,220)){
      new_poly2 <- new_poly2 %>%
        dplyr::select(-ID)
      new_poly2 <- st_as_sf(cbind(abyssal[8,],new_poly2))
      abyssal_poly <- rbind(abyssal_poly, new_poly2)
    } else if (hadal_ecoregions[h]==135){
      new_poly2 <- new_poly2 %>%
        dplyr::select(-ID) %>% 
        st_cast(to = "POLYGON") %>% 
        mutate(latitude = st_coordinates(st_centroid(geometry))[,2])
      new_poly2_a <- new_poly2 %>% 
        filter(latitude>(-2)) %>% 
        dplyr::select(-latitude) %>% 
        st_union()
      new_poly2_b <- new_poly2 %>% 
        filter(latitude<(-2)) %>% 
        dplyr::select(-latitude) %>% 
        st_union()
      new_poly2_a <- st_as_sf(cbind(abyssal[3,],new_poly2_a))
      new_poly2_b <- st_as_sf(cbind(abyssal[6,],new_poly2_b))
      abyssal_poly <- rbind(abyssal_poly, new_poly2_a, new_poly2_b)
      rm(new_poly2_a, new_poly2_b)
    }
    
    rm(corr_poly, new_poly, overlap_ri, depth_poly, depth, new_ri, new_r,
       select_depth, overlap_rr, bbox_r, bbox_one, hadal_one,
       new_ri2, new_poly2, new_r2, overlap_ri2)
  }
  save.image("outputs/hadal/remove_hadal_abyssal_from_coastal.RData")
  
}

load("outputs/hadal/remove_hadal_abyssal_from_coastal.RData")

eco_hadal <- st_make_valid(eco_no_hadal) %>%
  mutate(had_id = NA,
         had_n = NA_character_) %>%
  dplyr::select(ID, type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id, had_n, had_id, geometry)

hadal_poly <- st_make_valid(st_as_sf(hadal_poly)) %>%
  rename(rlm_n = rlm) %>%
  group_by(type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id, ID, had_id, had_n) %>%
  summarize(geometry = st_union(geometry)) %>%
  dplyr::select(ID, type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id, had_n, had_id, geometry)

abyssal_poly <- st_make_valid(st_as_sf(abyssal_poly)) %>%
  rename(rlm_n = rlm) %>%
  group_by(type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id, ID, had_id, had_n) %>%
  summarize(geometry = st_union(geometry)) %>%
  dplyr::select(ID, type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id, had_n, had_id, geometry)

eco_hadal <- rbind(eco_hadal, hadal_poly, abyssal_poly)
# transform geometry collection into multipolygon
st_geometry(eco_hadal[60226,]) <- st_geometry(st_collection_extract(eco_hadal[60226,], type = "POLYGON"))
st_geometry(eco_hadal[100307,]) <- st_geometry(st_collection_extract(eco_hadal[100307,], type = "POLYGON"))
st_geometry(eco_hadal[140385,]) <- st_geometry(st_collection_extract(eco_hadal[140385,], type = "POLYGON"))
st_geometry(eco_hadal[180461,]) <- st_geometry(st_collection_extract(eco_hadal[180461,], type = "POLYGON"))

eco_hadal <- eco_hadal %>%
  dplyr::select(-ID) %>% 
  distinct()
eco_hadal$ID <- 1:nrow(eco_hadal)

# save file
st_write(obj = eco_hadal, dsn="outputs/hadal/provinces_p7s3_abyssal_corr.shp",
         append = FALSE)
