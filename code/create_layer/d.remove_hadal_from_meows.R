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
#hadal_ecoregions <- c(74,75,76,79,81,82,91,92,93,139,147,148,150,151,153,155,159,163,164,
#                      174,175,176,185,195,204,205,223,247)
hadal_ecoregions <- c(63,64,65,219,220,111,119,132,120,153,154,147,148,135,136,
                      134,130,126,128,129,125,127,121,51,48,47,46,53,54,122,123,124,
                      176,177,178)
# had_n <- data.frame(c("Sunda Java Trench",
#                       "Philippine Trench",
#                       "Marianas Trench",
#                       "Japan Trench",
#                       "Kurile-Kamchatka Trench",
#                       "Aleutian Trench",
#                       "Tonga Trench",
#                       "Kermadec Trench",
#                       "Middle America Trench",
#                       "Cayman Trench",
#                       "Puerto Rico Trench",
#                       "Peru-Chile Trench",
#                       "South-Sandwich Trench",
#                       "Papua Deep",
#                       "Indonesia Deep",
#                       "Vanuatu Deep",
#                       "Izu-Bonin Trench")) # using Jamieson et al., 2010
had_n <- data.frame(c("Aleutian-Japan",
                      "Philippine",
                      "Mariana",
                      "Bougainville-New Hebrides",
                      "Tonga-Kermadec",
                      "Peru-Chile",
                      "Java",
                      "Puerto Rico",
                      "Romanche",
                      "Southern Antilles")) # using Belayev 1989

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

# load("creating_layer/remove_hadal_from_coastal.RData")
# ggplot(eco[hadal_ecoregions,]) + geom_sf(color = NA) + theme_bw() +
#   geom_sf(data = eco[hadal_ecoregions[1],], fill = "red", color= NA)

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
  
  new_r <- aggregate(depth, fact = 4, fun = min)
  new_r[new_r>(-2000)] <- NA
  
  overlap_ri <- coverage_fraction(new_r, hadal_one)[[1]]
  overlap_ri[overlap_ri==0] <- NA
  overlap_ri[overlap_ri>0] <- 1
  new_ri <- new_r*overlap_ri
  new_poly <- rasterToPolygons(new_ri)
  new_poly <- st_as_sf(new_poly, crs = st_crs(eco)) %>%
    mutate(ID = 1) %>%
    group_by(ID) %>%
    summarize(geometry = st_union(geometry))
  
  ggplot(hadal_one) + geom_sf() + theme_bw() +
    geom_sf(data = new_poly, fill = "red", alpha = 0.5)
  ggplot(new_poly) + geom_sf()
  
  corr_poly <- st_difference(hadal_one, new_poly)
  
  ggplot(world) + geom_sf(data = new_poly, fill = "orange")
  
  # modify coastal ecoregion
  st_geometry(eco_no_hadal[which(eco_no_hadal$eco_id == hadal_ecoregions[h]),]) <- st_geometry(corr_poly)
  
  # add geometry of hadal region
  if(hadal_ecoregions[h]==74){
    new_poly <- new_poly %>%
      st_cast(to = "POLYGON")
    poly_1 <- new_poly %>%
      group_by(row.names(new_poly)) %>%
      mutate(lon = st_bbox(geometry)[3]) %>%
      filter(lon<163.1) %>%
      group_by(ID) %>%
      summarize(geometry = st_union(geometry)) %>%
      dplyr::select(-ID)
    poly_1 <- data.frame(c(hadal[5,],poly_1))
    
    poly_2 <- new_poly %>%
      st_cast(to = "POLYGON") %>%
      group_by(row.names(new_poly)) %>%
      mutate(lon = st_bbox(geometry)[3]) %>%
      filter(lon>163.1) %>%
      group_by(ID) %>%
      summarize(geometry = st_union(geometry)) %>%
      dplyr::select(-ID)
    poly_2 <- data.frame(c(hadal[6,],poly_2))
    
    hadal_poly <- rbind(poly_1, poly_2)
    rm(poly_1, poly_2)
  } else if (hadal_ecoregions[h] %in% c(46,47,48,51,53,54)){
    new_poly <- new_poly %>%
      dplyr::select(-ID)
    hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[1,], new_poly)))
  } else if (hadal_ecoregions[h] %in% c(122,123,124.125)){
    new_poly <- new_poly %>%
      dplyr::select(-ID)
    hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[2,], new_poly)))
  } else if (hadal_ecoregions[h] %in% c(121,127,126,128,129)){
    new_poly <- new_poly %>%
      dplyr::select(-ID)
    hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[3,], new_poly)))
  } else if (hadal_ecoregions[h] %in% c(130,134,135,136,148)){
    new_poly <- new_poly %>%
      dplyr::select(-ID)
    hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[4,], new_poly)))
  } else if (hadal_ecoregions[h] %in% c(147,153,154)){
    new_poly <- new_poly %>%
      dplyr::select(-ID)
    hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[5,], new_poly)))
  } else if (hadal_ecoregions[h] %in% c(176,177,178)){
    new_poly <- new_poly %>%
      dplyr::select(-ID)
    hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[6,], new_poly)))
  } else if (hadal_ecoregions[h] %in% c(111,119,120,132)){
    new_poly <- new_poly %>%
      dplyr::select(-ID)
    hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[7,], new_poly)))
  } else if (hadal_ecoregions[h] %in% c(63,64,65)){
    new_poly <- new_poly %>%
      dplyr::select(-ID)
    hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[8,], new_poly)))
  # } else if (hadal_ecoregions[h] == 195){
  #   new_poly <- new_poly %>%
  #     dplyr::select(-ID)
  #   hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[9,], new_poly)))
  } else if (hadal_ecoregions[h] %in% c(219,220)){
    new_poly <- new_poly %>%
      dplyr::select(-ID)
    hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[10,], new_poly)))}
  # } else if (hadal_ecoregions[h] %in% c(91,92)){
  #   new_poly <- new_poly %>%
  #     dplyr::select(-ID)
  #   hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[11,], new_poly)))
  # } else if (hadal_ecoregions[h] %in% c(204,205)){
  #   new_poly <- new_poly %>%
  #     dplyr::select(-ID)
  #   hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[12,], new_poly)))
  # } else if (hadal_ecoregions[h] ==247){
  #   new_poly <- new_poly %>%
  #     dplyr::select(-ID)
  #   hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[13,], new_poly)))
  # } else if (hadal_ecoregions[h] %in% c(163,164)){
  #   new_poly <- new_poly %>%
  #     dplyr::select(-ID)
  #   hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[14,], new_poly)))
  # } else if (hadal_ecoregions[h] == 159){
  #   new_poly <- new_poly %>%
  #     dplyr::select(-ID)
  #   hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[15,], new_poly)))
  # } else if (hadal_ecoregions[h] == 176){
  #   new_poly <- new_poly %>%
  #     dplyr::select(-ID)
  #   hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[16,], new_poly)))
  # } else if (hadal_ecoregions[h] %in% c(79,150)){
  #   new_poly <- new_poly %>%
  #     dplyr::select(-ID)
  #   hadal_poly <- rbind(hadal_poly, data.frame(c(hadal[17,], new_poly)))
  # } 
  
  save.image("outputs/hadal/remove_hadal_from_coastal.RData")
  
  rm(corr_poly, new_poly, overlap_ri, depth_poly, depth, new_ri, new_r,
     select_depth, overlap_rr, bbox_r, bbox_one, hadal_one)
}

load("outputs/hadal/remove_hadal_from_coastal.RData")

eco_hadal <- st_make_valid(eco_no_hadal) %>%
  dplyr::select(-Shape_Area, -Shape_Leng, -Regions, -percent) %>%
  mutate(had_id = NA,
         had_n = NA_character_) %>%
  dplyr::select(type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id, ID, had_id, had_n, geometry)

hadal_poly <- st_make_valid(st_as_sf(hadal_poly)) %>%
  rename(rlm_n = rlm) %>%
  group_by(type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id, ID, had_id, had_n) %>%
  summarize(geometry = st_union(geometry)) %>%
  dplyr::select(type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id, ID, had_id, had_n, geometry)

eco_hadal <- rbind(eco_hadal, hadal_poly)

st_write(obj = eco_hadal, 
         dsn = "outputs/hadal/provinces_p7s.shp")






