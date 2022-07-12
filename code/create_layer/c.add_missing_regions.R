###############################################################################
#### Set up marine regions for the whole ocean sea floor
#### Code to characterize missing regions and associate a polygon to them
#### Coding and data processing: Aurore Maureaud
#### July 2022
################################################################################

rm(list = ls())

### Libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(RColorBrewer)
library(raster)
library(geosphere)
library(fasterize)
library(smoothie)
library(exactextractr)
library(Morpho)
library(rgl)
library(units)


################################################################################
### Load data & fix shapefile attributes
################################################################################
# Load seafloor shapefile = meows-deepsea
seafloor_meow_deepsea <- st_read(dsn = "outputs/seafloor", layer = "seafloor_meows-deepsea") %>% 
  mutate(type = case_when(layer=="abyssal_fixbathyal_Rfix" ~ "abyssal",
                          layer=="GOODSprovinces_bathyal_Rfix" ~ "bathyal",
                          is.na(layer) ~ "MEOW"),
         meow_prov_name = ifelse(str_detect(Province, "[a-z]")==TRUE,Province,NA),
         abyssal_id = ifelse(str_detect(Province, "[a-z]")==FALSE & type=="abyssal",Province,NA),
         bathyal_id = ifelse(str_detect(Province, "[a-z]")==FALSE & type=="bathyal",Province,NA),
         abyssal_name = case_when(abyssal_id=="1" ~ "Arctic Basin",
                                  abyssal_id=="2" ~ "North Atlantic",
                                  abyssal_id=="3" ~ "Brazil Basin",
                                  abyssal_id=="4" ~ "Angola, Guinea, Sierra Leone Basins",
                                  abyssal_id=="5" ~ "Argentine Basin",
                                  abyssal_id=="6" ~ "Antarctica Basin",
                                  abyssal_id=="7" ~ "Antarctica West",
                                  abyssal_id=="8" ~ "Indian",
                                  abyssal_id=="9" ~ "Chile, Peru, Guatemala Basins",
                                  abyssal_id=="10" ~ "South Pacific",
                                  abyssal_id=="11" ~ "Equatorial Pacific",
                                  abyssal_id=="12" ~ "North Central Pacific",
                                  abyssal_id=="13" ~ "North Pacific",
                                  abyssal_id=="14" ~ "West Pacific Basins",
                                  is.na(abyssal_id) ~ "NA"),
         bathyal_name = case_when(bathyal_id=="1" ~ "Arctic",
                                  bathyal_id=="2" ~ "Northern Atlantic Boreal",
                                  bathyal_id=="3" ~ "Northern Pacific Boreal",
                                  bathyal_id=="4" ~ "North Atlantic",
                                  bathyal_id=="5" ~ "Southeast Pacific Ridges",
                                  bathyal_id=="6" ~ "New Zealand-Kermadec",
                                  bathyal_id=="7" ~ "Cocos Plate",
                                  bathyal_id=="8" ~ "Nazca Plate",
                                  bathyal_id=="9" ~ "Antarctic",
                                  bathyal_id=="10" ~ "Subantarctic",
                                  bathyal_id=="11" ~ "Indian",
                                  bathyal_id=="12" ~ "West Pacific",
                                  bathyal_id=="13" ~ "South Pacific",
                                  bathyal_id=="14" ~ "North Pacific",
                                  is.na(bathyal_id) ~ "NA")) %>% 
  rename(meow_eco_id = ECO_CODE_X,
         meow_prov_id = PROV_CODE,
         meow_rlm_id = RLM_CODE,
         meow_eco_name = ECOREGION,
         meow_rlm_name = REALM,
         deepsea_prov_id = ID) %>% 
  dplyr::select(-path, -layer, -ALT_CODE, -ECO_CODE, -Lat_Zone, -Name, -Province)
seafloor_meow_deepsea$ID <- c(1:nrow(seafloor_meow_deepsea))


# Load holes shapefile - done by Griffy Vigneron from the low res land layer and the seafloor_meows_deepsea layer
# with news polygons missing last time + removed odd lines
holes <- st_read(dsn = "outputs/SeafloorMeowsANDNELandHoles", layer = "SeafloorMeowsANDNELandHoles")
holes <- holes %>% 
  st_cast("POLYGON") %>% 
  mutate(ID = c(1:nrow(holes)),
         Shape_Area = drop_units(st_area(geometry)),
         Shape_Leng = drop_units(st_length(geometry))) %>% 
  dplyr::select(-Id) %>% 
  filter(!ID %in% c(1713,2210)) # remove arctic polygon, as well as Caspian sea
# there are 2210 holes corresponding to uncharacterized regions

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
### Loop to correct for missing areas
################################################################################
# <- seafloor_meow_deepsea
#problems <- c()

# try for h=382
# problem at h=35, 89, 161, 174, 189, 265, 395, 419
#problems <- c(35,89,161,174,189,265,395,419,693,694,695,696,
#              701)
#load("outputs/fill_holes/results_fill_holes.RData")

load("outputs/fill_holes/envt_2.RData")

for(h in 1530:nrow(holes)){
  print(h)
  
  hole_one <- holes[h,]
  
  # extract lat/long boundaries
  bbox_one <- st_bbox(hole_one)
  
  # create bounding box for the raster shapefile & polygons
  x_range <- abs(abs(bbox_one$xmax) - abs(bbox_one$xmin))
  y_range <- abs(abs(bbox_one$ymax) - abs(bbox_one$ymin))
  new_bbox <- as.vector(bbox_one)
  new_bbox[1] <- new_bbox[1] -0.1*x_range
  new_bbox[2] <- new_bbox[2] -0.1*y_range
  new_bbox[3] <- new_bbox[3] +0.1*x_range
  new_bbox[4] <- new_bbox[4] +0.1*y_range
  
  if(new_bbox[1]<(-180)){new_bbox[1] <- (-180)}
  if(new_bbox[2]<(-90)){new_bbox[2] <- (-90)}
  if(new_bbox[3]>180){new_bbox[3] <- 180}
  if(new_bbox[4]>90){new_bbox[4] <- 90}
  
  # get the depth raster(s) around that zone
  # intersect between the new_bbox and the select files
  new_bbox_r <- raster(nrow=1, ncol=1, xmn = new_bbox[1], xmx = new_bbox[3],
                       ymn = new_bbox[2], ymx = new_bbox[4])
  
  overlap_rr <- coverage_fraction(new_bbox_r, select_files_r)
  select_depth <- c()
  for(r in 1:8){
    if(values(overlap_rr[[r]])==1){select_depth[length(select_depth)+1] <- depth_files$obj[r]}
    if(values(overlap_rr[[r]])>0 && values(overlap_rr[[r]]<1)){select_depth[length(select_depth)+1] <- depth_files$obj[r]}
  }
  
  # extract the zone around the bbox from the raster
  if(length(select_depth)>0){
    if(length(select_depth)>1){
      for(k in 1:length(select_depth)){
        depth_k <- crop(get(select_depth[k]), y = extent(new_bbox[1],new_bbox[3],new_bbox[2],new_bbox[4]))
        if(k==1){depth <- depth_k}
        else{depth <- merge(depth, depth_k)}
        rm(depth_k)
      }
    }
    if(length(select_depth)==1){
      depth <- crop(get(select_depth), y = extent(new_bbox[1],new_bbox[3],new_bbox[2],new_bbox[4]))
    }
    
    while((dim(depth)[1]*dim(depth)[2])<10){
      new_bbox[1] <- new_bbox[1] -0.3*x_range
      new_bbox[2] <- new_bbox[2] -0.3*y_range
      new_bbox[3] <- new_bbox[3] +0.3*x_range
      new_bbox[4] <- new_bbox[4] +0.3*y_range
      if(length(select_depth)>1){
        for(k in 1:length(select_depth)){
          depth_k <- crop(get(select_depth[k]), y = extent(new_bbox[1],new_bbox[3],new_bbox[2],new_bbox[4]))
          if(k==1){depth <- depth_k}
          if(k>1){depth <- merge(depth, depth_k)}
          rm(depth_k)
        }
      }
      if(length(select_depth)==1){
        depth <- crop(get(select_depth), y = extent(new_bbox[1],new_bbox[3],new_bbox[2],new_bbox[4]))
      }
    }
    
    # print final dimensions
    print(dim(depth)[1]*dim(depth)[2])
    
    # extract necessary polygons
    depth_spdf <- as(depth, "SpatialPixelsDataFrame")
    depth_df <- as.data.frame(depth_spdf)
    colnames(depth_df) <- c("depth", "x", "y")
    
    # select polygons from shapefile and depth raster
    depth_pts <- data.frame(rasterToPoints(depth))
    depth_pts_sf <- st_as_sf(depth_pts, coords = c("x","y"), crs = st_crs(seafloor_meow_deepsea))
    select_polygons <- st_intersects(depth_pts_sf, seafloor_meow_deepsea)
    select_polygons <- unique(unlist(select_polygons))
    select_polygons <- seafloor_meow_deepsea[select_polygons,]
    
    if(nrow(select_polygons)==1){
      new_poly <- select_polygons %>% st_drop_geometry()
      st_geometry(new_poly) <- st_geometry(holes[h,])
      seafloor_meow_deepsea_filled <- rbind(seafloor_meow_deepsea_filled, new_poly)
      save(seafloor_meow_deepsea_filled, file="outputs/fill_holes/results_fill_holes_2.RData")
      save.image("outputs/fill_holes/envt_2.RData")
      rm(new_poly)
      print("One polygon")
    }
    
    if(nrow(select_polygons)>1 && dim(depth)[2]==1){
      new_poly <- select_polygons[1,] %>% st_drop_geometry()
      st_geometry(new_poly) <- st_geometry(holes[h,])
      seafloor_meow_deepsea_filled <- rbind(seafloor_meow_deepsea_filled, new_poly)
      save(seafloor_meow_deepsea_filled, file="outputs/fill_holes/results_fill_holes_2.RData")
      save.image("outputs/fill_holes/envt_2.RData")
      rm(new_poly)
      print("Column polygon")}
    
    if(nrow(select_polygons)>1 && dim(depth)[2]>1){
      select_polygons_merged <- st_union(select_polygons) # a bit long to run, only 3 polygons
      xx <- coverage_fraction(depth, select_polygons_merged)
      
      depths_empty <- as.data.frame(as(xx[[1]], "SpatialPixelsDataFrame")) %>% 
        rename(coverage = layer) %>% 
        filter(coverage<1) %>% 
        left_join(depth_pts, by=c('x','y'))
      
      depths_full <- as.data.frame(as(xx[[1]], "SpatialPixelsDataFrame")) %>% 
        rename(coverage = layer) %>% 
        filter(coverage==1) %>% 
        left_join(depth_pts, by=c('x','y'))
      
      # query will be depth_pts_empty transformed in 3D matrix
      depth_pts_empty_df <- depths_empty %>% 
        rename(z = names(depths_empty)[4]) %>% 
        mutate(z = z) %>% 
        dplyr::select(x,y,z)
      depth_pts_empty_mat <- data.matrix(depth_pts_empty_df)
      
      # target will be depth_pts transformed in 3D matrix
      depth_pts_df <- depths_full %>% 
        rename(z = names(depths_empty)[4]) %>% 
        mutate(z = z) %>% 
        dplyr::select(x,y,z)
      depth_pts_mat <- data.matrix(depth_pts_df)
      
      # try out the function on depth example
      xx <- mcNNindex(target = depth_pts_mat, query = depth_pts_empty_mat, k=1)
      depth_pts_empty_closest <- data.frame(cbind(depth_pts_empty_mat, xx)) %>% 
        mutate(x_closest = NA,
               y_closest = NA,
               poly = NA_character_)
      
      types <- c(select_polygons$ID)
      pts <- st_as_sf(depth_pts_df, coords = c("x","y"), crs= st_crs(select_polygons))
      tt <- unlist(st_intersects(pts, select_polygons, sparse=T))
      
      for(i in 1:nrow(depth_pts_empty_closest)){
        # assign closest coords
        depth_pts_empty_closest$x_closest[i] <- depth_pts_df$x[depth_pts_empty_closest$V4[i]]
        depth_pts_empty_closest$y_closest[i] <- depth_pts_df$y[depth_pts_empty_closest$V4[i]]
        # assign polygon
        depth_pts_empty_closest$poly[i] <- types[tt[[depth_pts_empty_closest$V4[i]]]]
      }
      
      # rasterize the closest points and aggregate a higher scale
      # create new grid with lower resolution
      # same for rasters
      new_r <- aggregate(depth, fact = 4, fun = mean)
      
      overlap_ri <- coverage_fraction(new_r, holes[h,])[[1]]
      overlap_ri[overlap_ri==0] <- NA
      overlap_ri[overlap_ri>0] <- 1
      new_ri <- new_r*overlap_ri
      
      new_poly <- rasterToPolygons(new_ri)
      new_poly <- st_as_sf(new_poly, crs = st_crs(seafloor_meow_deepsea_filled))
      new_poly$ID <- c(1:nrow(new_poly))
      
      depth_pts_empty_closest_sf <- st_as_sf(depth_pts_empty_closest, coords = c('x','y'), 
                                             crs = st_crs(new_poly))
      new_poly_closest <- st_join(new_poly, depth_pts_empty_closest_sf, left=T) %>% 
        st_drop_geometry() %>% 
        group_by(ID, poly) %>% summarise(n=length(poly)) %>% slice_max(n, with_ties = FALSE)
      
      new_poly <- left_join(new_poly, new_poly_closest, by = "ID") %>% 
        filter(n>0) %>% 
        group_by(poly) %>% 
        summarize(geometry = st_union(geometry))
      
      # get attributes from seafloor shapefile
      select_polygons <- select_polygons %>% 
        filter(ID %in% new_poly$poly) %>% 
        st_drop_geometry()
      new_poly <- new_poly %>% 
        mutate(poly = as.numeric(as.vector(poly)))
      new_poly <- left_join(select_polygons, new_poly, by=c("ID"="poly"))
      new_poly <- st_as_sf(new_poly, crs = st_crs(seafloor_meow_deepsea_filled))
      seafloor_meow_deepsea_filled <- rbind(seafloor_meow_deepsea_filled,new_poly)
      
      save(seafloor_meow_deepsea_filled, file="outputs/fill_holes/results_fill_holes_2.RData")
      save.image("outputs/fill_holes/envt_2.RData")
      print("Multi polygons")
      
      rm(new_poly, new_poly_closest, new_r, new_ri, types, depth_pts_empty_closest,
         depth_pts_empty_df, depth_pts_empty_mat,
         depths_empty, depths_full,select_polygons_merged,
         xx, depth_pts_mat)
      
    }
    
    rm(depth_pts, depth_pts_sf, select_polygons, depth_spdf, depth_df, depth,
       overlap_rr, new_bbox_r, select_depth, new_bbox, x_range, y_range, hole_one,
       bbox_one)
    
  }
  
  else{problems[length(problems)+1] <- h
  print("length(select_depth) not positive")
  save.image("outputs/fill_holes/envt_2.RData")}
  
}



### PLOTS
# plot polygons
ggplot() + geom_sf(data = hole_one, fill="grey") + theme_bw() +
  geom_sf(data = select_polygons, aes(fill=as.factor(ID))) +
  coord_sf(xlim = c(new_bbox[1], new_bbox[3]), ylim = c(new_bbox[2], new_bbox[4])) +
  coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y)))

# # plots 3D closest polygon
ggplot()+
  geom_point(data = depth_pts_empty_closest, aes(x = x, y = y, colour = poly), size=2) +
  geom_sf(data = seafloor_meow_deepsea, fill=NA, colour="black") +
  coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y)))

# # plot depth
ggplot() + geom_tile(data = depth_df, aes(x=x, y=y, fill = depth)) +
  scale_fill_gradientn(colours = terrain.colors(4)) + theme_bw() +
  geom_sf(data = seafloor_meow_deepsea, fill=NA, colour="black") +
  coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y)))

# # plot smoothed polygons
ggplot() + geom_sf(data = new_r, aes(fill = poly), colour = NA)
ggplot() + geom_sf(data = new_poly, aes(fill = as.factor(ID)), colour = NA) +
  geom_sf(data = seafloor_meow_deepsea, fill=NA, colour="black") +
  coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y)))

ggplot() + geom_sf(data = new_poly, colour = NA, fill="grey") + theme_minimal()+
  geom_sf(data = seafloor_meow_deepsea, fill="black", colour=NA) +
  coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y)))



### Run the loop for problems
load("outputs/fill_holes/envt.RData")
problems_pb <- c()
seafloor_meow_deepsea_filled_pb <- seafloor_meow_deepsea_filled

# for(h in 5:length(problems)){
#   print(h)
#   
#   hole_one <- holes %>% filter(ID == holes$ID[problems[h]])
#   
#   # extract lat/long boundaries
#   bbox_one <- st_bbox(hole_one)
#   
#   # create bounding box for the raster shapefile & polygons
#   x_range <- abs(abs(bbox_one$xmax) - abs(bbox_one$xmin))
#   y_range <- abs(abs(bbox_one$ymax) - abs(bbox_one$ymin))
#   new_bbox <- as.vector(bbox_one)
#   new_bbox[1] <- new_bbox[1] -0.1*x_range
#   new_bbox[2] <- new_bbox[2] -0.1*y_range
#   new_bbox[3] <- new_bbox[3] +0.1*x_range
#   new_bbox[4] <- new_bbox[4] +0.1*y_range
#   
#   if(new_bbox[1]<(-180)){new_bbox[1] <- (-180)}
#   if(new_bbox[2]<(-90)){new_bbox[2] <- (-90)}
#   if(new_bbox[3]>180){new_bbox[3] <- 180}
#   if(new_bbox[4]>90){new_bbox[4] <- 90}
#   
#   # get the depth raster(s) around that zone
#   # intersect between the new_bbox and the select files
#   new_bbox_r <- raster(nrow=1, ncol=1, xmn = new_bbox[1], xmx = new_bbox[3],
#                        ymn = new_bbox[2], ymx = new_bbox[4])
#   
#   overlap_rr <- coverage_fraction(new_bbox_r, select_files_r)
#   select_depth <- c()
#   for(r in 1:8){if(values(overlap_rr[[r]])==1){select_depth[length(select_depth)+1] <- depth_files$obj[r]}
#     if(values(overlap_rr[[r]])>0 & values(overlap_rr[[r]]<1)){select_depth[length(select_depth)+1] <- depth_files$obj[r]}
#     }
#   
#   # extract the zone around the bbox from the raster
#   if(length(select_depth)>0){
#     if(length(select_depth)>1){
#       for(k in 1:length(select_depth)){
#         depth_k <- crop(get(select_depth[k]), y = extent(new_bbox[1],new_bbox[3],new_bbox[2],new_bbox[4]))
#         if(k==1){depth <- depth_k}
#         else{depth <- merge(depth, depth_k)}
#       }
#     }
#     if(length(select_depth)==1){
#       depth <- crop(get(select_depth), y = extent(new_bbox[1],new_bbox[3],new_bbox[2],new_bbox[4]))
#     }
#     
#     print(dim(depth)[1]*dim(depth)[2])
#     
#       # extract necessary polygons
#       depth_spdf <- as(depth, "SpatialPixelsDataFrame")
#       depth_df <- as.data.frame(depth_spdf)
#       colnames(depth_df) <- c("depth", "x", "y")
#       
#       # select polygons from shapefile and depth raster
#       depth_pts <- data.frame(rasterToPoints(depth))
#       depth_pts_sf <- st_as_sf(depth_pts, coords = c("x","y"), crs = st_crs(seafloor_meow_deepsea))
#       select_polygons <- st_intersects(depth_pts_sf, seafloor_meow_deepsea)
#       select_polygons <- unique(unlist(select_polygons))
#       select_polygons <- seafloor_meow_deepsea[select_polygons,]
#     
#     if(nrow(select_polygons)==1){
#       new_poly <- select_polygons %>% st_drop_geometry()
#       st_geometry(new_poly) <- st_geometry(holes[h,])
#       seafloor_meow_deepsea_filled_pb <- rbind(seafloor_meow_deepsea_filled_pb, new_poly)
#       rm(new_poly)
#     }
#     else if (nrow(select_polygons)>1 && dim(depth)[2]==1){
#       new_poly <- select_polygons[1,] %>% st_drop_geometry()
#       st_geometry(new_poly) <- st_geometry(holes[problems[h],])
#       seafloor_meow_deepsea_filled_pb <- rbind(seafloor_meow_deepsea_filled_pb, new_poly)
#       rm(new_poly)}
#     else if (nrow(select_polygons)>1 && dim(depth)[1]==1){
#       new_poly <- select_polygons[1,] %>% st_drop_geometry()
#       st_geometry(new_poly) <- st_geometry(holes[problems[h],])
#       seafloor_meow_deepsea_filled_pb <- rbind(seafloor_meow_deepsea_filled_pb, new_poly)
#       rm(new_poly)}
#     else if (nrow(select_polygons)==0){print("Caspian Sea")}
#     else{
#       select_polygons_merged <- st_union(select_polygons) # a bit long to run, only 3 polygons
#       xx <- coverage_fraction(depth, select_polygons_merged)
#       
#       depths_empty <- as.data.frame(as(xx[[1]], "SpatialPixelsDataFrame")) %>% 
#         rename(coverage = layer) %>% 
#         filter(coverage<1) %>% 
#         left_join(depth_pts, by=c('x','y'))
#       
#       depths_full <- as.data.frame(as(xx[[1]], "SpatialPixelsDataFrame")) %>% 
#         rename(coverage = layer) %>% 
#         filter(coverage==1) %>% 
#         left_join(depth_pts, by=c('x','y'))
#       
#       # query will be depth_pts_empty transformed in 3D matrix
#       depth_pts_empty_df <- depths_empty %>% 
#         rename(z = names(depths_empty)[4]) %>% 
#         mutate(z = z) %>% 
#         dplyr::select(x,y,z)
#       depth_pts_empty_mat <- data.matrix(depth_pts_empty_df)
#       
#       # target will be depth_pts transformed in 3D matrix
#       depth_pts_df <- depths_full %>% 
#         rename(z = names(depths_empty)[4]) %>% 
#         mutate(z = z) %>% 
#         dplyr::select(x,y,z)
#       depth_pts_mat <- data.matrix(depth_pts_df)
#       
#       # try out the function on depth example
#       xx <- mcNNindex(target = depth_pts_mat, query = depth_pts_empty_mat, k=1)
#       depth_pts_empty_closest <- data.frame(cbind(depth_pts_empty_mat, xx)) %>% 
#         mutate(x_closest = NA,
#                y_closest = NA,
#                poly = NA_character_)
#       
#       types <- c(select_polygons$ID)
#       pts <- st_as_sf(depth_pts_df, coords = c("x","y"), crs= st_crs(select_polygons))
#       tt <- unlist(st_intersects(pts, select_polygons, sparse=T))
#       
#       for(i in 1:nrow(depth_pts_empty_closest)){
#         # assign closest coords
#         depth_pts_empty_closest$x_closest[i] <- depth_pts_df$x[depth_pts_empty_closest$V4[i]]
#         depth_pts_empty_closest$y_closest[i] <- depth_pts_df$y[depth_pts_empty_closest$V4[i]]
#         # assign polygon
#         depth_pts_empty_closest$poly[i] <- types[tt[[depth_pts_empty_closest$V4[i]]]]
#       }
#       
#       # rasterize the closest points and aggregate a higher scale
#       # create new grid with lower resolution
#       # same for rasters
#       if(problems[h] %in% c(395,693,946)){new_r <- depth}
#       else{
#         new_r <- aggregate(depth, fact = 4, fun = mean)
#         }
#       new_ri <- crop(new_r, extent(holes[problems[h],]))
#       new_ri <- mask(new_ri, holes[problems[h],])
#       
#       new_poly <- rasterToPolygons(new_ri)
#       new_poly <- st_as_sf(new_poly, crs = st_crs(seafloor_meow_deepsea_filled))
#       new_poly$ID <- c(1:nrow(new_poly))
#       
#       depth_pts_empty_closest_sf <- st_as_sf(depth_pts_empty_closest, coords = c('x','y'), 
#                                              crs = st_crs(new_poly))
#       new_poly_closest <- st_join(new_poly, depth_pts_empty_closest_sf, left=T) %>% 
#         st_drop_geometry() %>% 
#         group_by(ID, poly) %>% summarise(n=length(poly)) %>% slice_max(n, with_ties = FALSE)
#       
#       new_poly <- left_join(new_poly, new_poly_closest, by = "ID") %>% 
#         filter(n>0) %>% 
#         group_by(poly) %>% 
#         summarize(geometry = st_union(geometry))
#       
#       # get attributes from seafloor shapefile
#       select_polygons <- select_polygons %>% 
#         filter(ID %in% new_poly$poly) %>% 
#         st_drop_geometry()
#       new_poly <- new_poly %>% 
#         mutate(poly = as.numeric(as.vector(poly)))
#       new_poly <- left_join(select_polygons, new_poly, by=c("ID"="poly"))
#       new_poly <- st_as_sf(new_poly, crs = st_crs(seafloor_meow_deepsea_filled))
#       seafloor_meow_deepsea_filled_pb <- rbind(seafloor_meow_deepsea_filled_pb,new_poly)
#       
#       rm(new_poly, new_poly_closest, new_r, new_ri, types, depth_pts_empty_closest,
#          depth_pts_empty_df, depth_pts_empty_mat,
#          depths_empty, depths_full,select_polygons_merged,
#          xx, depth_pts_mat)
#       
#     }
#     
#     rm(depth_pts, depth_pts_sf, select_polygons, depth_spdf, depth_df, depth,
#        overlap_rr, new_bbox_r, select_depth, new_bbox, x_range, y_range, hole_one,
#        bbox_one)
#     
#     save(seafloor_meow_deepsea_filled_pb, file="outputs/fill_holes/results_fill_holes_problems.RData")
#     save.image("outputs/fill_holes/envt_problems.RData")
#   }
#   
#   else{problems_pb[length(problems_pb)+1] <- problems[h]
#   save.image("outputs/fill_holes/envt_problems.RData")}
#   
# }


################################################################################
### QC & SAVE
################################################################################
### Finalize shapefile
load("outputs/fill_holes/results_fill_holes_problems.RData")
load("outputs/fill_holes/results_fill_holes_2.RData")

seafloor_meow_deepsea_final <- seafloor_meow_deepsea_filled %>% 
  group_by(deepsea_prov_id,meow_eco_name,meow_prov_id,meow_rlm_id,meow_rlm_name,
           meow_eco_id,type,meow_prov_name,abyssal_id,bathyal_id,
           abyssal_name,bathyal_name,ID) %>%
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()

xx <- st_is_valid(seafloor_meow_deepsea_final) # all true so shapefile valid!

st_write(obj = seafloor_meow_deepsea_final, dsn = "~/Yale University/Marine Biogeography/outputs/fill_holes/seafloor_meow_deepsea_final_2.shp")
st_write(obj = seafloor_meow_deepsea_filled, dsn = "~/Yale University/Marine Biogeography/outputs/fill_holes/seafloor_meow_deepsea_filled_2.shp")

windows()
ggplot() + geom_sf(data = seafloor_meow_deepsea_final, fill="black", colour=NA)
ggplot() + geom_sf(data = seafloor_meow_deepsea, fill="black", colour=NA)

# add holes layers
#holes <- st_read(dsn = "outputs/ClippedAndMergedFiles", layer = "MissingAreas")
land <- st_read(dsn = "regions_base/land/ne_10m_land", layer = "ne_10m_land")
ggplot() + 
  geom_sf(data = land, fill="lightgreen", colour=NA) +
  geom_sf(data = seafloor_meow_deepsea_final, fill="black", colour=NA)
ggplot() + geom_sf(data = seafloor_meow_deepsea, fill="black", colour=NA)


# plot all regions
seafloor_meow_deepsea_final <- seafloor_meow_deepsea_final %>% 
  mutate(name = coalesce(meow_eco_name, abyssal_name),
         name = coalesce(name, bathyal_name))

ggplot() +
  geom_sf(data = seafloor_meow_deepsea_final, aes(fill = as.factor(name))) +
  theme_bw() + theme(legend.position = "none")

require(RColorBrewer)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))

seafloor_meow_deepsea_final %>% 
  filter(!is.na(abyssal_name)) %>% 
  ggplot() +
  geom_sf(aes(fill = abyssal_id)) +
  theme_bw() + theme(legend.position = "none") +
  scale_fill_gradientn(colours = jet.colors(10),guide = "colourbar")
