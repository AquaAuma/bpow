rm(list = ls())

### Libraries
library(sf)
sf::sf_use_s2(FALSE)
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
library(foreach)


################################################################################
### Load data & fix shapefile attributes
################################################################################
# load provinces_p7s3
seafloor_meow_deepsea <- st_read("outputs/hadal/provinces_p7s3.shp")

# load holes shapefile
holes <- st_read("outputs/arcpro/post-processing_1/holes_p6s2_correct.shp") %>% 
  st_cast("POLYGON") %>% 
  mutate(Shape_Area = drop_units(st_area(geometry)),
         Shape_Leng = drop_units(st_length(geometry))) %>% 
  dplyr::select(-Id)
holes$ID <- c(1:nrow(holes))

# for(i in 1:nrow(holes)){
#   png(file = paste0("figures/holes/",i,".png"))
#   print(ggplot(holes[i,]) + geom_sf())
#   dev.off()
# }

holes <- holes %>% 
  filter(ID != 1553)

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

seafloor_meow_deepsea_filled <- data.frame()
problems <- c()

for(h in 601:602) {
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
    select_polygons <- st_intersects(depth_pts_sf, seafloor_meow_deepsea) # that line takes time
    select_polygons <- unique(unlist(select_polygons))
    select_polygons <- seafloor_meow_deepsea[select_polygons,]
    
    if(nrow(select_polygons)==1){
      new_poly <- select_polygons %>% st_drop_geometry()
      st_geometry(new_poly) <- st_geometry(holes[h,])
      seafloor_meow_deepsea_filled <- rbind(seafloor_meow_deepsea_filled, new_poly)
      save(seafloor_meow_deepsea_filled, file="outputs/holes/results_fill_holes.RData")
      save.image("outputs/holes/envt.RData")
      print("One polygon")
    }
    
    if(nrow(select_polygons)>1 && dim(depth)[2]==1){
      new_poly <- select_polygons[1,] %>% st_drop_geometry()
      st_geometry(new_poly) <- st_geometry(holes[h,])
      seafloor_meow_deepsea_filled <- rbind(seafloor_meow_deepsea_filled, new_poly)
      save(seafloor_meow_deepsea_filled, file="outputs/holes/results_fill_holes.RData")
      save.image("outputs/holes/envt.RData")
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
      
      # 3D NNA from the R morpho package
      # https://www.rdocumentation.org/packages/Morpho/versions/2.9/topics/mcNNindex
      xx <- mcNNindex(target = depth_pts_mat, query = depth_pts_empty_mat, k=1)
      depth_pts_empty_closest <- data.frame(cbind(depth_pts_empty_mat, xx)) %>% 
        mutate(x_closest = NA,
               y_closest = NA,
               poly = NA_character_)
      
      types <- c(select_polygons$ID)
      pts <- st_as_sf(depth_pts_df, coords = c("x","y"), crs= st_crs(select_polygons))
      tt <- unlist(st_intersects(pts, select_polygons, sparse=T))
      
      depth_pts_empty_closest <- depth_pts_empty_closest %>% 
        mutate(x_closest = depth_pts_empty_closest$x[depth_pts_empty_closest$V4],
               y_closest = depth_pts_empty_closest$y[depth_pts_empty_closest$V4],
               poly = types[tt[depth_pts_empty_closest$V4]])
      
      # rasterize the closest points and aggregate a higher scale
      # create new grid with lower resolution
      # same for rasters
      if(nrow(depth)<4 | ncol(depth)<4 ){new_r <- depth
      } else {
        new_r <- aggregate(depth, fact = 4, fun = mean)
      }
      
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
      new_poly <- st_as_sf(new_poly, crs = st_crs(seafloor_meow_deepsea))
      seafloor_meow_deepsea_filled <- rbind(seafloor_meow_deepsea_filled, new_poly)
      save(seafloor_meow_deepsea_filled, file="outputs/holes/results_fill_holes.RData")
      save.image("outputs/holes/envt.RData")
      print("Multi polygons")
      
      rm(new_poly_closest, new_r, new_ri, types, depth_pts_empty_closest,
         depth_pts_empty_df, depth_pts_empty_mat,
         depths_empty, depths_full,select_polygons_merged,
         xx, depth_pts_mat)
      
    }
    
    rm(depth_pts, depth_pts_sf, select_polygons, depth_spdf, depth_df, depth,
       overlap_rr, new_bbox_r, select_depth, new_bbox, x_range, y_range, hole_one,
       bbox_one)
    
  } else{problems[length(problems)+1] <- h
  print("length(select_depth) not positive")
  save.image("outputs/holes/envt.RData")}
  
}


################################################################################
### QC & SAVE
################################################################################
### Finalize shapefile
# load("outputs/holes/results_fill_holes_problems.RData")
# load("outputs/holes/results_fill_holes.RData")

seafloor_meow_deepsea_final <- st_as_sf(do.call("rbind", seafloor_meow_deepsea_filled)) %>% 
  group_by(ID, type, prov_n, prov_id, eco_n, eco_id, rlm_n, rlm_id,
           had_n, had_id) %>%
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()

xx <- st_is_valid(seafloor_meow_deepsea_final) # all true so shapefile valid!

st_write(obj = seafloor_meow_deepsea_final, dsn = "outputs/holes/seafloor_meow_deepsea_final.shp")

