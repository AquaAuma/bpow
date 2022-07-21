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
library(ggpattern)


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

# pick between 26, 222, & 229, 1526, 1749, 1756, 1153
h <- 14
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
  
  # # plot depth
  # png(paste0("figures/figure_2/depth_",h,".png"))
  # print(ggplot() + geom_tile(data = depth_df, aes(x=x, y=y, fill = depth)) +
  #         scale_fill_gradientn(colours = terrain.colors(4)) + theme_bw() +
  #         geom_sf(data = seafloor_meow_deepsea, fill=NA, colour="black") +
  #         coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y))))
  # dev.off()
  
  # select polygons from shapefile and depth raster
  depth_pts <- data.frame(rasterToPoints(depth))
  depth_pts_sf <- st_as_sf(depth_pts, coords = c("x","y"), crs = st_crs(seafloor_meow_deepsea))
  select_polygons <- st_intersects(depth_pts_sf, seafloor_meow_deepsea) # that line takes time
  select_polygons <- unique(unlist(select_polygons))
  select_polygons <- seafloor_meow_deepsea[select_polygons,]
  
  # plot polygons
  # png(paste0("figures/figure_2/hole_polygons_",h,".png"))
  # print(ggplot() + geom_sf(data = hole_one, fill="grey") + theme_bw() +
  #         geom_sf(data = select_polygons, aes(fill=as.factor(ID))) +
  #         coord_sf(xlim = c(new_bbox[1], new_bbox[3]), ylim = c(new_bbox[2], new_bbox[4])) +
  #         coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y))))
  # dev.off()
  
  if(nrow(select_polygons)==1){
    new_poly <- select_polygons %>% st_drop_geometry()
    st_geometry(new_poly) <- st_geometry(holes[h,])
    seafloor_meow_deepsea_filled <- rbind(seafloor_meow_deepsea_filled, new_poly)
    print("One polygon")
  }
  
  if(nrow(select_polygons)>1 && dim(depth)[2]==1){
    new_poly <- select_polygons[1,] %>% st_drop_geometry()
    st_geometry(new_poly) <- st_geometry(holes[h,])
    seafloor_meow_deepsea_filled <- rbind(seafloor_meow_deepsea_filled, new_poly)
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
    
    # # plots 3D closest polygon
    # png(paste0("figures/figure_2/closest_",h,".png"))
    # print(ggplot()+
    #         geom_point(data = depth_pts_empty_closest, aes(x = x, y = y, colour = as.factor(poly)), size=2) +
    #         geom_sf(data = seafloor_meow_deepsea, fill=NA, colour="black") +
    #         coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y))))
    # dev.off()
    
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
    select_polygons_2 <- select_polygons %>% 
      filter(ID %in% new_poly$poly) %>% 
      st_drop_geometry()
    new_poly <- new_poly %>% 
      mutate(poly = as.numeric(as.vector(poly)))
    new_poly <- left_join(select_polygons_2, new_poly, by=c("ID"="poly"))
    new_poly <- st_as_sf(new_poly, crs = st_crs(seafloor_meow_deepsea))
    seafloor_meow_deepsea_filled <- rbind(seafloor_meow_deepsea_filled,new_poly)
    
    # # plot smoothed polygons
    # png(paste0("figures/figure_2/poly_",h,".png"))
    # print(ggplot() + geom_sf(data = new_poly, aes(fill = as.factor(ID)), colour = NA) +
    #         geom_sf(data = seafloor_meow_deepsea, fill=NA, colour="black") +
    #         coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y))))
    # dev.off()
    # 
    # png(paste0("figures/figure_2/poly_",h,".png"))
    # print(ggplot() + geom_sf(data = new_poly, colour = NA, fill="grey") + theme_minimal()+
    #         geom_sf(data = seafloor_meow_deepsea, fill="black", colour=NA) +
    #         coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y))))
    # dev.off()
  }
}



### PLOTS
  
png(paste0("figures/figure_2/poly_",h,".png"),
           width = 16*200, height = 10*200, res = 200)
print(ggplot() + 
  geom_tile(data = depth_df, aes(x=x, y=y, fill=depth)) +
  scale_fill_gradientn(colours = terrain.colors(4)) +
  geom_sf(data = hole_one, fill="black", colour = "black", alpha = 0.1) + theme_bw() +
  geom_sf_pattern(data = select_polygons, fill=NA, colour="black",
                  aes(pattern = prov_n,
                  pattern_angle = prov_n),
                  pattern_fill= "black",
                  pattern_colour = "black",
                  pattern_density = 0.2,
                  pattern_alpha = 0.2,
                  pattern_frequency = 0.4) +
  coord_sf(xlim = c(new_bbox[1], new_bbox[3]), ylim = c(new_bbox[2], new_bbox[4])) +
  coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y))) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("") + ylab("") +
  theme(axis.text=element_text(size=20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm')) +
  labs(pattern = "Province", fill = "Depth(m)"))
dev.off()


png(paste0("figures/figure_2/poly_inferred_",h,".png"),
    width = 16*200, height = 10*200, res = 200)
print(ggplot() + 
        geom_tile(data = depth_df, aes(x=x, y=y, fill=depth)) +
        scale_fill_gradientn(colours = terrain.colors(4)) +
        geom_sf(data = new_poly, fill=NA, colour = "black") + theme_bw() +
        geom_sf_pattern(data = select_polygons, fill=NA, colour="grey",
                        aes(pattern = prov_n,
                            pattern_angle = prov_n),
                        pattern_fill= "black",
                        pattern_colour = "black",
                        pattern_density = 0.2,
                        pattern_alpha = 0.2,
                        pattern_frequency = 0.4) +
        geom_sf_pattern(data = new_poly, fill=NA, colour="black",
                        aes(pattern = prov_n,
                            pattern_angle = prov_n),
                        pattern_fill= "black",
                        pattern_colour = "black",
                        pattern_density = 0.2,
                        pattern_alpha = 0.2,
                        pattern_frequency = 0.4) +
        coord_sf(xlim = c(new_bbox[1], new_bbox[3]), ylim = c(new_bbox[2], new_bbox[4])) +
        coord_sf(xlim = c(min(depth_df$x),max(depth_df$x)), ylim = c(min(depth_df$y), max(depth_df$y))) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        xlab("") + ylab("") +
  theme(axis.text=element_text(size=20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm')) +
  labs(pattern = "Province", fill = "Depth(m)"))
dev.off()



library(rayshader)
depth_map <- ggplot() + 
  geom_tile(data = depth_df, aes(x=x, y=y, fill=depth)) +
  scale_fill_gradientn(colours = terrain.colors(4)) + theme_bw()

plot_gg(depth_map)

