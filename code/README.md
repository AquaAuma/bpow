1. create_layer, describing all steps to create the GIS layer performed on R
- a.fix_deep_sea_layers.R fix deep sea layers before first Arc processing
- b.remove_irregularities.R
- c.layer_post-processing_1.R
- d.remove_hadal_from_meows.R remove hadal zones after the first Arc processing
- e.add_missing_regions.R 3D nearest neighbor analysis on all missing zones after the first Arc processing
- f.full_areas.R pulls together all polygons from d. and e. to create the bpow shapefile
- g.rasterize.R optional rasterization of the layer
- h.fix_geometry_prior_clipp.R helps checking the geometry validity before land clipp
- i.add_attributes.R add the right layer attributed to finalize the layer product
2. application, testing the layer on 9 example species 
- application.R linking information from the layer, species occurrences, and expert knowledge
3. make_figures, to produce the figures in the manuscript
- make_figure_2.R
- make_figure_3.R
