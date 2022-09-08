# Code and figures for "A global biogeographic regionalization of the benthic ocean"

## Methods
The methods.Rmd Rmarkdown file details all methods to create the bpow GIS layer.

## Code
Folder with the R codes used for the analyses separated into two parts:
1. create_layer, describing all steps to create the GIS layer performed on R
- a.fix_deep_sea_layers.R fix deep sea layers before first Arc processing
- b.remove_irregularities.R
- c.layer_post-processing_1.R
- d.remove_hadal_from_meows.R remove hadal zones after the first Arc processing
- e.add_missing_regions.R 3D nearest neighbor analysis on all missing zones after the first Arc processing
- f.full_areas.R pulls together all polygons from d. and e. to create the bpow shapefile
2. technical validation
- technical_validation.R to test the layer on 9 species occurrences and expert knowledge

## Data
- data/gbif_spp/ GBIF download for select species to conduct the technical validation, August 2022
- data/obis_spp/ OBIS download for select species to conduct the technical validation, August 2022

## Outputs
- outputs/arcpro/ outputs per processing
- outputs/deep_sea/ layer files after R code a.
- outputs/bpow/ includes the versions of the layer up to the steps detailed in methods.Rmd
- outputs/hadal/ includes the temporary data from the for loop in d.
- outputs/holes/ includes all the outputs from the HPC run from e. before f. 
