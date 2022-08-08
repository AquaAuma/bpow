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
Folder with base data used for the analyses
- meows_ecos: Marine Ecoregions of the World from Spalding et al., 2007 https://academic.oup.com/bioscience/article/57/7/573/238419
- goods_provinces: Bathyal and Abyssal provinces from Watling et al., 2013 https://www.sciencedirect.com/science/article/pii/S0079661112001693
- gebco_2020_ascii: depth raster of the world from the GEBCO project https://www.gebco.net/

## Outputs
1. arcpro outputs per processing
2. deep_sea layer files after R code a.
3. bpow includes the versions of the layer up to the steps detailed in methods.Rmd
4. hadal includes the temporary data from the for loop in d.
5. holes includes all the outputs from the HPC run from e. before f. 
