# Code and figures for "A global biogeographic classification for the global benthic ocean"

## Code
Folder with the R codes used for the analyses separated into two parts:
1. create_layer, describing all steps to create the GIS layer performed on R
- a.fix_deep_sea_layers.R fix deep sea layers before first Arc processing
- b.remove_hadal_from_meows.R remove hadal zones after the first Arc processing
- c.add_missing_regions.R 3D nearest neighbor analysis on all missing zones after the first Arc processing
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
