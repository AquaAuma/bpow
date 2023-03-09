# Benthic Provinces of the World

This repository contains the codes and methods used to produce the shapefile layer associated with "A global biogeographic regionalization of the benthic ocean", by Aurore A. Maureaud, Gabriel Reygondeau, Kate Ingenloff, Jessiva G. Vigneron, Les Watling, Kevin Winner, Walter Jetz.

Main contact: Aurore A. Maureaud, aurore.aqua@gmail.com

## Methods
The methods.Rmd Rmarkdown file details all methods to create the benthic provinces of the world (bpow) GIS layer.

## Refs
The file `bpow_refs.bib` lists important references cited in the methods.Rmd file.

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

## Figures
Folder with figure outputs
- figures/figure_2/ inset maps
- figures/figure_3/ global layer by bathymetric type
- figures/figure_4/ global maps of technical validation + summary results
- figures/methods/ maps snapshots from Arc of Q.

## Data
- data/gbif_spp/ GBIF download for select species to conduct the technical validation, August 2022 https://www.gbif.org
- data/obis_spp/ OBIS download for select species to conduct the technical validation, August 2022 https://obis.org
- data/gebco_2020_ascii/ GEBCO downloaded from https://www.gebco.net
- data/meows_ecos/ MEOWs downloaded from https://www.marineregions.org/, data linked to Spalding et al. (2007)
- data/goods_provinces/ access given by Les Watling, data linked to Watling et al. (2013)

## Outputs
- outputs/arcpro/ outputs per processing
- outputs/deep_sea/ layer files after R code a.
- outputs/bpow/ includes the versions of the layer up to the steps detailed in methods.Rmd
- outputs/hadal/ includes the temporary data from the for loop in d.
- outputs/holes/ includes all the outputs from the HPC run from e. before f. 
- outputs/application/ includes the outputs from the species application summary figures

## Author contributions
- code: Aurore A. Maureaud
- methods: Aurore A. Maureaud, Gabriel Reygondeau, Kate Ingenloff, Kevin Winner, Griffy Vigneron
