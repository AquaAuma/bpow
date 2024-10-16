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

[![DOI](https://zenodo.org/badge/513255061.svg)](https://zenodo.org/badge/latestdoi/513255061)

This repository contains the codes and methods used to produce the shapefile layer associated with "A global biogeographic regionalization of the benthic ocean", by Aurore A. Maureaud, Gabriel Reygondeau, Kate Ingenloff, Jessiva G. Vigneron, Les Watling, Kevin Winner, Walter Jetz.

Main contact: Aurore A. Maureaud, aurore.aqua@gmail.com

<img src ="https://github.com/AquaAuma/bpow/blob/main/Picture1.png" width ="200">

## Methods
The `methods.Rmd` Rmarkdown file details all methods to create the benthic provinces of the world (bpow) GIS layer. Methods are summarized in the file because both R v4.0.3 and ArcPro 2.8.3 softwares were used, and this document gives an overall overview of the methods and steps involved. This document is also summarized in Figure 1. 

## Structure of the repository
- **code** includes all R scripts used to create the layer, and summarized in `methods.Rmd`
- **figures** includes all outputs figures produced for the manuscripts
- **outputs** includes the results from the application analysis
- **data** includes the data (but may not be uploaded depending on file size)

## Author contributions
- **code**: Aurore A. Maureaud
- **methods**: Aurore A. Maureaud, Gabriel Reygondeau, Kate Ingenloff, Kevin Winner, Griffy Vigneron

## Refs
The file `bpow_refs.bib` lists important references cited in the methods.Rmd file.
