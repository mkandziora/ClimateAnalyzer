####################################################
# Number of scripts to analyse the tropical Alpine areas
# based on climatic data and elevation
# written by Martha Kandziora, 2020
#####################################################

#on metacentrum ondemand:
#  apt-get update
#  apt-get install libgdal-dev libproj-dev

system("apt-get update")
system("apt-get -y install libgdal-dev libproj-dev")
system("apt-get -y install libudunits2-dev")

# Script to set all the basic settings. Make sure you set the variables exactly as described in the comments, including capital and non-capital letters.
#base = '/storage/brno3-cerit/home/kandziom/'
base <- '/home/rstudio/'



path_to_libraries <- paste0(base, 'tropAlpR/libraries')


#setwd(wd)
# install.packages('geosphere', lib=path_to_libraries)
# load libraries
library(testit, lib.loc = path_to_libraries)
library(sp, lib.loc=path_to_libraries)
library(raster, lib.loc=path_to_libraries)
library(dismo, lib.loc=path_to_libraries)
library(ade4, lib.loc=path_to_libraries)
library(ape, lib.loc=path_to_libraries)
library(gbm, lib.loc=path_to_libraries)
library(crayon, lib.loc=path_to_libraries)
library(pillar, lib.loc=path_to_libraries)
library(backports, lib.loc=path_to_libraries)
library(ecospat, lib.loc=path_to_libraries)
library(ggplot2, lib.loc=path_to_libraries)
library(ggmap, lib.loc=path_to_libraries)
library(rgdal, lib.loc=path_to_libraries)
library(elevatr, lib.loc=path_to_libraries)
library(progress, lib.loc=path_to_libraries)
library(curl, lib.loc=path_to_libraries)
library(rgeos, lib.loc=path_to_libraries)
library(farver, lib.loc=path_to_libraries)
library(labeling, lib.loc=path_to_libraries)
library(rnaturalearth, lib.loc=path_to_libraries)
library(geosphere, lib.loc=path_to_libraries)

#library(usethis, lib.loc=path_to_libraries)
#library(ps, lib.loc=path_to_libraries)
#library(desc, lib.loc=path_to_libraries)
#library(devtools, lib.loc=path_to_libraries)
#library(callr, lib.loc=path_to_libraries)
#library(prettyunits, lib.loc=path_to_libraries)

#devtools::install_github("vqv/ggbiplot", args = c('--library="./libraries"'))
#library(ggbiplot, lib.loc=path_to_libraries)
#install.packages('rworldmap',lib=path_to_libraries)
library(rworldmap, lib.loc=path_to_libraries)
library(NbClust, lib.loc=path_to_libraries)

library(stringr, lib.loc = path_to_libraries)

#install.packages('ggfortify',lib=path_to_libraries)
library(ggfortify, lib.loc=path_to_libraries)
library(factoextra, lib.loc = path_to_libraries)
library(ggpubr, lib.loc = path_to_libraries)
library(rstatix, lib.loc = path_to_libraries)
library(corrplot, lib.loc = path_to_libraries)

library(abind, lib.loc = path_to_libraries)
library(sf, lib.loc = path_to_libraries)

#library(stars, lib.loc = path_to_libraries)

#########################################
# How to get started:
# 1. download worldclim data:wget  https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip and unpack
# 1b. per bioclim: wget  ftp://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/bio/*
# 2. install the different libraries: install.packages("package-name")
# 3. set the correct variables in the next section
################################################################################################

