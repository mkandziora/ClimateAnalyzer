####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################


path_to_libraries <- '/home/rstudio/data/ClimateDelimitation2023/Rlibraries/'
setwd("/home/rstudio/data/ClimateDelimitation2023")


# on metacentrum ondemand:
#  apt-get update
#  apt-get install libgdal-dev libproj-dev

# prepare your system
system("apt-get update")
system("apt-get -y install libgdal-dev libproj-dev")
system("apt-get -y install libudunits2-dev")


require(renv, lib.loc = path_to_libraries, attach.required = T)

needed.packages = renv::dependencies("./ClimateAnalyzer")
list.of.packages = unique(needed.packages$Package)
list.of.packages = list.of.packages[!list.of.packages %in% c('list.of.packages', "renv")]


require(withr, lib.loc = path_to_libraries, attach.required = T)
require(sp, lib.loc = path_to_libraries, attach.required = T)
require(curl, lib.loc = path_to_libraries, attach.required = T)
require(cowplot, lib.loc = path_to_libraries, attach.required = T)

require(ggplot2, lib.loc = path_to_libraries, attach.required = T)
lapply(list.of.packages, require, character.only = TRUE, lib.loc=path_to_libraries, attach.required = T)


require(rworldmap, lib.loc = path_to_libraries, attach.required = T)
require(rgdal, lib.loc = path_to_libraries, attach.required = T)
require(slippymath, lib.loc = path_to_libraries, attach.required = T)

require(curl, lib.loc = path_to_libraries, attach.required = T)
require(progress, lib.loc = path_to_libraries, attach.required = T)
require(abind, lib.loc = path_to_libraries, attach.required = T)
require(s2, lib.loc = path_to_libraries, attach.required = T)

require(stars, lib.loc = path_to_libraries, attach.required = T)
require(backports, lib.loc = path_to_libraries, attach.required = T)

require(ggpubr, lib.loc = path_to_libraries, attach.required = T)
require(labeling, lib.loc = path_to_libraries, attach.required = T)
require(farver, lib.loc = path_to_libraries, attach.required = T)
require(ggfortify, lib.loc = path_to_libraries, attach.required = T)
require(cubelyr, lib.loc = path_to_libraries, attach.required = T)
require(fasterize, lib.loc = path_to_libraries, attach.required = T)
require(terra, lib.loc = path_to_libraries, attach.required = T)

require(RStoolbox, lib.loc = path_to_libraries, attach.required = T)
require(future, lib.loc = path_to_libraries, attach.required = T)



