####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

# change path to libraries and working directory according to your set up.

path_to_libraries = '/home/rstudio/data/ClimateDelimitation2023/Rlibraries/'
setwd("/home/rstudio/data/ClimateDelimitation2023")


#####
dir.create(paste0(base, "data/", "elevation"))

install.packages('renv', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)

needed.packages = renv::dependencies("./ClimateAnalyzer")
list.of.packages = unique(needed.packages$Package)

# list.of.packages <- c("rgeos", "crayon", "withr", "curl",
#                       "digest", "ggplot2", "ggmap", "rworldmap", "geosphere", "ape", "backports",
#                       "gbm", "ade4", "ecospat", "testit", "pillar", "elevatr", "progress", "farver",
#                       "labeling", "rnaturalearth", "ggfortify", "sp", "raster", "dismo", "rgdal")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages(lib.loc=path_to_libraries)[,"Package"])]
new.packages = new.packages[!"list.of.packages" %in% new.packages]
if(length(new.packages)) install.packages(new.packages, 
                                          lib=path_to_libraries, repos='http://cran.us.r-project.org', 
                                          dependecies=TRUE)


# some packages might need to be installed by hand
# print('install package')
install.packages('rworldmap', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
install.packages('abind', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
install.packages('cowplot', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)

install.packages('stars', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
install.packages('plotrix', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
install.packages('mclust', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
install.packages('pracma', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)

#install.packages('ecospat', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
install.packages('cubelyr', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
install.packages('fasterize', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
install.packages('terra', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
install.packages('future', lib=path_to_libraries, repos='http://cran.us.r-project.org', dependecies=TRUE)
