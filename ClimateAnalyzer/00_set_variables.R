####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

#################################
# zoom_level for elevation (0-14)  
# my computer cannot do above 10...
# I use 7 - ca 1km resolution at the equator:
# https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
zoom_level <- 7

#####
# predefine resolution and projection
crs <- CRS("+proj=longlat")
res <- c(0.008333333, 0.008333333)

main_wd <- file.path(base, 'Results/')



# set up the different limits for the various bioclim variables
name_of_var10alpH <- c(10, -3, 6.7)
name_of_var11alpH <- c(11, -3, 6.7)

name_of_var10alp <- c(10, -3, 10)
name_of_var11alp <- c(11, -3, 10)

name_of_var10 <- c(10, -3, 18)
name_of_var11 <- c(11, -3, 18)

name_of_var3 <- c(3, 50, 300)
name_of_var4 <- c(4, 0, 48)

name_of_var15 <- c(15, 0, 40)
name_of_var15b <- c(15, 41, 300)

name_of_var13 <- c(13, 200, 600)
name_of_var13b <- c(13, 0, 149)

#name_of_var14 <- c(14, 25, 300)
#name_of_var14b <- c(14, 0, 24)

name_of_var12 <- c(12, 0, 2000)
name_of_var12b <- c(12, 2001, 1000000)




var_listalpH <- list(name_of_var10alpH, name_of_var11alpH, name_of_var3)
var_listalp <- list(name_of_var10alp, name_of_var11alp, name_of_var3)
var_list <- list(name_of_var10, name_of_var11, name_of_var3)
var_listele <- list("elevation",   name_of_var3)
# var_listgrow <- list("growing_season_64_94_median",   name_of_var3)
var_listgrow <- list("growing_season_64_94_mean",   name_of_var3)


raster_list <- list("Chelsa", 'mpi-esm',  'chelsa_miroc_esm') # "future_mpi26", "future_MIROC26" 
area_list <- list("SouthAmerica",    "Asia",  "Africa", "Hawaii") 
