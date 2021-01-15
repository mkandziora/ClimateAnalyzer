name_of_var10alp <- c(10, -3, 10)
name_of_var11alp <- c(11, -3, 10)
name_of_var10 <- c(10, -3, 18)
name_of_var11 <- c(11, -3, 18)
var_listalp <- list(name_of_var10alp, name_of_var11alp)
var_list <- list(name_of_var10, name_of_var11)
#var_listalp = list(name_of_var10alp, name_of_var11alp, name_of_var4)
#var_list = list(name_of_var10, name_of_var11, name_of_var4)

area1 <- 'Hawaii'
area1 <- 'Africa'
area1 <- "Asia"
area1 <- "SouthAmerica"

area <- area1
var_name1 <- var_listalp
var_name2 <- var_name1
climatename1 <- "Chelsa"
climatename2 <- 'chelsa_miroc_esm'
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=climatename2)  
climatename2 <- "mpi-esm"
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=climatename2)

climatename1 <- "mpi-esm"
climatename2 <- 'chelsa_miroc_esm'
#compare_extent(var_name1, var_name2, area1, climatename1, 
#               area2=NA, climatename2=climatename2)


var_name1 <- var_list
var_name2 <- var_name1
climatename1 <- "Chelsa"
climatename2 <- 'chelsa_miroc_esm'
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=climatename2)  
climatename2 <- "mpi-esm"
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=climatename2)

climatename1 <- "mpi-esm"
climatename2 <- 'chelsa_miroc_esm'
#compare_extent(var_name1, var_name2, area1, climatename1, 
#               area2=NA, climatename2=climatename2)



var_name1 <- var_list

var_name1 <- "elevation"
var_name2 <- var_listalp
climatename1 <- "Chelsa"
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=NA)

climatename1 <- "mpi-esm"
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=NA)

climatename1 <- 'chelsa_miroc_esm'
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=NA)  



area1 <- 'Hawaii'
area1 <- 'Africa'
area1 <- "Asia"
area1 <- "SouthAmerica"

area <- area1
var_name1 <- var_listalp
var_name2 <- "GMBA"
climatename1 <- "Chelsa"
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=NA)

var_name1 <- var_listalp
var_name2 <- "testolin_alpine_cluster"
climatename1 <- "Chelsa"
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=NA)

var_name1 <- var_listalp
var_name2 <- "growing_season_64_94_median"
climatename1 <- "Chelsa"
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=NA)

###############3
climatename1 <- "Chelsa"
var_name1 <- "elevation"
var_name2 <- var_listalp
compare_extent(var_name1, var_name2, area1, climatename1, 
               area2=NA, climatename2=NA)  


###################################

climatename <- "Chelsa"
climatename <- 'chelsa_miroc_esm'
climatename <- "mpi-esm"

var_name1 <- var_listalp
var_name2 <- var_name1
var_name3 <- var_name2
var_name4 <- var_name1

area1 <- 'Hawaii'
area2 <- 'Africa'
area3 <- "Asia"
area4 <- "SouthAmerica"

boxplot_all_single(var_name1, var_name2, var_name3, var_name4, area1, area2, area3, area4, climatename, tropical=TRUE)

# ################333
# # compare extents between areas, climates, etc ....function does to mmany - check!
# area_list = list('SouthAmerica', "Africa", "Hawaii", "Asia")
# raster_list = list( 'mpi-esm', "Chelsa", 'chelsa_miroc_esm')
#
# area_count = 1
# for(area in area_list){
#   print(area)
#   raster_count = 1
#   for(raster_name in raster_list){
#     print(raster_name)
#     compare_extent(var_cont1[[1]], var_cont1[[2]], area, raster_name,
#                    area2=NA, climatename2=NA)
#     compare_extent(var_cont1[[1]], var_cont1[[1]], area, raster_name,
#                    area2=NA, climatename2=NA)
#     compare_extent(var_cont1[[2]], var_cont1[[2]], area, raster_name,
#                    area2=NA, climatename2=NA)
#     if(raster_count == 2){
#       compare_extent(var_cont1[[1]], var_cont1[[2]], area, raster_name1,
#                      area2=area, climatename2=raster_name)
#       compare_extent(var_cont1[[1]], var_cont1[[1]], area, raster_name1,
#                      area2=area, climatename2=raster_name)
#       compare_extent(var_cont1[[2]], var_cont1[[2]], area, raster_name1,
#                      area2=area, climatename2=raster_name)
#       if(raster_count == 3){
#         compare_extent(var_cont1[[1]], var_cont1[[2]], area, raster_name1,
#                        area2=area, climatename2=raster_name)
#         compare_extent(var_cont1[[1]], var_cont1[[2]], area, raster_name2,
#                        area2=area, climatename2=raster_name)
#         compare_extent(var_cont1[[1]], var_cont1[[1]], area, raster_name1,
#                        area2=area, climatename2=raster_name)
#         compare_extent(var_cont1[[1]], var_cont1[[1]], area, raster_name2,
#                        area2=area, climatename2=raster_name)
#         compare_extent(var_cont1[[2]], var_cont1[[2]], area, raster_name1,
#                        area2=area, climatename2=raster_name)
#         compare_extent(var_cont1[[2]], var_cont1[[2]], area, raster_name2,
#                        area2=area, climatename2=raster_name)
#
#       }
#       raster_count =3
#       raster_name2 = raster_name
#     }
#     raster_count = 2
#     raster_name1 = raster_name
#   }
# }