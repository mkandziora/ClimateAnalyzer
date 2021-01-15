#################################
# set area to Andes or EA - automatically sets xmin/max, etc
# also possible to say: tropics or temperate, SouthAmerica
#


# Define your minimum und maximum elevation in meters.
min_ele <- 3800
max_ele <- 8000

elevation_threshold <- 3800

#zoom_level for elevation (0-14)  # my computer cannot do above 10...I tried 7 - that worked
zoom_level <- 5

#####
# predefine now resolution and projection
crs <- CRS("+proj=longlat")
res <- c(0.008333333, 0.008333333)

main_wd <- file.path(base, 'tropAlpR_2')

#area = "Asia"
