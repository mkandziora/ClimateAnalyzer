local <- 'ondemand'   # set to ondemand for GUI
#local = FALSE
#local = TRUE

if(local==TRUE){
  base <- '/home/blubb/sync-TP-T470s/tropAlp_climate/tropAlpR/'
  source(paste0(base, 'tropAlpR_dev/00_set_stage_local.R'))
  
}else if(local=='ondemand') {
  base <- '/home/rstudio/'
  source(paste0(base, 'ClimateAnalyzer/00_set_stage_ondemand.R'))
}else{
  base <- '/storage/brno3-cerit/home/kandziom/'
  source(paste0(base, 'tropAlpR_dev/00_set_stage_meta.R'))
}

source(paste0(base, 'ClimateAnalyzer/wrapper.R'))
source(paste0(base, 'ClimateAnalyzer/make_polygon_raster.R'))
source(paste0(base, 'ClimateAnalyzer/set_variables.R'))
source(paste0(base, 'ClimateAnalyzer/makeClimateData.R'))

source(paste0(base, 'ClimateAnalyzer/compareClimate.R'))
source(paste0(base, 'ClimateAnalyzer/compareArea.R'))
source(paste0(base, 'ClimateAnalyzer/TODO_make_climate_plots.R'))
source(paste0(base, 'ClimateAnalyzer/00_set_your_data.R'))

###############

assign("base", base, envir = .GlobalEnv)

##############
setwd(main_wd)

# make overview maps for different climates
area_list <- list("SouthAmerica", "Africa", "Hawaii", "Asia")
raster_list <- list('mpi-esm', "Chelsa", 'chelsa_miroc_esm')
for(area in area_list){
  print(area)
  for(raster_name in raster_list){
    print(raster_name)
    datafolder <- get_path_data(raster_name)
    make_over_view_map(area, datafolder)
  }
}

#############
# calculate data for each climate and area per definition

# 
area_list <- list("Hawaii", "Africa", "Asia",'SouthAmerica')
raster_list <- list('mpi-esm', "Chelsa", 'chelsa_miroc_esm')
for(area in area_list){
  print(area)
  # for(raster_name in raster_list){
  #  print(raster_name)

  path_to_folder <- get_path_data(raster_name)

  var_list <- list(name_of_var10alp, name_of_var11alp)
  #wrapperInput(area, var_list,  raster_name)

  var_list <- list(name_of_var10, name_of_var11)
  #wrapperInput(area, var_list,  raster_name)

  #wrapperInput(area, "elevation",  raster_name)
  #wrapperInput(area, "GMBA",  raster_name)
  #wrapperInput(area, "testolin_alpine_cluster",  raster_name)

  var_name <- "growing_season_74_94_median"
  #wrapperInput(area, var_name,  raster_name)

  var_name <- "growing_season_64_94_median"
  wrapperInput(area, var_name,  raster_name)
  var_name <- "growing_season_54_94"
  #wrapperInput(area, var_name,  raster_name)

  var_name <- "growing_season_52_94"
  #wrapperInput(area, var_name,  raster_name)

}
}

################################################
# calculate area size for definition - reprojection of area to equal area projection
# "SouthAmerica", 
area_list <- list("Africa", "Hawaii", "Asia")
raster_list <- list('mpi-esm', "Chelsa", 'chelsa_miroc_esm')
for(area in area_list){
  print(area)
  for(raster_name in raster_list){
    
    if(area == "Africa"){
      area2 <- "EA"
      proj <- "+proj=laea +lon_0=22.5 +lat_0=0 +datum=WGS84 +units=m +no_defs"
    }else if(area == "Asia"){
      area2 <- "Kinabalu"
      proj <- "+proj=cea +lon_0=123.046875 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
    }else if(area == "Hawaii"){
      area2 <- "Hawaii"
      proj <- "+proj=aea +lon_0=-151.171875 +lat_1=9.6025264 +lat_2=25.5486889 +lat_0=17.5756077 +datum=WGS84 +units=m +no_defs"
    }else if(area == "SouthAmerica"){
      area2 <- "Andes"
      proj <- "+proj=laea +lon_0=-63.984375 +lat_0=0 +datum=WGS84 +units=m +no_defs"
    }
    
    print(raster_name)
    print(var_name)
    pp <- make_pol(var_name, raster_name, area)
    plot(pp)
    
    if(var_name %in% c("GMBA",  "testolin_alpine_cluster" )){
      pp <- gBuffer(pp, byid=TRUE, width=0)
    }
    
    ext <- get_extent_area(area2, ncol=3600, nrow=1800)
    pp <- crop(pp, extent(ext))
    plot(pp)
    pp <- spTransform(pp, CRSobj=crs(proj))
    plot(pp)
    sz <- get_area_size2(pp)
    prefix <- fn_prefix(var_name)
    png(paste(prefix, raster_name, "areasize.png", sep="_"))
    plot(pp)
    legend("topleft", legend=paste(raster_name, "size: ", sz))
    dev.off()
  }
}




#######################

area = 'SouthAmerica'
#area = 'EA'
#area = 'Africa'
#area = 'Hawaii'
#area = "Asia" 
#area = 'Kinabalu'

# set to Chelsa or worldclim
#raster_name = 'chelsa_miroc_esm'
raster_name = 'mpi-esm'
raster_name = "Chelsa"
#raster_name = 'worldclim'


name_of_var10alp <- c(10, -3, 10)
name_of_var11alp <- c(11, -3, 10)
name_of_var10 <- c(10, -3, 18)
name_of_var11 <- c(11, -3, 18)
name_of_var3 <- c(3, 60, 80)
name_of_var4 <- c(4, 0, 48)


var_name = c(10, 6.4, 18)
var_name = c(19, 500, 3000)
var_name = c(12, 0, 2000)
var_name <- list(name_of_var10alp, name_of_var11alp)
var_name <- list(name_of_var10, name_of_var11)
var_name = "testolin_alpine_cluster"
var_name = "GMBA"
var_name = "elevation"
var_name = "growing_season_54_94"
var_name = "growing_season_64_94"
var_name = "growing_season_52_94"
var_name = "growing_season_74_94"
var_name = "growing_season_64_94_mean"
var_name = "growing_season_64_94_median"
wrapperInput(area, var_name,  raster_name)


var_listalp = list(name_of_var10alp, name_of_var11alp)
var_list = list(name_of_var10, name_of_var11)
var_list = list(name_of_var10alp, name_of_var11alp, name_of_var3)
var_list = list(name_of_var10alp, name_of_var11alp, name_of_var4)
var_list = list(name_of_var10, name_of_var11, name_of_var4)


wrapperInput(area, var_list,  raster_name)
