####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

base <- '/home/rstudio/data/ClimateDelimitation2023/'
setwd(base)
assign("base", base, envir = .GlobalEnv)

source(paste0(base, 'ClimateAnalyzer/00_set_stage_ondemand.R'))
source(paste0(base, 'ClimateAnalyzer/wrapper.R'))
source(paste0(base, 'ClimateAnalyzer/make_polygon_raster.R'))
source(paste0(base, 'ClimateAnalyzer/00_set_variables.R'))
source(paste0(base, 'ClimateAnalyzer/makeClimateData.R'))
source(paste0(base, 'ClimateAnalyzer/compareClimate.R'))
source(paste0(base, 'ClimateAnalyzer/compareArea.R'))
source(paste0(base, 'ClimateAnalyzer/make_climate_plots.R'))
source(paste0(base, 'ClimateAnalyzer/00_set_your_data.R'))
source(paste0(base, 'ClimateAnalyzer/makeElevationData.R'))

###############

for(area1 in area_list){
  print(area1)
  assign("area", area1, envir = .GlobalEnv)
  

  var_name1 <- var_listalpH
  var_name2 <- var_name1
  climatename1 <- "Chelsa"
  climatename2 <- 'chelsa_miroc_esm'
  compare_extent(var_name1, var_name2, area1, climatename1,
                 area2=NA, climatename2=climatename2)
  climatename2 <- "mpi-esm"
  compare_extent(var_name1, var_name2, area1, climatename1,
                 area2=NA, climatename2=climatename2)
  var_name2 <- "growing_season_64_94_median"
  compare_extent(var_name1, var_name2, area1, climatename1, 
                 area2=NA, climatename2=NA)

  var_name1 <- var_listalp
  var_name2 <- var_name1
  climatename1 <- "Chelsa"
  climatename2 <- 'chelsa_miroc_esm'
  compare_extent(var_name1, var_name2, area1, climatename1, 
                 area2=NA, climatename2=climatename2)  
  climatename2 <- "mpi-esm"
  compare_extent(var_name1, var_name2, area1, climatename1, 
                 area2=NA, climatename2=climatename2)

  var_name1 <- var_list
  var_name2 <- var_name1
  climatename1 <- "Chelsa"
  climatename2 <- 'chelsa_miroc_esm'
  compare_extent(var_name1, var_name2, area1, climatename1, 
                 area2=NA, climatename2=climatename2)  
  climatename2 <- "mpi-esm"
  compare_extent(var_name1, var_name2, area1, climatename1, 
                 area2=NA, climatename2=climatename2)

  
  var_name1 <- var_list
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
}




# compare to elevation
for(area1 in area_list){
  print(area1)
  climatename1 <- "Chelsa"
  for(min_ele in c(1800, 2500)){
    assign("min_ele", min_ele, envir = .GlobalEnv)
    var_name1 <- list("elevation", c(3, 50, 300))
    var_name2 <- var_list
    climatename1 <- "Chelsa"
    compare_extent(var_name1, var_name2, area1, climatename1,
                   area2=NA, climatename2=NA)
  }
  for(min_ele in c(3800, 4100)){
    assign("min_ele", min_ele, envir = .GlobalEnv)
    var_name1 <- list("elevation", c(3, 50, 300))
    var_name2 <- var_listalpH
   compare_extent(var_name1, var_name2, area1, climatename1, 
                 area2=NA, climatename2=NA)
   var_name1 <- "growing_season_64_94_mean"
   compare_extent(var_name1, var_name2, area1, climatename1, 
                  area2=NA, climatename2=NA)
    
  }
  for(min_ele in c(3200, 3500, 3800)){
    assign("min_ele", min_ele, envir = .GlobalEnv)
    var_name1 <- list("elevation", c(3, 50, 300))
    var_name2 <- var_listalp
    compare_extent(var_name1, var_name2, area1, climatename1,
                   area2=NA, climatename2=NA)
  }
}
#########################################################################



area4 <- 'Hawaii'
area3 <- 'Africa'
area2 <- "Asia"
area1 <- "SouthAmerica"

var_name1 <- var_listalp
var_name2 = "growing_season_64_94_median"

varlist = list(var_list, var_listalp, var_listalpH) #, "growing_season_64_94_median", "elevation")
raster_list <- list("Chelsa", 'mpi-esm',  'chelsa_miroc_esm')
for(raster_name in raster_list){
  print(raster_name)
  for(var_name1 in varlist){
        boxplot_all_area(var_name1, area1, area2, area3, area4, raster_name)
  }
}

climatename1 = "Chelsa"
climatename2 = 'chelsa_miroc_esm'
climatename3 <- "mpi-esm"

var_name1 <- var_listalp
area_sublist = list("SouthAmerica", "Asia", "Africa")
boxplot_all_climate2(var_name1,  area_sublist, climatename1, climatename2, climatename3)


var_namel =  c(var_listalp, var_list, var_listalpH, "growing_season_64_94_median", "elevation")
for(var_name in var_namel){  
  for(climatename in raster_list){  
    for(area in area_list){
      print("load 1")
      prefix1 <- fn_prefix(var_name)
      setwd(main_wd)
      setwd(area)
      setwd(climatename)
      print(prefix1)
      setwd(prefix1)
      
      #crop_mask <- load_grd(var_name)
      pp <- load_shp(var_name)
      
      ext = get_extent_area(area)
      
      # get plot to locate area
      bc_bbox <- st_bbox(ext)
      bc_big <- get_map(location = c(lon=mean(bc_bbox[1], bc_bbox[3]), lat=mean(bc_bbox[2], bc_bbox[4])), zoom = 5, source = "osm")

      rlang::last_trace()
      
      fpp <- fortify(pp)
      fn <- paste0(area, "_polygons_on_googlemap.png")
      
      ggmap(bc_big) +         
        geom_polygon(data = fpp, aes(long, lat, group=group), col="red" ) 

      ggsave(fn)
    }
  }
}

###############################################################3

compare_clim_acrossallcontinents(var_listalp, area_list, "Chelsa")
