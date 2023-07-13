####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

#########################################
# adapt base to your working directory

base <- '/home/rstudio/data/ClimateDelimitation2023/'
setwd(base)
assign("base", base, envir = .GlobalEnv)

source(paste0(base, 'ClimateAnalyzer/00_set_stage_ondemand.R'))
source(paste0(base, 'ClimateAnalyzer/wrapper.R'))
source(paste0(base, 'ClimateAnalyzer/makePolygonDelim.R'))
source(paste0(base, 'ClimateAnalyzer/00_set_variables.R'))
source(paste0(base, 'ClimateAnalyzer/makeClimateData.R'))
source(paste0(base, 'ClimateAnalyzer/compareClimate.R'))
source(paste0(base, 'ClimateAnalyzer/compareArea.R'))
source(paste0(base, 'ClimateAnalyzer/make_climate_plots.R'))
source(paste0(base, 'ClimateAnalyzer/00_set_your_data.R'))

###############

main_wd
setwd(main_wd)

##############

# # make overview maps for different climates
# area_list <- list("SouthAmerica", "Africa", "Hawaii", "Asia")
# raster_list <- list('mpi-esm', "Chelsa", 'chelsa_miroc_esm')
# for(area in area_list){
#   print(area)
#   for(raster_name in raster_list){
#     print(raster_name)
#     datafolder <- get_path_data(raster_name)
#     make_over_view_map(area, raster_name, datafolder)
#   }
# }
##############


#############
# calculate data for each climate and area per definition

for(area in area_list){
  print(area)
  for(raster_name in raster_list){
       wrapperInput(area, var_list,  raster_name)
       wrapperInput(area, var_listalp,  raster_name)
       wrapperInput(area, var_listalpH, raster_name)
   }
  wrapperInput(area, var_listgrow, 'Chelsa')
}


for(min_ele in c( 1800, 2500, 3200, 3500, 3800, 4100)){  # 
  assign("min_ele", min_ele, envir = .GlobalEnv)
  for(area in area_list){
    print(area)
    assign("area", area, envir = .GlobalEnv)
    
    wrapperInput(area, var_listele, 'Chelsa')
  }
}


################################################
# calculate area size for definition - reprojection of area to equal area projection
var_all = list(var_list, var_listalp, var_listalpH, var_listgrow)
for(area in area_list){
  print(area)
  for(raster_name in raster_list){
    for(var_name in var_all){
        if(area == "Africa"){
          proj <- "+proj=laea +lon_0=22.5 +lat_0=0 +datum=WGS84 +units=m +no_defs"
        }else if(area == "Asia"){
          proj <- "+proj=cea +lon_0=123.046875 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
        }else if(area == "Hawaii"){
          proj <- "+proj=aea +lon_0=-151.171875 +lat_1=9.6025264 +lat_2=25.5486889 +lat_0=17.5756077 +datum=WGS84 +units=m +no_defs"
        }else if(area == "SouthAmerica"){
          proj <- "+proj=laea +lon_0=-63.984375 +lat_0=0 +datum=WGS84 +units=m +no_defs"
        }
        
        pp <- make_pol(var_name, raster_name, area)
        plot(pp)
  
        ext <- get_extent_area(area, ncol=3600, nrow=1800)
        pp <- st_crop(pp, st_bbox(ext), crop=T)
        pp <- st_transform(pp, crs=crs(proj))
        sz <- get_area_size2(pp)
        prefix <- fn_prefix(var_name)
        
        png(paste(prefix, raster_name, area, "areasize_sameproj.png", sep="_"))
        plot(pp)
        legend("topleft", legend=paste(raster_name, "size: ", sz))
        dev.off()
    }
  }
  
}

####
for(min_ele in c(1800, 2500, 3200, 3500, 3800, 4100)){
  assign("min_ele", min_ele, envir = .GlobalEnv)
  for(area in area_list){
    print(area)
    raster_name = "Chelsa"
    var_name = var_listele

      if(area == "Africa"){
        proj <- "+proj=laea +lon_0=22.5 +lat_0=0 +datum=WGS84 +units=m +no_defs"
      }else if(area == "Asia"){
        proj <- "+proj=cea +lon_0=123.046875 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
      }else if(area == "Hawaii"){
        proj <- "+proj=aea +lon_0=-151.171875 +lat_1=9.6025264 +lat_2=25.5486889 +lat_0=17.5756077 +datum=WGS84 +units=m +no_defs"
      }else if(area == "SouthAmerica"){
        proj <- "+proj=laea +lon_0=-63.984375 +lat_0=0 +datum=WGS84 +units=m +no_defs"
      }
      
     
      pp <- make_pol(var_name, raster_name, area)

      ext <- get_extent_area(area, ncol=3600, nrow=1800)
      pp <- st_crop(pp, st_bbox(ext), crop=T)
      pp <- st_transform(pp, crs=crs(proj))
      plot(pp, max.plot = 1)
      sz <- get_area_size2(pp)
      prefix <- fn_prefix(var_name)
      png(paste(prefix, raster_name, area, "areasize_sameproj.png", sep="_"))
      plot(pp)
      legend("topleft", legend=paste(raster_name, "size: ", sz))
      dev.off()
    }
  }


################################3
# calculate Puna and Paramo extents

var_listalpPuna <- list(name_of_var10alp, name_of_var11alp, name_of_var3, name_of_var12)
var_listalpParamo <- list(name_of_var10alp, name_of_var11alp, name_of_var3, name_of_var12b)
for(area in area_list){
  for(raster_name in raster_list){
    print(raster_name)
    wrapperInput(area, var_listalpParamo,  raster_name)
    wrapperInput(area, var_listalpPuna,  raster_name)
  }
}
