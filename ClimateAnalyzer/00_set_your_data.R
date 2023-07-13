####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################


# the get_path_data function is crucial and 
# need to be adapted by every user before starting to 
# analyse the areas and climates.

get_path_data <- function(climate_data){
  if(climate_data == 'chelsa_miroc_esm'){
    datafolder <- paste0(base, "data/chelsa_miroc_esm/")
  }else if(climate_data == 'mpi-esm'){
    datafolder <- paste0(base, "data/mpi-esm/")
  }else if(climate_data == "Chelsa"){
    datafolder <- paste0(base, "data/Chelsa/")
  }else if(climate_data == 'worldclim'){
    datafolder <- paste0(base, "tropAlpR/worldclim/")
  }else if(climate_data == 'GSL'){
    datafolder <- paste0(base, "data/growing_season/gs_length/")
  }else if(climate_data == 'GST'){
    datafolder <- paste0(base, "data/growing_season/gs_temp/")
  }else if(climate_data == 'future_MIROC26'){
    datafolder <- paste0(base, "data/future_MIROC26/")
  }else if(climate_data == 'future_mpi26'){
    datafolder <- paste0(base, "data/future_mpi26/")
  }else if(climate_data == 'futureV21'){
    datafolder <- paste0(base, "data/futureV21/")
  }else if(climate_data == 'chelsaV21'){
    datafolder <- paste0(base, "data/chelsaV21/")
  }
  return(datafolder)
}


get_extent_area <-function(area, ncol=3600, nrow=1800, crs=NA){
  # define extent of area of interest: coordinates in decimal.
  # Make sure min is min and max is max value. It will not work if not.
  if(is.na(crs)){
    crs <- sp::CRS("+proj=longlat")
  }

  if(area == 'Andes'){
    x_min <- -100
    x_max <- -60
    y_min <- -23
    y_max <- 23
  }else if(area == 'SouthAmerica'){
    x_min <- -100
    x_max <- -20
    y_min <- -60
    y_max <- 20

  }else if(area == 'EA'){
    x_min <- 25
    x_max <- 50
    y_min <- -23
    y_max <- 23
  }else if(area == 'Africa'){
    x_min <- -30
    x_max <- 55
    y_min <- -40
    y_max <- 35

  }  else if(area == 'tropics'){
    x_min <- -65
    x_max <- -40
    y_min <- -30
    y_max <- 30
  }else if(area == 'temperate'){
    x_min <- 0
    x_max <- 20
    y_min <- 44
    y_max <- 50

  }else if(area == 'Hawaii'){
    x_min <- -163
    x_max <- -153
    y_min <- 17
    y_max <- 25

  }else if(area == 'Kinabalu'){
    x_min <- 90
    x_max <- 160
    y_min <- -23
    y_max <- 23
  }else if(area == 'Asia'){
    x_min <- 70
    x_max <- 160
    y_min <- -20
    y_max <- 40

  }else if(area == 'world'){
    x_min <- -180
    x_max <- 180
    y_min <- -23
    y_max <- 23
  }else{
    print('Area not yet predefined....')
  }

  extend <- raster(ncol=ncol, nrow=nrow, xmn=x_min, xmx=x_max, ymn=y_min, ymx=y_max, crs=crs)
  return(st_as_stars(extend))
}
