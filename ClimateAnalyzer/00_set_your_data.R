# Martha Kandziora
# 2021
# these functions are crucial and need to be adapted by every user before starting to analyse the areas and climates.

get_path_data <- function(climate_data){
  if(climate_data == 'chelsa_miroc_esm'){
    datafolder <- paste0(base, "tropAlpR/chelsa_miroc_esm/")
  }else if(climate_data == 'mpi-esm'){
    datafolder <- paste0(base, "tropAlpR/mpi-esm/")
  }else if(climate_data == "Chelsa"){
    datafolder <- paste0(base, "tropAlpR/chelsa/")
  }else if(climate_data == 'worldclim'){
    datafolder <- paste0(base, "tropAlpR/worldclim/")
  }else if(climate_data == 'elevation'){
    datafolder <- paste0(base, "tropAlpR/elevation/")
  }else if(climate_data == 'GSL'){
    datafolder <- paste0(base, "tropAlpR_2/growing_season/gs_length/")
  }else if(climate_data == 'GST'){
    datafolder <- paste0(base, "tropAlpR_2/growing_season/gs_temp/")
  }
  return(datafolder)
}


get_extent_area <-function(area, ncol=3600, nrow=1800, crs=NA){
  # define extent of area of interest: coordinates in decimal.
  # Make sure min is min and max is max value. It will not work if not.
  if(is.na(crs)){
    crs <- CRS("+proj=longlat")
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
    x_max <- 50
    y_min <- -50
    y_max <- 40

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
  return(extend)
}
