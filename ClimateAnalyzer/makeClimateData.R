generate_climate_data_polygon <- function(var_comb, climatename, area, tropical=FALSE){
  datafolder <- get_path_data(climatename)
  prefix <- fn_prefix(var_comb)
  print("tropical")
  print(tropical)
  
 # fn = paste(prefix, "sdm_all.data", sep = "_")
  fn <- paste(prefix, "presvals_all.rds", sep="_")
  if(tropical == TRUE){
    fn <- paste(prefix, "tropical_presvals_all.rds", sep = "_")
  }
  crop_mask <- load_grd(var_comb)
  pp <- load_shp(var_comb)
  
  if(!file.exists(fn)){
    #  Now create a RasterStack of bioclim variables.
    if(tropical == TRUE){
      if(area == "Africa"){
        area2 <- "EA"
      }else if(area == "Asia"){
        area2 <- "Kinabalu"

      }else if(area == "Hawaii"){
        area2 <- "Hawaii"

      }else if(area == "SouthAmerica"){
        area2 <- "Andes"
      }
      
      ext <- get_extent_area(area2, ncol=3600, nrow=1800)
      pp <- crop(pp, extent(ext))
      crop_mask <- crop(crop_mask, extent(ext))
    }

    files <- list.files(path=datafolder,  pattern='tif',  full.names=TRUE )
    predictors <- stack(files)
    
    plot(crop_mask)
    print('make presence values')
    bg2 <- randomPoints(crop_mask, 1000, ext=extent(pp),tryf = 25)
    plot(bg2)
    presvals_all <-  extract(predictors, bg2)
    saveRDS(presvals_all, file=fn)
    
    # ##########
    # 
    # print('make absence values')
    # # We first create a ‘circles’ model, using an arbitrary radius of 50 km
    # 
    # x_all <- dismo::circles(bg2, d= 50000, lonlat=TRUE)
    # plot(newmap, xlim = c(xmin, xmax), ylim = c(ymin, ymax), asp = 1, lwd = 0.2) # Andes
    # plot(x_all,add=TRUE)
    # ## Loading required namespace: rgeos
    # pol_all = polygons(x_all)
    # 
    # # sample randomly from all circles
    # samp_all = spsample(pol_all, 25000, type='random', iter=25)
    # plot(samp_all)
    # # get unique cells
    # cells_all <- cellFromXY(crop_mask, samp_all)
    # length(cells_all)
    # ## [1] 250
    # cells_all <- unique(cells_all)
    # length(cells_all)
    # ## [1] 167
    # xy_all <- xyFromCell(crop_mask, cells_all)
    # 
    # plot(pol_all, axes=TRUE)
    # points(xy_all, cex=0.5, pch=20, col='blue')
    # 
    # 
    # # Note that the blue points are not all within the polygons (circles), as they now represent the centers of the selected cells from mask.
    # # We could choose to select only those cells that have their centers within the circles, using the overlay function.
    # xy_all = na.omit(xy_all)
    # spxy_all <- SpatialPoints(xy_all, proj4string=crs)
    # 
    # # does not work for 03: do points_all = as.data.frame(spxy_all); xyInside_all = spxy_all
    # o_all <- over(spxy_all, geometry(x_all))
    # xyInside_all <- xy_all[!is.na(o_all), ]
    # 
    # points_all = as.data.frame(xyInside_all)
    # 
    # plot(pol_all, axes=TRUE)
    # points(points_all, cex=0.75, pch=20, col='blue')
    # 
    # ##############################################################
    # ### Produce bioclim data for buffered points
    # absvals_all <- extract(predictors, xyInside_all)
    # 
    # pb <- c(rep(1, nrow(presvals_all)), rep(0, nrow(absvals_all)))
    # sdmdata_all <- data.frame(cbind(pb, rbind(presvals_all, absvals_all)))
    # #sdmdata_all <- data.frame(cbind(pb, rbind(presvals_all, absvals_all)))
    # 
    # head(sdmdata_all)
    # tail(sdmdata_all)
    # 
    # summary(sdmdata_all)
    # 
    # fn = paste(prefix, "summary_sdm.data", sep = "_")
    # if(tropical == TRUE){
    #   fn = paste(prefix, "tropical_summary_all.data", sep = "_")
    # }
    # write.csv(summary(sdmdata_all), file = fn)
    # 
    # fn = paste(prefix, "sdm_all.data", sep = "_")
    # if(tropical == TRUE){
    #   fn = paste(prefix, "tropical_sdm_all.data", sep = "_")
    # }
    # write.csv(sdmdata_all, file = fn)
  } else{
    #sdmdata_all = load_climate_table(fn)
    #summary(sdmdata_all)
    presvals_all <- load_climate_table(fn)
    summary(presvals_all)
  }
  return(presvals_all)
}


load_climate_table <- function(fn){
  #climate = read.csv(fn)
  climate <- readRDS(fn)
  if(ncol(climate) == 1 ){
    climate <- read.csv(fn, sep="/t")
  }
  climate <- climate[, 2:ncol(climate)]
  climate <- na.omit(climate)
  return(climate)
}


get_min_max_data <- function(data, name=NA){
  min_val <- apply(data, 2, min)
  max_val <- apply(data, 2, max)
  
    combined <- data.frame(min_val, max_val)
  if(is.na(name)){
    name <- "all"
  }
  len_col <- nrow(combined)
  names(combined) <- c(paste(name, "min_val", sep = "_"), paste(name, "max_val", sep = "_"))
  #combined$names  = c("pb", seq(1:19))
  combined$names <- seq(1:len_col)
  
  return(combined)
}


