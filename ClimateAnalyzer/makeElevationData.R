plot_elev_treshold_locality <- function(area, var_name, pp, elevation_threshold){
  print('plot_elev_treshold_locality')
  source(paste0(base, 'ClimateAnalyzer/set_variables.R'))
  source(paste0(base, 'ClimateAnalyzer/make_maps.R'))
  
  prefix <- fn_prefix(var_name)
  
  ele <- get_elevation(area, zoom_level, crs, res)
 
  mts_rr <- rasterize_polygon(var_name, pp)
  if(area == "Africa"){
    ext <- get_extent_area("EA", ncol=3600, nrow=1800)
    ele <- crop(ele, extent(ext))
  }
 
  crop_mask <- mask(mts_rr, ele)

  Spol <- make_minmax_polygon(crop_mask)
  
  # get lowest values of elevation
  # https://stackoverflow.com/questions/48021657/how-to-extract-xy-coordinates-from-raster-where-its-highest-value-is-located-wit
  
  # create SpatialPolygonsDataFrame
  centroids <- coordinates(Spol)
  x <- centroids[,1]
  y <- centroids[,2]
  SPDF <- SpatialPolygonsDataFrame(Spol, data=data.frame(x=x, y=y, row.names=row.names(Spol)))
  
  # extract max value of raster for each SpatialPolygon
  maxext <- raster::extract(crop_mask, SPDF, cellnumbers=TRUE, df=TRUE)
  ext_low <- maxext
  ext_high <- maxext
  maxext
  
  ext_low$layer[maxext$layer>elevation_threshold] <- NA
  ext_low <- ext_low[complete.cases(ext_low$layer), ]
  

  points_low <- xyFromCell(crop_mask, ext_low$cell)
  elev_points_low <- extract(ele, points_low)
  plot_points_low <- data.frame(x, y, elev_points_low)
  
  fn <- paste(prefix, "map_lowelevation.png", sep="_")
  x <- points_low[, 1]
  y <- points_low[, 2]
  
  # get plot to locate area
  bc_bbox <- make_bbox(lat = y, lon = x, f = 0.2)
  bc_big <- get_map(location = bc_bbox, zoom = 5, source = "google", maptype = "terrain")
  
  png(fn)
  ggmap(bc_big) + 
    #geom_polygon(data = fortify(pp), aes(x, y, group = group) ) + 
    geom_point(data = as.data.frame(plot_points_low), aes(x, y, color=elev_points_low), size=0.02)
  dev.off()
  
  ext_high$layer[maxext$layer<elevation_threshold] <- NA
  ext_high <- ext_high[complete.cases(ext_high$layer), ]
  points_high <- xyFromCell(crop_mask, ext_high$cell)
  elev_points_high <- extract(ele, points_high)
  if(!is.null(elev_points_high)){
    x <- points_high[, 1]
    y <- points_high[, 2]
    plot_points_high <- data.frame(x, y, elev_points_high)
    fn <- paste(prefix, "map_highelevation2.png", sep="_")
    
    
    # get plot to locate area
    bc_bbox <- make_bbox(lat = y, lon = x, f = 0.2)
    bc_big <- get_map(location = bc_bbox, zoom = 5, source = "google", maptype = "terrain")
    
    png(fn)
    ggmap(bc_big) + 
      #geom_polygon(data = fortify(pp), aes(x, y, group = group) ) + 
      geom_point(data = as.data.frame(plot_points_high), aes(x, y, color=elev_points_high), size=0.02)
    dev.off()
  }
  
}


rasterize_polygon <- function(var_name, pp){
  # get elevation and crop it to pre-defined extent
  prefix <- fn_prefix(var_name)
  if(!file.exists(paste(prefix, 'elevation_mask.grd'))){
    plot(pp)
    
    ele <- get_elevation(area, zoom_level, crs, c(0.004333333, 0.004333333))
    if(area == "Africa"){
      area <- "EA"
      ext <- get_extent_area("EA", ncol=3600, nrow=1800)
      ele <- crop(ele, extent(ext))
    }
    plot(ele)
    mts_rr <- rasterize(pp, ele, mask=TRUE, small=TRUE)
    plot(mts_rr)
    plot_grd(mts_rr, area, paste(prefix, "elevation.png", sep="_"))
    
    writeRaster(mts_rr, paste(prefix, 'elevation_mask.grd', sep="_"), overwrite=TRUE)
  }else{
    # load elevation data
    mts_rr <- raster(paste(prefix, 'elevation_mask.grd', sep="_"))
  }
  return(mts_rr)
  
}

get_elevation_profile <- function(var_name, pp){
  prefix <- fn_prefix(var_name)
  
  print('get_elevation_profile')
  mts_gmba_rr <- rasterize_polygon(var_name, pp)
  
  plot(mts_gmba_rr)
  
  with(mts_gmba_rr, hist(mts_gmba_rr[mts_gmba_rr >= 0], breaks=seq(0,8000,by=500)))
  
  # hist(mts_gmba_rr,
  #       breaks = c(-5000, 0, 2000, 3000, 4000, 4500, 8000), 
  #      main="Histogram mts_rr",
  #       col="wheat3",  # changes bin color
  #       xlab= "Elevation (m)")  # label the x-axis
  
  
  plot(mts_gmba_rr, 
       breaks = c(0,2000, 3000, 3500, 4000, 4500, 8000), 
       col = terrain.colors(7),
       main="Elevation")
  
  
  png(paste(prefix, "hist_elevation.png", sep="_"))
  #hist(mts_gmba_rr,
  #     breaks = c(0, 2000, 3000, 3500, 4000, 4500,6000, 8000), 
  #     main="Histogram mts_gmba_rr",
  #     col="wheat3",  # changes bin color
  #     xlab= "Elevation (m)")  # label the x-axis
  with(mts_gmba_rr, hist(mts_gmba_rr[mts_gmba_rr >= 0], breaks=seq(0,8000,by=500)))
  
  dev.off()
  
  
  png(paste(prefix, "map_elevation.png", sep="_"))
  plot(mts_gmba_rr, 
       breaks = c(0, 2000, 3000, 3500, 4000, 4500, 8000), 
       col = terrain.colors(7),
       main="Elevation")
  dev.off()
}


