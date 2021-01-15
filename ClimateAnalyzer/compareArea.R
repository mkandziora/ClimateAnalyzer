comparePolygons <- function(pp1, pp2, area, fn, tropical=FALSE){
  
  
  if(tropical == TRUE){
    fn <- paste("tropical", fn, sep="_")
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
    
    
    ext <- get_extent_area(area2, ncol=3600, nrow=1800)
    pp1 <- crop(pp1, extent(ext))
    plot(pp1)
    pp1 <- spTransform(pp1, CRSobj=crs(proj))
    plot(pp1)
    
    pp2 <- gBuffer(pp2, byid=TRUE, width=0)
    pp2 <- crop(pp2, extent(ext))
    plot(pp2)
    pp2 <- spTransform(pp2, CRSobj=crs(proj))
    plot(pp2)
  }else{
    proj = "+proj=longlat +datum=WGS84 +no_defs"
  }

  
  areasizes <- calculate_area_of_diff_insersect(pp1, pp2)
  
  ele <- get_elevation(area, zoom_level, crs, res)
  ele <- projectRaster(ele, crs=crs(proj))
  plot(ele)
  raster1 <- load_raster(pp1, ele)
  plot(raster1)
  diffp1 <- gDifference(pp1, pp2)
  
  if(class(diffp1) == "SpatialPolygons"){
    diff1 <- rasterize(diffp1, ele, mask=TRUE, small=TRUE)
    plot(diffp1)
  }
  diffp2 <- gDifference(pp2, pp1)
  if(class(diffp2) == "SpatialPolygons"){
    diff2 <- rasterize(diffp2, ele, mask=TRUE, small=TRUE)
    plot(diff2)  
  }


  
  xmin <- ele@extent@xmin
  xmax <- ele@extent@xmax
  ymin <- ele@extent@ymin
  ymax <- ele@extent@ymax

  # REWRITE TO GGPLOT - not working currently
  
  newmap <- getMap(resolution = "low")
  newmap <- spTransform(newmap, CRSobj=crs(proj))
  
  png(fn)
  plot(newmap, xlim = c(xmin, xmax), ylim = c(ymin, ymax), asp = 1, lwd = 0.2) # Andes
  plot(raster1,
       col=colorRampPalette("#E69F00")(255),   # create a color ramp of grey colors
       legend=TRUE,
       main="Extend1 - diff 1-2 - diff2-1 - orange/blue/green \n ",
       sub = areasizes,
       axes=FALSE, add=TRUE)
  if(class(diffp2) == "SpatialPolygons"){
    plot(diff2,
         col=colorRampPalette("#0072B2")(255), legend=FALSE,
         add=TRUE)
  }
 
  if(class(diffp1) == "SpatialPolygons"){
    plot(diff1,
         col=colorRampPalette("#009E73")(255), legend=FALSE,
         add=TRUE)
    }
  # plot(diff1,
  #      col=colorRampPalette(c("#009E73"))(255),legend=FALSE,
  #      add=TRUE)
  dev.off()
}


calculate_area_of_diff_insersect <- function(pp1, pp2){
  diffp1 <- gDifference(pp1, pp2)
  diffp2 <- gDifference(pp2, pp1)
  areasize_overlap <- get_area_size2(intersect(pp1, pp2))
  
  areasize2 <- ""
  if(class(diffp1) == "SpatialPolygons"){
    areasize2 <- get_area_size2(diffp1)
  }
  areasize1 <- ""
  if(class(diffp2) == "SpatialPolygons"){
    areasize1 <- get_area_size2(diffp2)
  }
  
  area_legend <- paste("Overlap area", areasize_overlap, "diff1-2",
                       areasize2, "diff2-1", areasize1)
  print(area_legend)
  return(area_legend)
}

load_raster <- function(pp, ext){
  
  raster_1 <- rasterize(pp, ext, mask=TRUE, small=TRUE) # getCover=TRUE
  plot(raster_1)
  
  return(raster_1)
}

get_area_size2 <- function(pp){
  # provides somewhat smaller area sizes. than get_area_size. Problem are projections....
  cell_size<-area(pp, na.rm=TRUE, weights=FALSE)
  #delete NAs from vector of all raster cells
  ##NAs lie outside of the rastered region, can thus be omitted
  cell_size<-cell_size[!is.na(cell_size)]
  #compute area [km2] of all cells in geo_raster
  raster_area<-length(cell_size)*median(cell_size)/1000000
  #print area according to raster object
  print(paste("Area (raster):", round(raster_area, digits=1),"km2; projection:", crs(pp)))
  return(raster_area)
  
}



