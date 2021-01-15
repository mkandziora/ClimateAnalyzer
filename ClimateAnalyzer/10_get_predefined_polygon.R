GMBA_alpine <- function(area, pp){
  
  pp <- readOGR(file.path(base, 'tropAlpR', "gmba_definition/GMBA\ mountain\ definition_V1.1.shp"))
  
  # 1 to 3 is regarded as alpine
  one <- pp[pp@data$tvzcode %in% c(1, 2, 3), ]
  one <- pp[pp@data$tvzcode == 1, ]
  two <- pp[pp@data$tvzcode == 2, ]
  three <- pp[pp@data$tvzcode == 3, ]
  
  plot(one)
  plot(two)
  plot(three)
  pp2 <- union(two, three)
  plot(pp2)
  
  ext <- get_extent_area(area, ncol=3600, nrow=1800)
  pp2 <- crop(pp2, extent(ext))
  plot(pp2)  
  # ele = get_elevation("world", 1, crs, res)
  # mask_crop2 = rasterize(pp2, ele, mask=TRUE)
  # 
  inter1 <- data.frame(pp2@data$longitude, pp2@data$latitude)
  #add a category (required for later rasterizing/polygonizing)
  inter1 <- cbind(inter1, cat = rep(1L, nrow(inter1)),stringsAsFactors = FALSE)
  names(inter1) <- c("y", "x", "z")
  ##convert to spatial points
  coordinates(inter1) <- ~pp2.data.longitude + pp2.data.latitude
  
  #gridify your set of points
  gridded(inter1) <- TRUE
  
  t <- inter1@data[, 1:2]
  yourshapefile_df <- fortify(t )
  plot(yourshapefile_df)
  inter1 <- yourshapefile_df
  #convert to raster
  r <- raster(inter1)
  
  #convert raster to polygons
  sp <- rasterToPolygons(r, dissolve = T)
  
  #addition transformation to distinguish well the set of polygons
  polys <- slot(sp@polygons[[1]], "Polygons")
  output <- SpatialPolygons(
    Srl = lapply(1:length(polys),
                 function(x){
                   p <- polys[[x]]
                   
                   # #applying spline.poly function for smoothing polygon edges
                   # px <- slot(polys[[x]], "coords")[,1]
                   # py <- slot(polys[[x]], "coords")[,2]
                   # bz <- spline.poly(slot(polys[[x]], "coords"),100, k=3)
                   # bz <- rbind(bz, bz[1,])
                   # slot(p, "coords") <- bz               
                   
                   # create Polygons object
                   poly <- Polygons(list(p), ID = x)
                   return(poly)
                 }),
    proj4string = CRS("+init=epsg:4326")
  )
  
  #plot
  plot(sp, border = "gray", lwd = 2) #polygonize result
  plot(output, border = "red",  add = TRUE) #smoothed polygons
  return(ele)
}

translate_predefined <- function(var_name, area){
  
  predef <- c("testolin_alpine_cluster", "GMBA")
  testit::assert(var_name %in% predef)
  
  if(!file.exists(paste(var_name, area, 'shape.shp', sep="_"))){
    if(var_name == "testolin_alpine_cluster"){
      
      mts <- readOGR(file.path(base, 'tropAlpR', "Testolin_2020_data/alpine_clusters.shp"))
      
      #plot(mts@polygons[[1]])
      
      if(area == "SouthAmerica"){
        pp <- mts[mts@data$continent == "South America",]
      }else  if(area == "Asia"){
        pp <- mts[mts@data$continent == "Asia",]
      }else  if(area == "Hawaii"){
        pp <- mts[mts@data$continent == "Oceania",]
      } else if(area == "Africa"){
        pp <- mts[mts@data$continent == "Africa",]
      }
      
      
    }else if(var_name == "GMBA")
      if(area == "SouthAmerica"){
        pp <- readOGR(file.path(base, 'tropAlpR', "gmba_definition/GMBA\ Mountain\ Inventory_v1.2-SouthAmerica.shp"))
      }else  if(area == "Asia"){
        pp1 <- readOGR(file.path(base, 'tropAlpR', "gmba_definition/GMBA\ Mountain\ Inventory_v1.2-Asia.shp"))
        pp2 <- readOGR(file.path(base, 'tropAlpR', "gmba_definition/GMBA\ Mountain\ Inventory_v1.2-Australia.shp"))
        pp <- union(pp1, pp2)
      }else  if(area == "Hawaii"){
        
        pp <- readOGR(file.path(base, 'tropAlpR', "gmba_definition/GMBA\ Mountain\ Inventory_v1.2-Oceania.shp"))
      }else if(area == "Africa"){
        pp <- readOGR(file.path(base, 'tropAlpR', "gmba_definition/GMBA\ Mountain\ Inventory_v1.2-Africa.shp"))
        
      }
    
    
    plot(pp)
    ele <- get_elevation(area, zoom_level, crs, res)
    
    mask_crop2 <- rasterize(pp, ele, mask=TRUE)
    plot(mask_crop2)
    
    
    shapefile(pp, paste(var_name, area, 'shape.shp', sep="_"), overwrite=TRUE)
    #test = shapefile('test.shp')
    writeRaster(mask_crop2, paste(var_name, area, 'mask.grd', sep="_"), overwrite=TRUE)
    #test2 = raster('test2.grd')
    
    xmin <- mask_crop2@extent@xmin
    xmax <- mask_crop2@extent@xmax
    ymin <- mask_crop2@extent@ymin
    ymax <- mask_crop2@extent@ymax
    
    newmap <- getMap(resolution = "low")
    
    png(paste(var_name, area, 'map.png', sep="_"))
    plot(newmap, xlim = c(xmin, xmax), ylim = c(ymin, ymax), asp = 1, lwd = 0.2) # Andes
    plot(mask_crop2,add=TRUE)
    dev.off()
    
    png(paste(var_name, area, 'pol.png', sep="_"))
    plot(newmap, xlim = c(xmin, xmax), ylim = c(ymin, ymax), asp = 1, lwd = 0.2) # Andes
    plot(pp, add=TRUE)
    dev.off()
  }else{
    fn <- paste(var_name, area, 'shape.shp', sep="_")
    pp <- shapefile(fn)
  }
  
  return(pp)
  
}



