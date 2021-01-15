# cut down the bioclimatic layers to area needed

MakeCombinePolygons <- function(area, pol_comb_list){
  
  prefix <- fn_prefix(pol_comb_list)
  fn_pp <- paste(prefix, 'shape.shp', sep="_")
  if(!file.exists(fn_pp)){
    print('create combined polygon')
    print(pol_comb_list)
    i <- 1
    
    for(element in var_list){
      print(element)
      
      if(i == 1){
        ext <- get_extent_area(area)
        
        pp <- ext
        #grd = ext
        #i = 2
      }else{
        #grd = grd_small
        pp <- pp_small
      }
      prefix_single <- fn_prefix(element)
      
      relpath <- paste0("../", prefix_single, "/")
      #fn_grd = paste(relpath, prefix_single, '_mask.grd', sep="")
      fn_pp <- paste0(relpath, prefix_single, '_shape.shp')

      if(i == 1){
        #grd_small = raster(fn_grd)
        pp_small <- shapefile(fn_pp)
        i <- 2
      }else{
        #grd_new = raster(fn_grd)
        #plot(grd_new)
        pp_new <- shapefile(fn_pp)
        plot(pp_new)
        #grd_small = intersect(grd, grd_new)
        pp_small <- intersect(pp, pp_new)

        #plot(grd_small)
        plot(pp_small)
        
        #elem_comb = fn_prefix(pol_comb_list)
        
        #fn = paste(elem_comb, '_mask.png', sep="")
        #plot_grd(grd_small, area, fn)
      }
      elem_comb <- fn_prefix(pol_comb_list)
      
      plot(pp_small)
      
      grd_small <- rasterize_polygon(pol_comb_list, pp_small)
      plot(grd_small)
      
      plot_grd(grd_small, area, paste(elem_comb, 'mask.png', sep="_"))
      plot_grd(pp_small, area, paste(elem_comb, 'pol.png', sep="_"))
      
      fn_grd <- paste0(elem_comb, '_mask.grd')
      fn_pp <- paste0(elem_comb, '_shape.shp')
      
      writeRaster(grd_small, fn_grd, overwrite=TRUE)
      shapefile(pp_small, fn_pp, overwrite=TRUE)
    }
  }else{
    pp_small <- shapefile(paste(prefix, "shape.shp", sep="_"))
  }
  #plot_grd(grd_small, area, paste(prefix, 'mask.png', sep="_"))
  plot_grd(pp_small, area, paste(prefix, 'pol.png', sep="_"))
  return(pp_small)
}


MakeReducePolygonClim <- function(area, var_name, climatename){
  
  print('create single polygon from climate')
  print(var_name)  
  
  # assign bioclim_comb the variable you currently want to use: it will generate images and polygons. 
  # Look at the CHELSA_bio10_bioclim#.....png at the end, see if you like it, if not maybe make a new one, e.g. var_comb11_b
  if(file.exists(file.path(area))){
    dir.create(area)
    if(file.exists(file.path(area, climatename))){
      dir.create(file.path(area, climatename))
    }
  }
  prefix <- fn_prefix(var_name)
  fn_grd <- paste("../", strsplit(prefix, "_")[[1]][1], 'area_mask.grd', sep="_")
  fn_pp <- paste(prefix, 'shape.shp', sep="_")
  fn_grd2 <- paste(prefix, 'mask.grd', sep="_")
  
  if(!file.exists(fn_pp)){
    
    path_to_folder <- get_path_data(climatename)
    files <- list.files(path=path_to_folder,  pattern='tif',  full.names=TRUE )
    print(files)
    
    mask <- raster(files[var_name[1]])
    #plot(mask)
    
    if(!file.exists(fn_grd)){
      grd_area <- raster_cut_to_area(mask, area, var_name)
      writeRaster(grd_area, fn_grd, overwrite=TRUE)
    }else{
      grd_area <- raster(fn_grd)
    }
    plot(grd_area)
    
    grd_minmax <- raster_cut_to_minmax(grd_area, var_name, climatename)
    writeRaster(grd_minmax, fn_grd2, overwrite=TRUE)
    plot(grd_minmax)
    
    pp_min_max <- make_minmax_polygon(grd_minmax)
    shapefile(pp_min_max, fn_pp, overwrite=TRUE)
    plot(pp_min_max)
    plot_grd(grd_minmax, area, fn=paste(prefix, 'mask.png', sep="_"))
    
    
  }else{
    pp_min_max <- shapefile(fn_pp)
  }
  plot_grd(pp_min_max, area, fn=paste(prefix, 'shape.png', sep="_"))
  return(pp_min_max)
}


MakeReducePolygonElevation <- function(area, var_name){
  #source(file.path(base, 'tropAlpR_dev/00_set_continent.R'))
  
  print('create area_elevation_files')
  print(area)  
  
  prefix <- fn_prefix("elevation")
  
  fn_grd <- paste("../", prefix, 'area_mask.grd', sep="_")
  fn_pp <- paste(prefix, 'shape.shp', sep="_")
  fn_grd2 <- paste(prefix, 'mask.grd', sep="_")
  
  if(!file.exists(fn_grd2)){
    ele <- get_elevation(area, zoom_level, crs, res)
    
    if(!file.exists(fn_grd)){
      grd_area <- raster_cut_to_area(ele, area, "elevation")
      writeRaster(grd_area, fn_grd, overwrite=TRUE)
    }else{
      grd_area <- raster(fn_grd)
    }

    grd_minmax <- raster_cut_to_minmax(grd_area, "elevation")
    pp_min_max <- make_minmax_polygon(grd_minmax)
    
    shapefile(pp_min_max, fn_pp, overwrite=TRUE)
    writeRaster(grd_minmax, fn_grd2, overwrite=TRUE)
    
    
  }else{
    grd_minmax <- raster(fn_grd2)
    pp_min_max <- shapefile(fn_pp)
  }
  plot(grd_minmax)
  plot(pp_min_max)
  plot_grd(grd_minmax, area, fn=paste(prefix, 'mask.png', sep="_"))
  plot_grd(pp_min_max, area, fn=paste(prefix, 'shape.png', sep="_"))
  return(pp_min_max)
}

makeGrowingSeasonPol <- function(var_name, area){
  print('create growing season polygon')
  prefix <- fn_prefix(var_name)
  prefix_new <- paste(strsplit(prefix, "_")[[1]][1], strsplit(prefix, "_")[[1]][2], sep="_")
  
  calc_option = strsplit(var_name, "_")[[1]][5]
  
  fn_grd <- paste0(prefix, '_mask.grd')
  fn_pp <- paste(prefix, 'shape.shp', sep="_")
  if(!file.exists(fn_grd)){
    fn_grd <- paste0(prefix, 'length_mask.grd')
    fn_pp <- paste0(prefix, 'length_shape.shp')
    if(!file.exists(fn_grd)){
      print("make growing season length")
      
      fn_all <- paste0("../", prefix_new, "_", calc_option, '_all_length_mask.grd')
      if(!file.exists(fn_all)){
        files_path <- get_path_data("GSL")
        files_stack <- get_GSarea(files_path, area)
        
        print("calc")
        if(calc_option == "mean"){
          mean_val <- calc(files_stack, fun = mean)
        }else if(calc_option == "median"){
          mean_val <- calc(files_stack, fun = median)
        }
        
        mean_val[mean_val <= -25000] <- NA
        writeRaster(mean_val, fn_all, overwrite=TRUE)
        plot_grd(mean_val, area, paste(prefix_new, calc_option, 'all_length_mask.png', sep="_"))
      }else{
        mean_val <- raster(fn_all)
      }
      plot(mean_val)
      
      grow_season_length <- as.integer(strsplit(var_name, "_")[[1]][4])
      mean_val[mean_val < grow_season_length] <- NA
      #mean_val[mean_val >= 250] <- NA
      
      plot(mean_val)
      gs_length <- mean_val
      gs_length_pp <- make_minmax_polygon(gs_length)
      plot(gs_length_pp)

      plot_grd(gs_length, area, paste(prefix, 'length_mask.png', sep="_"))
      plot_grd(gs_length_pp, area, paste(prefix, 'length_pol.png', sep="_"))
      #
      writeRaster(gs_length, fn_grd, overwrite=TRUE)
      shapefile(gs_length_pp, fn_pp, overwrite=TRUE)
    }else{
      gs_length <- raster(fn_grd)
      gs_length_pp <- shapefile(fn_pp)
      
    }
    
    fn_pp <- paste(prefix, 'temp_shape.shp', sep="_")
    fn_grd <- paste(prefix, 'temp_mask.grd', sep="_")
    if(!file.exists(fn_grd)){
      print("make growing season temp")
      prefix_new <- paste(strsplit(prefix, "_")[[1]][1], strsplit(prefix, "_")[[1]][2], sep="_")
      fn_all <- paste0("../", prefix_new, calc_option,'_all_temp_mask.grd')
      if(!file.exists(fn_all)){
        files_path <- get_path_data("GST")
        files_stack <- get_GSarea(files_path, area)
        
        print("calc")
        if(calc_option == "mean"){
          mean_val <- calc(files_stack, fun = mean)
        }else if(calc_option == "median"){
          mean_val <- calc(files_stack, fun = median)
        }
        
        plot_grd(mean_val, area, paste(prefix_new, calc_option, 'all_temp_mask.png', sep="_"))
        writeRaster(mean_val, fn_all, overwrite=TRUE)
      }else{
        mean_val <- raster(fn_all)
      }

      grow_season_temp <- strsplit(var_name, "_")[[1]][3]
      grow_season_temp <- as.numeric(paste(strsplit(grow_season_temp, "")[[1]][1],
                                           strsplit(grow_season_temp, "")[[1]][2], sep = "."))
      mean_val[mean_val <= grow_season_temp] <- NA
      plot(mean_val)

      gs_temp <- mean_val
      gs_temp_pp <- make_minmax_polygon(mean_val)

      plot_grd(gs_temp, area, paste(prefix, 'temp_mask.png', sep="_"))
      plot_grd(gs_temp_pp, area, paste(prefix, 'temp_pol.png', sep="_"))
      #
      writeRaster(gs_temp, fn_grd, overwrite=TRUE)
      shapefile(gs_temp_pp, fn_pp, overwrite=TRUE)
      
    }else{
      gs_temp <- raster(fn_grd)      
      gs_temp_pp <- shapefile(fn_pp)

    }
    
    print("make treeline model")
    
    pp_small <- intersect(gs_length_pp, gs_temp_pp)
    plot(pp_small)
    
    grd_small <- intersect(gs_length, gs_temp)
    plot(grd_small)
    
    # I first calculate the area which is below the treeline, gs length > 94 and temp >6.4,
    # then I calculate the intersect, to see where both var meet, then get alpine by doing inverse
    grd_small = mask(grd_small, pp_small, inverse = TRUE)
    plot(grd_small)
    
    grd_small_pp <- make_minmax_polygon(grd_small)
    plot(grd_small_pp)
    
    plot_grd(grd_small, area, paste(prefix, 'mask.png', sep="_"))
    plot_grd(grd_small_pp, area, paste(prefix, 'pol.png', sep="_"))
    
    fn_grd <- paste0(prefix, '_mask.grd')
    fn_pp <- paste0(prefix, '_shape.shp')
    writeRaster(grd_small, fn_grd, overwrite=TRUE)
    shapefile(grd_small_pp, fn_pp, overwrite=TRUE)
    
  }else{
    pp_small <- shapefile(paste(prefix, "shape.shp", sep="_"))
  }
  plot_grd(pp_small, area, paste(prefix, 'pol.png', sep="_"))
  return(pp_small)
}


get_GSarea <- function(files_path, area){
  # load growing season and crop to area
  all_files <- list.files(files_path,
                          full.names = TRUE,
                          pattern = ".tif") #take all the ascii files in    
  files_stack <- stack(all_files) #stack them together 
  nam <- 1979:2013
  
  names(files_stack) <- nam
  
  ext <- get_extent_area(area)
  files_stack <- crop(files_stack, ext)
  return(files_stack)
}


plot_grd <- function(grd, area, fn){
  # make plots that are cut to tropical definition
  if(area == "Africa"){
    area <- "EA"
  }else if(area == "Asia"){
    area <- "Kinabalu"
  }else if(area == "Hawaii"){
    area <- "Hawaii"
  }else if(area == "SouthAmerica"){
    area <- "Andes"
  }
  
  ext <- get_extent_area(area)
  xmin <- ext@extent@xmin
  xmax <- ext@extent@xmax
  ymin <- ext@extent@ymin
  ymax <- ext@extent@ymax
  
  newmap <- getMap(resolution = "low")
  newmap <- spTransform(newmap, CRSobj=crs("+proj=longlat +datum=WGS84 +no_defs"))
  png(fn)
  plot(newmap, xlim = c(xmin, xmax), ylim = c(ymin, ymax), asp = 1, lwd = 0.2) # Andes
  plot(grd,add=TRUE)
  dev.off()
  
  if(class(grd) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
    # get plot to locate area
    bc_bbox <- bbox(ext)
    bc_big <- get_map(location = bc_bbox, zoom = 5, source = "google", maptype = "terrain")
    
    fpp <- fortify(grd)
    fn <- paste("tropical", fn, sep="_")

    png(fn)
    ggmap(bc_big) + 
    #  geom_point(data = as.data.frame(plot_points_high), aes(x, y, color=elev_points_high), size=0.02)
    
    geom_polygon(data = fpp, aes(long, lat, group = group) ) 
    #geom_polygon(aes(x=long, y=lat, group=group),
    #geom_point(data = as.data.frame(plot_points_high), aes(x, y, color=elev_points_high),
    #           fill='purple', size=.2, 
     #          color='purple', data=fpp, alpha=0.5)
    dev.off()
  }
 
}


raster_cut_to_area <- function(mask, area, var_name){
  # crop raster to area
  print("raster cut to area")
  prefix <- fn_prefix(var_name)
  
  ext <- get_extent_area(area)
  crop_mask <- raster::crop(mask, extent(ext))
  # issue with res?
  crop_mask <- projectRaster(crop_mask, crs=ext@crs, res=c(0.008333333, 0.008333333))
  
  fn_png <- paste(strsplit(prefix, "_")[[1]][1], 'mask.png', sep="_")
  plot_grd(crop_mask, area, fn_png)
  return(crop_mask)
}


make_over_view_map <- function(area, climatename, path_to_folder){
  # generate climate overview maps for area
  ext <- get_extent_area(area)
  
  #load climate files
  files <- list.files(path=path_to_folder,  pattern='tif',  full.names=TRUE )
  
  # makes small images for the region with the values of the respective bioclim and saves it to file
  for(layer in files){
    print(layer)
    name <- strsplit(strsplit(layer, '/')[[1]][7], '.tif')[[1]]
    mask <- raster(layer)
    r2 <- crop(mask, extent(ext))
    plot(r2)
    
    png(paste(area, name, 'area_map.png', sep="_"))
    plot(r2)
    dev.off()
  }
}


raster_cut_to_minmax <-function(grd_area, var_name, climatename){
  print("raster cut to min max")
  if(typeof(var_name) == "character"){
    if(var_name == "elevation"){
      source(paste0(base, 'ClimateAnalyzer/set_variables.R'))
      
      min_ele <- min_ele
      grd_area[grd_area <= min_ele] <- NA
      mask2 <- grd_area
    }
  }else{
    bioclim <- var_name[1]
    min <- var_name[2]
    max <- var_name[3]
    if(bioclim %in% c(1,2,3,4,5,6,7,8,9,10,11)){    
      min <- min*10
      max <- max*10
    }
    
    if(climatename %in% c('chelsa_miroc_esm',"mpi-esm")){
      if(bioclim %in% c(1,2,3,4,5,6,7,8,9,10,11)){
        if(min == 0){
          max <- (max/10+273)*10
          
        }else{
          min <- (min/10+273)*10
          max <- (max/10+273)*10
        }
        
        
        grd_area <- cut_to_continent(grd_area, area)
      }
    }
    min
    max
    mask2 <- grd_area
    
    print("cut min")
    mask2[grd_area <= min] <- NA
    print("cut max")
    mask2[grd_area >= max] <- NA
    
  }
  #print("trim mask")
  #mask2 = trim(mask2)
  #plot(mask2)
  return(mask2)
}



cut_to_continent <- function(mask2, area){
  print("cut past climate to continent")
  source(paste0(base, 'ClimateAnalyzer/set_variables.R'))
  
  mask2 <- projectRaster(mask2, crs=crs, res=res)
  
  if (area == 'SouthAmerica'){
    sa <- rnaturalearth::ne_countries(continent = "South America")
    na <- rnaturalearth::ne_countries(continent = "North America")
    sa <- spTransform(sa,crs)
    na <- spTransform(na,crs)
    x <- bind(sa, na)
  }else if (area %in% c('EA', "Africa")){
    af <- rnaturalearth::ne_countries(continent = "Africa")
    #na = rnaturalearth::ne_countries(continent = "North America")
    af <- sp::spTransform(af,crs)
    x <- af
    #na <- sp::spTransform(na,crs)
    #x <- bind(sa, na)
  }else if (area == 'Hawaii'){
    na <- rnaturalearth::ne_countries(continent = "North America")
    na <- sp::spTransform(na,crs)
    x <- na
  }else if (area %in% c('Asia', "Kinabalu")){
    sa <- rnaturalearth::ne_countries(continent = "Asia")
    na <- rnaturalearth::ne_countries(continent = "Oceania")
    sa <- spTransform(sa,crs)
    na <- spTransform(na,crs)
    x <- bind(sa, na)
  }
  plot(x)
  
  # mask_3_b <- raster::crop(mask2, extent(x))
  ext2 <- get_extent_area(area, crs=crs)
  mask_3_b <- raster::crop(mask2, ext2)
  
  mask_3_b <- mask(mask_3_b, x)
  return(mask_3_b)
  
}


make_minmax_polygon <- function(grd_minmax){
  print("make min max polygon")
  e <- extent(grd_minmax)
  # coerce to a SpatialPolygons object
  # p <- as(e, 'SpatialPolygons')
  
  ### this makes the actual extend we are interested in
  #To get a polygon that surrounds cells that are not NA
  # make all values the same. Either do
  r <- grd_minmax > -Inf
  # or alternatively
  # r <- reclassify(x, cbind(-Inf, Inf, 1))
  print("...aggregate...")
  # reduces resolution a bit
  r_agg <- aggregate(r, 7)
  #r_agg_newcrs <- projectRaster(r_agg, crs=crs(grd_minmax), res=res(grd_minmax))
  
  print("...rasterToPolygons...")
  # convert to polygons (you need to have package 'rgeos' installed for this to work)
  pp <- rasterToPolygons(r_agg, na.rm=TRUE, dissolve=TRUE)
  
  return(pp)
}



get_elevation <- function(area, zoom_level, crs, res){
  fn <- paste0('elevation_', area, '_zoom', zoom_level, '.grd')
  ext <- get_extent_area(area, ncol=3600, nrow=1800)
  if(file.exists(file.path(base, 'tropAlpR', fn))){
    #print('reloading elevation...')
    ele <- raster(file.path(base, 'tropAlpR', fn))
    
    
    ele <- crop(ele, extent(ext))
  }else{
    print('...downloading elevation data...')
    
    ele <- elevatr::get_elev_raster(ext, z=zoom_level)
    print('...writing to file...')
    writeRaster(ele, file.path(base, 'tropAlpR', fn))
  }
  return(ele)
}


get_area_size <- function(pp){
  #calculate area [m2] of the polygon
  sqm<-areaPolygon(pp)
  sqm
  #convert sqm to km2
  sqkm<-sqm/10000000
  sqkm
  return(sqkm)
}
