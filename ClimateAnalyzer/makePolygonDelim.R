####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

fix_crs_future <- function(climatename, area, wd){
  print("fix future crs")
  path_to_folder <- get_path_data(climatename)
  print(path_to_folder)
  files <- list.files(path=path_to_folder,   pattern='.tif',  full.names=TRUE ) #
  if(climatename %in% c("future_mpi26", "future_MIROC26")){
    path_to_folder <- get_path_data(climatename)
    print(path_to_folder)
    files <- list.files(path=path_to_folder,   pattern='.tif',  full.names=TRUE ) #
    files_new = list.files(path=wd,   pattern='.tif',  full.names=FALSE ) #
    for(fn in files){
      fn_new = strsplit(fn, "/")[[1]]
      fn_new = paste0("reproj", fn_new[length(fn_new)])
      print(fn_new)
      if(!fn_new %in% files_new){
        r = raster(fn)
        old_crs = crs(r)
        new_crs = st_crs(4326) # WGS 84 has EPSG code 4326
        r2 = st_as_stars(r)
        ext <- get_extent_area(area)
        r2 <- st_crop(r2, st_bbox(ext), crop=T)
        r2 = st_as_stars(r2)
        r3 = st_warp(r2, crs=new_crs)
        write_stars(r3, paste0(wd, "/", fn_new))
      }
    }
    files <- list.files(path=wd,   pattern='.tif',  full.names=TRUE ) #
    files = files[grep("reproj", files)]
  }
 return(files)
}


MakeCombinePolygons <- function(area, pol_comb_list, climatename){
  
  prefix <- fn_prefix(pol_comb_list)
  fn_pp <- paste(prefix, 'shape.shp', sep="_")
  if(!file.exists(fn_pp)){
    print('create combined polygon')
    print(pol_comb_list)
    i <- 1
    
    for(element in pol_comb_list){
      prefix_single <- fn_prefix(element)
      relpath <- paste0("../", prefix_single, "/")
      fn_pp <- paste0(relpath, prefix_single, '_shape.shp')
      if(!file.exists(fn_pp)){
        print(paste0("Area was not recovered for the settings: ", prefix_single))
        return(NA)
      }
      if(i == 1){
        new_pol_comb = list(element)
        pp <- get_extent_area(area)
        pp_small <- read_sf(fn_pp)
        i <- 2
        layer_name = c("layer1")
        names(pp_small)[1] = layer_name 
      }else{
        new_pol_comb[[i]] <- element
        prefix_new_comb <- fn_prefix(new_pol_comb)
        pp <- st_as_sf(pp_small)
        pp_new <- st_as_sf(read_sf(fn_pp))

        print("intersect")
        pp_new = st_make_valid(pp_new)
        pp_small <- st_intersection(pp, pp_new, dimension = "polygon")
        pp_small = st_make_valid(pp_small)
        pp_small = st_union(pp_small)
        pp_small = st_make_valid(pp_small)
        
        names(pp_small)[1] =  c(layer_name, paste0("layer", i)) 
        
        i <- i + 1
        
        plot_grd(pp_small, area, paste(prefix_new_comb, 'pol.png', sep="_"))
      }
      if(length(new_pol_comb) == length(pol_comb_list)){
        elem_comb <- fn_prefix(pol_comb_list)
        fn_pp <- paste0(elem_comb, '_shape.shp')
        write_sf(pp_small, fn_pp, overwrite=TRUE)
        
        print("rasterize combined polygon")
        ele <- get_elevation(area, zoom_level, crs, res)

        fn = paste0(elem_comb, climatename, "elevation.RDA")
        grd_small <- rasterize_polygon(pol_comb_list, pp_small, ele, fn)
        plot_grd(pp_small, area, paste(elem_comb, 'pol.png', sep="_"))
      }
    }
  }else{
    pp_small <- read_sf(paste(prefix, "shape.shp", sep="_"))
  }
  plot_grd(pp_small, area, paste(prefix, 'pol.png', sep="_"))
  return(pp_small)
}


MakeReducePolygonClim <- function(area, var_name, climatename){
  
  print('create single polygon from climate')

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
    print(path_to_folder)
    
    files <- list.files(path=path_to_folder,   pattern='.tif',  full.names=TRUE ) #
    files = fix_crs_future(climatename, area, "../")

    fn = files[var_name[1]]
    if(climatename %in% c("future_MIROC26", "future_mpi26")){
      fn=files[grep(paste0("_", var_name[1], "_" ), files)]
    }else if(climatename %in% c("futureV21", "chelsaV21")){
      fn=files[grep(paste0("bio", var_name[1]), files)]
    }
    mask <- read_stars(fn)

    if(!file.exists(fn_grd)){
      grd_area <- raster_cut_to_area(mask, area, var_name)
      write_stars(grd_area, fn_grd, overwrite=TRUE)
      grd_area <- read_stars(fn_grd, proxy=F)
    }else{
      grd_area <- read_stars(fn_grd, proxy=F)
    }
    plot(grd_area)

    grd_minmax <- raster_cut_to_minmax(grd_area, var_name, climatename, fn_grd)
    write_stars(grd_minmax, fn_grd2)
    pp_min_max <- make_minmax_polygon(grd_minmax)
    print(st_dimension(pp_min_max))
    if(!is.na(st_dimension(pp_min_max))){
      print('some area retrieved...')
      write_sf(st_as_sf(pp_min_max), fn_pp)
      print(pp_min_max)
      plot(grd_minmax)
      plot_grd(grd_minmax, area, fn=paste(prefix, 'mask.png', sep="_"))
      plot_grd(pp_min_max, area, fn=paste(prefix, 'shape.png', sep="_"))
    }else{
      print(paste0("Setting ", prefix, " does not retrieve any suitable area in the region."))
      return(NA)
    }
  }else{
    pp_min_max <- read_sf(fn_pp)
    plot_grd(pp_min_max, area, fn=paste(prefix, 'shape.png', sep="_"))
  }
  return(pp_min_max)
}


MakeReducePolygonElevation <- function(area, var_name){
  print('create area_elevation_files')
  prefix <- fn_prefix("elevation")
  
  fn_grd <- paste("../",prefix,  zoom_level,'area_mask.grd', sep="_")
  fn_pp <- paste(prefix,  'shape.shp', sep="_")
  fn_grd2 <- paste(prefix, 'mask.grd', sep="_")
  
  if(!file.exists(fn_grd2)){
    #ele <- get_elevation(area, zoom_level, crs, res)
    ele = ele.main
    if(!file.exists(fn_grd)){
      grd_area <- raster_cut_to_area(ele, area, "elevation")
      write_stars(grd_area, fn_grd, overwrite=TRUE)
    }else{
      grd_area <- read_stars(fn_grd, proxy=F)
    }
    grd_minmax <- raster_cut_to_minmax(grd_area, "elevation")
    plot(grd_minmax)
    pp_min_max <- make_minmax_polygon(grd_minmax)
    write_sf(pp_min_max, fn_pp, overwrite=TRUE)
    write_stars(grd_minmax, fn_grd2, overwrite=TRUE)
  }else{
    grd_minmax <- read_stars(fn_grd2)
    pp_min_max <- read_sf(fn_pp)
  }
  par(mar=c(1,1,1,1))
  plot_grd(grd_minmax, area, fn=paste(prefix, 'mask.png', sep="_"))
  plot_grd(pp_min_max, area, fn=paste(prefix, 'shape.png', sep="_"))
  return(pp_min_max)
}


makeGrowingSeasonPol <- function(var_name, area){
  print('create growing season polygon')
  prefix <- fn_prefix(var_name)
  prefix_new <- paste(strsplit(prefix, "_")[[1]][1], strsplit(prefix, "_")[[1]][2], sep="_")
  
  calc_option = strsplit(var_name, "_")[[1]][5]
  
  fn_grd_f <- paste0(prefix, '_mask.grd')
  fn_pp_f <- paste(prefix, 'shape.shp', sep="_")
  if(!file.exists(fn_grd_f)){
    fn_pp <- paste(prefix, "GSL", '_shape.shp', sep="_")
    fn_grd <- paste(prefix, "GSL", '_mask.grd', sep="_")
    if(!file.exists(fn_grd)){
      print("make growing season length")
      grow_season_length <- as.integer(strsplit(var_name, "_")[[1]][4])
      subset_GS_to_x(var_name, prefix_new, calc_option, "GSL", grow_season_length,  area)
    }
    gs_length <- read_stars(fn_grd, proxy=F)    
    gs_length_pp = read_sf(fn_pp)
    # gs_length_pp = st_make_valid(gs_length_pp)
    # if(st_is_valid(gs_length_pp) == FALSE){
    #   gs_length_pp = st_buffer(gs_length_pp, 0.0)
   # }
    print(st_is_valid(gs_length_pp))
    
  ##############################################################
    
    fn_pp <- paste(prefix, 'GST', '_shape.shp', sep="_")
    fn_grd <- paste(prefix, 'GST', '_mask.grd', sep="_")
    if(!file.exists(fn_grd)){
      print("make growing season temp")
      grow_season_temp <- strsplit(var_name, "_")[[1]][3]
      grow_season_temp <- as.numeric(paste(strsplit(grow_season_temp, "")[[1]][1],
                                           strsplit(grow_season_temp, "")[[1]][2], sep = "."))
      subset_GS_to_x(var_name, prefix_new, calc_option, "GST", grow_season_temp, area)
    }
    gs_temp <- read_stars(fn_grd, proxy=F)      
    gs_temp_pp = read_sf(fn_pp)
    # gs_temp_pp = st_make_valid(gs_temp_pp)

    #########################################################################
    print("make treeline model")
    
    plot(gs_length_pp)
    plot(gs_temp_pp)
    pp_small <- st_intersection(gs_length_pp, gs_temp_pp,  dimension = "polygon")  # 
    pp_small <- st_make_valid(pp_small)
    
    pol_valid = unique(st_is_valid(pp_small))
    if(FALSE %in% pol_valid){
      sf_use_s2(FALSE)
      pp_small =  st_union(pp_small)
      pp_small <- st_make_valid(pp_small)
      sf_use_s2(TRUE)
    }else{
      pp_small =  st_union(pp_small)
      pp_small <- st_make_valid(pp_small)
    }
    gs_length.terra = as(gs_length, 'Raster') #+ terra::rast() 
    gs_temp.terra = as(gs_temp, 'Raster') #+ terra::rast() 
    
    grd_small <- intersect(gs_length.terra, gs_temp.terra)
    plot(grd_small)
    
    print("make inverse")
    # I first calculate the area which is below the treeline, gs length > 94 and temp >6.4,
    # then I calculate the intersect, to see where both var meet, then get alpine by doing inverse
  
    pp.sp = as(pp_small, "Spatial")
    grd.terra = mask(grd_small, pp.sp, inverse=TRUE)
    grd_small = st_as_stars(grd.terra)
    # grd_small = grd_small[!pp_small]
    plot(grd_small)

    grd_small_pp <- make_minmax_polygon(grd_small)

    plot_grd(grd_small, area, paste(prefix, 'mask.png', sep="_"))
    plot_grd(grd_small_pp, area, paste(prefix, 'pol.png', sep="_"))
    
    fn_grd <- paste0(prefix, '_mask.grd')
    fn_pp <- paste0(prefix, '_shape.shp')
    
    grd_small = st_as_stars(grd_small)
    
    write_stars(grd_small, fn_grd, overwrite=TRUE, progress=T)
    write_sf(grd_small_pp, fn_pp, overwrite=TRUE)
    
  }else{
    grd_small_pp <- read_sf(fn_pp_f)
  }
  plot_grd(grd_small_pp, area, paste(prefix, 'pol.png', sep="_"))
  return(grd_small_pp)
}


subset_GS_to_x <- function(var_name, prefix_new, calc_option, type, val, area){
  prefix <- fn_prefix(var_name)
  
  fn_pp <- paste(prefix, type, '_shape.shp', sep="_")
  fn_grd <- paste(prefix, type, '_mask.grd', sep="_")
  fn_all <- paste0("../", prefix_new, "_", calc_option, '_all_', type, '_mask.grd')
  if(!file.exists(fn_all)){
    files_path <- get_path_data(type)
    files_stack <- get_GSarea(files_path, area)
    
    print("calc")
    if(calc_option == "mean"){
      mean_val <-
        files_stack %>%
        st_apply(c("x", "y"), mean, na.rm = TRUE) 
    }else if(calc_option == "median"){
      mean_val <-
        files_stack %>%
        st_apply(c("x", "y"), median, na.rm = TRUE) 
      # mean_val <- st_apply(files_stack, MARGIN=1, FUN=median, na.rm=T)
    }
    # mean_val[mean_val <= -25000] <- NA  # not working? for stars and only there to make it faster in raster package
    print("write stars")
    write_stars(mean_val, fn_all, overwrite=TRUE, progress=T) # turn stars proxy into stars object by writing it out. alternative st_as_stars() - often other operations dont work otherwise
    mean_val <- read_stars(fn_all, proxy = F)
  }else{
    mean_val <- read_stars(fn_all, proxy=F)
  }
  mean_val[mean_val < val] <- NA # to calcualte area below treeline
  
  write_stars(mean_val, "tmp.grd", overwrite=TRUE, progress=T)
  mean_val = read_stars("tmp.grd", proxy=F)
  unlink("tmp.grd")
  unlink("tmp.gri")
  
  gs_length <- mean_val
  gs_length_pp <- make_minmax_polygon(gs_length)

  write_stars(gs_length, fn_grd, overwrite=TRUE)
  gs_length <- read_stars(fn_grd, proxy = F)
  write_sf(gs_length_pp, fn_pp, overwrite=TRUE)

  plot_grd(gs_length, area, paste(prefix, type, '_mask.png', sep="_"))
  plot_grd(gs_length_pp, area, paste(prefix, type, '_pol.png', sep="_"))
}

get_GSarea <- function(files_path, area){
  # load growing season and crop to area
  fn = strsplit(files_path, "/")[[1]]

  fn = fn[length(fn)]
  if(!file.exists(paste0(fn, ".grd"))){
    all_files <- list.files(files_path,
                            full.names = TRUE,
                            pattern = ".tif") #take all the ascii files in    
    files_stack <- read_stars(all_files) #stack them together 
   
    nam <- 1979:2013
    names(files_stack) <- nam
    
    ext <- get_extent_area(area)
    files_stack <- st_crop(files_stack,st_bbox(ext), crop=T)
    
    print("write GS")
    write_stars(files_stack, paste0(fn, ".grd"), overwrite=TRUE, progress=T)
    files_stack = read_stars(paste0(fn, ".grd"), proxy=F)
    
    files_stack[files_stack <= -25000] <- NA
    
    write_stars(files_stack, paste0(fn, ".grd"), overwrite=TRUE, progress=T)
    files_stack = read_stars(paste0(fn, ".grd"), proxy=F)
    
  }else{
    files_stack = read_stars(paste0(fn, ".grd"), proxy=F)
  }
  return(files_stack)
}


plot_grd <- function(grd, area, fn){
  # make plots that are cut to tropical definition
  
  #ext <- get_extent_area(area)
  ext_bb = st_bbox(grd)
  xmin <- ext_bb[1]-5
  xmax <- ext_bb[3]+5
  ymin <- ext_bb[2]-5
  ymax <- ext_bb[4]+5
  
  newmap <- rworldmap::getMap(resolution = "low")
  newmap <- spTransform(newmap, CRSobj=crs("+proj=longlat +datum=WGS84 +no_defs"))
  
  png(fn)
  plot(newmap, xlim = c(xmin, xmax), ylim = c(ymin, ymax), asp = 1, lwd = 0.2,
       axes=TRUE) # Andes
  try(
    plot(grd, col="red", border="red", add=TRUE)
  )
  dev.off()
  
}


raster_cut_to_area <- function(mask, area, var_name){
  # crop raster to area
  print("raster cut to area")
  plot(mask)
  print(area)
  prefix <- fn_prefix(var_name)
  ext <- get_extent_area(area)
  crop_mask <- st_crop(mask, st_bbox(ext))
  fn_png <- paste(strsplit(prefix, "_")[[1]][1], 'mask.png', sep="_")
  plot_grd(crop_mask, area, fn_png)
  return(crop_mask)
}


make_over_view_map <- function(area, climatename, path_to_folder){
  # generate climate overview maps for area
  ext <- get_extent_area(area)
  
  #load climate files
  files <- list.files(path=path_to_folder,  pattern='tif',  full.names=FALSE )
  
  # makes small images for the region with the values of the respective bioclim and saves it to file
  for(layer in files){
    print(layer)
    name <- strsplit(layer, '.tif')[[1]]
    mask <- read_stars(paste0(path_to_folder, layer))
    r2 <- st_crop(mask, st_bbox(ext))
    png(paste(area, name, 'area_map.png', sep="_"))
    plot(r2)
    dev.off()
  }
}


raster_cut_to_minmax <-function(grd_area, var_name, climatename, fn_grd=F){
  print("raster cut to min max")
  if(typeof(var_name) == "character"){
    if(var_name == "elevation"){
      #source(paste0(base, 'ClimateAnalyzer/00_set_variables.R'))
      
      #min_ele <- min_ele
      # TODO check if working
      grd_area[grd_area <= min_ele] <- NA
      mask2 <- grd_area
    }
  }else{
    bioclim <- var_name[1]

    if(climatename %in% c('chelsa_miroc_esm',"mpi-esm", "future_MIROC26", "future_mpi26", "futureV21", "chelsaV21" )){
      if(!file.exists(paste0("./", bioclim, "_cutcontinent.grd"))){
        grd_area <- cut_to_continent(grd_area, area)
        write_stars(grd_area, paste0("./", bioclim, "_cutcontinent.grd"), overwrite=TRUE)
      }else{
        grd_area = read_stars(paste0("./", bioclim, "_cutcontinent.grd"))
      }
    }
    
    ## add fix function
    var_new = fix_min_max(climatename, var_name)
    
    min <- var_new[2]
    max <- var_new[3]
  
    write_stars(grd_area, "tmp.tif")
    grd_area = read_stars("tmp.tif", proxy=F)
    mask2 = grd_area
    mask2[grd_area <= min] <- NA
    mask2[grd_area >= max] <- NA
    write_stars(mask2, "tmp.tif")
    mask2 = read_stars("tmp.tif", proxy=F)
    unlink("tmp.tif")
  }
  return(mask2)
}


fix_min_max <- function(climatename, var_name){
  print("fix min max")
  bioclim <- var_name[1]
  min <- var_name[2]
  max <- var_name[3]
  if(bioclim %in% c(1,2,3,4,5,6,7,8,9,10,11)){    
    min <- min*10
    max <- max*10
  }
  
  # past v1.2 are in kelvin
  if(climatename %in% c('chelsa_miroc_esm',"mpi-esm")){
    print('biclim')
    # check this variables if something is going wrong with the cutting: only sure about 3,4, 10,11
    if(bioclim %in% c(1,2,5,6,7,8,9,10,11)){
      if(min == 0){
        max <- (max/10+273)*10
      }else{
        min <- (min/10+273)*10
        max <- (max/10+273)*10
      }
    }
  }
  
  if(climatename %in% c("future_MIROC26", "future_mpi26", "futureV21")){
    print('change future var')
   
    if(bioclim %in% c(1,5,6,8,9,10,11)){
      print('adapt')
      print(max)
      if(min == 0){
        max <- (max/10)
      }else{
        min <- (min/10)
        max <- (max/10)
        
      }
    }
  
    if(climatename %in% c("futureV21")){
      if(bioclim %in% c(2,3,4,7)){
        print('adapt')
        print(max)
        print(min)
        if(min == 0){
          max <- max/1000
        }else{
          min <- min/1000
          max <- max/1000
        }
      }
    }
  }
  
  
  if(climatename %in% c("chelsaV21")){
    if(bioclim %in% c(1,5,6,8,9,10,11)){
      print('adapt')
      print(max)
      print(min)
      #   pp1/10-273
      if(min == 0){
        max <- (max/10)
      }else{
        min <- (min/10)
        max <- (max/10)
      }
    }
    if(bioclim %in% c(2,3,4,7)){
      print('adapt')
      print(max)
      print(min)
      if(min == 0){
        max <- max/1000
      }else{
        min <- min/1000
        max <- max/1000
      }
    }
  }
  var_new = list(bioclim, min, max)
  return(var_new)
}


cut_to_continent <- function(mask2, area){
  print("crop climate raster to continent")
  source(paste0(base, 'ClimateAnalyzer/00_set_variables.R'))
  
  #mask2 <- projectRaster(mask2, crs=crs, res=res)
  
  if (area == 'SouthAmerica'){
    sa <- rnaturalearth::ne_countries(continent = "South America")
    na <- rnaturalearth::ne_countries(continent = "North America")
    sa <- spTransform(sa,crs)
    na <- spTransform(na,crs)
    x <- bind(sa, na)
  }else if (area %in% c('EA', "Africa")){
    af <- rnaturalearth::ne_countries(continent = "Africa")
    na = rnaturalearth::ne_countries(continent = "Asia")
    af <- sp::spTransform(af,crs)
    #x <- af
    na <- sp::spTransform(na,crs)
    x <- bind(af, na)
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
  mask2.r = as(mask2, 'Raster') 
  mask_3_b <- raster::crop(mask2.r, st_bbox(ext2))
  mask_3_b <- mask(mask_3_b, x)
  plot(mask_3_b)
  writeRaster(mask_3_b,"tmp.grd", overwrite=TRUE)
  mask_3_b = read_stars("tmp.grd")
  unlink("tmp.grd")
  unlink("tmp.gri")
  
  return(mask_3_b)
  
}


make_minmax_polygon <- function(grd_minmax){
  print("make min max polygon")
 
  ### this makes the actual extend we are interested in
  #To get a polygon that surrounds cells that are not NA
  # make all values the same. Either do
  r <- grd_minmax > -Inf
  print("...rasterToPolygons...")
  pp <- sf::st_as_sf(stars::st_as_stars(r), as_points = FALSE, merge = TRUE) # requires the sf, sp, raster and stars packages
  pp <- st_make_valid(pp)

  pol_valid = unique(st_is_valid(pp))
  if(FALSE %in% pol_valid){
    sf_use_s2(FALSE)
    pp =  st_union(pp)
    pp <- st_make_valid(pp)
    pp = st_buffer(pp, 0.0)
    pp <- st_make_valid(pp)
    sf_use_s2(TRUE)
    plot(pp)
  }else{
    pp =  st_union(pp)
    pp <- st_make_valid(pp)
  }
  return(pp)
}


get_elevation <- function(area, zoom_level, crs, res){
  print("get elevation")
  print(area)
  fn <- paste0('elevation_', area, '_zoom', zoom_level, '.grd')
  print(fn)
  ext <- get_extent_area(area, ncol=3600, nrow=1800)
  if(file.exists(file.path(base, 'data/elevation', fn))){
    print('...reloading elevation...')
    ele <- read_stars(file.path(base, 'data/elevation', fn), proxy=F)
    ele <- st_crop(ele, st_bbox(ext))
    ele = st_as_stars(ele)
  }else{
    print('...downloading elevation data...')
    ext2 = st_bbox(ext) %>% sf::st_as_sfc() 
    ext2 = sf::as_Spatial(ext2)
    ele <- elevatr::get_elev_raster(ext2, z=zoom_level)
    print('...writing to file...')
    ele = st_as_stars(ele)
    ele <- st_crop(ele, st_bbox(ext))
    write_stars(ele, file.path(base, 'data/elevation', fn))
    ele = read_stars(file.path(base, 'data/elevation', fn), proxy=F)
  }
  return(ele)
}


recalculate_past_climate <- function(clim1, climatename1){
  if(climatename1 %in% c('chelsa_miroc_esm',"mpi-esm")){
   for(i in c(1, 5, 6, 8:11)){   # 2, 3, 7 excluded bc range, 4 excluded bc standard deviation
      clim1[, i] = (clim1[, i]/10-273)*10
   }
    for(i in c(12:19)){  
      #for(i in 1:11){
      
      clim1[, i] = (clim1[, i]/10)
    }
  }
  return(clim1)
}
