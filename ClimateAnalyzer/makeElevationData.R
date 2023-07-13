####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################


rasterize_polygon <- function(var_name, pp, ele, fn){
  print("rasterize polygon")
  # get elevation and crop it to pre-defined extent
  prefix <- fn_prefix(var_name)
  
  ele.c=st_crop(ele,st_bbox(pp))

  pp =  st_transform(pp, crs=st_crs(ele.c))

  plot(pp)
  if(!file.exists(fn)){
    print("...sample points...")
    pts = st_sample(pp, 2500)
    
    plot(pp)
    plot(pts, add=T, col='green')
    print("...extract points...")
    
    plot_elevations = st_extract(ele.c, pts)
    saveRDS(plot_elevations, fn)
  }else{
    plot_elevations = readRDS(fn)
  }
  hit.ele =  plot_elevations %>% st_drop_geometry()
  hit.ele = as.vector(hit.ele)[[1]]
  hit.ele = hit.ele[hit.ele >= 0 ]
  return(hit.ele)
}

get_elevation_profile <- function(var_name, pp, ele.o){
  prefix <- fn_prefix(var_name)

  print('get_elevation_profile')
  fn = paste0(prefix, "elevation.RDA")
  ele <- rasterize_polygon(var_name, pp, ele.o, fn)

  png(paste(prefix, "hist_elevation.png", sep="_"))
  hist(ele,
      breaks = seq(0, 8000, by=200),
      main="Histogram of elevation",
      col="wheat3",  # changes bin color
      xlab= "Elevation (m)")  # label the x-axis
  dev.off()
  
  
  png(paste(prefix, "map_elevation.png", sep="_"))
  plot(ele, maxpixels= 1000000,
       breaks = c(0, 2000, 3200, 3800, 4500, 8000), 
       col = terrain.colors(5),
       main="Elevation")
  dev.off()
  
}


