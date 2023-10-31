####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

comparePolygons <- function(pp1, pp2, area, fn, tropical=FALSE){
     par(mar=c(1,1,1,1))
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
       pp1 <- st_crop(pp1, st_bbox(ext), crop=T)
       pp1 <- st_transform(pp1, CRSobj=st_crs(proj))
       pp1 = st_make_valid(pp1)
       
       pp2 <- st_crop(pp2, st_bbox(ext))
       pp2 <- st_transform(pp2, CRSobj=st_crs(proj))
       pp2 = st_make_valid(pp2)
       
     }else{
       proj = "+proj=longlat +datum=WGS84 +no_defs"
     }
     
     ##############
     # calc overlap and difference
     print("make elevation raster")
     ele <- get_elevation(area, zoom_level, crs, res)
     #ele = ele.main
     if(st_crs(ele) != st_crs(proj)){
       ele <- st_transform(ele, crs=crs(proj))
     }
     ele = st_crop(ele, st_bbox(pp2))
     ele = st_as_stars(ele)
     ele.r = terra::rast(as(ele, "Raster"))
     
     #####################################################################
     raster1 <- load_raster(pp1, ele)
     
     pol_list = list(raster1)
     print("calc differences and overlap")
     diffp1 = calc_diff_pp(pp1, pp2)
     if(length(diffp1) > 0 ){
       diff1 = terra::rasterize(st_as_sf(diffp1), ele.r)
       diff1 = st_as_stars(diff1)
       pol_list = append(pol_list , diffp1)
     }
     
     diffp2 = calc_diff_pp(pp2, pp1)
     if(length(diffp2) > 0 ){
       diff2 = terra::rasterize(st_as_sf(diffp2), ele.r)
       diff2 = st_as_stars(diff2)
       pol_list = append(pol_list, diffp2)
     }
     
     pp.intersect = st_intersection(pp1, pp2, dimension = "polygon")
     pp.intersect = st_make_valid(pp.intersect)
     pp.intersect = st_union(pp.intersect)
     pp.intersect = st_make_valid(pp.intersect)
     
     areasizes_text <- calculate_area_of_diff_insersect(diffp1, diffp2, pp.intersect)
     if(tropical == TRUE){
       areasizes_text = paste0("tropical_", areasizes_text)
     }
     
     ext_bb = get_largest_extent(pol_list)
     print(ext_bb)
     xmin <- ext_bb[1]-5
     xmax <- ext_bb[3]+5
     ymin <- ext_bb[2]-5
     ymax <- ext_bb[4]+5
     #ext_bb = st_bbox(ele)
     
  
   ext <- raster(ncol=18000, nrow=9000, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, crs=crs)
   afr2 = as(diff1, "Raster")
   spdf <- as(afr2, "SpatialPixelsDataFrame")
   mydf <- as.data.frame(spdf)
   colnames(mydf) <- c("value", "x", "y")
   wm=ne_countries(scale = 50, returnclass = "sf") 


   p1 =   ggplot() +
     
     geom_sf(data = wm, colour="black", fill=NA, lwd=0.11) + # , alpha=1
     coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
     ggspatial::annotation_scale(location = "br")+
     scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
     scale_y_continuous(limits = c( ymin, ymax), expand = c(0, 0))+
     theme(panel.background = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_blank())
p1 = p1 + 
     geom_stars(
       data = raster1,
       na.action=na.omit,
       aes(x = x, y = y), fill="#E69F00")#+
   if(length(diffp2)>0){
     p1 = p1 + geom_stars(data = diff2,  na.action=na.omit, fill="#0072B2") +  #blue
       geom_sf(data = sf::st_as_sf(diffp2),
               col = "#0072B2", 
               fill="#0072B2", lwd=0.051
            #    linewidth = 0.2
       )
   }


   if(length(diffp1)>0){
     p1 = p1 + geom_stars(data = diff1,  na.action=na.omit, fill="#990000")+ # red
       geom_sf(data = sf::st_as_sf(diffp1),
               col = "#990000", 
               fill="#990000",lwd=0.051
             #  linewidth = 0.2
       )
   }
 
   p1
   ggsave(fn,plot=p1,  dpi=600)
   
}


get_largest_extent <- function(varlist){
  ymin=180
  xmin=180
  ymax=-180
  xmax=-180
  for(element in varlist){
    bb = st_bbox(element)
    print(bb)
    if(bb[1]< xmin){xmin = bb[1]}
    if(bb[2]< ymin){ymin = bb[2]}
    
    if(bb[3]> xmax){xmax = bb[3]}
    if(bb[4]> ymax){ymax = bb[4]}
  }
  
  new_bb = st_bbox(c(xmin = as.integer(xmin), xmax = as.integer(xmax), ymax = as.integer(ymax), ymin = as.integer(ymin)), crs = st_crs(crs))
  return(new_bb)
}


calc_diff_pp <- function(pp1, pp2){
  #diffp <- st_difference(st_as_sf(pp1), st_as_sf(pp2), dimension = "polygon")
  diffp <- st_difference(pp1, pp2, dimension = "polygon")
  
  diffp = st_make_valid(diffp)
  diffp = st_union(diffp)
  diffp = st_make_valid(diffp)
  diffp = st_collection_extract(diffp, type = "POLYGON")
  return(diffp)
}


calculate_area_of_diff_insersect <- function(diffp1, diffp2, pp.intersect){

  areasize_overlap <- get_area_size2(pp.intersect)
  areasize2 <- ""

  if(class(diffp1)[1] %in% c(  "sfc_MULTIPOLYGON", "sfc", "sf", "sfc_GEOMETRYCOLLECTION")){
    areasize2 <- get_area_size2(diffp1)
  }
  areasize1 <- ""
  if(class(diffp2)[1] %in% c(  "sfc_MULTIPOLYGON", "sfc", "sf", "sfc_GEOMETRYCOLLECTION")){
      areasize1 <- get_area_size2(diffp2)
  }
  area_legend <- paste("Overlap area", areasize_overlap, "\n diff1-2",
                       areasize2, "\n diff2-1", areasize1)
  print(area_legend)
  return(area_legend)
}


load_raster <- function(pp, ext){
  ext = terra::rast(as(ext, "Raster"))
  raster_1 = terra::rasterize(st_as_sf(pp), ext)
  raster_1 = st_as_stars(raster_1)
  #raster_1 <- st_rasterize(pp, ext, mask=TRUE, small=TRUE) # getCover=TRUE
  #plot(raster_1)
  return(raster_1)
}


get_area_size2 <- function(pp){
  area.calc = sum(st_area(pp)) #/ 1000000
  area.calc = units::set_units(area.calc, km^2)
  print(paste("Area (raster):", round(area.calc, digits=1),"km2; projection:", crs(pp)))
  return(area.calc)
}


get_area_size2.sameproj<- function(pp){
  proj <- "+proj=cea +lon_0=0 +lat_0=0 +datum=WGS84 +units=m +no_defs"
  
  pp = st_transform(pp, proj)
  area.calc = sum(st_area(pp)) #/ 1000000
  area.calc = units::set_units(area.calc, km^2)
  
  print(paste("Area:", round(area.calc, digits=1),"km2; projection:", crs(pp)))
  return(area.calc)
}

