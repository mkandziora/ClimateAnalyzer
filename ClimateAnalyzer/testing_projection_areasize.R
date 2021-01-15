var_list <- list(name_of_var10alp, name_of_var11alp)


prefix <- fn_prefix(name_of_var10alp)


pp <- shapefile(paste(prefix, "shape.shp", sep="_"))
ext <- get_extent_area(area, ncol=6600, nrow=2800, crs=NA)

t <- extract(ext, pp, weights = TRUE)
plot(t)


 # crop mask but do not loose outer pixels that have not the center of the pixel inside the polygon
 cls <- cellFromPolygon(ext, pp, weights = TRUE)[[1]][, "cell"]
 ext[][-cls] <- NA

ext <- trim(ext)
#
plot(ext)
plot(pp, add = TRUE)



mask <- raster(paste("", prefix, "area_mask.grd", sep="_"))
plot(mask)

get_area_size2(pp)


area(mask)
pp <- rasterToPolygons(mask, na.rm=TRUE, dissolve=TRUE)
gArea(pp[1, ])
get_area_size2(pp)
sum(get_area_size(pp))

pp2 <- shapefile(paste(prefix, "shape.shp", sep="_"))
gArea(pp2[1, ])
get_area_size2(pp2)
get_area_size(pp2)

area(pp, na.rm = TRUE);  cellStats(mask, 'sum')
area(pp, na.rm = TRUE)

require(geosphere)
polys <- rasterToPolygons(pp)
polys_sub <- subset(pp)
sum(areaPolygon(polys_sub))/1E6  #in km2

sum(areaPolygon(polys))/1E6  #in km2


###########################################

#get good projection
lonlat2UTM <- function(lonlat) {
  utm <- (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

epsg_utm_auk <- lonlat2UTM(c(174.7, -36.9))
epsg_utm_lnd <- lonlat2UTM(st_coordinates(london))
st_crs(epsg_utm_auk)$proj4string
#> [1] "+proj=utm +zone=60 +south +datum=WGS84 +units=m +no_defs"
st_crs(epsg_utm_lnd)$proj4string
#> [1] "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs"
