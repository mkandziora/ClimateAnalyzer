# growing season 
# download data:
# for i in {1979..2013}
# do
# echo $i
# fn="https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/exchelsa/gst/CHELSA_gsl_"

# fn="${fn}$i"
# fn="${fn}_V1.2.1.sdat.tif "
# echo "${fn}"
# wget ${fn}
# done
############################################################################

makeGrowingSeasonPol <- function(var_name){

  prefix <- fn_prefix(var_name)
  fn_pp <- paste(prefix, 'shape.shp', sep="_")
  if(!file.exists(fn_pp)){
    print('create growing season polygon')
    print(var_name)

    files_path <- "/home/rstudio/tropAlpR_2/growing_season/gs_length" #path where my files are    
    all_files <- list.files(files_path,
                            full.names = TRUE,
                            pattern = ".tif") #take all the ascii files in    
    files_stack <- stack(all_files) #stack them together 
  
    
    ext <- get_extent_area(area)
    files_stack <- crop(files_stack, ext)
    mean_val <- calc(files_stack, fun = mean)
    mean_val[mean_val <= 94] <- NA
    #mean_val[mean_val >= 250] <- NA
    
    plot(mean_val)
    gs_length <- mean_val
    gs_length_pp <- make_minmax_polygon(gs_length)
    plot(gs_length_pp)
    
    files_path <- "/home/rstudio/tropAlpR_2/growing_season/gs_temp" #path where my files are    
    all_files <- list.files(files_path,
                            full.names = TRUE,
                            pattern = ".tif") #take all the ascii files in    
    files_stack <- stack(all_files) #stack them together 
    nam <- c(1979:2013)
    
    names(files_stack) <- nam
    
    ext <- get_extent_area(area)
    files_stack <- crop(files_stack, ext)
    mean_val <- calc(files_stack, fun = mean)
    mean_val[mean_val >= 5.4] <- NA
    plot(mean_val)
    gs_temp <- mean_val
    gs_temp_pp <- make_minmax_polygon(mean_val)
    pp_small <- intersect(gs_length_pp, gs_temp_pp)
    plot(pp_small)
      
    grd_small <- intersect(gs_length, gs_temp)
    
      
      
      
    plot_grd(grd_small, area, paste(elem_comb, 'mask.png', sep="_"))
    plot_grd(pp_small, area, paste(elem_comb, 'pol.png', sep="_"))
    
    fn_grd <- paste0(elem_comb, '_mask.grd')
    fn_pp <- paste0(elem_comb, '_shape.shp')
    
    writeRaster(grd_small, fn_grd, overwrite=TRUE)
    shapefile(pp_small, fn_pp, overwrite=TRUE)
    
  }else{
    pp_small <- shapefile(paste(prefix, "shape.shp", sep="_"))
  }
  #plot_grd(grd_small, area, paste(prefix, 'mask.png', sep="_"))
  plot_grd(pp_small, area, paste(prefix, 'pol.png', sep="_"))
  return(pp_small)
}
###########################################################################
# 
# area = "SouthAmerica"
# 
# setwd(main_wd)
# setwd(area)
# dir.create("growing_season")
# setwd("growing_season")
# 
# files_path <- "/home/rstudio/tropAlpR_2/growing_season/gs_length" #path where my files are    
# all_files <- list.files(files_path,
#                         full.names = TRUE,
#                         pattern = ".tif") #take all the ascii files in    
# files_stack <- stack(all_files) #stack them together 
# nam = c(1979:2013)
# 
# names(files_stack) = nam
# 
# ext = get_extent_area(area)
# files_stack = crop(files_stack, ext)
# mean_val <- calc(files_stack, fun = mean)
# mean_val[mean_val <= 94] <- NA
# #mean_val[mean_val >= 250] <- NA
# 
# plot(mean_val)
# gs_length = mean_val
# gs_length_pp = make_minmax_polygon(gs_length)
# plot(gs_length_pp)
# 
# files_path <- "/home/rstudio/tropAlpR_2/growing_season/gs_temp" #path where my files are    
# all_files <- list.files(files_path,
#                         full.names = TRUE,
#                         pattern = ".tif") #take all the ascii files in    
# files_stack <- stack(all_files) #stack them together 
# nam = c(1979:2013)
# 
# names(files_stack) = nam
# 
# ext = get_extent_area(area)
# files_stack = crop(files_stack, ext)
# mean_val <- calc(files_stack, fun = mean)
# mean_val[mean_val >= 5.4] <- NA
# plot(mean_val)
# gs_temp = mean_val
# gs_temp_pp = make_minmax_polygon(mean_val)
# pp_small = intersect(gs_length_pp, gs_temp_pp)
# plot(pp_small)
# 
# #############################################################################################################################
# 
# means = stackApply(files_stack, indices=names(files_stack), fun=mean)
# means
# plot(means)
# 
# 
# files_brick <- brick(all_files) #stack them together 
# files_brick = crop(files_brick, ext)
# 
# # Calculate mean
# r_mean <- calc(files_brick, mean)
# # Calculate median
# r_median <- calc(r_brick, median)
# # Calculate sd
# r_sd <- calc(r_brick, sd)
# 
# 
# plot(files_stack$X1979)
# plot(files_stack$X1980)
# 
# plot(mean(files_stack$X1979,files_stack$X1980))
# 
# 
# u = st_apply(X = files_stack, MARGIN = 1:2, FUN = function(x) mean(x))
# 
# x = read_stars(all_files)
# 
# ext = get_extent_area(area)
# x = st_crop(x, ext)
# 
# t = st_as_stars(x)
# 
# 
# s = st_apply(t, c("x", "y"), mean)
# s = st_apply(x, 1:2, mean, na.rm = TRUE)
# plot(s, breaks = "equal")
# 
# plot(st_as_sf(st_apply(x, 1, mean, na.rm = TRUE)), reset = FALSE, pch = 16,
#      ylim = st_bbox(ext)[c(2,4)])
# plot(ext, add=TRUE)
# 
# #####################################################
# z = read_stars(all_files, quiet = TRUE)
# z
# 
# z2 = z["CHELSA_gsl_2013_V1.2.1.sdat.tif"]
# z3 = z["CHELSA_gsl_2012_V1.2.1.sdat.tif"]
# zz = z2 + z3
# 
# bb = st_bbox(c(xmin = ext@extent@xmin, ymin = ext@extent@ymin, xmax = ext@extent@xmax, ymax = ext@extent@ymax), crs=st_crs(z))
# ysub = z2[bb]
# st_dimensions(ysub)
# ##      from  to                   offset  delta  refsys point values x/y
# ## x      41 281                        0   0.25      NA    NA   NULL [x]
# ## y      80 360                       90  -0.25      NA    NA   NULL [y]
# ## zlev    1   1                    0 [m]     NA      NA    NA   NULL    
# ## time    1   9 1981-09-01 02:00:00 CEST 1 days POSIXct    NA   NULL
# class(ysub) # still no data here!!
# ## [1] "stars_proxy" "stars"
# plot(ysub, reset = FALSE) # plot reads the data, at resolution that is relevant
# plot(st_as_sfc(bb), add = TRUE, lwd = .5, border = 'red')
# 
# plot(st_apply(z, c("x", "y"), mean))
# 
# zz = read_stars(all_files, quiet = TRUE)
# plot(st_apply(zz, c("x", "y"), mean))
# 
# 
# min2 = function(x) if(all(is.na(x))) NA else min(x, na.rm = TRUE)
# max2 = function(x) if(all(is.na(x))) NA else max(x, na.rm = TRUE)
# s_min = st_apply(zz, 1:2, min2)
# s_max = st_apply(zz, 1:2, max2)
# 
# z %>% filter(z > -10000 ) -> x7
# plot(x7, breaks = "equal", col = hcl.colors(11, "Spectral"), key.pos = 4)
# 
# plot(s_min, breaks = "equal", col = hcl.colors(11, "Spectral"), key.pos = 4)
# plot(s_max, breaks = "equal", col = hcl.colors(11, "Spectral"), key.pos = 4)
# 
# (a = aggregate(z, st_as_sf(DE_NUTS1), mean, na.rm = TRUE))
# pol <- z %>% st_bbox() %>% st_as_sfc() %>% st_centroid() %>% st_buffer(300)
# x <- x[,,,1]
# plot(x[pol])
# 
# #################################################3
# gimms = files_stack
# 
# fun <- function(x) { 
#   gimms.ts = ts(x, start=c("X1979",1), end=c("X2013", 35), frequency=35)
#   x <- aggregate(gimms.ts) 
#   return(x)
# }
# gimms.sum <- calc(gimms, fun)
# gimms.sum=gimms.sum/24
# plot(gimms.sum)
# 
# time <- 1:nlayers(gimms.sum) 
# fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }}
# gimms.slope=calc(gimms.sum, fun)
# gimms.slope=gimms.slope*25
# plot(gimms.slope)
# 
# fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[8] }}
# p <- calc(gimms.sum, fun=fun)
# plot(p, main="p-Value")