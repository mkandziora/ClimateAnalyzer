####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

compare_climate <- function(clim1, clim2, fn){
  print(clim1)
  minmax1 <- get_min_max_data(clim1, "val1")
  minmax2 <- get_min_max_data(clim2, "val2")
  
  merged_data <- merge(minmax1, minmax2, merge="names")
  
  p1 <-   ggplot(merged_data) + 
    geom_point(aes(x=names, y=val1_min_val),colour="red") + 
    geom_point(aes(x=names, y=val2_min_val),colour="red", shape = 6) + 
    geom_point(aes(x=names,y=val1_max_val),colour="blue") +
    geom_point(aes(x=names,y=val2_max_val),colour="blue", shape = 6) +
    ylab("Y") +
    theme_bw()
  
  plot(p1)
  fn2 <- paste(fn, "min_max_value_climate.png", sep="_")
  ggsave(file=fn2)
  
  make_minmax_diff_climate(merged_data, fn) 
}


make_minmax_diff_climate <- function(merged_data, fn) {
  
  merged_data$mindiff <- (merged_data$val1_min_val - merged_data$val2_min_val)
  merged_data$maxdiff <- merged_data$val1_max_val - merged_data$val2_max_val
  
  p1 <- ggplot(merged_data) + 
    geom_point(aes(x=names, y=mindiff),colour="red") + 
    geom_point(aes(x=names,y=maxdiff),colour="blue") #+ ylim(-200,200)

  fn2 <- paste(fn, "min_max_difference_climate.png", sep="_")
  ggsave(file=fn2)
  
}



compare_clim_acrossallcontinents <- function(var_name, arealist, climatename){
  source(paste0(base, 'ClimateAnalyzer/makeClimateData.R'))
  
  prefix <- fn_prefix(var_name)
  print(prefix)
  fn <- paste(prefix, "presvals_all.rds", sep = "_")
  
  i=0
  merged_data = data.frame(matrix(nrow=0, ncol=4))
  names(merged_data) = c("min", "max", "bioclim", "area")
  for(area in arealist){
    i=i+1
    relpath1 <- paste("Results", area, climatename, prefix, fn, sep="/")
    pol_clim1 <- load_climate_table(relpath1)
    pol_clim1 =  pol_clim1 %>% st_drop_geometry()
    poly_climate_combined_1 <- get_min_max_data(pol_clim1, paste0("val", i))
    poly_climate_combined_1$area <- area
    names(poly_climate_combined_1) = names(merged_data)
   
    merged_data <- rbind(merged_data, poly_climate_combined_1)
  }

  p=ggplot(merged_data, aes(x=factor(bioclim), y=min, col=area)) +
    geom_point() + 
   scale_y_continuous(limits = c(min(merged_data$min), max(merged_data$max)))
  p2=ggplot(merged_data, aes(x=factor(bioclim), y=max, col=area)) +
    geom_point()+ 
   scale_y_continuous(limits = c(min(merged_data$min), max(merged_data$max)))
  ggarrange(p, p2,
            labels = c("A", "B"),
            ncol = 2, nrow = 1)
  
  fn <- paste(prefix, "min-max_all_climate.png", sep="_")
  ggsave(file=fn)
 
}
