compare_climate <- function(clim1, clim2, fn){
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


compare_clim_acrosscontinents <- function(var_name, area1, area2, climatename){
  source(paste0(base, 'ClimateAnalyzer/makeClimateData.R'))
  
  prefix <- fn_prefix(var_name)
  
  fn <- paste(prefix, "sdm_all.data", sep = "_")
  relpath1 <- paste(area1, climatename, var_name, fn, sep="/")
  relpath2 <- paste(area2, climatename, var_name, fn, sep="/")
  
  pol_clim1 <- load_climate_table(relpath1)
  pol_clim1 <- pol_clim1[, -1]
  poly_climate_combined_1 <- get_min_max_data(pol_clim1, "val1")
  
  pol_clim2 <- load_climate_table(relpath2)
  pol_clim2 <- pol_clim2[, -1]
  
  poly_climate_combined_2 <- get_min_max_data(pol_clim2, "val2")
  
  merged_data <- merge(poly_climate_combined_1, poly_climate_combined_2, merge=c("area", "name"))
  write.csv(merged_data, paste(var_name, area1, area2, "min_max.csv", sep="_"))
  
  poly_climate_combined_1$area <- area1
  poly_climate_combined_2$area <- area2
  
  names(poly_climate_combined_2) <- names(poly_climate_combined_1)
  
  
  p1 <-   ggplot() + 
    geom_point(data = poly_climate_combined_1,  aes(x=names, y=val1_min_val,  color = "light green")) + 
    geom_point(data = poly_climate_combined_2,  aes(x=names, y=val1_min_val,  color = "light blue")) + 
    geom_point(data = poly_climate_combined_1,  aes(x=names, y=val1_max_val,  color = "dark green")) + 
    geom_point(data = poly_climate_combined_2,  aes(x=names, y=val1_max_val,   color = "dark blue")) +
    
    ylab("Max/Min Climatic value") +  theme(legend.position="right") + 
    scale_colour_manual(name = 'Climate variables',  values = c("light green" = "light green", "light blue" = "light blue", "dark green" = "dark green", "dark blue" = "dark blue"),
                        labels = c('maximum area1',  'maximum area2', 'minimum area1', 'minimum area2' )) 
  
  
  #  theme_bw()
  plot(p1)
  fn <- paste(var_name, area1, area2, "min_max_value_climate.png", sep="_")
  ggsave(file=fn)
  
  
  merged_data$mindiff <- (merged_data$val1_min_val - merged_data$val2_min_val)
  merged_data$maxdiff <- merged_data$val1_max_val - merged_data$val2_max_val
  
  # png(paste(paste(elem_comb1, elem_comb2, sep='_'), "min_max_difference_climate.png", sep="_"))
  p1 <- ggplot(merged_data, aes(names, mindiff)) + 
    geom_point(aes(x=names, y=mindiff, colour="black")) + 
    geom_point(aes(x=names,y=maxdiff,colour="red")) +  xlab("Bioclimatic Variables") + ylab("Difference between areas") + 
    scale_colour_manual(name = 'Climate variables', 
                        values =c('black'='black','red'='red'), labels = c('minimum','maximum'))
  plot(p1)
  fn <- paste(area1, area2, "min_max_difference_climate.png", sep="_")
  ggsave(plot=p1, file=fn)

}