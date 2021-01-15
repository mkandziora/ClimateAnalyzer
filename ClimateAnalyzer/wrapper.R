
fn_prefix <- function(var_name){
  # provide name for each setting to use in files
  if(typeof(var_name) == "character"){
    if(var_name == "elevation"){
      prefix <- paste("elevation", min_ele, sep="_")
    }else if(var_name %in% c("testolin_alpine_cluster", "GMBA")){
      prefix <- paste(var_name, sep="_")
    }else if(grepl("growing_season", var_name, fixed = TRUE)){
      prefix <- paste(var_name, sep="_")
    }
  }else if(typeof(var_name) == "list"){
    prefix <- 'comb'
    for(element in var_name){
      print(element)
      bioclim <- element[1]
      min <- element[2]
      max <- element[3]
      prefix <- paste0(prefix, bioclim, '-', min, '-', max, "_")
    }
  }else{
    bioclim <- var_name[1]
    min <- var_name[2]
    max <- var_name[3]
    prefix <- paste(bioclim, min, max, sep="_")
  }
  return(prefix)
}


load_shp <- function(var_comb){
  prefix <- fn_prefix(var_comb)
  pp <- shapefile(paste(prefix, 'shape.shp', sep="_"))
  
  return(pp)
}

load_grd <- function(var_comb){
  prefix <- fn_prefix(var_comb)
  grd <- raster(paste(prefix, 'mask.grd', sep="_"))
  plot(grd)
  return(grd)
}

wrapperInput <- function(area, var_name,  climatename){
  source(paste0(base, 'ClimateAnalyzer/make_polygon_raster.R'))
  source(paste0(base, 'ClimateAnalyzer/makeClimateData.R'))
  source(paste0(base, 'ClimateAnalyzer/makeElevationData.R'))
  source(paste0(base, 'ClimateAnalyzer/set_variables.R'))
  source(paste0(base, '10_get_predefined_polygon.R'))
  

  setwd(main_wd)
  
  dir.create(file.path(area), showWarnings = FALSE)
  setwd(area)
  dir.create(file.path(climatename), showWarnings = FALSE)
  setwd(climatename)
  
  prefix <- fn_prefix(var_name)
  dir.create(file.path(prefix), showWarnings = FALSE)
  setwd(file.path(prefix))
  
  pp <- make_pol(var_name, climatename, area)
 
  plot(pp)
  print('analyze elevation')
  plot_elev_treshold_locality(area, var_name, pp, elevation_threshold)
  get_elevation_profile(var_name, pp)
  get_area_size(pp)
  
  print("analyze climate")
  generate_climate_data_polygon(var_name, climatename)
  generate_climate_data_polygon(var_name, climatename, area, tropical = TRUE)
  
  glob <- make_pca(var_name, climatename)
  make_uncorrelated_pca(var_name, glob)
}

make_pol <- function(var_name, climatename, area){
  if(typeof(var_name) == "character"){
    prefix <- fn_prefix(var_name)
    
    setwd(main_wd)
    setwd(area)
    setwd(climatename)
    setwd(prefix)
    if(var_name == "elevation"){
      pp <- MakeReducePolygonElevation(area, var_name)
    }else if(var_name %in% c("testolin_alpine_cluster", "GMBA")){
      pp <- translate_predefined(var_name, area)
    }else if(grepl("growing_season", var_name, fixed = TRUE)){
      pp <- makeGrowingSeasonPol(var_name, area)
    }
  }else if(typeof(var_name) == "list"){
    for(element in var_name){
      #print(element)
      
      prefix <- fn_prefix(element)
      setwd(main_wd)
      setwd(area)
      setwd(climatename)
      dir.create(file.path(prefix), showWarnings = FALSE)
      setwd(prefix)
      
      pp_sub <- MakeReducePolygonClim(area, element, climatename)
    }
    prefix <- fn_prefix(var_name)
    
    setwd(main_wd)
    setwd(area)
    setwd(climatename)
    setwd(prefix)
    pp <- MakeCombinePolygons(area, var_name)
    
  }else{
    
    pp <- MakeReducePolygonClim(area, var_name, climatename)
    
  }
  return(pp)
}


compare_extent <-function(var_name1, var_name2, area1, climatename1, 
                          area2=NA, climatename2=NA){
  single_area <- FALSE
  if(is.na(climatename2)){
    climatename2 <- climatename1
  }
  if(is.na(area2)){
    area2 <- area1
    single_area <- TRUE
    
  }
  print("load 1")
  prefix1 <- fn_prefix(var_name1)
  setwd(main_wd)
  setwd(area1)
  setwd(climatename1)
  print(prefix1)
  setwd(prefix1)
  pol1 <- make_pol(var_name1, climatename1, area1)
  
  plot(pol1)
  clim1 <- generate_climate_data_polygon(var_name1, climatename1, area1, tropical=TRUE)
  
  print("load 2")
  prefix2 <- fn_prefix(var_name2)
  setwd(main_wd)
  setwd(area2)
  setwd(climatename2)
  setwd(prefix2)
  pol2 <- make_pol(var_name2, climatename2, area2)
  plot(pol2)
  clim2 <- generate_climate_data_polygon(var_name2, climatename2, area2, tropical=TRUE)
  
  setwd(main_wd)
  
  if(single_area == TRUE){
    print("compare single area")
    
    setwd(area1)
    fn <- paste(area1, prefix1, prefix2, climatename1, climatename2 , "overlap_area.png", sep="_")
    comparePolygons(pol1, pol2, area1, fn, tropical=FALSE)
  }
 
  print("compare climate")
  fn <- paste(area1, area2, prefix1, climatename1, prefix2, climatename2, sep="_")
  compare_climate(clim1, clim2, fn)

}


load_climate_data <- function(var_name1, climatename1, area1, tropical){
  print("load climate data")
  prefix1 <- fn_prefix(var_name1)
  setwd(main_wd)
  setwd(area1)
  setwd(climatename1)
  setwd(prefix1)
  clim1 <- generate_climate_data_polygon(var_name1, climatename1, area1, tropical=tropical)
  setwd(main_wd)
  return(clim1[, 2:20])
}


boxplot_all <- function(var_name1, var_name2, var_name3, var_name4, area1, area2, area3, area4, climatename, tropical=FALSE){
  clim1 <- load_climate_data(var_name1, climatename, area1, tropical=tropical)
  clim2 <- load_climate_data(var_name2, climatename, area2, tropical=tropical)
  clim3 <- load_climate_data(var_name3, climatename, area3, tropical=tropical)
  clim4 <- load_climate_data(var_name4, climatename, area4, tropical=tropical)
  


  d <- rbind(cbind(stack(clim1), group=paste(toString(var_name1), area1)), 
             cbind(stack(clim2), group=paste(toString(var_name2), area2)),
             cbind(stack(clim3), group=paste(toString(var_name3), area3)),
             cbind(stack(clim4), group=paste(toString(var_name4), area4)))

  
  p <- ggplot(d, aes(group, values)) +
    geom_boxplot(aes(color=group)) +
    facet_wrap(~ind, scales='free_y') #+
    #scale_color_manual(values = c("#00AFBB", "#E7B800"))
 # p + stat_compare_means(method = "t.test")
  # Display the significance level instead of the p-value
  # Adjust label position
 # p + compare_means(
#    aes(label = ..p.signif..), label.x = 1.5, label.y = 40
 # )
  p 
  ggsave(paste("boxplot_climate_all", climatename, ".png", sep="_"), p)
}


boxplot_all_single <- function(var_name1, var_name2, var_name3, var_name4, area1, area2, area3, area4, climatename, tropical=FALSE){
  glob1 <- load_glob(var_name1, area1, climatename)
  glob2 <- load_glob(var_name2, area2, climatename)
  glob3 <- load_glob(var_name3, area3, climatename)
  glob4 <- load_glob(var_name4, area4, climatename)
  cor_var1 <- get_cor_var(glob1)
  cor_var1
  cor_var2 <- get_cor_var(glob2)
  cor_var2
  cor_var3 <- get_cor_var(glob3)
  cor_var3
  cor_var4 <- get_cor_var(glob4)
  cor_var4
  select_var <- c(3, 15, 19, 18, 12)
  
  sub_clim1 <- clim1[, select_var]
  sub_clim2 <- clim2[, select_var]
  sub_clim3 <- clim3[, select_var]
  sub_clim4 <- clim4[, select_var]
  sub_clim1$group <- area1
  sub_clim2$group <- area2
  sub_clim3$group <- area3
  sub_clim4$group <- area4
  
  # for t-test we need same sameple size
  random_sub1 <- sub_clim1[sample(nrow(sub_clim1), 7000), ]
  random_sub2 <- sub_clim2[sample(nrow(sub_clim2), 7000), ]
  random_sub3 <- sub_clim3[sample(nrow(sub_clim3), 7000), ]
  random_sub4 <- sub_clim4[sample(nrow(sub_clim4), 7000), ]
  
  esub<- rbind(random_sub1, random_sub2, random_sub3, random_sub4)
  # esub$'3' = as.factor(esub$'3')
  # esub$'15' = as.factor(esub$'15')
  # esub$'18' = as.factor(esub$'18')
  # esub$'19' = as.factor(esub$'19')
  # esub$'12' = as.factor(esub$'12')
  # esub$'group' = as.factor(esub$'group')
  
  names(esub) <- c(select_var, "group")
  # model <- lm(cbind(esub$'3', esub$'15') ~ esub$group, data=esub)
  # model <- lm((esub$'3') ~ esub$group, data=esub)
  # 
  # Manova(model, test.statistic = "Pillai")
  for(bioclim in select_var){
    df1 <- sub_clim1[, c(bioclim, 6)]
    df2 <- sub_clim2[, c(bioclim, 6)]
    df3 <- sub_clim3[, c(bioclim, 6)]
    df4 <- sub_clim4[, c(bioclim, 6)]
    
    
    df <- rbind(df1, df2, df3, df4)
    my_comparisons <- list( c("Africa", "Asia"), c("Africa", "Hawaii"), c("Africa", "SouthAmerica"), c("Asia", "Hawaii"), c("Asia", "SouthAmerica"), c("Hawaii", "SouthAmerica") )
    p <- ggplot(df, aes(df[,2], df[,1])) +
      geom_boxplot(aes(color=group)) 
    p <- p + stat_compare_means(comparisons = my_comparisons)
    p
    ggsave(paste("boxplot_climate_all",bioclim, climatename, ".png", sep="_"), p)
  }
  
  clim1 <- load_climate_data(var_name1, climatename, area1, tropical=tropical)
  clim2 <- load_climate_data(var_name2, climatename, area2, tropical=tropical)
  clim3 <- load_climate_data(var_name3, climatename, area3, tropical=tropical)
  clim4 <- load_climate_data(var_name4, climatename, area4, tropical=tropical)
  

  
  d <- rbind(cbind(stack(clim1), group=paste(toString(var_name1), area1)), 
             cbind(stack(clim2), group=paste(toString(var_name2), area2)),
             cbind(stack(clim3), group=paste(toString(var_name3), area3)),
             cbind(stack(clim4), group=paste(toString(var_name4), area4)))
  e<- rbind(cbind((clim1), group=paste(toString(var_name1), area1)), 
             cbind((clim2), group=paste(toString(var_name2), area2)),
             cbind((clim3), group=paste(toString(var_name3), area3)),
             cbind((clim4), group=paste(toString(var_name4), area4)))
  glob <- na.omit(clim1)
  
  M <-cor(glob, method="pearson")
  corrplot(M, type="upper", order="hclust")
  
  
  p <- ggplot(d, aes(group, values)) +
    geom_boxplot(aes(color=group)) +
    facet_wrap(~ind, scales='free_y') #+
  #scale_color_manual(values = c("#00AFBB", "#E7B800"))
  # p + stat_compare_means(method = "t.test")
  # Display the significance level instead of the p-value
  # Adjust label position
  # p + compare_means(
  #    aes(label = ..p.signif..), label.x = 1.5, label.y = 40
  # )
  p 
  ggsave(paste("boxplot_climate_all", climatename, ".png", sep="_"), p)
  
  
  clim1$group <- area1
  clim2$group <- area2
  clim3$group <- area3
  clim4$group <- area4
  
  for(bioclim in 1:19){
    df1 <- clim1[, c(bioclim, 20)]
    df2 <- clim2[, c(bioclim, 20)]
    df3 <- clim3[, c(bioclim, 20)]
    df4 <- clim4[, c(bioclim, 20)]
    
    
    df <- rbind(df1, df2, df3, df4)
    #df = esub
    my_comparisons <- list( c("Africa", "Asia"), c("Africa", "Hawaii"), c("Africa", "SouthAmerica"), c("Asia", "Hawaii"), c("Asia", "SouthAmerica"), c("Hawaii", "SouthAmerica") )
    p <- ggplot(df, aes(df[,2], df[,1])) +
      geom_boxplot(aes(color=group)) 
    p <- p + stat_compare_means(comparisons = my_comparisons)
    p
    ggsave(paste("boxplot_climate_all",bioclim, climatename, ".png", sep="_"), p)
  }
}

load_glob <- function(var_name, area, climatename){
  print("load glob")
  prefix1 <- fn_prefix(var_name)
  setwd(main_wd)
  setwd(area)
  setwd(climatename)
  setwd(prefix1)
  glob4 <- make_pca(var_name, climatename)
  return(glob4)
}

get_cor_var <- function(glob1){
  cor_glob <- cor(glob1, method="pearson")
  cor_glob <- cor_glob[rowSums(is.na(cor_glob)) < 18, ]
  cor_glob <- cor_glob[, colSums(is.na(cor_glob)) < 18 ]
  
  hclust_call(var_name, cor_glob)
  
  # find highly correlated variables
  cor_var <- caret::findCorrelation(cor_glob, cutoff = 0.75, exact = TRUE, names =TRUE)
  return(cor_var)
}
