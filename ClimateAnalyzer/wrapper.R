####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

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
      if(typeof(element) == "character"){
        if(element == "elevation"){
          prefix <- paste("elevation", min_ele, sep="_")
        }else if(element %in% c("testolin_alpine_cluster", "GMBA")){
          prefix <- paste(element, sep="_")
        }else if(grepl("growing_season", element, fixed = TRUE)){
          prefix <- paste(prefix, element, sep="_")
        }
      }else{
        print(element)
        bioclim <- element[1]
        min <- element[2]
        max <- element[3]
        prefix <- paste0(prefix, bioclim, '-', min, '-', max, "_")
      }
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
  pp <- st_read(paste(prefix, 'shape.shp', sep="_"))
  return(pp)
}

      
load_grd <- function(var_comb){
  prefix <- fn_prefix(var_comb)
  grd <- read_stars(paste(prefix, 'mask.grd', sep="_"))
  return(grd)
}

    
wrapperInput <- function(area, var_name,  climatename){
  source(paste0(base, 'ClimateAnalyzer/makePolygonDelim.R'))
  source(paste0(base, 'ClimateAnalyzer/makeClimateData.R'))
  source(paste0(base, 'ClimateAnalyzer/makeElevationData.R'))
  source(paste0(base, 'ClimateAnalyzer/00_set_variables.R'))
  assign("area", area, envir = .GlobalEnv)
  setwd(main_wd)
  dir.create(file.path(area), showWarnings = FALSE)
  setwd(area)
  dir.create(file.path(climatename), showWarnings = FALSE)
  setwd(climatename)
  prefix <- fn_prefix(var_name)
  dir.create(file.path(prefix), showWarnings = FALSE)
  setwd(file.path(prefix))
  
  ele.main <- get_elevation(area, zoom_level, crs, res)
  assign("ele.main", ele.main, envir = .GlobalEnv)
  
  pp <- make_pol(var_name, climatename, area)
  if(!is.na(pp)){
    plot(pp)
    
    print('analyze elevation')
    ele=ele.main
    get_elevation_profile(var_name, pp, ele) 
    
    print("analyze climate")
    generate_climate_data_polygon(var_name, climatename, area)
    
    glob <- make_pca(var_name, climatename)
    make_uncorrelated_pca(var_name, glob)  
  }
}

    
make_pol <- function(var_name, climatename, area){
  if(typeof(var_name) == "character"){ # non combined layers
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
    prefix <- fn_prefix(var_name)
    fn_pp <- paste0(prefix, '_shape.shp')
    if(!file.exists(fn_pp)){
      for(element in var_name){
        prefix2 <- fn_prefix(element)
        setwd(main_wd)
        setwd(area)
        setwd(climatename)
        dir.create(file.path(prefix2), showWarnings = FALSE)
        setwd(prefix2)
        print(element)
        if(!typeof(element) == "double"){
          if(element == "elevation"){
            pp <- MakeReducePolygonElevation(area, element)
          }else if(grepl("growing_season", element, fixed = TRUE)){
            pp <- makeGrowingSeasonPol(element, area)
          }
        }else{
          pp = MakeReducePolygonClim(area, element, climatename)
        }
      }
    }

    setwd(main_wd)
    setwd(area)
    setwd(climatename)
    setwd(prefix)
    pp <- MakeCombinePolygons(area, var_name, climatename)
    print(pp)
    #print(st_dimension(pp))
    
    if(  is.na(pp) || st_dimension(pp) < 2) {
      print('Stopping as no overlapping areas had been retrieved.')
      return(NA)
      }
  }else{
    pp <- MakeReducePolygonClim(area, var_name, climatename)
  }
  pp = st_as_sf(pp)
  pp = st_union(pp)
  pp= st_make_valid(pp)
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
  
  ele.main <- get_elevation(area1, zoom_level, crs, res)
  assign("ele.main", ele.main, envir = .GlobalEnv)
  ele = ele.main
  
  print("load 1")
  prefix1 <- fn_prefix(var_name1)
  setwd(main_wd)
  setwd(area1)
  setwd(climatename1)
  print(prefix1)
  setwd(prefix1)
  pol1 <- make_pol(var_name1, climatename1, area1)
  fn1 = paste(prefix1, climatename1, "elevation.RDA", sep="_")
 
  plot(pol1)
  clim1 <- generate_climate_data_polygon(var_name1, climatename1, area1, tropical=FALSE)
  
  
  print("load 2")
  prefix2 <- fn_prefix(var_name2)
  setwd(main_wd)
  setwd(area2)
  setwd(climatename2)
  setwd(prefix2)
  pol2 <- make_pol(var_name2, climatename2, area2)
  fn2 = paste(prefix2,  climatename2, "elevation.RDA",sep="_")
 
  #clim2 <- generate_climate_data_polygon(var_name2, climatename2, area2, tropical=TRUE)
  clim2 <- generate_climate_data_polygon(var_name2, climatename2, area2, tropical=FALSE)
  
  setwd(main_wd)
  
  if(single_area == TRUE){
    print("compare single area")
    
    setwd(area1)
    fn <- paste(area1, prefix1, prefix2, climatename1, climatename2 , "overlap_area.png", sep="_")
    plot(pol1)
    plot(pol2, add=T)
    comparePolygons(pol1, pol2, area1, fn, tropical=FALSE)
    
    fn <- paste(area1, prefix1, prefix2, climatename1, climatename2 , "overlap_hist.png", sep="_")
    compare_hist(ele1, ele2, area1, fn, tropical=FALSE)
  }
  print("compare climate")
  fn <- paste(area1, area2, prefix1, climatename1, prefix2, climatename2, sep="_")
  compare_climate(clim1, clim2, fn)
}
   

compare_hist <- function(ele1, ele2, area, fn, tropical){
  ele1 = ele1[ele1>=0]
  ele2 = ele2[ele2>=0]
  
  png(fn)
  hist(ele1, maxpixels = 1000000,
       breaks = seq(0,10000,by=200),
       main="Histogram of elevation",
       col='blue',  # changes bin color
       xlab= "Elevation (m)")  # label the x-axis
  #with(mts_gmba_rr, hist(mts_gmba_rr[mts_gmba_rr >= 0], breaks=))
  hist(ele2,maxpixels = 1000000,
       breaks = seq(0,10000,by=200),
       add=TRUE,
       main="Histogram of elevation",
       col=rgb(1,0,0,0.5),  # changes bin color
       xlab= "Elevation (m)")  # label the x-axis
  dev.off()
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
  if(ncol(clim1) == 20){
    clim1 = clim1[, 2:20]
  }
  clim1 = as.data.frame(clim1)
  return(clim1)
}


boxplot_all_area <- function(var_name1, area1, area2, area3, area4, climatename, tropical=FALSE){
  prefix1 = fn_prefix(var_name1)
  clim1 <- load_climate_data(var_name1, climatename, area1, tropical=tropical)
  clim2 <- load_climate_data(var_name1, climatename, area2, tropical=tropical)
  clim3 <- load_climate_data(var_name1, climatename, area3, tropical=tropical)
  clim4 <- load_climate_data(var_name1, climatename, area4, tropical=tropical)
  clim1 = translate_biovar(clim1)
  clim2 = translate_biovar(clim2)
  clim3 = translate_biovar(clim3)
  clim4 = translate_biovar(clim4)
  
  d <- rbind(cbind(stack(clim1), group=area1), 
             cbind(stack(clim2), group=area2),
             cbind(stack(clim3), group=area3),
             cbind(stack(clim4), group=area4))
  
  print(head(d))
  
  d$region <- factor(d$group,
                         levels = c(area1, area2, area3, area4),ordered = TRUE)
  
  print(head(d))
  my_comparisons <- list( c(area1, area2), c(area1, area3), c(area1, area4), c(area2, area3), c(area2, area4), c(area3, area4) )
  p <- ggplot(d, aes(region, values)) +
    geom_boxplot(aes(color=region), width=3.7, show.legend = FALSE) + # theme(legend.position="bottom") + 
    facet_wrap(~ind, scales='free_y', ncol=4)  +
    theme(axis.text.x = element_text(angle = 45, hjust=0.75) )+
    scale_x_discrete(labels=c("SouthAmerica" = "tropalpAndes", 
                              "Asia" = "tropalpAsia",
                              "Africa" = "Afroalpine",
                              "Hawaii" = "tropalpHawaii"))+
    scale_color_manual(values=c("orange", "#FC4E07",  "#00AFBB", "purple"))+
    theme(axis.title.x=element_blank())
  
 p = p + stat_compare_means(comparisons = my_comparisons, label="p.signif",  size =2)

 p 
  ggsave(paste("boxplot_climate_all", prefix1, climatename, ".png", sep="_"), width=10, height = 10, dpi=400)
}

  
boxplot_all_climate <- function(var_name1, var_name2, area1, climatename, tropical=FALSE){
  prefix1 = fn_prefix(var_name1)
  prefix2 = fn_prefix(var_name2)
  
  clim1 <- load_climate_data(var_name1, climatename, area1, tropical=tropical)
  clim2 <- load_climate_data(var_name2, climatename, area1, tropical=tropical)
  clim1 = translate_biovar(clim1)
  clim2 = translate_biovar(clim2)
  
  d <- rbind(cbind(stack(clim1), group=paste(toString(var_name1))), 
             cbind(stack(clim2), group=paste(toString(var_name2))))
  
  p <- ggplot(d, aes(group, values)) +
    geom_boxplot(aes(color=group)) +
    facet_wrap(~ind, scales='free_y') #+
  
  p 
  ggsave(paste("boxplot_climate", prefix1, prefix2, area1, climatename, ".png", sep="_"))
}

boxplot_all_climate2 <- function(var_name1,  area_list, climatename1, climatename2, climatename3, tropical=FALSE){
  prefix1 = fn_prefix(var_name1)
  d = data.frame(matrix(ncol=4))
  names(d) =  c("values" ,"ind" ,   "group", "area" )

  for(area1 in area_list){
    
    clim1 <- load_climate_data(var_name1, climatename1, area1, tropical=tropical)
    clim2 <- load_climate_data(var_name1, climatename2, area1, tropical=tropical)
    clim3 <- load_climate_data(var_name1, climatename3, area1, tropical=tropical)
   
    clim1 = translate_biovar(clim1)
    clim2 = translate_biovar(clim2)
    clim3 = translate_biovar(clim3)

    clim1 = recalculate_past_climate(clim1, climatename1)
    clim2 = recalculate_past_climate(clim2, climatename2)
    clim3 = recalculate_past_climate(clim3, climatename3)
    
    d.s <- rbind(cbind(stack(clim1), group=climatename1), 
               cbind(stack(clim2), group=climatename2), 
               cbind(stack(clim3), group=climatename3))
    d.s$area = area1
    d = rbind(d,d.s)
  }
  d = na.omit(d)
  d$area <- factor(d$area , levels=unlist(area_list))
  neworder =c("Annual Mean Temp.", "Temp. Diurnal Range", "Isothermality", "Temp. Seasonality", "Max Temp. Warmest M.", "Min Temp. Coldest M.", 
                                           "Temp. Annual Range", "Mean Temp. Wettest Qu.", "Mean Temp. Driest Qu.",  "Mean Temp. Warmest Qu.", "Mean Temp. Coldest Qu.",
                                           "Annual Prec.", "Prec. Wettest M.", "Prec. Driest M.", "Prec. Seasonality", "Prec. Wettest Qu." , "Prec. Driest Qu.", "Prec. Warmest Qu.", "Prec. Coldest Qu." ) 
    
  d2 = dplyr::arrange(transform(d,
                    ind=factor(ind,levels=neworder)),ind)  
  my_comparisons <- list( c(climatename1, climatename2), c(climatename1, climatename3), c(climatename2, climatename3))
  
  p <- ggplot(d2, aes(area, values, fill=group)) +
    geom_boxplot(aes(fill=group), show.legend = TRUE)  +
    facet_wrap(~ind, scales='free_y', ncol=3) + theme(legend.position="bottom") 
  p = p + stat_compare_means(comparisons = my_comparisons, label="p.signif",  size =2)+
    theme(axis.text.x = element_text(angle = 45, hjust=0.75) ) +
   scale_x_discrete(labels = c('tropalpAndes','tropalpAsia','Afroalpine'))+
    scale_fill_discrete(labels=c("current", "MIROC", "MPI"))+
    theme(axis.title.x=element_blank())  
  p 
  ggsave(paste("boxplot_climate", prefix1, climatename1, climatename2, climatename3, ".png", sep="_"), width=14, height=14, dpi=400)
}

  
translate_biovar <- function(df){
  translate_df = c("Annual Mean Temp.", "Temp. Diurnal Range", "Isothermality", 
                   "Temp. Seasonality", "Max Temp. Warmest M.", 
                   "Min Temp. Coldest M.", "Temp. Annual Range", 
                   "Mean Temp. Wettest Qu.", "Mean Temp. Driest Qu.", 
                   "Mean Temp. Warmest Qu.", "Mean Temp. Coldest Qu.", "Annual Prec.", 
                   "Prec. Wettest M.", "Prec. Driest M.", "Prec. Seasonality", 
                   "Prec. Wettest Qu.", "Prec. Driest Qu.", 
                   "Prec. Warmest Qu.", "Prec. Coldest Qu.")
  names(df) = translate_df
  return(df)
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
  
  names(esub) <- c(select_var, "group")
  
  
  setwd("boxplot")
  for(bioclim in select_var){
    fn = paste("boxplot_climate_all", bioclim, climatename, ".png", sep="_")
    
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
    ggsave(fn)
  }
  setwd("../")
  
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
  
  setwd("boxplots")
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
  ggsave(paste("boxplot_climate_all", climatename, ".png", sep="_"))
   
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
    ggsave(paste("boxplot_climate_all",bioclim, climatename, ".png", sep="_"))
  }
  setwd("../")
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