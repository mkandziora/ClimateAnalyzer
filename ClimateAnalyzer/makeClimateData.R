####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

generate_climate_data_polygon <- function(var_comb, climatename, area, tropical=FALSE){
  datafolder <- get_path_data(climatename)
  prefix <- fn_prefix(var_comb)
  print("tropical")
  print(tropical)
  
 # fn = paste(prefix, "sdm_all.data", sep = "_")
  fn <- paste(prefix, "presvals_all.rds", sep="_")
  if(tropical == TRUE){
    fn <- paste(prefix, "tropical_presvals_all.rds", sep = "_")
  }
  pp <- load_shp(var_comb)
  
  if(!file.exists(fn)){
    #  Now create a RasterStack of bioclim variables.
    files <- list.files(path=datafolder,  pattern='tif',  full.names=TRUE )
    files = fix_crs_future(climatename, area, "../")
    
    
    predictors <- read_stars(files)
    print(predictors)
    print('make presence values')
    points = 1000
    if(area == "Hawaii"){
      points = 100
    }

    pp <- st_as_sf(pp)
    
    # Create the random points (here, 5 random points for each polygon)
    points <- st_sample(pp, size = points, type = "random")
    print(points)
    print(class(points))
    plot(pp)
    plot(points,add=T)
    #pts <- st_bbox(pp) |> st_as_sfc() |> st_sample(points)
    (presvals_all <- st_extract(predictors, points))
    
    print(presvals_all)
    saveRDS(presvals_all, file=fn)
   
  } else{
    presvals_all <- load_climate_table(fn)
    summary(presvals_all)
    
  }
  presvals_all = presvals_all %>% st_drop_geometry()
  fn = gsub('.{3}$', 'csv', fn)
  write.csv(presvals_all, fn)
  return(presvals_all)
}


load_climate_table <- function(fn){
  print(fn)
  climate <- readRDS(fn)
  if(ncol(climate) == 1 ){
    climate <- read.csv(fn, sep="/t")
  }
  climate <- climate[, 1:ncol(climate)]
  climate <- na.omit(climate)
  return(climate)
}


get_min_max_data <- function(data, name=NA){
  print(data)
  min_val <- apply(data, 2, min)
  max_val <- apply(data, 2, max)
  
  combined <- data.frame(min_val, max_val)
  if(is.na(name)){
    name <- "all"
  }
  
  len_col <- nrow(combined)
  names(combined) <- c(paste(name, "min_val", sep = "_"), paste(name, "max_val", sep = "_"))
  combined$names <- seq(1:len_col)
  
  return(combined)
}



plot_min_max <- function(var_name, glob){
  prefix <- fn_prefix(var_name)
  
  min_max_clim <- get_min_max_data(glob)
  min_max_clim
  
  p1 <-ggplot(min_max_clim) + 
    geom_point(aes(x=names, y=all_min_val),colour="red") + 
    geom_point(aes(x=names,y=all_max_val),colour="blue") +
    ylab("Y") +
    theme_bw()
  p1
  plot(p1)
  fn <- paste(prefix, "min_max_correlated_sdm.png", sep="_")
  ggsave(file=fn)
  
}


make_pca <- function(var_name, climatename){
  #### make a pca of the climatic data
  print('make a pca of the climatic data')
  prefix <- fn_prefix(var_name)

  presvals_all <- generate_climate_data_polygon(var_name, climatename, area)

  datafolder <- get_path_data(climatename)
  files <- list.files(path=datafolder,  pattern='tif',  full.names=TRUE )
  print(files)
  predictors <- read_stars(files)

  bio_names <- c("bio_1", "bio_2", "bio_3/10", "bio_4/10", "bio_5", "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  names(predictors) <- bio_names
  names(presvals_all) <- bio_names
  
  
  presvals_all <- presvals_all %>% st_drop_geometry()
  

  glob <- na.omit(presvals_all[,1:19])
  # make pca
  pca.glob <-  ade4::dudi.pca( glob,scannf=F,nf=2)
  factoextra::fviz_contrib(pca.glob, choice = "var", axes = 1, top = 10)
  factoextra::fviz_pca_var(pca.glob)

  png(paste(prefix, "glob_pca_contribution.png", sep="_"))
  factoextra::fviz_contrib(pca.glob, choice = "var", axes = 1, top = 10)
  factoextra::fviz_pca_var(pca.glob)
  dev.off()
  
  pca.test <- prcomp(glob)
  plot_min_max(var_name, glob)

 
  p1 <- autoplot(pca.test, # data = iris, colour = 'Species',
                 loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE, loadings.label.size = 3)
  plot(p1)
  fn <- paste(prefix, "glob_pca_loadings.png", sep='_')
  ggsave(file=fn)
  
  len_col <- ncol(glob)
  summary(glob[,1:len_col])
  boxplot(glob[,1:len_col],horizontal=TRUE,col="grey",staplewex=1,axes=TRUE)

  
  glob[,4] = glob[,4]/10
  glob[,3] = glob[,3]/10
  

  glob2 = reshape2::melt(glob[,1:11])
    p1 = ggplot(data = glob2, aes(x=variable, y=value/10)) + geom_boxplot(aes(fill=variable))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  glob2 = reshape2::melt(glob[,12:len_col])
  p2 = ggplot(data = glob2, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  
  ggarrange(p1, p2,
            labels = c("A", "B"),
            ncol = 2, nrow = 1)
  fn <- paste(prefix, "boxplot_sdm.png", sep="_")
  ggsave(file=fn)
 
 
  return(glob)
}


hclust_call <- function(var_name, cor_glob){
  #############################################
  #### check correlations: this is just to get an idea. correlated variables are vars that have a value somewhat greater than 0.7-0.8. 
  # People often choose them by hand as they have their favourites and then throw out, the other ones.
  # to get an idea use the hierachical clustering or the findCorrelates:
  # if you want to define your own, write your own select variables that includes what you like to keep: select = c('climX', 'climY', ...)
  
  print('calculate h.clust')
  prefix <- fn_prefix(var_name)
  # Generate tree that groups variables according to correlation values

#  cor_glob[complete.cases(cor_glob[,1]), ]
  bio.dist<-as.dist(1-abs(cor_glob))
  bio.class<-hclust(bio.dist,method = "average", members = NULL)
  plot(bio.class)
  
  png(paste(prefix, 'hclust.png', sep="_"))
  plot(bio.class)
  dev.off()
  
}


make_uncorrelated_pca <- function(var_name, glob){
  print('make a uncorrelated pca of the climatic data')

  prefix <- fn_prefix(var_name)

  cor_glob <- cor(glob, method="pearson")
  cor_glob <- cor_glob[rowSums(is.na(cor_glob)) < 18, ]
  cor_glob <- cor_glob[, colSums(is.na(cor_glob)) < 18 ]
  
  hclust_call(var_name, cor_glob)
  
  # find highly correlated variables
  caret::findCorrelation(cor_glob, cutoff = 0.75, exact = TRUE, names =TRUE)
  drop_var <- caret::findCorrelation(cor_glob, cutoff = 0.75, exact = TRUE, names =TRUE)
  
  select <- setdiff(colnames(cor_glob), drop_var)
  
  # remove correlated variables
  glob_new <- subset(glob, select=select)
  
  cor_glob_new <- cor(glob_new, method="pearson")
  cor_glob_new
  caret::findCorrelation(cor_glob_new, cutoff = 0.75, exact = TRUE, names =TRUE) # this reports still somewhat correlated variables
  
  pca.globsubset <-  ade4::dudi.pca( glob_new,scannf=F,nf=2)
  #ecospat.plot.contrib(contrib=pca.globsubset$co, eigen=pca.globsubset$eig)
  
  png(paste(prefix, "uncorrelated_glob_pca_contribution.png", sep="_"))
  factoextra::fviz_contrib(pca.globsubset, choice = "var", axes = 1, top = 10)
  factoextra::fviz_pca_var(pca.globsubset)
  dev.off()
  
  # contributions of var to PCA axes
  pca.globsubset$c1
  
  # plot pca
  pca.test <- prcomp(glob_new)
  #ggbiplot::ggbiplot(pca.test,choices=c(1,2), obs.scale = 1, var.scale = 1, ellipse = TRUE, title= "PCA global") 
  
  scores.globclim <- pca.globsubset$li # PCA scores for the species native distribution
  
  pca_res <- prcomp(glob_new, scale. = TRUE)
  

  p1 <- autoplot(pca.test, # data = iris, colour = 'Species',
                 loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE, loadings.label.size = 3)
  plot(p1)
  fn <- paste(prefix, "uncorrelated_glob_pca_loadings.png", sep="_")
  ggsave(file=fn)


  len_col <- ncol(glob_new)
  summary(glob_new[,1:len_col])
  boxplot(glob_new[,1:len_col],horizontal=TRUE,col="grey",staplewex=1,axes=TRUE)
  
  png(paste(prefix, "subset_boxplot_sdm.png", sep="_"))
  boxplot(glob_new[,1:len_col],horizontal=TRUE,col="grey",staplewex=1,axes=TRUE)
  dev.off()

  return(glob_new)
}

