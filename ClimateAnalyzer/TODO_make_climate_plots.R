#######################
# second piece of code. You must have run 01 first and have the variable loaded. If you wnt to run as an Rscript - copy both scripts into one.
###################################################

make_pca <- function(var_name, climatename){
  #### make a pca of the climatic data
  print('make a pca of the climatic data')
  prefix <- fn_prefix(var_name)

  presvals_all <- generate_climate_data_polygon(var_name, climatename)

  datafolder <- get_path_data(climatename)
  files <- list.files(path=datafolder,  pattern='tif',  full.names=TRUE )
  predictors <- stack(files)
  # add correct names to all the different variables
  bio_names <- c("pb", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5", "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  names(sdm_data) <- bio_names
  bio_names <- c("bio_1", "bio_2", "bio_3", "bio_4", "bio_5", "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  names(predictors) <- bio_names
  colnames(presvals_all) <- bio_names
  
  glob <- na.omit(presvals_all)
  
  # make pca
  pca.glob <-  ade4::dudi.pca( glob,scannf=F,nf=2)
  ecospat.plot.contrib(contrib=pca.glob$co, eigen=pca.glob$eig)
  
  png(paste(prefix, "glob_pca_contribution.png", sep="_"))
  ecospat.plot.contrib(contrib=pca.glob$co, eigen=pca.glob$eig)
  dev.off()
  
  pca.test <- prcomp(glob)
  
  get_min_max(var_name, glob)
  #pca_res <- prcomp(glob, scale. = TRUE)
  
  autoplot(pca.test, loadings=TRUE)
  #autoplot(pca.test, data = glob, colour = 'columnname')

  p1 <- autoplot(pca.test, # data = iris, colour = 'Species',
                 loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE, loadings.label.size = 3)
  plot(p1)
  fn <- paste(prefix, "glob_pca_loadings.png", sep='_')
  ggsave(file=fn)
  
  len_col <- ncol(glob)
  summary(glob[,2:len_col])
  boxplot(glob[,2:len_col],horizontal=TRUE,col="grey",staplewex=1,axes=TRUE)
  
  png(paste(prefix, "boxplot_sdm.png", sep="_"))
  boxplot(glob[,2:len_col],horizontal=TRUE,col="grey",staplewex=1,axes=TRUE)
  title(var_name)
  dev.off()

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
  ecospat.plot.contrib(contrib=pca.globsubset$co, eigen=pca.globsubset$eig)
  
  png(paste(prefix, "uncorrelated_glob_pca_contribution.png", sep="_"))
  ecospat.plot.contrib(contrib=pca.globsubset$co, eigen=pca.globsubset$eig)
  dev.off()
  
  # contributions of var to PCA axes
  pca.globsubset$c1
  
  # plot pca
  pca.test <- prcomp(glob_new)
  #ggbiplot::ggbiplot(pca.test,choices=c(1,2), obs.scale = 1, var.scale = 1, ellipse = TRUE, title= "PCA global") 
  
  scores.globclim <- pca.globsubset$li # PCA scores for the species native distribution
  
  pca_res <- prcomp(glob_new, scale. = TRUE)
  
  autoplot(pca.test, loadings=TRUE)
  #autoplot(pca.test, data = glob_new, colour = 'columnname')
  autoplot(pca_res, loadings=TRUE)
  #autoplot(pca_res, data = glob_new, colour = 'columnname')
  
  
  # png(paste(toString(name_of_polygon), "uncorrelated_glob_pca_loadings.png", sep="_"))
  p1 <- autoplot(pca.test, # data = iris, colour = 'Species',
                 loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE, loadings.label.size = 3)
  plot(p1)
  fn <- paste(prefix, "uncorrelated_glob_pca_loadings.png", sep="_")
  ggsave(file=fn)
  #fviz_pca_ind(pca.test,
  #             col.ind = "cos2", # Color by the quality of representation
  #             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  #             repel = TRUE     # Avoid text overlapping
  #)

  len_col <- ncol(glob_new)
  summary(glob_new[,1:len_col])
  boxplot(glob_new[,1:len_col],horizontal=TRUE,col="grey",staplewex=1,axes=TRUE)
  
  png(paste(prefix, "subset_boxplot_sdm.png", sep="_"))
  boxplot(glob_new[,1:len_col],horizontal=TRUE,col="grey",staplewex=1,axes=TRUE)
  dev.off()

  return(glob_new)
}

###############################################################################################
## TODO merge to one function with fn as argumetn
get_min_max <- function(var_name, glob){
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


get_min_max_uncor <- function(var_name, glob_new){
  prefix <- fn_prefix(var_name)

  ############
  min_max_clim <- get_min_max_data(glob_new)
  min_max_clim

  
  p1 <- ggplot(min_max_clim) + 
    geom_point(aes(x=names, y=all_min_val),colour="red") + 
    geom_point(aes(x=names,y=all_max_val),colour="blue") +
    ylab("Y") +
    theme_bw()
  p1
  plot(p1)
  fn <- paste(prefix, "min_max_sdm.png", sep="_")
  ggsave(file=fn)
  
  
  
}


######################################################################

make_clara <-function(name_of_polygon, glob_new, clusternumber=2, mask_crop2){
  print('make clara analyses')
  # check 
  sdm_cluster <- glob_new
  
  sdm_cluster <- na.omit(sdm_cluster)
  
  # pca.test = prcomp(glob_new)
  # p1 <- fviz_nbclust(data.frame(na.omit(glob_new[,2])), cluster::clara,method = "gap_stat", nboot = 50)+
  #   labs(subtitle = "Gap statistic method")
  # plot(p1)
  # fn = paste(toString(name_of_polygon), "cluster_eval.png", sep = "_")
  # ggsave(file=fn, width=100, height=100, dpi=300, limitsize = FALSE)
  # clara.res <- cluster::clara(sdm_cluster, clusternumber, samples = 50, pamLike = TRUE, correct.d = FALSE)
  # plot(clara.res)
  
    # find optimal numbers of cluster
  #factoextra::fviz_nbclust(sdm_cluster, kmeans, 'wss')
  
  # factoextra::fviz_nbclust(sdm_cluster, cluster::clara, method = "silhouette")+
  #    theme_classic()
  
  p1 <- fviz_nbclust(sdm_cluster,cluster::clara,method = "gap_stat", nboot = 50)+
    labs(subtitle = "Gap statistic method")
  plot(p1)
  fn <- paste(toString(name_of_polygon), "cluster_eval.png", sep = "_")
  ggsave(file=fn, width=100, height=100, dpi=100, limitsize = FALSE)
  

  # 
  # res.nbclust <- NbClust(sdm_cluster, distance = "euclidean",
  #                        min.nc = 2, max.nc = 9, 
  #                        method = "complete", index ="all")
  # res.nbclust
  # factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")
  
  # set number of clusters
  clusternumber <- 8
  clara.res <- cluster::clara(sdm_cluster, clusternumber, samples = 50, pamLike = TRUE, correct.d = FALSE)
  plot(clara.res)
  
  # Add clustering result to the Data
  dd <- cbind(sdm_cluster, cluster = clara.res$cluster)
  head(dd, n = 4)

  clara.res$medoids

  #######################################
  
  p1 <-  fviz_cluster(clara.res, 
                      # palette = c("#00AFBB", "#FC4E07", "#E7B800"), # color palette
                      ellipse.type = "t", # Concentration ellipse
                      geom = "point", pointsize = 1,
                      ggtheme = theme_classic()
  )
  
  plot(p1)
  fn <- paste(toString(name_of_polygon), "cluster_nr.png", sep = "_")
  ggsave(file=fn, width=100, height=100, dpi=100, limitsize = FALSE)
  
  
  climate_clus <- cbind(glob_new, cluster = clara.res$cluster)
  len_clim <- ncol(climate_clus)
  
  clus1 <- climate_clus[climate_clus[, len_clim] ==1,]
  clus2 <- climate_clus[climate_clus[, len_clim] ==2,]
  fn <- paste(toString(name_of_polygon), "clus1_sdm.data", sep = "_")
  write.csv(summary(clus1), file = fn)
  fn <- paste(toString(name_of_polygon), "clus2_sdm.data", sep = "_")
  write.csv(summary(clus2), file = fn)

  # map the corresponding cluster
  pp <- load_shp(name_of_polygon, type, datafolder)
  #increase number of cluster accrding to results above
  bg2 <- randomPoints(mask_crop2, 5000, ext=extent(pp))
  if(length(bg2) < 4000){
    bg2 <- randomPoints(mask_crop2, 10000, ext=extent(pp))
    bg2
  }
  
  bg2 <- na.omit(bg2)
  coord_cluster <- cbind(bg2, cluster = clara.res$cluster)
  clus1_coord <- coord_cluster[coord_cluster[, 3] ==1,]
  clus2_coord <- coord_cluster[coord_cluster[, 3] ==2,]
  clus3_coord <- coord_cluster[coord_cluster[, 3] ==3,]
  clus4_coord <- coord_cluster[coord_cluster[, 3] ==4,]
  clus5_coord <- coord_cluster[coord_cluster[, 3] ==5,]
  clus6_coord <- coord_cluster[coord_cluster[, 3] ==6,]
  clus7_coord <- coord_cluster[coord_cluster[, 3] ==7,]
  clus8_coord <- coord_cluster[coord_cluster[, 3] ==8,]
  clus9_coord <- coord_cluster[coord_cluster[, 3] ==9,]
  
  newmap <- getMap(resolution = "low")
  
  plot(newmap, xlim = c(-80, -50), ylim = c(-60, 30), asp = 1)
  points(clus1_coord[,1], clus1_coord[,2], col = "red", cex = .6)
  points(clus2_coord[,1], clus2_coord[,2], col = "blue", cex = .6)
  points(clus3_coord[,1], clus3_coord[,2], col = "pink", cex = .6)
  points(clus4_coord[,1], clus4_coord[,2], col = "blue", cex = .6)
  points(clus5_coord[,1], clus5_coord[,2], col = "pink", cex = .6)
  points(clus6_coord[,1], clus6_coord[,2], col = "blue", cex = .6)
  points(clus7_coord[,1], clus7_coord[,2], col = "pink", cex = .6)
  points(clus8_coord[,1], clus8_coord[,2], col = "blue", cex = .6)
  points(clus9_coord[,1], clus9_coord[,2], col = "pink", cex = .6)
  
  fn <- paste(toString(name_of_polygon), "cluster2.png", sep = "_")
  png(fn)
  plot(newmap, xlim = c(-80, 0), ylim = c(-30, 30), asp = 1)
  points(clus1_coord[,1], clus1_coord[,2], col = "red", cex = .6)
  points(clus2_coord[,1], clus2_coord[,2], col = "blue", cex = .6)
  dev.off()
  
  
  p1 <-  ggplot(newmap, xlim = c(-80, 0), ylim = c(-30, 30), asp = 1) + 
    geom_point(aes(clus1_coord[,1], clus1_coord[,2]),colour="red") + 
    geom_point(aes(clus1_coord[,1], clus1_coord[,2]),colour="blue") +
    ylab("Y") +
    theme_bw()
  
  
  plot(p1)
  fn <- paste(toString(name_of_polygon), "cluster_nr.png", sep = "_")
  ggsave(file=fn, width=100, height=100, dpi=100, limitsize = FALSE)
  
  
  #####################
  #### Other cluster tools
  
  ######################3
  ### cluster cluster1
  sdm_cluster_split <- clus1
  
  sdm_cluster_split <- na.omit(sdm_cluster_split)
  
  # find optimal numbers of cluster
  factoextra::fviz_nbclust(sdm_cluster_split, cluster::clara, method = "silhouette")+
    theme_classic()
  
  len_bg <- ncol(sdm_cluster_split)
  # set number of clusters
  clara.res1 <- cluster::clara(sdm_cluster_split[,1:len_bg-1], 2, samples = 100, pamLike = TRUE, correct.d = FALSE)
  plot(clara.res1)
  
}

# 
# #################################################33
# mydata=glob_new
# 
# #mydata=clus1_coord
# # Ward Hierarchical Clustering
# d <- dist(mydata, method = "euclidean") # distance matrix
# fit <- hclust(d, method="ward")
# plot(fit) # display dendogram
# groups <- cutree(fit, k=2) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters
# rect.hclust(fit, k=2, border="red") 
# 
# 
# ############33
# 
# 
# # Ward Hierarchical Clustering with Bootstrapped p values
# library(pvclust, lib.loc = path_to_libraries)
# fit <- pvclust(mydata[,1:len_bg-1], method.hclust="ward",
#                method.dist="euclidean")
# plot(fit) # dendogram with p values
# # add rectangles around groups highly supported by the data
# pvrect(fit, alpha=.95) 
# 
# 
# 
# ############
# 
# # Model Based Clustering
# library(mclust, lib.loc = path_to_libraries)
# fit <- Mclust(mydata)
# plot(fit) # plot results
# summary(fit) # display the best model 
# 
# 
# #############
# 
# # K-Means Clustering with 5 clusters
# fit <- kmeans(mydata, 5)
# 
# # Cluster Plot against 1st 2 principal components
# 
# # vary parameters for most readable graph
# library(cluster)
# clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# 
# # Centroid Plot against 1st 2 discriminant functions
# library(fpc)
# plotcluster(mydata, fit$cluster) 
# 
# #################
# # comparing 2 cluster solutions
# library(fpc)
# cluster.stats(d, fit1$cluster, fit2$cluster) 
# 
# cluster::pam(mydata)
# 
# fpc::pamk(mydata)
# 
# #######################################
