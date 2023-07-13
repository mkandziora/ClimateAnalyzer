####################################################
# ClimateAnalyzer:
# Set of scripts to analyse tropical alpine areas
# based on climatic data and elevation
#
# written by Martha Kandziora, 2020+
#####################################################

#########################################
# adapt base to your working directory

base <- '/home/rstudio/data/ClimateDelimitation2023/'
setwd(base)
assign("base", base, envir = .GlobalEnv)

source(paste0(base, 'ClimateAnalyzer/00_set_stage_ondemand.R'))
source(paste0(base, 'ClimateAnalyzer/wrapper.R'))
source(paste0(base, 'ClimateAnalyzer/makePolygonDelim.R'))
source(paste0(base, 'ClimateAnalyzer/00_set_variables.R'))
source(paste0(base, 'ClimateAnalyzer/makeClimateData.R'))
source(paste0(base, 'ClimateAnalyzer/compareClimate.R'))
source(paste0(base, 'ClimateAnalyzer/compareArea.R'))
source(paste0(base, 'ClimateAnalyzer/make_climate_plots.R'))
source(paste0(base, 'ClimateAnalyzer/00_set_your_data.R'))

##############################################################


climatename = "Chelsa"
var_name = var_listalp
clim.all = data.frame(matrix(nrow = 1, ncol = 20))
for(area in area_list){
  pp <- make_pol(var_name, climatename, area)
  
  prefix <- fn_prefix(var_name)
  
  fn <- paste(prefix, "presvals_all.rds", sep="_")
  clim  <- load_climate_table(fn)
  clim = st_drop_geometry(clim)
  if(area == "SouthAmerica"){area="tropalpAndes"}
  if(area == "Asia"){area="tropalpAsia"}
  if(area == "Africa"){area="Afroalpine"}
  
  if(area == "Hawaii"){area="tropalpHawaii"}
  
  clim[20] = area
  names(clim.all) = names(clim)
  clim.all = rbind(clim.all, clim)
}

clim.all2 = na.omit(clim.all)

clim.pca = dudi.pca(clim.all2[1:19], scannf=F,nf=2)



res.pca <- prcomp(clim.all2[1:19],  retx = TRUE, center = TRUE, scale. = TRUE)
res.pca
cat = c("red", "purple", "purple", "purple", "red", "red", "purple", "red", "red", "red", "red", 
        "blue", "blue","blue","light blue","blue","blue","blue","blue")
p2 = fviz_pca_var(res.pca, repel = FALSE, axes = c(1, 2),
                col.var = names(clim.all2[1:19]),
             palette = cat)
p2           

groups <- as.factor(clim.all2$V20)

p1 = ggplotify::as.ggplot(~s.class(clim.pca$li,
        fac = groups,  # color by groups
        col = c( "#00AFBB", "orange", "#FC4E07", "purple"), add.plot=FALSE
        )
)
p1


ggarrange(p1, p2)
