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

bioonly = clim.all2[1:19]
bioonly = translate_biovar(bioonly)

res.pca <- prcomp(bioonly,  retx = TRUE, center = TRUE, scale. = TRUE)
res.pca

names(bioonly)

neworder =c("Annual Mean Temp.", "Temp. Diurnal Range", "Isothermality", "Temp. Seasonality", "Max Temp. Warmest M.", "Min Temp. Coldest M.", 
            "Temp. Annual Range", "Mean Temp. Wettest Qu.", "Mean Temp. Driest Qu.",  "Mean Temp. Warmest Qu.", "Mean Temp. Coldest Qu.",
            "Annual Prec.", "Prec. Wettest M.", "Prec. Driest M.", "Prec. Seasonality", "Prec. Wettest Qu." , "Prec. Driest Qu.", "Prec. Warmest Qu.", "Prec. Coldest Qu." ) 
all.df$bioclim <- factor(names(bioonly),
                         labels = neworder)


cat = c("red", , "purple", "purple", "red", "red", "purple", "red", "red", "red", "red", 
        "blue", "blue","blue","light blue","blue","blue","blue","blue")
p2 = fviz_pca_var(res.pca, repel = TRUE, axes = c(1, 2),
                col.var = names(bioonly),
             palette = cat) + theme(legend.position = "none")+
  scale_color_manual(values = c("Annual Mean Temp." = "red",
                                "Temp. Diurnal Range" = "purple",
                                "Isothermality" = "purple",
                                "Temp. Seasonality" = "purple",
                                "Max Temp. Warmest M." = "red",
                                "Min Temp. Coldest M." = "red",
                                "Temp. Annual Range" = "purple",
                                "Mean Temp. Wettest Qu." = "red",
                                "Mean Temp. Driest Qu." = "red",
                                "Mean Temp. Warmest Qu." = "red",
                                "Mean Temp. Coldest Qu." = "red",
                                
                                "Annual Prec." = "blue",
                                "Prec. Wettest M." = "blue",
                                "Prec. Driest M." = "blue",
                                "Prec. Seasonality" = "light blue", 
                                "Prec. Wettest Qu."  = "blue",
                                "Prec. Driest Qu." = "blue",
                                "Prec. Warmest Qu." = "blue",
                                "Prec. Coldest Qu."  = "blue") ) 
p2           

groups <- as.factor(clim.all2$V20)

p1 = ggplotify::as.ggplot(~s.class(clim.pca$li,
        fac = groups,  # color by groups
        col = c( "#00AFBB", "orange", "#FC4E07", "purple"), add.plot=FALSE
        )
)
p1

setwd(base)
ggarrange(p1, p2, labels="AUTO")
ggsave("pca_plot.png", height=7, width=18, dpi=400)
