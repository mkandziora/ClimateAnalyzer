# ClimateAnalyzer
 
### Short introduction

ClimateAnalyzer is a set of script written in R to delimit areas based on bioclimatic variables. 

The scripts have been developed to delimit tropical alpine areas based on bioclimatic variables from CHELSA. The work has been presented in Kandziora et al (under review) "The ghost of past climate acting on present-day plant diversity: lessons from a climate-based delimitation of the tropical alpine ecosystem". Corresponding data can be found under https://github.com/mkandziora/tropicalAlpineDelimitation.

In order to repeat the analysis users have to rerun script 01 and 02. Script 01 is delimiting the areas based on the bioclimatic variables and script 02 compares different delimitations with each other.

The scripts can also be adapted to any other region, or use of different bioclimatic variables to delimit certain areas.


### Getting started 
0. download the bioclimatic raster layers (000_download_data.sh)
1. install all needed packages (check 000_install_packages.R and adapt 'path_to_libraries' and working directory)
2. load all packages (00_set_stage_ondemand.R)
3. load 00_set_variables.R - here the different bioclimatic limits are being defined, elevation, resolution and projection are set
4. make sure that the climate data is downloaded (see download_data.sh) and that the path to the raster layers is correcly provided (function 'get_path_data' in 00_set_your_data.R)
5. run 01_set_analyses.R to get the areas delimited by the bioclimatic variables
6. run 02_run_all_postanalyses.R to compare different areas to each other
7. run 03_get_climatespace.R to do a PCA and get the niche space per area

### Analyse your own region

1.  adapt regional limits and names within the function 'get_extent_area' (00_set_your_data.R)
