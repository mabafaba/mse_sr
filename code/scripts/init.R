setwd("/Users/m/paper_january17")
src<-"./code/data/all_LU.shp"
require("rgdal")
require("raster")
require("abind")
require("gdalUtils")
require("MASS")
source("./code/scripts/load_data.R")
source("./code/scripts/make_data.R")
source("./code/scripts/prep_data.R")
source("./code/scripts/analysis.R")
source("./code/scripts/plots.R")
source("./code/scripts/gis.R")

layerNames<-c("lu_ 1875_all","lu_1895_all","lu_1915_all","lu_1935_all","lu_1960_all","lu_1985_all","lu_2005_all")
years<-c(1875,1895,1915,1935,1960,1985,2005)

# parameters all
runs=10


# parameters synthetic
	# synthetic_names<-c("uniform random","segregated 32x32 fields","segregated 8x8 fields","segregated 2x2 fields","sorted","1/f noise","additive cascade")
	synthetic_names<-c("uniform random","segregated 8x8 fields","full segregation","sorted","1/f noise","additive cascade")
	synthetic_names_abb<-c("unif","segmed","halfhalf","sorted","1/f","casc")
	synthfig_letters<-paste0(letters[1:length(synthetic_names_abb)],")")
	names(synthfig_letters)<-synthetic_names_abb
	syntheticsize<-512
	geospace_aggregationfactor = 128
	lags_synthetic=c(3,13,65)
	synth_phasespace_plot_scales=c(2,3)


# parameters real
	pixelWidth_real<-50
	lagsINmeters_real<-c(50,150,450,1350,4050)
	# lagsINmeters_real<-c(50,150,450)
	
	lags_real=lagsINmeters_real/pixelWidth_real
	lags_real=2*round((lags_real+1)/2)-1



# parameters sensitivity
	runs_sens<-50

	sens_resolution_pixelWidth<-pixelWidth_real*2
	sens_resolution_lagsINmeters<-c(100,500,1300,4100)
	sens_resolution_lags<-sens_resolution_lagsINmeters/sens_resolution_pixelWidth
	sens_resolution_lags<-2*round((sens_resolution_lags+1)/2)-1

	sens_neighbourhoods_pixelWidth<-pixelWidth_real
	sens_neighbourhoods_lagsINmeters<-c(50,100,300,700,2000)
	sens_neighbourhoods_lags<-sens_neighbourhoods_lagsINmeters/sens_neighbourhoods_pixelWidth
	sens_neighbourhoods_lags<-2*round((sens_neighbourhoods_lags+1)/2)-1

	sens_bins_pixelWidth<-pixelWidth_real
	sens_bins_lagsINmeters<-lagsINmeters_real
	sens_bins_lags<-lags_real
	sens_bins_bins=6
