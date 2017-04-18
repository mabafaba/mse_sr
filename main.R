setwd("/Users/m/paper_january17")
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
src<-"./code/data/all_LU.shp"
layerNames<-c("lu_ 1875_all","lu_1895_all","lu_1915_all","lu_1935_all","lu_1960_all","lu_1985_all","lu_2005_all")
years<-c(1875,1895,1915,1935,1960,1985,2005)
# parameters both
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
# analysis synthetic
	lags<-lags_synthetic
	synthetic<-make_data(syntheticsize)
	results_synthetic<-analysis_synthetic(synthetic,lags=lags,geospace_aggregationfactor=geospace_aggregationfactor)
	synthetic_many_results<-analysis_synthetic_many(runs)
# plots synthetic
	plots_synthetic(results_synthetic,"synthetic",synthetic_many_results)

# parameters real
	pixelWidth_real<-50
	lagsINmeters_real<-c(50,150,450,1350,4050)
	# lagsINmeters_real<-c(50,150,450)
	
	lags_real=lagsINmeters_real/pixelWidth_real
	lags_real=2*round((lags_real+1)/2)-1

 	pixelWidth<-pixelWidth_real
	lagsINmeters<-lagsINmeters_real
	lags=lags_real

# analysis real
	data<-load_data(src,layerNames)
	results<-analysis(data)
	nullmod<-analysis_null(data,runs)
 #    saveRDS(results,"results_april_16.RDS")
	# saveRDS(nullmod,"nullmods_april_16.RDS")
 #    saveRDS(list(pw=pixelWidth,linm=lagsINmeters,lags=lags,runs=runs),"parameters_april_16.RDS")
	
	



# plots
	dev.off.all()
	plots(results,nullmod,"")
	plots_synthetic(results_synthetic,"synthetic",synthetic_many_results)
	dev.off.all()
# sweave
	Sweave("main2.Rnw")



