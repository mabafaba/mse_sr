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
lagsINmeters<-c(50,150,450,1350,4050)
years<-c(1875,1895,1915,1935,1960,1985,2005)
synthetic_names<-c("uniform random","segregated 32x32 fields","segregated 8x8 fields","segregated 2x2 fields","sorted","1/f noise","additive cascade")
syntheticsize<-128
pixelWidth<-50
lags=lagsINmeters/pixelWidth
lags=2*round((lags+1)/2)-1
geospace_aggregationfactor = 32
synth_phasespace_plot_scales=c(3,5)
runs=200

dev.off.all()

data<-load_data(src,layerNames)
results<-analysis(data)
saveRDS(results,"results.RDS")
nullmod<-analysis_null(data,runs)
saveRDS(nullmod,"nullmod.RDS")

plots(results,nullmod,"")

synthetic<-make_data(syntheticsize)
results_synthetic<-analysis_synthetic(synthetic,lags=lags,geospace_aggregationfactor=geospace_aggregationfactor)

synthetic_many_results<-analysis_synthetic_many(2)

plots_synthetic(results_synthetic,"synthetic")


Sweave("main2.Rnw")
dev.off.all()


plots

length(nullmod$randomised$work[[1]])
length(nullmod$compactmixed$work[[1]])

length(nullmod$randomised[[1]][[1]][])2