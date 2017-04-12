# runs
runs_sens<-2

# copy parameters real
 	pixelWidth<-pixelWidth_real


# parameters sensitivity: Pixelwidth
	pixelWidth<-pixelWidth_real*4
	lagsINmeters<-lagsINmeters_real
	lags=lags_real
# analysis sensitivity: Pixelwidth
	data_sens_resolution<-load_data(src,layerNames)
	results_sens_resolution<-analysis(data_sens_resolution)
	nullmod_sens_resolution<-analysis_null(data_sens_resolution,runs_sens)
	plots(results,nullmod,paste("sens_resolution_",pixelWidth))


# parameters sensitivity: Lags
	pixelWidth<-pixelWidth_real
	lagsINmeters<-c(100,200,300)
	lags=lagsINmeters/pixelWidth
	lags=2*round((lags+1)/2)-1
# analysis sensitivity: Lags
	data_sens_resolution<-load_data(src,layerNames)
	results_sens_resolution<-analysis(data_sens_resolution)
	nullmod_sens_resolution<-analysis_null(data_sens_resolution,runs_sens)
	plots(results,nullmod,paste("sens_lags_",paste(lagsINmeters,collapse="-"))
