# runs
runs_sens<-2



# parameters sensitivity: Pixelwidth
	pixelWidth<-pixelWidth_real*4
	lagsINmeters<-lagsINmeters_real
	lags=lags_real
# analysis sensitivity: Pixelwidth
	data_sens_resolution<-load_data(src,layerNames)
	results_sens_resolution<-analysis(data_sens_resolution)
	nullmod_sens_resolution<-analysis_null(data_sens_resolution,runs_sens)
	dev.off.all()
	pdf(paste0("./code/output/","sensitivity_resolution"
		,pixelWidth
		,paste(lagsINmeters,collapse="-")
		,"real_vs_null_entropies.pdf"),7,7)
		setParDefaults()
		plot_real_vs_null_entropies(results_sens_resolution,nullmod_sens_resolution)
		dev.off.all()

# parameters sensitivity: Lags
	pixelWidth<-pixelWidth_real
	lagsINmeters<-c(100,200,300)
	lags=lagsINmeters/pixelWidth
	lags=2*round((lags+1)/2)-1
# analysis sensitivity: Lags
	data_sens_resolution<-load_data(src,layerNames)
	results_sens_resolution<-analysis(data_sens_resolution)
	nullmod_sens_resolution<-analysis_null(data_sens_resolution,runs_sens)
	dev.off.all()
	pdf(paste0("./code/output/","sensitivity_lags"
		,pixelWidth
		,paste(lagsINmeters,collapse="-")
		,"real_vs_null_entropies.pdf"),7,7)
		setParDefaults()
		plot_real_vs_null_entropies(results_sens_resolution,nullmod_sens_resolution)
		dev.off.all()