# runs
runs_sens<-100



# parameters sensitivity: Pixelwidth
	pixelWidth<-pixelWidth_real*2
	lagsINmeters<-c(100,200,400,1400,4000)
	lags=lags_real
# analysis sensitivity: Pixelwidth
	data_sens_resolution_1<-load_data(src,layerNames)
	results_sens_resolution_1<-analysis(data_sens_resolution)
	nullmod_sens_resolution_1<-analysis_null(data_sens_resolution,runs_sens)
	saveRDS(results_sens_resolution_1,"sens_res_results_april_16.RDS")
	saveRDS(nullmod_sens_resolution_1,"sens_res_nullmods_april_16.RDS")
    saveRDS(list(pw=pixelWidth,linm=lagsINmeters,lags=lags,runs=runs_sens),"sens_res_parameters_april_16.RDS")



	dev.off.all()
	pdf(paste0("./code/output/","sensitivity_resolution"
		,pixelWidth
		,paste(lagsINmeters,collapse="-")
		,"real_vs_null_entropies.pdf"),7,7)
		setParDefaults()
		plot_real_vs_null_entropies(results_sens_resolution_1,nullmod_sens_resolution_1)
		dev.off.all()

# parameters sensitivity: Lags
	pixelWidth<-pixelWidth_real
	lagsINmeters<-c(50,100,300,700,2000)
	lags=lagsINmeters/pixelWidth
	lags=2*round((lags+1)/2)-1
# analysis sensitivity: Lags
	data_sens_resolution<-load_data(src,layerNames)
	results_sens_resolution<-analysis(data_sens_resolution)
	nullmod_sens_resolution<-analysis_null(data_sens_resolution,runs_sens)
	saveRDS(results_sens_resolution,"sens_lags_results_april_16.RDS")
	saveRDS(nullmod_sens_resolution,"sens_lags_nullmods_april_16.RDS")
    saveRDS(list(pw=pixelWidth,linm=lagsINmeters,lags=lags,runs=runs_sens),"sens_lags_parameters_april_16.RDS")



	dev.off.all()
	pdf(paste0("./code/output/","sensitivity_lags"
		,pixelWidth
		,paste(lagsINmeters,collapse="-")	
		,"real_vs_null_entropies.pdf"),7,7)
		setParDefaults()
		plot_real_vs_null_entropies(results_sens_resolution,nullmod_sens_resolution)
		dev.off.all()