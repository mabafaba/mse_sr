# parameters:
runs_sens<-100

sens_resolution_pixelWidth<-pixelWidth_real*2
sens_resolution_lagsINmeters<-c(100,200,400,1400,4000)
sens_resolution_lags<-sens_resolution_lagsINmeters/sens_resolution_pixelWidth
sens_resolution_lags<-2*round((sens_resolution_lags+1)/2)-1

sens_neighbourhoods_pixelWidth<-pixelWidth_real
sens_neighbourhoods_lagsINmeters<-c(50,100,300,700,2000)
sens_neighbourhoods_lags<-sens_neighbourhoods_lagsINmeters/sens_neighbourhoods_pixelWidth
sens_neighbourhoods_lags<-2*round((sens_neighbourhoods_lags+1)/2)-1

sens_bins_pixelWidth<-pixelWidth_real
sens_bins_lagsINmeters<-lagsINmeters_real
sens_bins_lags<-lags_real
sens_bins_lags=10


# RESOLUTION
# activate paramters: Resolution
pixelWidth<-sens_resolution_pixelWidth
lagsINmeters<-sens_resolution_lagsINmeters
lags<-sens_resolution_lags

# analysis sensitivity: 
	sens_resolution_pixelWidth
	sens_resolution_lags
	data_sens_resolution<-load_data(src,layerNames)
	results_sens_resolution<-analysis(data_sens_resolution)
	nullmod_sens_resolution<-analysis_null(data_sens_resolution,runs_sens)
	saveRDS(results_sens_resolution,"sens_res_results_april_16.RDS")
	saveRDS(nullmod_sens_resolution,"sens_res_nullmods_april_16.RDS")
    saveRDS(list(pw=pixelWidth,linm=lagsINmeters,lags=lags,runs=runs_sens),"sens_res_parameters_april_16.RDS")



	dev.off.all()
	pdf(paste0("./code/output/","sensitivity_resolution"
		,pixelWidth
		,paste(lagsINmeters,collapse="-")
		,"real_vs_null_entropies.pdf"),7,7)
		setParDefaults()
		plot_real_vs_null_entropies(results_sens_resolution_1,nullmod_sens_resolution_1)
		dev.off.all()



# NEIGHBOURHOOD SIZES
# parameters sensitivity: Lags
pixelWidth<-sens_neighbourhoods_pixelWidth
lagsINmeters<-sens_neighbourhoods_lagsINmeters
lags<-sens_neighbourhoods_lags
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




# BINS
# parameters sensitivity: bins

pixelWidth<-sens_bins_pixelWidth
lagsINmeters<-sens_bins_lagsINmeters
lags<-sens_bins_lags
sens_bins_lags=10

# analysis sensitivity: bins
	data_sens_bins<-load_data(src,layerNames)
	results_sens_bins<-analysis(data_sens_bins,bins=sens_bins_lags)
	nullmod_sens_bins<-analysis_null(data_sens_bins,runs_sens,bins=sens_bins_lags)
	saveRDS(results_sens_bins,"sens_lags_results_april_16.RDS")
	saveRDS(nullmod_sens_bins,"sens_lags_nullmods_april_16.RDS")
    saveRDS(list(pw=pixelWidth,linm=lagsINmeters,lags=lags,runs=runs_sens,bins=sens_bins_lags),"sens_lags_parameters_april_16.RDS")
	dev.off.all()
	pdf(paste0("./code/output/","sensitivity_lags"
		,pixelWidth
		,paste(lagsINmeters,collapse="-")	
		,"real_vs_null_entropies.pdf"),7,7)
		setParDefaults()
		plot_real_vs_null_entropies(results_sens_bins,nullmod_sens_bins)
	dev.off.all()

# RESET ACTIVE PARAMETERS TO REAL
pixelWidth<-pixelWidth_real
lagsINmeters<-lagsINmeters_real
lags<-lags_real
