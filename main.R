setwd("/Users/m/paper_january17")




# analysis synthetic
	lags<-lags_synthetic
	synthetic<-make_data(syntheticsize)
	results_synthetic<-analysis_synthetic(synthetic,lags=lags,geospace_aggregationfactor=geospace_aggregationfactor)
	synthetic_many_results<-analysis_synthetic_many(runs)
# plots synthetic
	plots_synthetic(results_synthetic,"synthetic",synthetic_many_results)

# parameters real
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
	Sweave("test.Rnw")
	Sweave("supplementary_material.Rnw")



