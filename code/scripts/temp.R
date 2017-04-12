
analyse_sensitivity_case<-function(basecase,pixelWidth=basecase$pixelWidth,lagsINmeters=basecase$lagsINmeters,sens.runs){
print(lagsINmeters)
print(pixelWidth)
  lags=lagsINmeters/pixelWidth
  lags=2*round((lags+1)/2)-1

  data<-load_data(src,layerNames)
  results<-analysis(data)
  nullmod<-analysis_null(data,sens.runs)
  lags=basecase$lags
  return(list(results=results,nullmod=nullmod,params=
    list(
      basecase=basecase
      ,pixelWidth=pixelWidth
      ,lagsINmeters=lagsINmeters
      ,runs=sens.runs
      )
    )
  )
}


make_and_plot_sensitivity_all<-function(pixelWidth_multi,lagsInMeters_multi,sens.runs){
	print("make_and_plot_sensitivity_all")
  basecase<-list(
    pixelWidth=pixelWidth,
    lagsINmeters=lagsINmeters,
    lags=lags
    )

  # define different parameters to test
  # sens.testpars.pixelWidth<-list(pixelWidth*2)
  # sens.testpars.lagsINmeters<-list(lagsINmeters/2,lagsINmeters[-c(1:2)])
  # sens.testpars.lags=sens.testpars.lagsINmeters/sens.testpars.pixelWidth
  # sens.testpars.lags=2*round((sens.testpars.lags+1)/2)-1

  # sens.testpars.pixelWidth
  # sens.testpars.lagsINmeters
  # sens.testpars.lags

  # test different rasterisation resolution:
  
  for (i in c(1:length(pixelWidth_multi))) {
  	print(pixelWidth_multi[[i]])
  	analyse_sensitivity_case(basecase=basecase,pixelWidth=pixelWidth_multi[[i]],sens.runs=sens.runs)
    real_vs_null_for_sensitivity(,prefix=paste0("resultion",i)) 
  }  

  # for (i in c(1:length(lagsInMeters_multi))) {
  # 	print(lagsINmeters=lagsInMeters_multi[[i]])
  # real_vs_null_for_sensitivity(analyse_sensitivity_case(basecase=basecase,lagsINmeters=lagsInMeters_multi[[i]],sens.runs=sens.runs),prefix=paste0("lags",i))
  #   }
return(0)
}





real_vs_null_for_sensitivity<-function(sensresults,prefix){
	print("real_vs_null_for_sensitivity")
	# entropies over time: real vs. null:
		pdf(paste0("./code/output/sensitivity/",prefix,"sensitivity"
			,".pdf"),7,7)
		setParDefaults()
		plot_real_vs_null_entropies(sensresults$results,sensresults$nullmod)
		title(sub=paste0("raster resolution: ",sensresults$params$pixelWidth,"m, Neighbourhood sizes: ",paste(sensresults$params$lagsINmeters,collapse="m, "),"m"))
		dev.off.all()
		jpeg(paste0("./code/output/sensitivity/",prefix,"sensitivity"
			,sensresults$params$pixelWidth,"_"
			,sensresults$params$lagsINmeters,"_"
			,sensresults$params$runs
			,".jpg"),7,7,res=300,units = 'in')
		setParDefaults()
		plot_real_vs_null_entropies(sensresults$results,sensresults$nullmod)
		title(sub=paste0("raster resolution: ",sensresults$params$pixelWidth,"m, Neighbourhood sizes: ",paste(sensresults$params$lagsINmeters,collapse="m, "),"m"))
		dev.off.all()
}



	
# sensitivity analysis (MUST be run after "parameters real" were called!)
	resolutionsToTest<-list(pixelWidth*2,pixelWidth*3)
	lagsToTest<-list(lagsINmeters/2,lagsINmeters[-c(1:2)])
	make_and_plot_sensitivity_all(pixelWidth_multi=resolutionsToTest
								  ,lagsInMeters_multi=lagsToTest
								  ,sens.runs=2)







