dev.off.all<-function(){
	if(length(dev.list())!=0){
		dev.off()
		dev.off.all()
	}

}
plots<-function(results,prefix, synth_phasespace_plot_scales){

	# probability rasters:
		# combined
		dev.off.all()
		jpeg(paste0("./code/output/",prefix,"probraster_combined.jpeg",30.0*30,30.0*15))
		for( i in c(1:7)){
			print(i)
		localProbabilities(results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01)
		localProbabilities_binary(results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01,threshold=0.05)

		combined_data_raster_plot(results,i)
		}
		dev.off.all()

	# probability rasters:
		
		# # all
		# dev.off.all()
		# jpeg("./code/output/probraster_all.jpeg",30.0*30,30.0*15)
		# for( i in c(1:7)){
		# # plot(results$data$all[[i]],breaks=2,col=c("white","black"))
		# localProbabilities(results$entropies$all[[i]]$mrep,results$data$all[[i]],quants=0.01)
		# combined_data_raster_plot(results,i)
		# }
		# dev.off.all()
}


# plots_synthetic(results_synthetic,"synthetic")

plots_synthetic<-function(results,prefix){

#plotting patterns only:
jpeg(paste0("./code/output/",prefix,"synthetic_patterns.jpeg"),30.0*15*length(results$entropies),30.0*15)
par(mfrow=c(1,length(results$entropies)))
for( i in c(1:length(results$entropies)) ){
		plotr(results$data[[i]])
		}
dev.off.all()







#plotting patterns aggregated:
jpeg(paste0("./code/output/",prefix,"synthetic_patterns_aggregated.jpeg"),30.0*15*length(results$lags[[1]]),30.0*15*length(results$lags))
par(mfrow=c(length(results$lags),length(results$lags[[1]])))
for( i in c(1:length(results$lags)) ){
	for( j in c(1:length(results$lags[[1]])) ){

		plotr(results$lags[[i]][[j]], cols=gray.colors(100,start = 0.1, end = 1))
		}
	}
dev.off.all()


	# plotting phase spaces:
	jpeg(paste0("./code/output/",prefix,"synthetic_patterns_phasespace.jpeg"),100*3*length(results$lags[[1]]),100*3)
		par(mfrow=c(1,length(results$entropies)))
		par(mar=c(4,3,4,3))
		subs=paste0("pattern ",letters[1:10],")")
		for( i in c(1:length(results$entropies)) ){
			eqscplot(as.vector(results$lags[[i]][[synth_phasespace_plot_scales[1]]]),as.vector(results$lags[[i]][[synth_phasespace_plot_scales[2]]])
				,xlim=c(0,1),ylim=c(0,1)
				,pch='.',col=rgb(0,0,0,0.05)
				,ratio = 1
				,tol=0.04
				,xaxp  = c(0, 1, 1)
				,yaxp  = c(0, 1, 1)
				,cex=5
				,xlab=expression(x[i]^{1})
				,ylab=expression(x[i]^{2})
				,main=subs[i])

			abline(v=c(1:2)/3,col="grey",lwd=0.3)
			abline(h=c(1:2)/3,col="grey",lwd=0.3)
		}
		# 		for( i in c(1:length(results$entropies)) ){
		# 	plotr(results$data[[i]],cols=gray.colors(100,start = 0.1, end = 1))

		# }
			dev.off.all()



# plotting probability rasters:
jpeg(paste0("./code/output/",prefix,"probraster_combinedone.jpeg"),30.0*30,30.0*15)
for( i in c(1:length(results$entropies)) ){
		localProbabilities(results$entropies[[i]]$mrep,results$data[[i]],quants=0.01)
		plotr(results$data[[i]])
		}
		dev.off.all()


# plotting geospace zones:
jpeg(paste0("./code/output/",prefix,"geospace_zones.jpeg"),30.0*70,30.0*10)
plot_synthetic_geospace_zones(results)
dev.off.all()

# plotting geospace entropies
jpeg(paste0("./code/output/",prefix,"geospace_entropy.jpeg"),30.0*70,30.0*10)
par(mfrow=c(1,7))
lapply(results$geospace$ents,barplot,ylim=c(results$geospace$entsmin,results$geospace$entsmax))
dev.off.all()

# plotting geospace frequencies
jpeg(paste0("./code/output/",prefix,"geospace_frequency.jpeg"),30.0*70,30.0*10)
par(mfrow=c(1,7))
lapply(results$geospace$probs,function(x){barplot(sort(x),col="black",space=0.5)})
dev.off.all()


}


plot_combined<-function(results){
  plot(years,unlist(results$combined),type="n",ylim=c(0,7),main="Spatial Entropy Categories Combined",xlab="year",ylab="entropy",frame=F,pch=20,xlim=c(1875,2020)
       ,col="black")
  abline(h=seq(0,7,0.2),col="grey")
  lines(years,unlist(results$combined),type="b",ylim=c(0,7),main="Spatial Entropy Categories Combined",xlab="year",ylab="entropy",frame=F,pch=20
       ,col="black")
  text(years[7]+10,results$combined[7],labels=c("observed"),cex=0.5)
  #randoms
  # lines(years,randoms.mean[,"combined"],col="black",lty=2)
  # text(years[7]+10,randoms.mean[7,"combined"],labels=c("randomised"),cex=0.5)

}
dev.off.all()

localProbabilities<-function(mrep,rasterbase,quants,plot=T){
	globalprobs<-table(mrep)/sum(table(mrep))
	localprobs<-as.vector(globalprobs[as.character(mrep)])
	probsraster<-  rasterbase
  # probsraster[]<-localprobs
	probsraster[]<-matrix(log(as.numeric(localprobs)),nrow(rasterbase),ncol(rasterbase))
	# breaks<-c(quantile(localprobs[which(localprobs!=max(localprobs))],probs=seq(0,1,quants)),1)
	if(plot){
		breaks=seq(-15,0.01,quants)
		print(min(probsraster[]))
		# plopt(probsraster,breaks=breaks,
		col=colorRampPalette(c("black", "white"))(length(breaks))
		print("plot")
		plotr(probsraster,min(probsraster[])-0.0000001,max(probsraster[]),cols=c("red",col))
		plotr(probsraster,min(probsraster[])-0.0000001,max(probsraster[]),cols=c("red",col),legend=T)

	}
	return(probsraster)
}

localProbabilities_binary<-function(mrep,rasterbase,quants,plot=T,threshold=0.05){
	globalprobs<-table(mrep)/sum(table(mrep))
	localprobs<-as.vector(globalprobs[as.character(mrep)])
	localprobs<-as.numeric(localprobs)
	localprobs[which(localprobs <= quantile(localprobs,threshold))]<-0
	localprobs[which(localprobs !=0 )]<-1


	probsraster<-  rasterbase
  # probsraster[]<-localprobs
	probsraster[]<-matrix((as.numeric(localprobs)),nrow(rasterbase),ncol(rasterbase))
	# breaks<-c(quantile(localprobs[which(localprobs!=max(localprobs))],probs=seq(0,1,quants)),1)
	if(plot){
		# plot(probsraster,breaks=breaks,
		col=c("black", "white")
		print("plot")
		plotr(probsraster)

	}
	return(probsraster)
}




combined_data_raster_plot<-function(results,i){
	tests<-results$data$live[[i]]
	tests[which(results$data$leisure[[i]][]!=0)]<-2
	tests[which(results$data$work[[i]][]!=0)]<-3
	tests[]<-tests[]/4
	print(table(tests[]))
	plotr(tests,cols=c("white","red","black","cyan","orange"),breaks=c(-1,0.1,0.4,0.6,1000))
}

plot_synthetic_geospace_zones<-function(results){
# dev.new(width=length(results$data)*5, height=5)
par(mfrow=c(1,7))
lapply(results$geospace$aggregated,plotr,cols= gray.colors(100,start = 0.1, end = 1))
}
# test<-geo_phase_space(synthetic,30.0*64)
# par(mfrow=c(5,7))
# lapply(test$data,plotr,cols= gray.colors(100,start = 0.1, end = 1))
# lapply(test$agg,plotr,cols= gray.colors(100,start = 0.1, end = 1))
# lapply(test$probs,function(x){barplot(sort(x),col="black",space=0.5)})
# lapply(test$ents,barplot,ylim=c(test$entsmin,test$entsmax))




