dev.off.all<-function(){
	if(length(dev.list())!=0){
		dev.off()
		dev.off.all()
	}

}
plots<-function(results,nullmod,prefix, synth_phasespace_plot_scales){

	# probability rasters:
		# combined
		dev.off.all()
		# pdf(paste0("./code/output/",prefix,"probraster_combined.pdf"),30,15)
		# for( i in c(1:7)){
		# 	print(i)
		# localProbabilities(results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01)
		# # localProbabilities_binary(results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01,threshold=0.05)

		# print("combined")
		# combined_data_raster_plot(results$data,i)
		# }
		# dev.off.all()


	# entropies over time: real vs. null:
		pdf(paste0("./code/output/",prefix,"real_vs_null_entropies.pdf"),10,10)
		plot_real_vs_null_entropies(results,nullmod)
		dev.off.all()
		jpeg(paste0("./code/output/",prefix,"real_vs_null_entropies.jpg"),10,10,res=300,units = 'in')
		plot_real_vs_null_entropies(results,nullmod)
		dev.off.all()
	# probability rasters and regular rasters vor real and null:
		pdf(paste0("./code/output/",prefix,"real_vs_null_rasters.pdf"),5*8*2,5*7)
		plot_real_vs_null_probability_rasters(results,nullmod,log=F)
		dev.off.all()
		jpeg(paste0("./code/output/",prefix,"real_vs_null_rasters.jpg"),5*8*2,5*7,res=100,units = 'in')
		plot_real_vs_null_probability_rasters(results,nullmod)
		dev.off.all()


	# plotting most unique and common places:

		# for(year in c(1:length(years)) ){
		# 	pdf(paste0("./code/output/",prefix,"most_unique",years[year],".pdf"),10,length(lags)*5)
		# 	plot_top_entropy(results,10,0,whichyears=year)
		# 	dev.off.all()
		# 	jpeg(paste0("./code/output/",prefix,"most_unique",years[year],".jpg"),10,length(lags)*5,res=100,units = 'in')
		# 	plot_top_entropy(results,10,0,whichyears=year)
		# 	dev.off.all()
		# }


}

plots_synthetic<-function(results,prefix,synthetic_many_results){

#plotting patterns only:
pdf(paste0("./code/output/",prefix,"synthetic_patterns.pdf"),15*length(results$entropies),17)
par(mfrow=c(1,length(results$entropies))
	,mai=c(2,0,0,0))
for( i in c(1:length(results$entropies)) ){
		plotr(results$data[[i]],cols=gray.colors(500, start = 0, end = 0.95))
		title(main = NULL, sub = paste0(letters[i],") ", synthetic_names[i])
			,cex.sub=10
			,outer=FALSE
			,line=+10)
		}
dev.off.all()

jpeg(paste0("./code/output/",prefix,"synthetic_patterns.jpg"),15*length(results$entropies),17,res=100,units = 'in')
par(mfrow=c(1,length(results$entropies))
	,mai=c(2,0,0,0))
for( i in c(1:length(results$entropies)) ){
		plotr(results$data[[i]],cols=gray.colors(500, start = 0, end = 0.95))
		title(main = NULL, sub = paste0(letters[i],") ", synthetic_names[i])
			,cex.sub=10
			,outer=FALSE
			,line=+10)
		}
dev.off.all()







#plotting patterns aggregated:
pdf(paste0("./code/output/",prefix,"synthetic_patterns_aggregated.pdf"),15*length(results$lags[[1]]),15*length(results$lags))
par(mfrow=c(length(results$lags),length(results$lags[[1]])))
for( i in c(1:length(results$lags)) ){
	for( j in c(1:length(results$lags[[1]])) ){

		plotr(results$lags[[i]][[j]], cols=gray.colors(100,start = 0.1, end = 1))
		}
	}
dev.off.all()

jpeg(paste0("./code/output/",prefix,"synthetic_patterns_aggregated.jpg"),15*length(results$lags[[1]]),15*length(results$lags),res=100,units = 'in')
par(mfrow=c(length(results$lags),length(results$lags[[1]])))
for( i in c(1:length(results$lags)) ){
	for( j in c(1:length(results$lags[[1]])) ){

		plotr(results$lags[[i]][[j]], cols=gray.colors(100,start = 0.1, end = 1))
		}
	}
dev.off.all()


	# plotting phase spaces:
	pdf(paste0("./code/output/",prefix,"synthetic_patterns_phasespace.pdf"),3*length(results$lags[[1]]),3)
		par(mfrow=c(1,length(results$entropies)))
		par(mar=c(4,3,4,3))
		subs=paste0("pattern ",letters[1:10],")")
		for( i in c(1:length(results$entropies)) ){
			eqscplot(as.vector(results$lags[[i]][[synth_phasespace_plot_scales[1]]]),as.vector(results$lags[[i]][[synth_phasespace_plot_scales[2]]])
				,xlim=c(0,1),ylim=c(0,1)
				,pch='.',col=rgb(0,0,0,0.01)
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
	dev.off.all()


	jpeg(paste0("./code/output/",prefix,"synthetic_patterns_phasespace.jpg"),3*length(results$lags[[1]]),3,res=100,units = 'in')
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
	dev.off.all()

# plotting probability rasters:
# pdf(paste0("./code/output/",prefix,"probraster_combinedone.pdf"),30,15)
# for( i in c(1:length(results$entropies)) ){
# 		localProbabilities(results$entropies[[i]]$mrep,results$data[[i]],quants=0.01)
# 		plotr(results$data[[i]])
# 		}
# 		dev.off.all()


# plotting geospace zones:
pdf(paste0("./code/output/",prefix,"geospace_zones.pdf"),70,10)
plot_synthetic_geospace_zones(results)
dev.off.all()

jpeg(paste0("./code/output/",prefix,"geospace_zones.jpg"),70,10,res=100,units = 'in')
plot_synthetic_geospace_zones(results)
dev.off.all()


# plotting geospace entropies
pdf(paste0("./code/output/",prefix,"geospace_entropy.pdf"),10,10)
# par(mfrow=c(1,1))
# mars<-par("mar")
# par(mar=c(mars[1]+1,mars[2:4]))
# mp<-barplot(unlist(results_synthetic$geospace$ents)
# 	# ,ylim=c(results$geospace$entsmin,results$geospace$entsmax)
# 	,col="black"
# 	,ylab="entropy"
# 	,las=2
# 	# ,axisnames = FALSE
# 	)
# text((c(1:7)*1.2)-0.5,0, labels = synthetic_names, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
plot_synthetic_entropies_geospace_points_w_confidence(synthetic_many_results)

dev.off.all()

jpeg(paste0("./code/output/",prefix,"geospace_entropy.jpg"),10,10,res=100,units = 'in')
# par(mfrow=c(1,1))
# par("mar")
# mars<-par("mar")
# par(mar=c(mars[1]+1,mars[2:4]))
# par(mar=c(12,12,12,12))
# mp<-barplot(unlist(results_synthetic$geospace$ents)
# 	# ,ylim=c(results$geospace$entsmin,results$geospace$entsmax)
# 	,col="black"
# 	,ylab="entropy"
# 	,las=2
# 	# ,axisnames = FALSE
# 	)
# text((c(1:7)*1.2)-0.5,0, labels = synthetic_names, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
plot_synthetic_entropies_geospace_points_w_confidence(synthetic_many_results)
dev.off.all()

# plotting geospace frequencies
pdf(paste0("./code/output/",prefix,"geospace_frequency.pdf"),21,3)
par(mfrow=c(1,7))
for (i in c(1:7)) {
	barplot(sort(results$geospace$probs[[i]]),col="black",space=0.5)
		title(main = NULL, sub = paste0(letters[i],") ", synthetic_names[i])
		,cex.sub=2
		,outer=FALSE
		,line=+2)
	
}
dev.off.all()
jpeg(paste0("./code/output/",prefix,"geospace_frequency.jpg"),21,3,res=100,units = 'in')
par(mfrow=c(1,7))
lapply(results$geospace$probs,function(x){barplot(sort(x),col="black",space=0.5)})
dev.off.all()

pdf(paste0("./code/output/",prefix,"boxplot.pdf"),10,10)
plot_synthetic_entropies_points_w_confidence(synthetic_many_results)
dev.off.all()
jpeg(paste0("./code/output/",prefix,"boxplot.jpg"),10,10,res=100,units = 'in')
plot_synthetic_entropies_points_w_confidence(synthetic_many_results)
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
dev.off.all()

}

localProbabilities<-function(mrep,rasterbase,quants,plot=T,log=T,ylab=""){
	globalprobs<-table(mrep)/sum(table(mrep))
	localprobs<-as.vector(globalprobs[as.character(mrep)])
	probsraster<-  rasterbase
	if(log){
	probsraster[]<-matrix(log(as.numeric(localprobs)),nrow(rasterbase),ncol(rasterbase))
    }else{
    probsraster[]<-matrix((as.numeric(localprobs)),nrow(rasterbase),ncol(rasterbase))
    }
	# breaks<-c(quantile(localprobs[which(localprobs!=max(localprobs))],probs=seq(0,1,quants)),1)
	if(plot){
		if(log){
		breaks=seq(-15,0.01,quants)
	}else{
		breaks=seq(0,1,quants)

	}
		print(min(probsraster[]))
		# plopt(probsraster,breaks=breaks,
		col=colorRampPalette(c("black", "white"))(length(breaks))
		print("plot")
		
		# if there's a problem, it might be because breaks=seq(-15 .. is higher than lowest value
		# probabilities are plotted as the log, so values theoretically up to -inf
		# in that case, return to this:
		
		# plotr(probsraster,min(probsraster[])-0.0000001,max(probsraster[]),cols=c(col))
		# plotr(probsraster,min(probsraster[])-0.0000001,max(probsraster[]),cols=c(col),legend=T)
		if(log){
		plotr(probsraster,-15,1,cols=c(col),ylab=ylab)
		plotr(probsraster,-15,1,cols=c(col),legend=T,ylab=ylab)
		}else{
		plotr(probsraster,0,1,cols=c(col),ylab=ylab)
		plotr(probsraster,0,1,cols=c(col),legend=T,ylab=ylab)			
		}

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


combined_data_raster_plot<-function(data,i,cropextend=NA){
	tests<-data$live[[i]]
	tests[which(data$leisure[[i]][]!=0)]<-2
	tests[which(data$work[[i]][]!=0)]<-3
	tests[]<-tests[]/4
	if(!is.na(cropextend[1])){
	tests<-crop(tests, extent(tests, cropextend[1], cropextend[2], cropextend[3], cropextend[4]))	
	}
	plotr(tests,cols=c(rgb(0.9,0.9,0.9),"red","black","cyan","orange"),breaks=c(-1,0.1,0.4,0.6,1000))
}

plot_synthetic_geospace_zones<-function(results){
# dev.new(width=length(results$data)*5, height=5)
par(mfrow=c(1,7))
lapply(results$geospace$aggregated,plotr,cols= gray.colors(100,start = 0.1, end = 1))
}
# test<-geo_phase_space(synthetic,64)
# par(mfrow=c(5,7))
# lapply(test$data,plotr,cols= gray.colors(100,start = 0.1, end = 1))
# lapply(test$agg,plotr,cols= gray.colors(100,start = 0.1, end = 1))
# lapply(test$probs,function(x){barplot(sort(x),col="black",space=0.5)})
# lapply(test$ents,barplot,ylim=c(test$entsmin,test$entsmax))












# plotting nullmodel and data results:::
plot_real_vs_null_entropies<-function(results,nullmod){
	par(mfrow=c(1,1))
	maxvalue<-max(unlist(lapply(
					results$entropies$combined
					,function(x){x$entropy})))
	minvalue<-0
	# real:
	plot(years,unlist(lapply(
					results$entropies$combined
					,function(x){x$entropy}))
		,type="l",ylim=c(minvalue,maxvalue)
		,xlab="year"
		,ylab="multiscale entropy"
		,xaxt="n"
		,lwd = 2)

	# x axis and vertical lines:
	axis(1, at=years, las=2)
	abline(v=years,col="grey",lwd=0.5)
# confidence intervals:
	 polygon(c(years,rev(years)),c(nullmod$compactmixed_many_confidence_intervals[1,],rev(nullmod$compactmixed_many_confidence_intervals[2,]))
	 	,col = rgb(0,0,0,0.3), border = FALSE)
	 polygon(c(years,rev(years)),c(nullmod$randomised_many_confidence_intervals[1,],rev(nullmod$randomised_many_confidence_intervals[2,]))
	 	,col = rgb(0,0,0,0.3), border = FALSE)
	 # segregated model doesn't have confidence intervals
	 # polygon(c(years,rev(years)),c(nullmod$compactsegregated_many_confidence_intervals[1,],rev(nullmod$compactsegregated_many_confidence_intervals[2,]))
	 # 	,col = rgb(0.5,0.5,0.5), border = FALSE)
	 lines(years, nullmod$compactmixed_many_entropies_mean, lwd = 2,col=rgb(0,1,0),type="b",pch=20)
	 lines(years, nullmod$randomised_many_entropies_mean, lwd = 2,col=rgb(1,0,0),type="b",pch=20)
	 lines(years, nullmod$compactsegregated_many_entropies_mean, lwd = 2,col=rgb(0,0,1),type="b",pch=20)
	 lines(years, nullmod$compactsegregated_many_entropies_mean, lwd = 2,col=rgb(0,0,1),type="b",pch=20)
	 lines(years,results$entropies$aspatial$entropies,col="black",lty=3,type="b",pch=20)
	# many lines:
	# apply(nullmod$randomised_many_entropies,1,function(x){lines(years,x,col=rgb(1,0,0,0.4),lty=3)})
	# apply(nullmod$compactmixed_many_entropies,1,function(x){lines(years,x,col=rgb(0,1,0,0.4),lty=3)})

 	#add red lines on borders of polygon
 	#lines(years, nullmod$compactmixed_many_confidence_intervals[2,], col="red",lty=2)
 	#lines(years, nullmod$compactmixed_many_confidence_intervals[1,], col="red",lty=2)

# legend:
	 legend(1980,0
	 	,legend= c("observed","random spread","random compact","segregated","non spatial","0.05 confidence")
	 	,col = c(rgb(0,0,0),rgb(1,0,0),rgb(0,1,0),rgb(0,0,1),rgb(0,0,0),rgb(0,0,0,0.3))
       ,border = NA
       ,lwd=c(rep(2,5),10)
       ,lty=c(rep(1,4),3,1)
       ,bty = "n"
       ,xjust = 0
       , yjust = 0)

}
dev.off.all()
# probability rasters:
plot_real_vs_null_probability_rasters<-function(results,nullmod,log=T){
	par(mfrow=c(7,8),mar=c(0.5,0.5,0.5,0.5)
		,oma=c(0,10,10,0))

  		par(xpd=NA)
	for(i in c(1:7)){
	localProbabilities(nullmod$randomised_results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01,log=log)
mtext(years[i],2)
	if(i==1){title("randomised spatially",cex=10)}
	combined_data_raster_plot(nullmod$randomised,i)
mtext(years[i],2)

	localProbabilities(nullmod$compactmixed_results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01,log=log)
	if(i==1){title("compact mixed",cex=10)}
	combined_data_raster_plot(nullmod$compactmixed,i)
mtext(years[i],2)

	localProbabilities(nullmod$compactsegregated_results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01,log=log)
	if(i==1){title("compact segregated",cex=10)}
	combined_data_raster_plot(nullmod$compactsegregated,i)
	
	localProbabilities(results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01,log=log)
	if(i==1){title("original data",cex=10)}
	combined_data_raster_plot(results$data,i)


	}

}
?text
plot_top_entropy <-function(results,howmanytop=5,howmanyflop=5,whichyears=c(1:7) ){
	distances<-(lags-1)/2

	if(length(whichyears)>1) {
	print("rows are years")
	par(mfrow=c(length(whichyears),((length(lags)*(howmanytop+howmanyflop)))))
	print(c(length(whichyears),((length(lags)*(howmanytop+howmanyflop)))))
	}else{
	par(mfrow=c((howmanytop+howmanyflop),length(lags)))
		print(c(length(lags),(howmanytop+howmanyflop)))
	}
	for(i in whichyears){
		localp<-lapply(results$entropies$combined,function(x){return(localProbabilities(x$mrep,results$data$all[[1]],quants=0.01,plot=F,log=T))})
		nrow(results$data$all[[1]])
		# vectors with probability ranks
		order<-lapply(localp,function(x){order(x[])})
		order<-order(localp[[i]][])
		str(order)
		all_order_xys<-raster_xy_from_vector_id(localp[[i]],order)
		is_fully_covered<-apply(all_order_xys,1,function(x){
		return(
		(x[1]>max(distances)) &&
		(x[1]< nrow(localp[[i]])-max(distances)) &&
		(x[2]>max(distances)) &&
		(x[2]< ncol(localp[[i]])-max(distances))
		)
		})
		order<-order[is_fully_covered]
		tops<-head(order,1000)
		flops<-tail(order,1000)
		tops<-sample(tops,howmanytop)
		flops<-sample(flops,howmanyflop)

		xytop<-raster_xy_from_vector_id(localp[[i]],tops)
		xyflop<-raster_xy_from_vector_id(localp[[i]],flops)

		if(howmanytop>0){
		for(t in c(1:howmanytop)){
			for(d in distances){
				maxdistance<-d
				topneighbourhoods<-apply(xytop,1,function(xy){
				return(
				c(xy[1]-maxdistance
				 ,xy[1]+maxdistance
				 ,xy[2]-maxdistance
				 ,xy[2]+maxdistance)
				)
				})
				combined_data_raster_plot(results$data,i,topneighbourhoods[,t])
			}
		}
		}
		if(howmanyflop>0){
		for(f in c(1:howmanyflop)){
			for(d in distances){
				maxdistance<-d
				flopneighbourhoods<-apply(xyflop,1,function(xy){
					return(
					c(xy[1]-maxdistance
					 ,xy[1]+maxdistance
					 ,xy[2]-maxdistance
					 ,xy[2]+maxdistance)
					)
					})
				combined_data_raster_plot(results$data,i,flopneighbourhoods[,f])
			}
		}
		}
		
	}
}



plot_synthetic_entropies_barchart_w_confidence<-function(synthetic_many_results){
synthetic_many_results$entropies
barplot(synthetic_many_results$entropies_mean,col="black")
text((c(1:7)*1.2)-0.5,0, labels = synthetic_names, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
synthetic_many_results$entropies[,]
boxplot(x = t(synthetic_many_results$entropies[,]),data=t(synthetic_many_results$entropies), main="",
        xlab="",
        names=  paste(letters[1:7],")",c(" uniform"," segregated:\n32*32 cells", " segregated\n8*8 cells"," segregated:\n2*2 cells"," sorted"," 1/f noise"," additive\ncascade"),sep="")
        ,ylab="multiscale entropy",
        outline = F,
        range=0,
        col="black",
        border="black"
        ,box="n"
        ,boxwex=0.3
        ,las=2
        )

}
plot_synthetic_entropies_geospace_barchart_w_confidence<-function(synthetic_many_results){
barplot(synthetic_many_results$entropies_geospace_mean,col="black")
text((c(1:7)*1.2)-0.5,0, labels = synthetic_names, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)

boxplot(x = t(synthetic_many_results$entropies_geospace[,]),data=t(synthetic_many_results$entropies_geospace), main="",
        xlab="",
        names=  paste(letters[1:7],")",c(" uniform"," segregated:\n32*32 cells", " segregated\n8*8 cells"," segregated:\n2*2 cells"," sorted"," 1/f noise"," additive\ncascade"),sep="")
        ,ylab="multiscale entropy",
        outline = F,
        range=0,
        col="black",
        border="black"
        ,box="n"
        ,boxwex=0.3
        ,las=2
        )

}


plot_synthetic_entropies_geospace_points_w_confidence<-function(synthetic_many_results){
plot(synthetic_many_results$entropies_geospace_mean,xaxt="n",pch=1,cex=1
	,ylim= c( min(synthetic_many_results$entropies_geospace_confidence_intervals)
			  ,max(synthetic_many_results$entropies_geospace_confidence_intervals))
			,xlab="n"
				,ylab="Enropy (geographical phase space)")
axis(1,at=1:length(synthetic_many_results$entropies_geospace_mean),labels=synthetic_names,las=2
	)


segments(
	c(1:length(synthetic_many_results$entropies_geospace_mean)),
	synthetic_many_results$entropies_geospace_confidence_intervals[1,],
	c(1:length(synthetic_many_results$entropies_geospace_mean)),
	synthetic_many_results$entropies_geospace_confidence_intervals[2,]
	,lwd=1)




}



plot_synthetic_entropies_points_w_confidence<-function(synthetic_many_results){
plot(synthetic_many_results$entropies_mean,xaxt="n",pch=1,cex=1
	,ylim= c( min(synthetic_many_results$entropies_confidence_intervals)
			  ,max(synthetic_many_results$entropies_confidence_intervals))
	,xlab="n"
	,ylab="Enropy (multiscale phase space)")
axis(1,at=1:length(synthetic_many_results$entropies_mean),labels=synthetic_names,las=2)

segments(
	c(1:length(synthetic_many_results$entropies_mean)),
	synthetic_many_results$entropies_confidence_intervals[1,],
	c(1:length(synthetic_many_results$entropies_mean)),
	synthetic_many_results$entropies_confidence_intervals[2,]
	,lwd=1)

}
