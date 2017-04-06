

1.




order<-lapply(localp,function(x){order(x[])})
tops<-order[(length(order)-howmanytop):length(order)]
flops<-order[1:howmanyflop]
maxdistance<-(max(lags)-1)/2
	
test<-raster(matrix(rep(c(1:3),8),3,8)+matrix(c(1:24)/50,3,8))
test[]<-sample(test[])

test[]<-sample(test[])
test[9]






test[]
plot(test)
test[9]
testm<-t(as.matrix(test))
testm[9]
testv<-test[]
testv[9]
row(testm)[9]
image(testm)

dev.off.all()










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
		,xaxt="n")

	# x axis and vertical lines:
	axis(1, at=years, las=2)
	abline(v=years,col="grey",lwd=0.5)

# confidence intervals:
	 polygon(c(years,rev(years)),c(nullmod$compactmixed_many_confidence_intervals[1,],rev(nullmod$compactmixed_many_confidence_intervals[2,]))
	 	,col = rgb(0.5,0.5,0.5), border = FALSE)
	 polygon(c(years,rev(years)),c(nullmod$randomised_many_confidence_intervals[1,],rev(nullmod$randomised_many_confidence_intervals[2,]))
	 	,col = rgb(0.5,0.5,0.5), border = FALSE)
	 # segregated model doesn't have confidence intervals
	 # polygon(c(years,rev(years)),c(nullmod$compactsegregated_many_confidence_intervals[1,],rev(nullmod$compactsegregated_many_confidence_intervals[2,]))
	 # 	,col = rgb(0.5,0.5,0.5), border = FALSE)
	 lines(years, nullmod$compactmixed_many_entropies_mean, lwd = 2,col=rgb(0,0.5,0))
	 lines(years, nullmod$randomised_many_entropies_mean, lwd = 2,col=rgb(0.5,0,0))
	 lines(years, nullmod$compactsegregated_many_entropies_mean, lwd = 2,col=rgb(0,0,0.5))

	# many lines:
	# apply(nullmod$randomised_many_entropies,1,function(x){lines(years,x,col=rgb(1,0,0,0.4),lty=3)})
	# apply(nullmod$compactmixed_many_entropies,1,function(x){lines(years,x,col=rgb(0,1,0,0.4),lty=3)})

 	#add red lines on borders of polygon
 	#lines(years, nullmod$compactmixed_many_confidence_intervals[2,], col="red",lty=2)
 	#lines(years, nullmod$compactmixed_many_confidence_intervals[1,], col="red",lty=2)

# legend:
	 legend(1980,0
	 	,legend= c("observed","random spread","random compact","segregated","0.05 confidence")
	 	,col = c(rgb(0,0,0),rgb(0.5,0,0),rgb(0,0.5,0),rgb(0,0,0.5),rgb(0.5,0.5,0.5))
       ,border = NA
       ,lwd=c(rep(2,4),10)
       ,bty = "n"
       ,xjust = 0
       , yjust = 0)

}
abline(h=5)
plot_real_vs_null_entropies(results,nullmod)




# probability rasters:
plot_real_vs_null_probability_rasters<-function(results,nullmod){
par(mfrow=c(7,8),mar=c(0.5,0.5,0.5,0.5))
for(i in c(1:7)){
localProbabilities(nullmod$randomised_results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01)
combined_data_raster_plot(nullmod$randomised,i)

localProbabilities(nullmod$compactmixed_results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01)
combined_data_raster_plot(nullmod$compactmixed,i)

localProbabilities(nullmod$compactsegregated_results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01)
combined_data_raster_plot(nullmod$compactsegregated,i)

localProbabilities(results$entropies$combined[[i]]$mrep,results$data$all[[i]],quants=0.01)
combined_data_raster_plot(results$data,i)

}
}


plot_real_vs_null_probability_rasters(results,nullmod)


















% For a pattern observed at $S$ different scales, the discrete multiscale entropy $H_m$ is given as

% \begin{equation}
% H_m = \sum_{1}^{N}{
% 	p(x_1,x_2, \ldots,x_S)
% 	\log{ p(x_1,x_2, \ldots,x_S)
% 		}
% 	}
% \end{equation} 
% where $p(x_1, x_2, \ldots, x_S)$ is the joint probability to observe a place with a mean value $x_1$ inside an area with mean value $x_2$ and so on up to $x_S$. $N$ is the number of unique values and depends on how values are discretised. For $C$ different characteristics c, this extends to

% \begin{equation}
%  H_m = \sum_{1}^{N}\sum_{1}^{C}{
% 	p(
% 		x_{1,c},x_{2,c}, \ldots,x_{S,c}
% 	) 
% 	\log{
% 			p(
% 				x_{1,c},x_{2,c}, \ldots,x_{S,c}
% 			)
% 		}
% 		}
% \end{equation} 







 We assume that the state of each place at a particular scale and a particular characteristic can only take a finite number of values. Denote $x_i^d$ the state of a place $x_i$ at scale $d$ considering all characteristics. Then, the state of each place at all scales can be fully characterized by a word $$x_i^1x_i^2\cdots x_i^n.$$

% Denote $\mathcal{A}$ the alphabet containing all the combinations of states forming words of the form $a^1a^2\cdots a^n$. It describes all the possible states a place in the system can take. Consider in particular $\mathcal{A}^\star$ the subset of $\mathcal{A}$ formed by all words $a$ such that there exists at least one $x_i\in\mathcal{S}$ for which $x_i^1x_i^2\cdots x_i^n=a^1a^2\cdots a^n$.

% Finally, consider the random variable $$X(a)=\log(1/p(a)),$$ where $p$ is probability of observing a state $a\in\mathcal{A}^\star$ in the system $\mathcal{S}$. Then, the multiscale entropy is defined as
% $$H_m:=\mathbb{E}(X)=-\sum_{a\in\mathcal{A}^\star}p(a)\log\left(p(a)\right).$$





########################################################
########################################################
########################################################
results$entropies$combined



par(mfrow=c(1,1))

	par(mfrow=c(1,1))




	plot((data$work[[7]]))
	plot((data$live[[7]]))
	plot((data$leisur[[7]]))

	dev.off()

localProbabilities
	pdf("./output/localprobs.pdf")

	for( i in c(1:7)){
	localProbabilities(results$entropies$all[[i]]$mrep,results$data$all[[i]],quants=0.2)
	}
	dev.off()

?rainbow

    plot(1:20,1:20,col=rainbow(20,end=0.5))
str(results$entropies[[1]])
1
results$entropies$combined[[i]]$mrep
str(results$entropies$combined)

results$data$all[[i]]

results$entropies$combined[[i]]$mrep
names(results$entropies)

for(i in 1:7){
print(results$entropies$combined[[i]]$entropy-(results$entropies$live[[i]]$entropy+results$entropies$work[[i]]$entropy+results$entropies$leisure[[i]]$entropy)
)
}

localProbabilities(results_synthetic[[1]],plot=T)
localProbabilities
names(results_synthetic)
results_synthetic$entropies[[1]]
par(mfrow=c(1,length(results_synthetic$data)))
pdf("./code/output/results_synthetic_probrasters.pdf",15,15)
for(i in c(1:length(results_synthetic$entropies))){
localProbabilities(results_synthetic$entropies[[i]]$mrep,results_synthetic$data[[i]],quants=0.2)
plotr(results_synthetic$data[[i]])
}

results_synthetic

dev.off()
getwd()
warnings()
results_synthetic$entropies[[1]]$mrep

localProbabilities







