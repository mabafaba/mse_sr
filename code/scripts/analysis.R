analysis<-function(data,bins=3){
	print("lags")
	lags_live<-lapply(data$live,function(x){return(specifiedlags_matrix(x,lags))})
	lags_work<-lapply(data$work,function(x){return(specifiedlags_matrix(x,lags))})
	lags_leisure<-lapply(data$leisure,function(x){return(specifiedlags_matrix(x,lags))})
	# lags_all<-lapply(data$all,function(x){return(specifiedlags_matrix(x,lags))})
  # print("entropies")
	# e_live<-labipply(lags_live,raster_entropy_reps)
	# e_work<-lapply(lags_work,raster_entropy_reps)
	# e_leisure<-lapply(lags_leisure,raster_entropy_reps)
	# e_all<-lapply(lags_all,raster_entropy_reps)
	print("combined")
	e_combined<-list()
	for(i in c(1:length(lags_live))){
		e_combined<-c(e_combined,
		  	list(multiraster_entropy_reps(
		    list(lags_live[[i]],lags_work[[i]],lags_leisure[[i]])
		    ,bins=bins)
	  	)
	  )
	}

  e_aspatial<-aspatial_entropy(data)

	results<-list(
		data=data,
		lags=list(
			live=lags_live,
			work=lags_work,
			leisure=lags_leisure
			#, all=lags_all
      )
		,
		entropies=list(
			#live=e_live,
			#work=e_work,
			#leisure=e_leisure,
			#all=e_all,
			combined=e_combined
     ,aspatial=e_aspatial
			)
		
		)


	print("return")
	return(results)
}



analysis_null<-function(data,runs=1,bins=3){
# matrices to store results
randomised_many<-matrix(NA,runs,7)
compactmixed_many<-matrix(NA,runs,7)
# do many times:
for (i in c(1:runs)) {
# track progress:
  print(paste(i,"/",runs))
# randomise data
randomised<-randomise_spatially(data)
compactmixed<-compact_mixed_use(data)

# analyse randomised data
randomised_results<-analysis(randomised,bins)
compactmixed_results<-analysis(compactmixed,bins)

# pull out entropy only:
randomised_many[i,]<-unlist(lapply(randomised_results$entropies$combined,function(x){return(x$entropy)}))
compactmixed_many[i,]<-unlist(lapply(compactmixed_results$entropies$combined,function(x){return(x$entropy)}))

# confidence_intervals:
randomised_many_confidence_intervals<-apply(randomised_many,2,confidence_intervals)
compactmixed_many_confidence_intervals<-apply(compactmixed_many,2,confidence_intervals)
}
compactsegregated_many<-matrix(NA,1,7)
compactsegregated<-compact_segregated(data)
compactsegregated_results<-analysis(compactsegregated,bins)
compactsegregated_many[1,]<-unlist(lapply(compactsegregated_results$entropies$combined,function(x){return(x$entropy)}))
compactsegregated_many_confidence_intervals<-NA

return(list(    
   randomised=randomised
  ,randomised_results=randomised_results
  ,randomised_many_entropies=randomised_many
  ,randomised_many_entropies_mean = colMeans(randomised_many)
  ,randomised_many_confidence_intervals = randomised_many_confidence_intervals

  ,compactmixed=compactmixed  
  ,compactmixed_results=compactmixed_results  
  ,compactmixed_many_entropies=compactmixed_many
  ,compactmixed_many_entropies_mean = colMeans(compactmixed_many)
  ,compactmixed_many_confidence_intervals = compactmixed_many_confidence_intervals

  ,compactsegregated=compactsegregated  
  ,compactsegregated_results=compactsegregated_results  
  ,compactsegregated_many_entropies=compactsegregated_many
  ,compactsegregated_many_entropies_mean = colMeans(compactsegregated_many)
  ,compactsegregated_many_confidence_intervals = compactsegregated_many_confidence_intervals

  ,runs=runs)
)
}


analysis_synthetic<-function(data,lags,geospace_aggregationfactor){
  # AGGREGATE PATTERNS
  print("analysis synthetic")
  all_lags_synthetic<-lapply(data,function(x){return(specifiedlags_matrix(x,lags))})
  all_es_synthetic<-lapply(all_lags_synthetic,raster_entropy_reps)
  geospace<-geo_phase_space(data,geospace_aggregationfactor)
  results<-list(
    data=data,
    lags=all_lags_synthetic,
    entropies=all_es_synthetic,
    geospace=geospace
    )
  return(results)
}
analysis_synthetic_many<-function(runs_synthetic){
  synthetic_many<-lapply(rep(syntheticsize,runs_synthetic),make_data)
  results_synthetic_many<-lapply(synthetic_many,analysis_synthetic,lags=lags,geospace_aggregationfactor=geospace_aggregationfactor)

  entropies_many<-do.call(cbind,lapply(results_synthetic_many,function(x){
            unlist(lapply(x$entropies,function(x){x$entropy}))
            }),
          )

  entropies_many_mean<-rowMeans(entropies_many)
  entropies_confidence_intervals<-apply(entropies_many,1,confidence_intervals)

  entropies_geospace_many<-do.call(cbind,lapply(results_synthetic_many,function(x){
            unlist(x$geospace$ents)
            }),
          )

  entropies_geospace_many_mean<-rowMeans(entropies_geospace_many)
  entropies_geospace_confidence_intervals<-apply(entropies_geospace_many,1,confidence_intervals)


  return(
    list(
    entropies=entropies_many
    ,entropies_mean=entropies_many_mean
    ,entropies_confidence_intervals=entropies_confidence_intervals

    ,entropies_geospace=entropies_geospace_many
    ,entropies_geospace_mean=entropies_geospace_many_mean
    ,entropies_geospace_confidence_intervals=entropies_geospace_confidence_intervals
    )
  )
}






rasterlist2all_lags<-function (rasterlist,lags){
return(lapply(rasterlist,function(x){return(specifiedlags_matrix(x,lags))}))
}





analysis_synthetic








specifiedlags_matrix<-function(m,lags){
  return(lapply(lags,function(x){
    return(aggregate_pattern_average_matrix(m,x))
    
  }))
}


aggregate_pattern_average_matrix<-function(pattern,lag,fixNas=T){

  if(lag>1){
  howManyValuesAdded<-pattern
  pattern<-focal(pattern,w=matrix(1,lag,lag),pad=T,padValue=0)
  howManyValuesAdded[]<-1
  howManyValuesAdded<-focal(howManyValuesAdded,w=matrix(1,lag,lag),pad=T,padValue=0)
  pattern<-pattern/howManyValuesAdded
  }
  return(pattern)
}

raster_entropy_reps<-function(raster,bins=3,returnProbabilityRaster=T){
  mrep<-raster2rep(raster,bins)
  mrep<-cbind("__",mrep,"__")
  #remove NAs:
  mrep<-mrep[which(apply(mrep,1,function(x){!any(is.na(x))})),]
  # each row factors
  mrep<-as.factor(unlist(lapply(apply(mrep,MARGIN=1,as.list),function(x){do.call(paste,rev(x))})))
  # probabilites
#   print(table(mrep))
  mprobs<-table(mrep)/sum(table(mrep))
  mentropy<-probs2entropy(mprobs)
  if(!returnProbabilityRaster){
  return(mentropy)
  }else{
    return(list(entropy=mentropy,mrep=mrep,mprobs=mprobs))
  }
}

multiraster_entropy_reps<- function(rasters,bins=3,returnProbabilityRaster=T){
  mrep1<-raster2rep(rasters[[1]],bins=bins)
  mrep2<-raster2rep(rasters[[2]],bins=bins)
  mrep3<-raster2rep(rasters[[3]],bins=bins)
  mrep<-cbind("__",mrep1,"//",mrep2,"//",mrep3,"__")
  #remove NAs:
  mrep<-mrep[which(apply(mrep,1,function(x){!any(is.na(x))})),]
  # each row factors
  mrep<-as.factor(unlist(lapply(apply(mrep,MARGIN=1,as.list),function(x){do.call(paste,rev(x))})))
  # probabilites
#   print(table(mrep))
  mprobs<-table(mrep)/sum(table(mrep))
  mentropy<-probs2entropy(mprobs)
if(!returnProbabilityRaster){
  return(mentropy)
}else{
  return(list(entropy=mentropy,mrep=mrep,mprobs=mprobs))
}
}



raster2rep<-function(raster,bins=3){
  # make 3d matrix
  m<-abind(lapply(raster,as.matrix),along=3)
  # prepare for binning: remove rounding errors
  m[which(m>1 & m < 1.0001)]<-1
  # bin the values
  m[,,]<-bindata(m,bins)
  # make reps
  mrep<-apply(m,MARGIN=c(3),c)
  return(mrep)
}

bindata<-function(data,bins){
  
  if(any((data[which(!is.na(data))]>1.0001))){
  warning("values > 1.0001 changed to 1 in bindata:")
  }
  data[which(data>=1)]<-1.0
  h<-cut(data,breaks=c(-1,seq(0,1,1/bins)[-1]),labels = FALSE)
  if(length(which(is.na(data)))<length(which(is.na(h)))){
    warning("bindata recieved input < 0 or > 1.0001. binning produced NAs, results wrong.")}
  return(h) 
}

probs2entropy<-function(probs){
  return(-sum(probs*log2(probs)))
}



geo_phase_space<-function(data,aggregationfactor,fun=mean){
  # aggregate
  agg<-lapply(data,function(x){return(aggregate(x,fact=aggregationfactor,fun=fun))})
  # as vector
  aggv<-lapply(agg,as.vector)
  # probabilities
  aggprobs<-lapply(aggv,function(x){x/sum(x)})
  # remove 0 probabilities
  aggprobsNoZero<-lapply(aggprobs,function(x){x[which(x!=0)]})
  # entropies
  ents<-lapply(aggprobsNoZero,probs2entropy)
  entsmin<-min(unlist(ents))
  entsmax<-max(unlist(ents))
  return(list(data=data,aggregated=agg,probs=aggprobs,ents=ents,entsmin=entsmin,entsmax=entsmax))
}

confidence_intervals<-function(data,confidence=0.05){
a <- mean(data)
s <- sd(data)
n <- length(data)
confidence<-1-(confidence/2)
error <- qnorm(confidence)*s/sqrt(n)
left <- a-error
right <- a+error
return(c(left,right))
}



aspatial_entropy<-function(data){
workcount<-unlist(lapply(data[["work"]],function(x){length(which(x[]==1))}))
livecount<-unlist(lapply(data[["live"]],function(x){length(which(x[]==1))}))
leisurecount<-unlist(lapply(data[["leisure"]],function(x){length(which(x[]==1))}))
totalcount<- unlist(lapply(data[["live"]],function(x){length(x[])}))
emptycount<- totalcount - workcount - leisurecount - livecount
emptycount
cbind(livecount,workcount,leisurecount,emptycount)
counts<-cbind(livecount,workcount,leisurecount,emptycount)
totalcount_noempty<-rowSums(counts[,1:3])
totalcount_noempty
livecount/totalcount_noempty

probs<-cbind(livecount/totalcount,workcount/totalcount,leisurecount/totalcount,emptycount/totalcount)
probs_noempty<-cbind(livecount/totalcount_noempty,workcount/totalcount_noempty,leisurecount/totalcount_noempty)
entropies<-apply(probs,1,probs2entropy)
entropies_noempty<-apply(probs_noempty,1,probs2entropy)
return(list(
counts= counts
,totalcount=totalcount
,totalcount_noempty=totalcount_noempty
,probs=probs
,probs_noempty=probs_noempty
,entropies=entropies
,entropies_noempty=entropies_noempty
  ))
}

# plot(years,entropies,type="l")
# lines(years,entropies_noempty,type="l")





