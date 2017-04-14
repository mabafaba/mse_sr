make_data<-function(width){
return(makeallsynthetic_random(width))
}




makeallsynthetic_random<-function(width){
#   unif<-(raster(matrix(runif(width^2),width,width)))
  unif<-raster(matrix(sample(c(0,1),(width^2),replace = T),width,width))
  cascade<-((makecascade(width,2)))
  cascmedian<-median(cascade)
  cascade[which(cascade<cascmedian)]<-0
  cascade[which(cascade>=cascmedian)]<-1
  cascade<-raster(cascade)
  # segregated2<-raster(makesegregated_random(width,2))
  halfhalf<-raster(makehalfhalf(width))
  segregated8<-raster(makesegregated_random(width,8))
  # segregated32<-raster(makesegregated_random(width,32))
  # segregated16<-raster(makesegregated_random(width,16))
  sorted<-(raster(matrix(c(1:width^2)/width^2,width,width)))
  oneoverf<-raster(makeOneOverF(width))
  return(list(unif,segregated8,halfhalf,sorted,oneoverf,cascade))
}



makehalfhalf<-function(width){
  halfhalf <- matrix(1,ncol=width,nrow=width,byrow = T)
  halfhalf[,1:round(width/2)]<-0
  return(halfhalf)
}

makecascade<-function(width,bins){
  cascade <- matrix(1,ncol=1,nrow=1,byrow = T)
  probs <- c(1.1,2,3,4)
  p <- matrix(probs,ncol=2,nrow=2,byrow = T)
  while(ncol(cascade)<width){
  p[]<-probs+(runif(4)/10000)
  cascade <- addLevel(cascade, p)
  }
  cascade<-cascade-min(cascade)
  cascade<-cascade/max(cascade)
  cascade<-equal_quantile_bin_data(cascade,bins)
  return(cascade)
}

addLevel <- function(m,p){
  nc<-ncol(m)
  nr <-nrow(m)
  ncNew<-nc*2
  nrNew<-nr*2
  #make a matrix twice as big
  m2 <- matrix(1, ncol=ncNew,nrow=nrNew)
  for (r in c(1:nr)){
    for (c in c(1:nc)){
      m2[((((r-1)*2))+1):((((r-1)*2))+2),((((c-1)*2))+1):((((c-1)*2))+2)]<- m[r,c]+p[sample.int(2),sample.int(2)]
    }
  }
  return(m2)
}

equal_quantile_bin_data<-function(data,splits){
  breaks<-c(0,quantile(data,probs = cumsum(rep(1/splits,splits))))
  for(i in c(1:splits)){
    print(i)
    data[which(data>=breaks[i] & data<=breaks[i+1])]<-(i-0.5)/splits
  }
  return(data)
}


makeOneOverF<-function(width){
p<-seq(0,1,1/(width-1))
m<-vapply(p,FUN = function(p){sample(c(1,0),size = width,replace = T,prob=c(p,1-p))},FUN.VALUE = p)
return(m)
}
makesegregated<-function(width,cellwidth){
  if(width %% cellwidth != 0){
    print("cant make sorted.. width not multiple of cellwidth")
    return();
  }else{
  cells<-width/cellwidth
  row<-rep(
    c(rep(0,cellwidth),rep(1,cellwidth)),
    cells/2)
  cellrowsA<-rep(row,cellwidth)
  cellrowsB<-rev(cellrowsA)
  sorted<-matrix(
    rep(c(cellrowsA,cellrowsB),cells/2)
    ,width,width)
  
  }
  
  return(sorted)
}
makesegregated_random<-function(width,cells){
  if(width %% cells != 0){
  print("cant make sorted.. width not multiple of cellwidth")
  return();
}else{
  m<-raster(matrix(sample(c(0,1),size = cells^2,replace = T),cells,cells))
  m<-disaggregate(m,fact=c(width/cells,width/cells))
  
  return(as.matrix(m))
}
}




# NULL MODEL FROM DATA
randomise_spatially<-function(data){
randomised<-data
for(i in 1:7){
live<-(data[["live"]][[i]][])
work<-(data[["work"]][[i]][])
leisure<-(data[["leisure"]][[i]][])

live[which(live!=0)]<-"live"
work[which(work!=0)]<-"work"
leisure[which(leisure!=0)]<-"leisure"
all<-live
all[which(work=="work")]<-"work"
all[which(leisure=="leisure")]<-"leisure"
all<-sample(all)

randomised[["live"]][[i]][]<-0
randomised[["work"]][[i]][]<-0
randomised[["leisure"]][[i]][]<-0

randomised[["live"]][[i]][which(all=="live")]<-1
randomised[["work"]][[i]][which(all=="work")]<-1
randomised[["leisure"]][[i]][which(all=="leisure")]<-1

randomised[["all"]][[i]][]<-sample(randomised[["all"]][[i]][])
}
return(randomised)
}

compact_mixed_use<-function(data){
  randomised<-data
  for(i in 1:7){

  liveNo<-length(which((data[["live"]][[i]][])!=0))
  workNo<-length(which((data[["work"]][[i]][])!=0))
  leisureNo<-length(which((data[["leisure"]][[i]][])!=0))
  newpixels<-c(rep("live",liveNo),rep("work",workNo),rep("leisure",leisureNo))
  newpixels<-sample(newpixels)
  newpixels<-c(rep(0,length(data[["live"]][[i]][])-length(newpixels)),newpixels)
 newpixelsmatrix <- matrix(newpixels
                          ,ncol(as.matrix(data[["live"]][[i]]))
                          ,nrow(as.matrix(data[["live"]][[i]]))
                          ,byrow=T)

  randomised[["live"]][[i]][]<-0
  randomised[["work"]][[i]][]<-0
  randomised[["leisure"]][[i]][]<-0

  randomised[["live"]][[i]][which(newpixelsmatrix=="live")]<-1
  randomised[["work"]][[i]][which(newpixelsmatrix=="work")]<-1
  randomised[["leisure"]][[i]][which(newpixelsmatrix=="leisure")]<-1
  } 
return(randomised)
}


compact_segregated<-function(data){
  randomised<-data
  for(i in 1:7){

  liveNo<-length(which((data[["live"]][[i]][])!=0))
  workNo<-length(which((data[["work"]][[i]][])!=0))
  leisureNo<-length(which((data[["leisure"]][[i]][])!=0))
  newpixels<-c(rep("live",liveNo),rep("work",workNo),rep("leisure",leisureNo))
  newpixels<-c(rep(0,length(data[["live"]][[i]][])-length(newpixels)),newpixels)
 newpixelsmatrix <- matrix(newpixels
                          ,ncol(as.matrix(data[["live"]][[i]]))
                          ,nrow(as.matrix(data[["live"]][[i]]))
                          ,byrow=T)

  randomised[["live"]][[i]][]<-0
  randomised[["work"]][[i]][]<-0
  randomised[["leisure"]][[i]][]<-0

  randomised[["live"]][[i]][which(newpixelsmatrix=="live")]<-1
  randomised[["work"]][[i]][which(newpixelsmatrix=="work")]<-1
  randomised[["leisure"]][[i]][which(newpixelsmatrix=="leisure")]<-1
  } 
return(randomised)
}





