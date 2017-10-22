synthetic_many_results<-synthe
plots_synthetic<-function(results,prefix,synthetic_many_results){
  #plotting patterns only:
  print("patterns")
  
  h<-15*length(results$entropies)/2
  pdf(paste0("./code/output/",prefix,"synthetic_patterns.pdf"),h2w(h)*scaleplots,(h+2)*scaleplots)
  par(mfrow=mfrowsynth
      ,mai=c(0.5,0.5,4,0.5))
  setParDefaults()
  for( i in c(1:length(results$entropies)) ){
    plotr(results$data[[i]],cols=gray.colors(500, start = 0, end = 1),bty="o")
    title(main = paste0(letters[i],") ", synthetic_names[i])
          ,cex.main=20
          ,outer=FALSE
          ,line=+13)
    box()
  }
  dev.off.all()
  
  jpeg(paste0("./code/output/",prefix,"synthetic_patterns.jpg"),h2w(h)*scaleplots,(h+2)*scaleplots,res=100,units = 'in')
  par(mfrow=mfrowsynth
      ,mai=c(0.5,0.5,4,0.5))
  setParDefaults()
  for( i in c(1:length(results$entropies)) ){
    plotr(results$data[[i]],cols=gray.colors(500, start = 0, end = 1),bty="o")
    box(which = "plot", lty = "solid",lwd=2)
    title(main = paste0(letters[i],") ", synthetic_names[i])
          ,cex.main=20
          ,outer=FALSE
          ,line=+13)
    box()
  }
  dev.off.all()
  
  
  # plotting geospace zones:
  print("geospace zones")
  h<-15*length(results$entropies)/2
  
  pdf(paste0("./code/output/",prefix,"geospace_zones.pdf"),h2w(h)*scaleplots,(h+2)*scaleplots)
  par(mfrow=mfrowsynth
      ,mai=c(2,0.5,0.5,0.5))
  setParDefaults()
  plot_synthetic_geospace_zones(results)
  dev.off.all()
  
  jpeg(paste0("./code/output/",prefix,"geospace_zones.jpg"),h2w(h)*scaleplots,(h+2)*scaleplots,res=100,units = 'in')
  par(mfrow=mfrowsynth
      ,mai=c(2,0.5,0.5,0.5))
  setParDefaults()
  plot_synthetic_geospace_zones(results)
  dev.off.all()
  
  
  
  
  
  
  
  #plotting geospace:
  # print("patterns aggregated")
  # pdf(paste0("./code/output/",prefix,"synthetic_patterns_aggregated.pdf"),15*length(results$lags[[1]]),15*length(results$lags))
  # par(mfrow=c(length(results$lags),length(results$lags[[1]])))
  # setParDefaults()
  # for( i in c(1:length(results$lags)) ){
  # 	for( j in c(1:length(results$lags[[1]])) ){
  
  # 		plotr(results$lags[[i]][[j]], cols=gray.colors(100,start = 0.1, end = 1),bty="o")
  # 		}
  # 	}
  # dev.off.all()
  
  # jpeg(paste0("./code/output/",prefix,"synthetic_patterns_aggregated.jpg"),15*length(results$lags[[1]]),15*length(results$lags),res=100,units = 'in')
  # par(mfrow=c(length(results$lags),length(results$lags[[1]])))
  # setParDefaults()
  # for( i in c(1:length(results$lags)) ){
  # 	for( j in c(1:length(results$lags[[1]])) ){
  
  # 		plotr(results$lags[[i]][[j]], cols=gray.colors(100,start = 0.1, end = 1),bty="o")
  # 		}
  # 	}
  # dev.off.all()
  
  
  
  # plotting phase spaces:
  print("phase spaces")
  h<-3*length(results$lags[[1]])/2
  pdf(paste0("./code/output/",prefix,"synthetic_patterns_phasespace.pdf"),h2w(h)*scaleplots,h*scaleplots)
  par(mfrow=mfrowsynth)
  par(mar=c(4,4,4,4),oma=c(0.5,3,0,0))
  setParDefaults()
  subs=paste0("",letters[1:10],")")
  plotdist1<-(lags_synthetic[synth_phasespace_plot_scales[1]]-1)/2
  plotdist2<-(lags_synthetic[synth_phasespace_plot_scales[2]]-1)/2
  xl<-bquote(x[i]^{d[.(synth_phasespace_plot_scales[1])]}~.(paste0(" (",plotdist1,"px)")))
  yl<-bquote(x[i]^{d[.(synth_phasespace_plot_scales[2])]}~.(paste0(" (",plotdist2,"px)")))
  
  for( i in c(1:length(results$entropies)) ){
    par(xpd=NA)
    eqscplot(as.vector(results$lags[[i]][[synth_phasespace_plot_scales[1]]]),as.vector(results$lags[[i]][[synth_phasespace_plot_scales[2]]])
             ,xlim=c(0,1),ylim=c(0,1)
             ,pch='.',col=rgb(0,0,0,0.01)
             ,ratio = 1
             ,tol=0.04
             ,cex=5
             ,xlab=xl
             ,ylab=yl
             ,cex.lab=2
             ,cex.axis=2
             ,xaxt="n"
             ,yaxt="n")
#     title(main = NULL, sub = paste0(letters[i],") ", synthetic_names[i])
#           ,cex.sub=3.333
#           ,outer=FALSE
#           ,line=+10)
    defaultaxis(xat=c(0:1),yat=c(0:1),xlab=c("0","1"),ylab=c("0","1"))
    gridlines<-cbind(
      c(c(1:2)/3,0,0)
      ,c(0,0,c(1:2)/3)
      ,c(c(1:2)/3,1,1)
      ,c(1,1,c(1:2)/3)
    )
    apply(gridlines,1,function(x){
      lines(c(x[1],x[3]),c(x[2],x[4]),col="grey")
    })
    
  }
  dev.off.all()
  
  jpeg(paste0("./code/output/",prefix,"synthetic_patterns_phasespace.jpg"),h2w(h)*scaleplots,h*scaleplots,res=100,units = 'in')
  par(mfrow=mfrowsynth)
  par(mar=c(4,3,4,3),oma=c(0.5,3,0,0))
  setParDefaults()
  subs=paste0("",letters[1:10],")")
  plotdist1<-(lags_synthetic[synth_phasespace_plot_scales[1]]-1)/2
  plotdist2<-(lags_synthetic[synth_phasespace_plot_scales[2]]-1)/2
  xl<-bquote(x[i]^{d[.(synth_phasespace_plot_scales[1])]}~.(paste0(" (",plotdist1,"px)")))
  yl<-bquote(x[i]^{d[.(synth_phasespace_plot_scales[2])]}~.(paste0(" (",plotdist2,"px)")))
  for( i in c(1:length(results$entropies)) ){
    par(xpd=NA)
    eqscplot(as.vector(results$lags[[i]][[synth_phasespace_plot_scales[1]]]),as.vector(results$lags[[i]][[synth_phasespace_plot_scales[2]]])
             ,xlim=c(0,1),ylim=c(0,1)
             ,pch='.',col=rgb(0,0,0,0.01)
             ,ratio = 1
             ,tol=0.04
             ,cex=5
             ,xlab=xl
             ,ylab=yl
#              ,main=subs[i]
              ,cex.lab=2
              ,cex.axis=2
             ,xaxt="n"
             ,yaxt="n")
#     title(main = NULL, sub = paste0(letters[i],") ", synthetic_names[i])
#           ,cex.sub=3.333
#           ,outer=FALSE
#           ,line=+10)
    defaultaxis(xat=c(0:1),yat=c(0:1),xlab=c("0","1"),ylab=c("0","1"))
    gridlines<-cbind(
      c(c(1:2)/3,0,0)
      ,c(0,0,c(1:2)/3)
      ,c(c(1:2)/3,1,1)
      ,c(1,1,c(1:2)/3)
    )
    apply(gridlines,1,function(x){
      lines(c(x[1],x[3]),c(x[2],x[4]),col="grey")
    })
    
  }
  dev.off.all()
  
  
  
  # plotting geospace entropies
  print("geospace entropy")
  h<-5*2/3
  pdf(paste0("./code/output/",prefix,"geospace_entropy.pdf"),5,5*2/3)
  setParDefaults()
  plot_synthetic_entropies_geospace_points_w_confidence(synthetic_many_results)
  dev.off.all()
  
  jpeg(paste0("./code/output/",prefix,"geospace_entropy.jpg"),5,5*2/3,res=100,units = 'in')
  setParDefaults()
  plot_synthetic_entropies_geospace_points_w_confidence(synthetic_many_results)
  dev.off.all()
  
  # plotting geospace entropies WIDE
#   print("geospace entropy wide")
#   pdf(paste0("./code/output/",prefix,"geospace_entropy_WIDE.pdf"),14,3)
#   setParDefaults()
#   plot_synthetic_entropies_geospace_points_w_confidence(synthetic_many_results)
#   dev.off.all()
#   
#   jpeg(paste0("./code/output/",prefix,"geospace_entropy_WIDE.jpg"),14,3,res=100,units = 'in')
#   setParDefaults()
#   plot_synthetic_entropies_geospace_points_w_confidence(synthetic_many_results)
#   dev.off.all()
  
  
  # plotting geospace frequencies
  print("geospace frequencies")
  h<-6
  pdf(paste0("./code/output/",prefix,"geospace_frequency.pdf"),h2w(h)*scaleplots,h*scaleplots)
  par(mfrow=mfrowsynth,mai=c(1,0.6,1,0),oma=c(0,0,0,0))
  setParDefaults()
  for (i in c(1:length(results$geospace$probs))) {
    if(i==1){
      barplot(sort(results$geospace$probs[[i]]),col="black",space=0.5,xlab="zones",ylab="relative frequency",cex.lab=2)
    }else{
      barplot(sort(results$geospace$probs[[i]]),col="black",space=0.5,xlab="",ylab="",cex.lab=2)
    }
    # title(main = NULL, sub = paste0(letters[i],") ", synthetic_names[i])
#     title(main = NULL, sub = paste0(letters[i],")")
#           
#           ,cex.sub=2
#           ,outer=FALSE
#           ,line=+2)
    
  }
  dev.off.all()
  jpeg(paste0("./code/output/",prefix,"geospace_frequency.jpg"),h2w(h)*scaleplots,h*scaleplots,res=100,units = 'in')
  par(mfrow=mfrowsynth,mai=c(1,0.6,1,0),oma=c(0,0,0,0))
  setParDefaults()
  for (i in c(1:length(results$geospace$probs))) {
    if(i==1){
      barplot(sort(results$geospace$probs[[i]]),col="black",space=0.5,xlab="zones",ylab="relative frequency",cex.lab=3)
    }else{
      barplot(sort(results$geospace$probs[[i]]),col="black",space=0.5,xlab="",ylab="",cex.lab=3)
    }    # title(main = NULL, sub = paste0(letters[i],") ", synthetic_names[i])
#     title(main = NULL, sub = paste0(letters[i],")")
#           
#           ,cex.sub=2
#           ,outer=FALSE
#           ,line=+2)
    
  }
  dev.off.all()
  h<-5*2/3
  pdf(paste0("./code/output/",prefix,"boxplot.pdf"),h2w(h),h)
  setParDefaults()
  plot_synthetic_entropies_points_w_confidence(synthetic_many_results)
  dev.off.all()
  jpeg(paste0("./code/output/",prefix,"boxplot.jpg"),5,5*2/3,res=100,units = 'in')
  setParDefaults()
  plot_synthetic_entropies_points_w_confidence(synthetic_many_results)
  dev.off.all()
  
}


plots_synthetic(results_synthetic,"synthetic",synthetic_many_results)

