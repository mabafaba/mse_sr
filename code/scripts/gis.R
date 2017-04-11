shp2raster_fast<-function(file,uniquename,layer,resolution, where="1=1"){
# rastersises a shapefile proportionally.
# resolution should give the width and height of each pixel in meters  
# destination file name must be unique, otherwise different objects overwrite each other in external file
  dest_filename<-paste("./code/run_temp/",layer,"_rasterise_tempfil",uniquename,Sys.time(),"e.tif",sep="")
    thisraster <- gdal_rasterize(src,dst_filename=dest_filename,tr = c(resolution,resolution),where=where,burn=1,l = layer,output_Raster = T)
  return(raster(thisraster,layer=names(thisraster)))
  
}


plotr<-function(raster,min=0,max=1,legend=F,cols=c("black","white"),breaks=NA,ylab="",bty="n"){
  # values must lie between 0 and 1
  raster[]<-round(raster[],6)
#   cols<-palette(gray(seq(0.95,0,len = 99)))
# cols<-c("blue","blue","red","red")
# cols<-c("#1133AA","#AA1133")
  par(bty= bty,xpd=NA)
  if(!legend){
    image(raster,
          col=cols, breaks=seq(min,max,length.out=length(cols)+1)
          ,axes=FALSE
          ,useRaster=F
          ) 
    par(bty= bty,xpd=NA) # remove the box
  }else if(!is.na(breaks)){
    plot(raster, legend.only=TRUE
         , col=cols,
         breaks=breaks,
         axes=FALSE,
         legend.width = 2,
         axis.args=list(at=seq(0,1,0.2),
                        labels=seq(0,1,0.2), 
                        cex.axis=0.6),
         legend.args=list(text='', side=4, font=2, line=2.5, cex=0.8))
  }
  else{

    image(raster, legend.only=TRUE
         , col=cols,
         legend.width = 2,

         axis.args=list(at=seq(0,1,0.2),
                        labels=seq(0,1,0.2), 
                        cex.axis=0.6),
         legend.args=list(text='', side=4, font=2, line=2.5, cex=0.8))



  }
}





# find the xy position in a raster based on the raster[] id
raster_xy_from_vector_id<-function(r,id){
m<-as.matrix(r)
return(cbind(row=col(t(m))[id],col=row(t(m))[id]))
}
