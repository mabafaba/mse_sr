load_data<-function(src,layerNames){
do.call(file.remove, list(list.files("./code/run_temp", full.names = TRUE)))

# load data and rasterize:
allrasters_working<-lapply(as.list(layerNames),function(x){
  return(shp2raster_fast(file = src,uniquename= "work",layer = x,resolution = pixelWidth,where = makeWhereSQLByGroup("working")))  
  })


allrasters_leisure<-allrasters_living<-lapply(as.list(layerNames),function(x){
  return(shp2raster_fast(file = src,uniquename= "leisure",layer = x,resolution = pixelWidth,where = makeWhereSQLByGroup("leisure")))  
})

allrasters_living<-lapply(as.list(layerNames),function(x){
  return(shp2raster_fast(file = src,uniquename= "live",layer = x,resolution = pixelWidth,where = makeWhereSQLByGroup("living")))  
})

allrasters_all<-lapply(as.list(layerNames),function(x){
  return(shp2raster_fast(file = src,uniquename= "all",layer = x,resolution = pixelWidth,where = ""))  
})


return(list(work=allrasters_working,live=allrasters_living,leisure=allrasters_leisure,all=allrasters_all))
}



load_data_as_vectors<-function(src,layerNames){
data<-lapply(as.list(layerNames),function(layer){
		return(readOGR(dsn = src, layer = layer))
	})
uniqueLabels<-levels(unlist(lapply(lapply(data,as.data.frame),function(x){x$LU})))

return(list(data=data,
			uniqueLabels=uniqueLabels)
	)
}



