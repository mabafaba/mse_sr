makeWhereSQLByGroup<-function(group){
  #makes a "where" statement for rgdal_rasterize(,,where= ... )
  working<- c("INS","INSL","O","GAR","IND",'UTL','AIR','RRS')
  living<-  c('APT','APTH','COT','DET','DETH','MEW','SDT','TER','LDG')
  leisure<- c('GEN','RET','OLD','AGR','CEM','EST','FRM','NRS','PRK','REC','WAT','CHR','SCH','STA','CLR')
  where<-""
  where<-as.vector(paste("LU like '",get(group),"'",sep=""))
  where<-lapply(where,paste)
  where<-do.call(paste,c(where,sep=" OR "))
  return(where)
}



