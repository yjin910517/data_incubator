styles<-read.csv("styles.csv")
df <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("id", "num_beer", "avg_rating","avg_abv","style_name","sub_cat","cat")
colnames(df) <- x

for (i in 1:length(styles$id)) {
  id<-styles$id[i]
  style_name<-as.character(styles$style[i])
  file_name<-paste("style_",id,".csv",sep="")
  beers<-read.csv(file_name,header=TRUE)
  beers$abvs<-as.numeric(as.character(beers$abvs))
  beers$ratings<-as.numeric(as.character(beers$ratings))
  num_beer<-dim(beers)[1]
  avg_rating<-sum(beers$ratings*beers$scores,na.rm=TRUE)/sum(beers$ratings,na.rm=TRUE)
  avg_abv<-mean(beers$abvs,na.rm=TRUE)
  
  if (i<22) {
    sub_cat<-"American Ales"
    cat<-"Ale"
  } else if (i<40) {
    sub_cat<-"Belgian / French Ales"
    cat<-"Ale"   
  } else if (i<59) {
    sub_cat<-"English Ales"
    cat<-"Ale"      
  } else if (i<60) {
    sub_cat<-"Finnish Ales"
    cat<-"Ale"      
  } else if (i<69) {
    sub_cat<-"German Ales"
    cat<-"Ale"      
  } else if (i<69) {
    sub_cat<-"German Ales"
    cat<-"Ale"      
  } else if (i<71) {
    sub_cat<-"Irish Ales"
    cat<-"Ale"      
  } else if (i<72) {
    sub_cat<-"Russian Ales"
    cat<-"Ale"      
  } else if (i<75) {
    sub_cat<-"Scottish Ales"
    cat<-"Ale"      
  } else if (i<83) {
    sub_cat<-"American Lagers"
    cat<-"Lager"      
  } else if (i<84) {
    sub_cat<-"Czech Lagers"
    cat<-"Lager"      
  } else if (i<87) {
    sub_cat<-"European Lagers"
    cat<-"Lager"      
  } else if (i<100) {
    sub_cat<-"German Lagers"
    cat<-"Lager"      
  } else if (i<102) {
    sub_cat<-"Japanese Lagers"
    cat<-"Lager"      
  } else {
    sub_cat<-"Hybrid"
    cat<-"Hybrid"     
  }
  
  
  line<-data.frame(id, num_beer, avg_rating,avg_abv,style_name,sub_cat,cat)
  names(line)<-x
  df<-rbind(df,line)
}

write.csv(df,file="styles_summary.csv")