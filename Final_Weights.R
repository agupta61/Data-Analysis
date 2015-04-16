#Please provide Level Values
#---------------------------
g<-c(4,2,3,4,4,4,4)

#Data Extraction
#---------------
vote<-read.csv(file='votings.csv',header=TRUE,stringsAsFactors=FALSE)

#Fetching only attribute values for computation
#----------------------------------------------
vote1<-vote[-c(1),-c(1)]

r<-nrow(vote1)
c<-ncol(vote1)

#Fetching attributes names
#-------------------------
catg<-vote[1,2:(c+1)]

#Converting values to numeric
#----------------------------
for (i in 1:c){
  vote1[,i]<-as.numeric(vote1[,i])
}

#Decision Tree computation
#-------------------------
vote1<-round(vote1[,1:c]/9,3)

for (i in seq(1,c, by=2)){  
  x<-vote1[,i]
  y<-vote1[,i+1]
  for(j in 1:r){
    if(x[j]>0) y[j]<-x[j]*-1 else x[j]<-y[j]*-1
  }
  vote1[,i]<-x+0.5
  vote1[,i+1]<-y+0.5
}

vote2<-rbind(catg,vote1)
h1<-unique(as.character(as.vector(vote2[1,])))
h2<-vote[-1,1]
d<-matrix(, , ncol = r)
d<-as.data.frame(d)
d<-d[complete.cases(d),]

for(i in h1){
  k<-1
  q<-rep(0,r)
  for(j in vote2[1,]){
    if (i==j) q<-q+as.numeric(vote2[-1,k])
    k<-k+1
  }
  d<-rbind(d,q)
}

d<-((d+2)*100)/5
k<-0
for(i in g){
  k<-k+i
  for(j in 1:ncol(d)){
    d[(k-i+1):k,j]<-round((d[(k-i+1):k,j]/sum(d[(k-i+1):k,j]))*100)
 }
}

names(d)<-h2
w<-data.frame(Avg_Weights=numeric())
for(i in 1:k) {w[i,1]<-round(mean(as.numeric(d[i,])))}
d<-cbind(w,d)
for(i in 1:(r+1)) {d[,i]<-paste(d[,i],"%",sep="")}
d<-cbind(d,attributes=h1)

#Exporting Decision Tree
#-----------------------
write.table(d, "rawdata.csv", sep=",", row.names=FALSE, col.names=TRUE)
