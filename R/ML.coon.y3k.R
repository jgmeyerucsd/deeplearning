### prepare coon lab y3k data for deep learning 
### read in the coon LFQ values, decompose them based on their standard deviation, and then 

setwd("~/GitHub/deeplearning/")
list.files()

### label-free quant values with KO strains as columns and measured values as rows
lfq1<-read.delim("lfq1.txt",stringsAsFactors = F,header = T)
lfq2<-read.delim("lfq2.txt",stringsAsFactors = F,header = T) ### missing values become NA

#head(lfq2,100)
#lfq2[1:10,1:10]

### LFQ st.dev. values with KO strains as columns and measured values as rows
lfq1sd<-read.delim("lfq1sd.txt",stringsAsFactors = F,header = T)
lfq2sd<-read.delim("lfq2sd.txt",stringsAsFactors = F,header = T) ### missing values become NA

## test decompostion in to possible real values using rnorm
?rnorm
#### helper function to take one value pair and decompose it

dcomp=function(mean=lfq1[1,3],sd=lfq1sd[1,3]){
  #print(mean)
  #print(sd)
  out<-rnorm(n=3,mean=mean,sd=sd)
  out
}


ncol(lfq1) ### starts at 3

in.means<-lfq1
in.sds<-lfq1sd

in.means<-lfq2
in.sds<-lfq2sd
### build empty dataframe to fill with 3x width
ncol(in.means)-2
dc.vals<-cbind(in.means,in.means[,3:ncol(in.means)],in.means[,3:ncol(in.means)])
ncol(dc.vals)

dc.vals[is.na(dc.vals)==FALSE]<-NA
head(dc.vals)

paste(c(1:3), colnames(in.means)[3:176])

### make new names for the columns
new.colnames<-c(colnames(in.means)[1:2],unlist(lapply(FUN=paste, colnames(in.means)[3:176],c(1:3), sep=".n")))
new.colnames

colnames(dc.vals)<-new.colnames
new.colnames2<-paste("F",new.colnames,sep=".")
colnames(dc.vals)<-new.colnames2
colnames(dc.vals)


dc.vals[,1:2]<-in.means[,1:2]
dc.vals[,1:2]

### dc.vals now ready empty mat



for(i in 3:ncol(in.means)){
    for(j in 1:nrow(in.means)){
      fill.cols<-3*(i-3)+c(3:5)
      dc.vals[j,fill.cols]<-dcomp(mean=in.means[j,i], sd=in.sds[j,i])
    }
  print(i)

}

head(dc.vals)
write.table(file="decomposedtest1.txt",dc.vals,quote=F, sep="\t")
write.table(file="decomposedtest2.txt",dc.vals,quote=F, sep="\t",row.names = F)

#lapply(FUN=dcomp, mean=mean, sd=sd)

dcomp(mean=lfq1[,3],sd=lfq1sd[,3])


###### all values decomposed (although clumsily)
#### read back in as matrix

lfq.all<-read.delim("decomposed_all.txt",stringsAsFactors = F,header = T) ### missing values become NA




