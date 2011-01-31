x<-commandArgs()
name<-x[3]
png("tmp.png", pointsize = 20);  # pointsize is size of text

d<-read.delim("tmp.rdata", header = F)
hh <- t(d[,2:4])

barplot(d[1:8,2:4],col=gray(1:0/1),beside = T,names.arg = as.character(d$V1),legend.text = c("MF","BP","CC"), xlab="year", ylab="num");

dev.off()
