x<-commandArgs()
name<-x[3]
png("tmp.png", pointsize = 20);  # pointsize is size of text

d<-read.delim("tmp.rdata", header = F)
barplot(d[,2],col=gray(1:0/1),beside = T,names.arg = as.character(d$V1),legend.text = c("xx"), xlab="year", ylab=name);

dev.off()
