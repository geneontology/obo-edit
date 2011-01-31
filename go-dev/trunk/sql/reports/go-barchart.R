x<-commandArgs()
name<-x[3]
png("tmp.png", pointsize = 20);  # pointsize is size of text
#postscript(file="tmp.eps", horizontal=TRUE, width=4, height=4,pointsize = 20);  # pointsize is size of text
#postscript(file="tmp.eps", width=6,height=6,horizontal=FALSE,pointsize = 12);  # pointsize is size of text

d<-read.delim("tmp.rdata", header = F)
rows <- 8; # only plot first n rows of data
 m <- matrix(c(d$V1[1:rows],d$V3[1:rows]), byrow = T); # grab columns 1 - 3 
# (they are titled V1 to V5 in the above file )
# nrow must = number of columns you want to plot

#barplot(d$V2,d$V3,d$V4,col=gray(1:0/1),beside = T,names.arg = as.character(d$V1),legend.text = c("xx"), xlab="year", ylab=name);
barplot(d[1:8,2:4],col=gray(1:0/1),beside = T,names.arg = as.character(d$V1),legend.text = c("xx"), xlab="year", ylab=name);

#barplot(m,col=gray(1:0/1),beside = T,names.arg = c(0:(rows-1)), angle=45,legend.text = c("xx"), xlab="number of annotations", ylab=name);
dev.off()
