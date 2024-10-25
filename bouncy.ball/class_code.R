hello <- function(cex=1) {
  plot(NA,xlim=c(0,1),ylim=c(0,1),
    axes=FALSE,bty="n",xlab="",ylab="")
  text(0.5,0.5,"Hello world!",cex=cex)
}

hello()
hello(4)

plot(NA,xlim=c(0,1),ylim=c(0,1))
text(0.5,0.5,"Hello world")

par(mar=rep(0,4))
plot(NA,xlim=c(0,1),ylim=c(0,1),asp=1,
  bty="n",axes=FALSE,xlab="",ylab="")
polygon(c(0,1,1,0),c(0,0,1,1))

x<-y<-0.5
col<-phytools::make.transparent("blue",0.2)
for(i in 1:200){
  plot(NA,xlim=c(0,1),ylim=c(0,1),asp=1,
    bty="n",axes=FALSE,xlab="",ylab="")
  polygon(c(0,1,1,0),c(0,0,1,1))
  points(x,y,pch=16,cex=1.5,col=col)
  x<-x+rnorm(n=1,sd=0.5)
  while(x>1 || x<0){
    if(x>1) x<-2-x
    if(x<0) x<--x
  }
  y<-y+rnorm(n=1,sd=0.5)
  while(y>1 || y<0){
    if(y>1) y<-2-y
    if(y<0) y<--y
  }
  Sys.sleep(0.1)
}



