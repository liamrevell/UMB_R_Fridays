bounce<-function(n=10,energy=0.02,ngen=100){
  x<-rep(0.5,n)
  y<-rep(0.5,n)
  if(length(energy)!=n) energy<-rep(energy,10)
  col<-randomcoloR::randomColor(n)
  for(i in 1:ngen){
    par(mar=rep(0,4),bg="black")
    plot(NA,xlim=c(0,1),ylim=c(0,1),asp=1,
      bty="n",axes=FALSE,xlab="",ylab="")
    polygon(c(0,1,1,0),c(0,0,1,1),border="white")
    points(x,y,pch=21,cex=2,bg=col,col="white")
    x<-x+rnorm(n=n,sd=energy)
    for(j in 1:n){
      while(x[j]>1 || x[j]<0){
        if(x[j]>1) x[j]<-2-x[j]
        if(x[j]<0) x[j]<--x[j]
      }
    }  
    y<-y+rnorm(n=n,sd=energy)
    for(j in 1:n){
      while(y[j]>1 || y[j]<0){
        if(y[j]>1) y[j]<-2-y[j]
        if(y[j]<0) y[j]<--y[j]
      }
    } 
    Sys.sleep(0.1)
  }
}


bounce(n=30,energy=rexp(n=30,rate=50))
