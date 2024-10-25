## our first R function
hello_world <- function(){
  cat("Hello world!\n")
}

args(hello_world)

hello_world()

hello_world<-function(x,y){
  cat("Hello world!\n")
  return(x*y)
}

Product<-hello_world(143,299)
Product

hello_world<-function(){
  par(mar=rep(0.1,4))
  plot(NA,xlim=c(0,1),ylim=c(0,1),bty="n",
    axes=FALSE,xlab="",ylab="")
  text(0.5,0.5,"Hello world!",cex=4)  
}

hello_world<-function(){
  par(mar=rep(0.1,4))
  cols<-rainbow(n=1000)
  for(i in 1:length(cols)){
    plot(NA,xlim=c(0,1),ylim=c(0,1),bty="n",
      axes=FALSE,xlab="",ylab="")
    text(0.5,0.5,"Hello world!",cex=4,
      col=cols[i])
    Sys.sleep(0.05)
  }
}

hello_world()

for(i in seq(2,10,by=2)){
  print(i^2)
  Sys.sleep(1)
}

x<-c(2,4,6)

for(n in x){
  print(n^2)
}

for(i in 1:5){
  print(i)
}

hello_world2<-function() print("Hello world!")
hello_world2()

## not allowed
2hello_world<-function() print("test")

for(i in 1:5){
  cat(paste(i,".\n",sep=""))
  for(j in 1:5){
    cat("\t")
    cat(paste(letters[j],", ",sep=""))
    Sys.sleep(0.1)
  }
  cat("\n")
  for(j in 1:5){
    cat("\t")
    cat(paste(LETTERS[j],", ",sep=""))
    Sys.sleep(0.1)
  }
  cat("\n")
}

success<-FALSE
while(!success){
 ## do something if succeeds
  success<-TRUE
}

i<-1
repeat {
  print(i)
  i<-i+1
  if(i>10) break
}

i<-1
while(i<=10){
  print(i)
  i<-i+1
  Sys.sleep(0.1)
}


phacker<-function(nmax=1000,plot=TRUE){
  x<-rnorm(n=10)
  y<-rnorm(n=10)
  col<-"lightblue"
  if(plot) plot(x,y,cex=1.5,pch=21,bg=col)
  fit<-lm(y~x)
  if(plot) abline(fit)
  p<-anova(fit)["Pr(>F)"][[1]][1]
  if(plot) legend("topleft",paste("P =",round(p,4)))
  n<-length(x)
  while(p[1]>0.05&&n<nmax){
    x<-c(x,rnorm(n=1))
    y<-c(y,rnorm(n=1))
    if(plot) dev.hold()
    if(plot) plot(x,y,cex=1.5,pch=21,bg=col)
    fit<-lm(y~x)
    if(plot) abline(fit)
    p<-anova(fit)["Pr(>F)"][[1]][1]
    if(plot) legend("topleft",paste("P =",round(p,4)))
    if(plot) dev.flush()
    if(plot) Sys.sleep(0.05)
    n<-length(x)
  }
  return(p)
}
Pvals<-replicate(100,phacker(plot=FALSE))
