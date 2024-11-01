
h<-function(){
  return(d*(w+y))
}

h

d<-1
w<-2
y<-3

h() ## works

h<-function(d,w,y){
  return(d*(w+y))
}

h() ## doesn't work

h<-function(d=0,w,y){
  d<-get("d",envir=.GlobalEnv)
  print(d)
  return(d*(w+y))
}

h()

assign("d",c(1,2),envir=.GlobalEnv)

NewEnvironmentForFun<-new.env(parent=.GlobalEnv)
assign("d",c(3,4),envir=NewEnvironmentForFun)

y<-2
z<-10

f<-function(x){
  y<-4  
  h<-function(){
    y<-get("y",envir=.GlobalEnv)
    return((x+y)/z)  
  }
  cat(paste("result of (x + y) / z = ",h(),
    "\n\n"))
}

f(3)

g<-function(x){
  assign("y",x*y,envir=.GlobalEnv)
}

g(4)

n<-function(){
  return((x+y)/z)
}

m<-function(x){
  y<-4
  cat(paste("result of (x + y) / z = ",n(),
    "\n\n"))
}
m(3)

plotDots<-function(x,y){
  plot(x,y,asp=1,bty="n",las=1,pch=21,
    bg="red")
  grid()
  lm_fit<-lm(y~x)
  assign("lastDots",lm_fit,envir=.GlobalEnv)
}

x<-rnorm(n=40)
y<--0.75*x+rnorm(n=40)

plotDots(x,y)

addLine<-function(){
  my_fit<-get("lastDots",envir=.GlobalEnv)
  abline(my_fit,lwd=2)
}

addLine()

foo<-function(){
  x<-x0
  for(i in 1:10){
    x<-x+1
    assign(paste("x",i,sep=""),x,envir=.GlobalEnv)
    cat("paste current value of x = ",x,"\n")
    Sys.sleep(3)
  }
}

foo()

.x<-10
.x
rm(list=ls(all.names=TRUE))

plotDots<-function(x,y){
  plot(x,y,asp=1,bty="n",las=1,pch=21,
    bg="red")
  grid()
  lm_fit<-lm(y~x)
  assign(".PlotDots",new.env(),envir=.GlobalEnv)
  ## .PlotDots<-new.env(parent=.GlobalEnv) ## does not work.
  assign("lastDots",lm_fit,envir=.PlotDots)
}

addLine<-function(){
  my_fit<-get("lastDots",envir=.PlotDots)
  clip(min(my_fit$model[,2]),max(my_fit$model[,2]),
    min(my_fit$model[,1]),max(my_fit$model[,2]))
  abline(my_fit,lwd=2)
}

x<-rnorm(n=10)
y<-rnorm(n=10)

plotDots(x,y)
addLine()
