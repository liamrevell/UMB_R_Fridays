pacman::p_load("rphylopic")



rphylopic::get_phylopic(uuid = 'c07ce7b7-5fb5-484f-83a0-567bb0795e18')-> frog



frogger <- function(iterations=100,starts=0.5){
  
  frog=frog
  
  x=starts
  
  x1=starts+0.1
  
  x2=starts-0.1
  
  y=starts
  
  y1=starts+0.1
  
  y2=starts-0.1
  
  
  
  for (i in 1:iterations){
    
    plot(NA,xlim=c(0,1),
      
      ylim=c(0,1),
      
      asp=1,
      
      bty="n",
      
      axes=FALSE,
      
      xlab="",
      
      ylab="")
    
    par(mar=rep(0.1,4))
    
    polygon(c(0,1,1,0),c(0,0,1,1))
    
    #points(x=0.5,y=0.5,pch=16,cex=1.5,col="red")
    
    x <- x+rnorm(n=1,sd=0.02)
    
    y <-x+rnorm(n=1,sd=0.02)
    
    x1 <- x1+rnorm(n=1,sd=0.02)
    
    y1 <- y1+rnorm(n=1,sd=0.02)
    
    x2 <- x2+rnorm(n=1,sd=0.04)
    
    y2 <- y2+rnorm(n=1,sd=0.04)
    
    points(x,y,pch=13,cex=1.5,col="blue")
    
    points(x1,y1,pch=13,cex=1.5,col="red")
    
    rphylopic::add_phylopic_base(img = frog,ysize = 0.1,x = x2,y = y2,col="darkgreen")
    
    
    
    Sys.sleep(0.1)
    
  }
  
}



frogger(iterations = 100,starts = 0.1)