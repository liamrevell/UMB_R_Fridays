
viviana_movement<-function(n=10,ngen=100){
  
  plot(NULL,xlim= c(0,1), ylim= c(0,1))

  dt <-data.frame(y= rep(0.5,n), 
    x = c(rep(0.5,n)), 
    color=c("blue", "red","green"))

  for (i in 1:ngen){
    for(i in 1:n){
      plot(NULL,xlim= c(0,1), ylim= c(0,1))
      dt$y[i] <- dt$y[i]+runif(n = 1, min = 0, max = 1)
      dt$x[i] <- dt$x[i]+runif(n = 1, min = 0, max = 1)
    
      points(dt$x, dt$y, col= dt$color)
      Sys.sleep(0.1)
    }
  }
}

viviana_movement()
