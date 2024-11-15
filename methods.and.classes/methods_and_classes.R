## this week we had a mix of participants with different prior
## experience so we tried to cover both very introductory "my
## first R program" kind of stuff, through S3 objects and 
## methods, and ellipsis arguments (...) 

## next level "Hello, world!" program in R language
hello <- function(times=1){
  ## prints "Hello, world!" times times
  for(i in 1:times){
    print("Hello, world!")
  }
  ## creates "hello" class object
  obj<-"Hello, world!"
  class(obj)<-"hello"
  ## assigns custom attribute
  attr(obj,"times")<-times
  ## returns object invisibly
  invisible(obj)
}

## our S3 print method for object class "hello"
print.hello<-function(x,...){
  ## get ellipsis arguments
  if(hasArg(details)) details<-list(...)$details
  else details<-FALSE
  ## logical test based on details
  if(details){
    cat(paste("Object of class \"hello\" with",
      attr(x,"times"),"hellos.\n"))
    for(i in 1:attr(x,"times")) print("Hello, world!")
  } else cat("One object of class \"hello\".\n")
}

hello(3)->object

hello(times=3)->x
x ## when we enter the name of variable into R
  ## we cause it to look for a print method

print(x) ## this is the same as just entering the object name

print(x,details=TRUE)

print(x,all=TRUE)

y<-c(-1,4,8)
y

class(y)

class(y)<-"hello"
y

## classes & generic methods are pervasive

X<-rnorm(n=10)
Y<-0.5*X+rnorm(n=10)

plot(X,Y)

FIT<-lm(Y~X)
FIT

class(FIT)

print(FIT)

unclass(FIT)

plot(FIT)

print.lm<-function(x,...){
  cat("Ha ha, I overwrote the print.lm method in stats.\n\n")
}

stats:::print.lm(FIT)

## class assignment

## we can get & assign the class of an object using class()
class(x)
class(x)<-"not hello"

class(x)<-"hello"

attr(x,"class")<-"not hello"
class(x)
x

class(x)<-"hello"

class(x)=="hello"

inherits(x,"hello")

## multiple classes
class(x)<-c("goodbye","hello")

class(x)

print(x)

## first class is prioritized
print.goodbye<-function(x,...){
  cat("Oops, we forgot a print method for objects of class \"goodbye\".")
}

print(x)

## flip order and print method will change
class(x)<-class(x)[2:1]

class(x)

x

print.goodbye(x)

print

## some additional code we messed with
times<-4 ## this had to do with scoping

hello()

hello(times=3)->object
object

object<-hello(times=4)
object



object<-hello()
object

unclass(object)
