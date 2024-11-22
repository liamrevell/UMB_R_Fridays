## this is a review of how S4 objects & methods work

joe<-list(
  name="Joseph Smith",
  salary=10000,
  status="full time"
  )
joe
class(joe)
class(joe)<-"employee"
class(joe)

joe

print.employee<-function(x,...){
  cat(paste("\n",
    x$name,"is a",x$status,"employee with salary",
    x$salary,".\n\n"))
}

joe

print(joe)

bob<-list(
  name="Robert Hunter",
  salary=80000,
  sattus="part time"
)
class(bob)<-"employee"
bob ## status is missing because we misspelled it

## S4 objects

## define class
setClass("employee",
  slots=c(
    name="character",
    salary="numeric",
    status="character"))

## create an object of this class
joe<-new("employee",
  name="Joseph Smith",
  salary=100000,
  status="full time")

joe

class(joe)

## empty object of class "employee"
new("employee")->bob

bob

## give Joe a raise
joe@salary<-105000
joe


joe@salary<-"$120,000" ## doesn't work because wrong type

joe ## the same as show(joe)

show(joe)

cat("\n",joe@name,"is a",joe@status,
  "employee with a salary of",joe@salary,"\n\n")

PRINT<-function(object){
  cat("\n",object@name,"is a",object@status,
    "employee with an annual salary of",
    object@salary,"\n\n")
}

PRINT(joe)

## create S4 generic function

setMethod(f="show",signature="employee",
  definition=PRINT)

rm(PRINT)

## this works
joe

setMethod(f="show",signature="employee",
  definition=function(object){
    cat("\n",object@name,"is a",object@status,
      "employee with an annual salary of",
      object@salary,"\n\n")
  })

joe ## this also works

POP<-function(x){
  colors<-sample(rainbow(n=100),20)
  for(i in 1:20){
    dev.hold()
    par(bg="black",mar=rep(0.1,4))
    plot(NA,xlim=c(0,1),ylim=c(0,1),bty="n",
      axes=FALSE,xlab="",ylab="")
    text(0.5,0.55,paste(x@name,"\nmakes ",
      "$",prettyNum(x@salary,big.mark=","),sep=""),
      cex=6,col=colors[i])
    dev.flush()
    Sys.sleep(0.1)
  }
}
POP(joe)

## create new S4 generic method

setGeneric("pop",
  function(x) cat("no pop method for this object class\n"))

## create S4 pop method for employee

setMethod(f="pop",signature="employee",
  definition=POP)


## defining a class with a validity check
setClass("employee",
  slots=c(
    name="character",
    salary="numeric",
    status="character"),
  validity=function(object){
    if(object@salary<0){
      return("Salary cannot be negative")
    } else TRUE
  }
)

## this works, even though validity check should fail
bob
bob@name<-"Robert Hooker"
bob@status<-"part time"
bob@salary<--55000
bob

## this fails validity check
susie<-new("employee",
  name="Susan Muller",
  salary=-10,
  status="full time")
susie
susie@salary<--80000
susie

## this assigns invalid value to salary
slot(susie,"salary",check=FALSE)<-"$80,000"
susie

## but this fails
validObject(susie)

## our effort to devise a "setter" function to check validity
## (awkward but basically works)
setter<-function(object,slot,value){
  non_updated<-object
  slot(object,slot)<-value
  if(validObject(object,test=TRUE)==TRUE) return(object)
  else {
    cat("invalid!!\n")
    return(non_updated)
  }
}
new_susie2<-setter(new_susie,"salary",-80000)
new_susie2
