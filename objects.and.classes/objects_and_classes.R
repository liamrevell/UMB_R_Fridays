## making an object of a class (S3 object)
bob<-list(
  name="Robert Johnson",
  id=90210,
  salary="$60,100" 
)

class(bob) ## class is "list"

## assign class attribute "employee"
attr(bob,"class")<-"employee"
## or
class(bob)<-"employee"

is.list(bob) ## bob is still a list

class(bob) ## check class

bob

## print method for "employee"
print.employee<-function(x, ...){
  cat("\nObject of class \"employee\" with elements:\n")
  cat(paste("\tname =",x$name,"\n"))
  cat(paste("\tID =",x$id,"\n\n"))
  cat(paste(x$name,"has a salary of",x$salary,"\n\n"))
}

## see what happens if you make the print method do nothing
## print.employee<-function(x, ...){}

## a silly example of a plot method
plot.employee<-function(x,...){
  plot(NA,xlim=c(0,1),ylim=c(0,1),axes=FALSE,
    xlab="",ylab="")
  text(0.5,0.5,x$name,cex=2)
}

## print(bob) or just entering the name are the same
print(bob)
bob

## plot
plot(bob)

## create another employee
susie<-list(
  name="Susan Perkins",
  id=02906,
  salary="$102,010"
)
class(susie)<-"employee"

susie

## create a new instance of the c() method
c.employee<-function(...){
  object<-list(...)
  class(object)<-"staff"
  object
}

museum_staff<-c(bob,susie)
museum_staff

## print method for object of class "staff"
print.staff<-function(x,...){
  cat(paste("\nStaff of ",length(x),
    ":\n",sep=""))
  cat("\tName\tID\tSalary\n")
  for(i in 1:length(x))
    cat(paste(x[[i]]$name,x[[i]]$id,x[[i]]$salary,"\n",
      sep="\t"))
  cat("\n")
}

## print using the method
museum_staff

## create new generic
fire<-function(staff,names,...) UseMethod("fire")

## make an instance
fire.staff<-function(staff,names,...){
  staff_nn<-sapply(staff,function(x) x$name)
  ii<-which(staff_nn%in%names)
  if(length(ii)>0) staff<-staff[-ii]
  else staff<-staff[-ii]
  class(staff)<-"staff"
  staff
}

## test
object<-fire(museum_staff,"Susan Perkins")
object
