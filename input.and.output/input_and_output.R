## we created a simple input file, input_file_1.txt, with
## numeric values on three lines

input_data_1<-scan("input_file_1.txt")
input_data_1

## input_file_2.txt contains numeric values with decimals
## on multiple lines

input_data_2<-scan("input_file_2.txt",what=9)
input_data_2
input_data_2[4]

## input_file_3.txt contains a mixture of text and numeric

input_data_3<-scan("input_file_3.txt") ## causes an error

?scan

## we need to supply a type via the scan argument what

## this creates a vector of strings
input_data_3<-scan("input_file_3.txt",what="")

## this creates a list of string, string, numeric,
## numeric, numeric
input_data_3<-scan("input_file_3.txt",
  what=list("","",0,0,0))

## input_file_4.txt contains labels followed by a series of
## numeric values separated by spaces

input_data_4<-scan("input_file_4.txt",
  what=list("",0,0,0,0,0,0))
input_data_4

## we basically just created the structure of a data frame
## object using scan()

names(input_data_4)<-paste("V",1:length(input_data_4))
as.data.frame(input_data_4)

## the next thing we can do is use readLines

input_data_4.2<-readLines("input_file_4.txt")
input_data_4.2

## input_file_5.txt consists of labels followed by a space
## followed by nucleotide sequences without spaces

input_data_5<-readLines("input_file_5.txt")
input_data_5

## try using scan (modified to resemble FASTA file)
input_data_5<-scan("input_file_5.txt",what=list("","",""))
input_data_5

## input_data_6 kind of looks like a CSV file
input_data_6<-scan("input_file_6.txt",what="",sep=",")
input_data_6

## try using readLines
input_data_7<-readLines("input_file_6.txt")

## readline, by contrast, takes input from the keyboard

Name<-readline(prompt="Enter your full name: ")

## write a function to read a FASTA file after (optionally) 
## prompting for a file name

readFASTA<-function(filename=NULL){
  if(is.null(filename))
    filename<-readline("Type in the file you want to read: ")
  dna<-scan(file=filename,what=list("",""),quiet=TRUE)
  labels<-sub(">","",dna[[1]])
  setNames(strsplit(dna[[2]],""),labels)
}

## prompts for file name
our_dna<-readFASTA()

## takes filename as input
our_dna<-readFASTA("fasta_file.fas")

our_dna

## computes base frequencies
base_freq<-summary(factor(unlist(our_dna),
  levels=c("A","C","G","T")))
base_freq

## updated function version that prints summary of base 
## frequencies

readFASTA<-function(filename=NULL){
  if(is.null(filename))
    filename<-readline("Type in the file you want to read: ")
  dna<-scan(file=filename,what=list("",""),quiet=TRUE)
  labels<-sub(">","",dna[[1]])
  DNA<-setNames(strsplit(dna[[2]],""),labels)
  base_freq<-summary(factor(unlist(DNA),
    levels=c("A","C","G","T")))
  base_freq<-base_freq/sum(base_freq)
  cat("Read FASTA file with base frequencies:\n")
  for(i in 1:length(base_freq)){
    cat("  ",names(base_freq)[i],": ",
      round(base_freq[i],2),"\n",sep="")
  }
  class(DNA)<-"fasta"
  DNA
}

our_dna<-readFASTA("fasta_file.fas")
our_dna

## summary method for "fasta" object class

summary.fasta<-function(object,...){
  cat("Object of class \"fasta\" with base frequencies:\n")
  base_freq<-summary(factor(unlist(object),
    levels=c("A","C","G","T")))
  base_freq<-base_freq/sum(base_freq)
  for(i in 1:length(base_freq)){
    cat("  ",names(base_freq)[i],": ",
      round(base_freq[i],2),"\n",sep="")
  }
}

summary(our_dna)


## review of how S3 methods like summary work
summary.factor<-function(object,...){
  base::summary.factor(object)/sum(base::summary.factor(object))
}

test_factor<-c("Male","Male","Female","Male")
test_factor<-as.factor(test_factor)

summary(test_factor)

