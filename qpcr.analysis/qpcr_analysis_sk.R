library(readxl)

f1<-"2024_08_14 Ef1a ISL 20mM" #.xlsx"
f2<-"2024_08_14 BMP2 ISL 20mM.xlsx"

get_excel_data<-function(filename){
  ## check to see if the file name ENDS with .xlsx
  tmp<-strsplit(filename,"")[[1]]
  ## read data from file
  if(paste(tmp[length(tmp)+-4:0],collapse="")==".xlsx"){
    dat<-read_xlsx(path=filename,skip=19)
  } else {
    dat<-read_xlsx(path=paste(filename,".xlsx",sep=""),
      skip=19)
  }
  dat
}

ave_by_sample<-function(dat){
  samps<-unique(dat$Sample)
  Cq<-vector()
  for(i in 1:length(samps)){
    ii<-which(dat$Sample==samps[i])
    Cq[i]<-mean(dat$Cq[ii])
  }
  Cq<-setNames(Cq,samps)
  names(Cq)[grep("reference",names(Cq),
    ignore.case=TRUE)]<-"Reference"
  delta.Cq<-Cq["Reference"]-Cq
  delta.Cq<-delta.Cq[
    -which(names(delta.Cq)=="Reference")]
  delta.Cq
}

gexp<-function(ref=NULL,file){
  if(is.null(ref)){
    dir_files<-list.files()
    ref<-dir_files[grep("Ef1a",dir_files,
      ignore.case=TRUE)]
  }
  ref_data<-get_excel_data(ref)
  target_data<-get_excel_data(file)
  
  ## ref
  
  return(target_data)
}

object<-gexp(ref=NULL,file=f2)

ave_by_sample(object)
