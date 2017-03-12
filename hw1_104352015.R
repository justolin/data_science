# cmd line inouts 
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_104352015.R -files input_file -out output_file", call.=FALSE)
}
# get the arguments from cmd line 
i<-1 
while(i < length(args))
{
  if(args[i] == "-files"){
    input_file <- args[i+1]
    i<-i+1
  }else if(args[i] == "-out"){
    out_file <- args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}
# read data
d <- read.csv(input_file)
nums <- sapply(d, is.numeric)
numd <- d[,nums]
# calculate maximum
maxcol <- round(apply(numd,2,max),2)
result <- rbind(c("set",colnames(numd)),c(args[1],maxcol))
#results output
write.table(result,out_file,sep=",",row.names = FALSE,col.names = FALSE)
