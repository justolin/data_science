########################
#### homework1 code ####
########################
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_104352015.R input", call.=FALSE)
} else if (length(args)==1) {
  d <- read.csv(args[1])
  nums <- sapply(d, is.numeric)
  numd <- d[,nums]
  ## summary maximum
  maxcol <- round(apply(numd,2,max),2)
  result <- rbind(c("set",colnames(numd)),c(args[1],maxcol))
  ## summary output csv
  write.table(result,args[2],sep=",",row.names = FALSE,col.names = FALSE)
}
