##### function
##### produce AUC #####
library('ROCR')
get_AUC <- function(pred_score,reference) {
  predict <- prediction(pred_score,reference)
  auc <- attributes(performance(predict,'auc'))$y.values[[1]]
  return (auc)
}

##### read parameters #####
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_104352015.R --target male/female --files meth1 meth2 ... --out result.csv", call.=FALSE)
}

##### parse parameters #####
i<-1
while(i < length(args))
{
  if(args[i] == "--target"){
    target<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("-", c(args[(i+1):length(args)],"-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :",target))
print(paste("output file:",out_f))
print(paste("files:",files))

if (target == "male"){
  target_n <- "female"
}else if(target == "female"){
  target_n <- "male"
}

##### read files #####
method <- c()
sensitivity <- c()
specificity <-c()
F1 <- c()
AUC <- c()
for(file in files){
  d <- read.table(file, header=TRUE , sep=",")
  TP <- length(which(d$prediction == target & d$reference == target))
  TN <- length(which(d$prediction == target_n & d$reference == target_n))
  FP <- length(which(d$prediction == target & d$reference == target_n))
  FN <- length(which(d$prediction == target_n & d$reference == target))
  sensitivity<-c(sensitivity, TP/(TP+FN))
  specificity<-c(specificity, TN/(TN+FP))
  F1 <- c(F1, 2*TP/(2*TP+FP+FN))
  AUC <- c(AUC, get_AUC(d$pred.score, d$reference))
  method <- c(method,gsub(".csv", "", basename(file)))
}

##### results formated and output #####
out_data<-data.frame(method = method, sensitivity = sensitivity, 
                     specificity = specificity,F1 = F1, AUC = AUC,
                     stringsAsFactors = FALSE)
max_index <- apply(out_data[,-1],2,which.max)
max_method <- c("highest",method[max_index])
out_data <- out_data[rank(out_data$method),]
out_data <- format(out_data, digits=2)
out_data <- rbind(out_data,max_method)
out_data <- sort()
if (grepl("csv",out_f) == TRUE){
  write.table(out_data,out_f, row.names = FALSE,sep=",")
}else if (grepl("csv",out_f) == FALSE){
  write.table(out_data, file=out_f, row.names = F, quote = F)
}

##### CODE END #####
