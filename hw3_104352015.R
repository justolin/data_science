library('ROCR')
##### function
##### produce AUC simulation #####
get_AUC <- function(pred_score,reference) {
  pre <- prediction(pred_score,reference)
  auc <- attributes(performance(pre,'auc'))$y.values[[1]]
  return (auc)
}

##### read parameters #####
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw3_104352015.R --target male/female --files file1 file2 ... --out result.csv", call.=FALSE)
}

##### parse parameters #####
i<-1
while(i < length(args))
{
  if(args[i] == "--target"){
    target<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("--", c(args[(i+1):length(args)],"--"))[1]
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

##### read files
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

##### initial results formated
out_data<-data.frame(method = method, sensitivity = sensitivity, 
                     specificity = specificity,F1 = F1, AUC = AUC,
                     stringsAsFactors = FALSE)

##### best two classifier performance
##### fisher's exact test
if (dim(out_data)[1] == 1) {
  stop("need multiple files to finish", call.=FALSE)
}

pvalue <-  c()

for (i in 2:dim(out_data)[[2]]){
  file1 <- files[order(out_data[,i], decreasing=TRUE)[1]]
  file2 <- files[order(out_data[,i], decreasing=TRUE)[2]]
  file1_d <- read.table(file1, header=TRUE , sep=",")
  file2_d <- read.table(file2, header=TRUE , sep=",")
  a <- length(which(file1_d$prediction == target & file2_d$prediction == target))
  b <- length(which(file1_d$prediction == target_n & file2_d$prediction == target))
  c <- length(which(file1_d$prediction == target & file2_d$prediction == target_n))
  d <- length(which(file1_d$prediction == target_n & file2_d$prediction == target_n))
  table <- as.table(matrix(c(a, b, c, d), ncol=2, byrow=TRUE))
  pvalue <- c(pvalue,fisher.test(table)$p.value)
}

##### create row which method is best
##### if fisher's exact test < 0.05 then show *
max_method <-c()

max_index <- apply(out_data[,-1],2,which.max)
for(j in 1:length(max_index)){
  max_method[j] <- ifelse(pvalue[j] > 0.05,method[max_index[j]],paste(method[max_index[j]],"*", sep = ""))
}
max_method <- c("highest",max_method)

##### final results and output
out_data <- format(out_data,digits=2)
out_data <- rbind(out_data,max_method)
write.table(out_data, file=out_f, row.names = F, quote = F ,sep=",")

##### CODE END #####
