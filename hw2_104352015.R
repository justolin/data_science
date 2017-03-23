##### function
##### produce AUC simulation #####
get_AUC <- function(pos.scores,neg.scores,n_sample=1000000) {
  # Arguments:
  #   pos.scores: vectors of positive observations' scores 
  #   neg.scores: vectors of negative observations' scores 
  #   n_samples : number of samples to approximate AUC
  pos.sample <- sample(pos.scores, n_sample, replace=TRUE)
  neg.sample <- sample(neg.scores, n_sample, replace=TRUE)
  mean(1.0*(pos.sample > neg.sample) + 0.5*(pos.sample==neg.sample))
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
    j<-grep("--", c(args[(i+1):length(args)], "--"))[1]
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
for(file in files)
{
  name <- gsub("meth","method",basename(file))
  file_names <- paste(name,".csv",sep = "")
  d <- read.table(file_names, header=TRUE , sep=",")
  TP <- length(which(d$prediction == target & d$reference == target))
  TN <- length(which(d$prediction == target_n & d$reference == target_n))
  FP <- length(which(d$prediction == target & d$reference == target_n))
  FN <- length(which(d$prediction == target_n & d$reference == target))
  sensitivity<-c(sensitivity, TP/(TP+FN))
  specificity<-c(specificity, TN/(TN+FP))
  F1 <- c(F1, 2*TP/(2*TP+FP+FN))
  pos <- subset(d,reference == target)[,4]
  neg <- subset(d,reference == target_n)[,4]
  AUC <- c(AUC, get_AUC(pos,neg))
  method<-c(method,name)
}

##### results formated and output #####
out_data<-data.frame(method = method, sensitivity = sensitivity, 
                     specificity = specificity,F1 = F1, AUC = AUC,
                     stringsAsFactors = FALSE)
max_index <- apply(out_data[,2:5],2,which.max)
max_method <- c("method",method[max_index])
out_data <- out_data[rank(method),]
out_data <- format(head(out_data), digits=2) 
out_data <- rbind(out_data,max_method)
write.table(out_data,out_f, row.names = FALSE,sep=",")

##### CODE END #####