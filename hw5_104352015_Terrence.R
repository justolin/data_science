##### packages needed #####
library.path <- cat(.libPaths())
install.packages("e1071",repos = "http://cran.us.r-project.org")
library("e1071", lib.loc = library.path)

##### read parameters #####
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw5_104352015_Terrence.R -fold n ¡Vout performance.csv", call.=FALSE)
}

##### parse parameters #####
i<-1
while(i < length(args)){
  if(args[i] == "-fold"){
    n_fold <- args[i+1]
    i <- i+1
  }else if(args[i] == "-out"){
    out_file <- args[i+1]
    i <- i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("fold :",n_fold,"folds"))
print(paste("output file:",out_file))

if (n_fold < 3) {
  stop("USAGE: You should specify fold larger not lower than 3", call.=FALSE)
}

##### data inputs #####
print("data loading")
data <- read.csv("Archaeal_tfpssm.csv",header=F)
data <- data[,-5603]

##### accuracy function #####
accuracy <- function(data_set){
  pred <- predict(train_model, data_set)
  accuracy <- sum(pred == data_set$V2)/length(pred)
  return(accuracy)
}

##### modeling #####
# sample from 1 to n_fold
data$id <- sample(1:n_fold, nrow(data), replace = TRUE)
list <- 1:n_fold

train_acc <- c()
calibr_acc <- c()
test_acc <- c()

for (i in 1:n_fold){
  print(paste("doing",i,"fold modeling"))
  ### create training set, calibration set and test set
  if(i == n_fold){
    train_set <- subset(data, id %in% list[c(-1,-i)])[,!(names(data) %in% c("id"))]
    calibr_set <- subset(data, id %in% list[1])[,!(names(data) %in% c("id"))]
    test_set <- subset(data, id %in% list[i])[,!(names(data) %in% c("id"))]
  }else{
    # select rows with id = i to create test set
    # select rows with id = i+1 to create calibration set
    # select rows with the remaing id to create train set
    train_set <- subset(data, id %in% list[c(-i,-i-1)])[,!(names(data) %in% c("id"))]
    calibr_set <- subset(data, id %in% list[i+1])[,!(names(data) %in% c("id"))]
    test_set <- subset(data, id %in% list[i])[,!(names(data) %in% c("id"))]
  }
  
  ### training set modeling
  train_model <- svm(V2 ~ ., data = train_set)
  
  ### predict and calculate accuracy for three set
  train_acc <-  c(train_acc,accuracy(train_set))
  calibr_acc <-  c(calibr_acc,accuracy(calibr_set))
  test_acc <-  c(test_acc,accuracy(test_set))
}
##### outputs processing and file output #####
row_name <- c("trainning","calibration","test")
accuracy_mean <- round(c(mean(train_acc),mean(calibr_acc),mean(test_acc)),2)
out_data <- data.frame("set" = row_name, "accuracy" = accuracy_mean, stringsAsFactors = F)
write.csv(out_data, file = out_file, row.names = F, quote = F)
