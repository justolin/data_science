library("e1071")
wine <- read.csv("wine data/wine_dat_white.csv")
wine$quality.f <- as.factor(wine$quality)

##### accuracy function #####
accuracy <- function(data_set,model){
  pred <- predict(model, data_set)
  accuracy <- sum(pred == data_set$quality)/length(pred)
  return(accuracy)
}

##### stratified function #####
stratified_index <- function(d,n_fold){
  id_results <- NULL
  category <- unique(d$quality)
  for (i in 1:length(category)){
    id <- as.numeric(row.names(d)) * (d$quality == category[[i]])
    id <- id[id != 0]
    id_f <- cbind(id,sample(1:n_fold, length(id), replace = TRUE))
    id_results <- rbind(id_results,id_f)
    colnames(id_results) <- c("id","strata")
  }
  return(id_results)
}

##### modeling #####
n_fold = 3 ## specify n of n fold cross validation 
exper = 30 ## specify experiment times of cross validation 


total_acc_null <- c()
total_acc_lin <- c()
total_acc_svm <- c()

results_null_all <- c()
results_lin_all <- c()
results_svm_all <- c()

for (j in 1:exper){
  
  results_null <- c()
  results_lin <- c()
  results_svm <- c()
  ### produce stratified sampling index 
  wine$id <- stratified_index(wine,n_fold)[,2]
  list <- 1:n_fold
  
  for (i in 1:n_fold){
    print(paste("doing",i,"fold modeling"))
    ### create training set and test set
    ### select rows with id = i to create test set
    ### select rows with the remaing id to create train set
    train_set <- subset(wine, id %in% list[c(-i)])[,!(names(wine) %in% c("id","quality.f"))]
    test_set <- subset(wine, id %in% list[i])[,!(names(wine) %in% c("id","quality.f"))]
    
    ### training set modeling - linear regression (null model) 
    null <- glm(quality ~ alcohol, data = train_set)
    
    ### predict for test set - linear regression (null model) 
    null_pred <- round(predict(null, test_set),0)
    
    ### predict results from train model - linear regression (null model) 
    null_pred <- cbind(null_pred,test_set$quality)
    results_null <- rbind(results_null,null_pred)
    results_null_all <- rbind(results_null_all,null_pred)
    
	  ### training set modeling - linear regression (all features) 
    linear <- glm(quality ~ ., data = train_set)
    
    ### predict for test set - linear regression (all features) 
    linear_pred <- round(predict(linear, test_set),0)
    
    ### predict results from train model - linear regression (all features) 
    linear_pred <- cbind(linear_pred,test_set$quality)
    results_lin <- rbind(results_lin,linear_pred)
    results_lin_all <- rbind(results_lin_all,linear_pred)
	
	  ####
	  train_set <- subset(wine, id %in% list[c(-i)])[,!(names(wine) %in% c("id","quality"))]
    test_set <- subset(wine, id %in% list[i])[,!(names(wine) %in% c("id","quality"))]
	
    ### training set modeling
    train_model <- svm(quality.f ~ .,data = train_set)
    
    ### predict results from train model
    prevalue <- as.character(predict(train_model, test_set))
    prevalue <- cbind(prevalue,as.character(test_set$quality))
    results_svm <- rbind(results_svm,prevalue)
    results_svm_all <- rbind(results_svm_all,prevalue)
    
  }
  
  print(paste(j,"experiment finish"))
  ### null model results
  total_acc_null <- c(total_acc_null,sum(results_null[,1] == results_null[,2])/dim(results_null)[[1]])
  ### linear regression model results
  total_acc_lin <- c(total_acc_lin,sum(results_lin[,1] == results_lin[,2])/dim(results_lin)[[1]])
  ### svm model results
  total_acc_svm <- c(total_acc_svm,sum(results_svm[,1] == results_svm[,2])/dim(results_svm)[[1]])

}

print(paste("mean of global accuracy rate of null model is"
            ,round(mean(total_acc_null),4)))
print(paste("mean of global accuracy rate of linear regression model is"
            ,round(mean(total_acc_lin),4)))
print(paste("mean of global accuracy rate of SVM model is"
            ,round(mean(total_acc_svm),4)))
print(paste("standard deviation of global accuracy rate of null model is"
            ,round(sqrt(var(total_acc_null)),4)))
print(paste("standard deviation of global accuracy rate of linear regression model is"
            ,round(sqrt(var(total_acc_lin)),4)))
print(paste("standard deviation of global accuracy rate of SVM model is"
            ,round(sqrt(var(total_acc_svm)),4)))

print("null model experiment average cofusion matrix")
print(round(table(results_null_all[,1],results_null_all[,2])/exper,0))
print("linear regression model average experiment cofusion matrix")
print(round(table(results_lin_all[,1],results_lin_all[,2])/exper,0))
print("SVM model experiment average cofusion matrix")
print(round(table(results_svm_all[,1],results_svm_all[,2])/exper,0))


