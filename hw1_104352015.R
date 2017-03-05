## data input
setwd("C:/Users/user/Desktop/SPHW1")
data <- read.csv("test.1.csv")
## summary smax
maxw <- round(max(data[,2]),2)
maxh <- round(max(data[,3]),2)
result <- rbind(c("set","weight","height"),c("test.1",maxw,maxh))
## summary output csv
write.table(result,"result.csv",sep=",",row.names = FALSE,col.names = FALSE)