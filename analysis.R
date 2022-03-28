#Assignment 4
#Submitted By: Abinaya Elanchezhian


#Question 1
mydata <- read.csv("vehicles.csv")
head(mydata)

#Question 2
str(mydata)
dim(mydata)
colnames(mydata)

#Question 3
mydata.features <- mydata
results <- kmeans(mydata.features, 2)
results
plot(mydata[c("horse", "length")], col=results$cluster) 
points(results$centers[,c("horse", "length")], col = 1:3,  pch = 8, cex=2)

#Question 4
results <- kmeans(mydata.features, 3)
results
plot(mydata[c("horse", "length")], col=results$cluster)
results <- kmeans(mydata.features, 4)
results
plot(mydata[c("horse", "length")], col=results$cluster)
results <- kmeans(mydata.features, 5)
results
plot(mydata[c("horse", "length")], col=results$cluster)

#Question 5
results <- kmeans(mydata.features, 3)
results
plot(mydata[c("horse", "length")], col=results$cluster)
points(results$centers[,c("horse", "length")], col = 1:3,  pch = 8, cex=2)
results <- kmeans(mydata.features, 4)
results
plot(mydata[c("horse", "length")], col=results$cluster)
points(results$centers[,c("horse", "length")], col = 1:3,  pch = 8, cex=2)
results <- kmeans(mydata.features, 5)
results
plot(mydata[c("horse", "length")], col=results$cluster)
points(results$centers[,c("horse", "length")], col = 1:3,  pch = 8, cex=2)

#Question 6
wssplot <- function(mydata, nc=15, seed=1234){
     wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
     for (i in 2:nc){
       set.seed(seed)
       wss[i] <- sum(kmeans(mydata, centers=i)$withinss)}
     plot(1:nc, wss, type="b", xlab="Number of Clusters",
          ylab="Within groups sum of squares")
     wss
   }
wssplot(mydata)

#Question 7
results <- kmeans(mydata.features, 2)
plot(mydata, col = results$cluster)