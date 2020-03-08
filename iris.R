# Read csv file 
data = read.csv("iris_data.csv")
# Explore the overall structure of the file
ncol(data) # number of features/variables
colnames(data) # name of the columns
nrow(data) # number of observations

# Have a look at the data - first and last 15 rows
head(data, n= 15)
tail(data, n= 15)

# Structure of data; Summary of the data; Type of variable - numeric and categorical 
str(data)

# Overall statisctics min, max, average, quantiles of numerical variables; 
# From categorical ones - shows how many obervations from every class the data set has 
# Аll classes are well-represented, equal obs from each class and it will not skew the stats (balance data set)
summary(data)


# Visual representation - histogram, of every variable 

# Histogram of first column, it looks like normal distribution, a little bit skewed to the left with fat left tail and long right tail
hist(data$sepal.length)
# Looks like perfect normal distribution
hist(data$sepal.width) 
# It is not a normal distriibution, there are two peaks - one in around 1 and andother one in 4 
hist(data$petal.length)
# The same as above - around 0.3 and 1.2
hist(data$petal.width)

# install.packages("ggplot2")
library(ggplot2)
ggplot(data,aes(x=data$sepal.length, fill=data$iris)) + geom_histogram()
ggplot(data,aes(x=data$sepal.width, fill=data$iris)) + geom_histogram()
ggplot(data,aes(x=data$petal.length, fill=data$iris)) + geom_histogram()
ggplot(data,aes(x=data$petal.width, fill=data$iris)) + geom_histogram()

# Comparison between two numeric and 1 categorical variable   

pairs(data[,1:4], pch = 21,  cex = 0.4, col = data$iris)

ggplot(data, aes(data$sepal.length, data$sepal.width,colour = data$iris)) + geom_point()

ggplot(data, aes(data$petal.length, data$petal.width,colour = data$iris)) + geom_point()

# install.packages('data.tables')

#data$iris = as.character(data$iris)
aggregate(data[,1:4], list(data$iris), mean)

aggregate(data[,1:4], list(data$iris), var)

# Comparison between 1 numeric and 1 categorical variable
hist(data[data$iris=='Iris-versicolor','petal.length'])

# Boxplots
ggplot(data, aes(x =data$iris, y = data$petal.length, fill = data$iris)) + geom_boxplot()
ggplot(data, aes(x =data$iris, y = data$sepal.length, fill = data$iris)) + geom_boxplot()
ggplot(data, aes(x =data$iris, y = data$petal.width, fill = data$iris)) + geom_boxplot()
ggplot(data, aes(x =data$iris, y = data$sepal.width, fill = data$iris)) + geom_boxplot()

# Violin plots
ggplot(data, aes(x=data$iris, y = data$petal.length, fill = data$iris)) + geom_violin()
ggplot(data, aes(x =data$iris, y = data$sepal.length, fill = data$iris)) + geom_violin()
ggplot(data, aes(x =data$iris, y = data$petal.width, fill = data$iris)) + geom_violin()
ggplot(data, aes(x =data$iris, y = data$sepal.width, fill = data$iris)) + geom_violin()

# Correlation between two numerical variables
cols <- c("#00AFBB","#FC4E07")
rho1 <- round(cor(data$sepal.length,data$sepal.width),3)
plot(data$sepal.length,data$sepal.width,col = cols, main=paste("rho: ", rho1))
abline(lm(data$sepal.width ~ data$sepal.length), lwd=2)

rho2 <- round(cor(data$petal.length,data$petal.width),3)
plot(data$petal.length,data$petal.width,col = cols, main=paste("rho: ", rho2))
abline(lm(data$petal.width ~ data$petal.length), lwd=2)

rho3 <- round(cor(data$sepal.length,data$petal.width),3)
plot(data$sepal.length,data$petal.width,col = cols, main=paste("rho: ", rho3))
abline(lm(data$petal.width ~ data$sepal.length), lwd=2)

rho4 <- round(cor(data$petal.length,data$sepal.width),3)
plot(data$petal.length,data$sepal.width,col = cols, main=paste("rho: ", rho4))
abline(lm(data$sepal.width ~ data$petal.length), lwd=2)

rho5 <- round(cor(data$petal.length,data$sepal.length),3)
plot(data$petal.length,data$sepal.length,col = cols, main=paste("rho: ", rho5))
abline(lm(data$sepal.length ~ data$petal.length), lwd=2)
  
rho6 <- round(cor(data$petal.width, data$sepal.width),3)
plot(data$petal.width,data$sepal.width,col = cols, main=paste("rho: ", rho6))
abline(lm(data$sepal.width ~ data$petal.width), lwd=2)

# Correlation Matrix  
cor(data[,1:4])
