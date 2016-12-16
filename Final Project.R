###################################
#STAT 448 Final Project Source Code
#Jian Zhang     jzhng148
###################################
library(gplots)
library(ResourceSelection)
library(biotools)

#import dataset
student <- read.csv("/Volumes/ZJ/Dropbox/STAT 448/Final Project/student.csv")
#add 'pass' column (1 stands for pass, 0 stands for fail)
student$pass = NA
student$pass = ifelse(student$G3<12,0,1)
#add 'A' column (1 stands for A, 0 stands for otherwise)
student$A = NA
student$A = ifelse(student$G3<18,0,1)
#drop 'G1' and 'G2' score
student <- student[, -c(31,32)]

#EDA
#Take a look at data
head(student[,c(1:31,33)])
str(student[,c(1:31,33)])
dim(student[,c(1:31,33)])

#histogram of response variable
student[,33] <- as.factor(student[,33])
plot(student$pass, type = "h", ylim = c(0,600), xlab = "pass")

#Number of students who pass the exam (n=510)
sum(student$pass==1)
##Number of students who pass the exam (n=534)
sum(student$pass==0)
#heatmap
cov_melt <- melt(cor(student[,c(3,7,8,13:15,24:30)]))
ggplot(data = cov_melt, aes(x=Var1, y=Var2, fill=value)) + geom_tile() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())
#boxplot
boxplot(student[,c(3,7,8,13:15,24:30)])


#Split data into training and testing sets
set.seed(1)
index <- sample(nrow(student), nrow(student)*0.8)
train <- student[index, ]
test <- student[-index,]

###################################################################
# Goal: Identify students who pass the final exam (final score equal or greater than 12) and others who don't 
# Methods: Logistic Regression, Linear Discriminant Analysis, Quadratic Discriminant Analysis
###################################################################
#Classification Rate
rate <- function(actual, predicted) {
   mean(actual == predicted)
}

#############Logistic Regression
logistic <- glm(pass ~ .-G3-type-A, data = train, family = "binomial")

#Since logistic regression provide probabilities, so we set cutoff value and classify them into two classes.
prediction <- function(pred, cutoff=0.5){
  ifelse(pred>cutoff, 1, 0)
}

#Prediction from Logistic Regression
lg_train_pred <- prediction(logistic$fitted.values)
lg_test_pred <- prediction(predict(logistic, newdata=test))
#Confusion Matrix of training set
table(lg_train_pred, train$pass)
#Confusion Matrix of testing set
table(lg_test_pred, test$pass)
#Get training classification rate of logistic regression (0.7413174)
rate(train$pass, lg_train_pred)
#Get training classification rate of logistic regression (0.6602871)
rate(test$pass, lg_test_pred)

#Summary statistics
summary(logistic)
#Goodness of Fit
hoslem.test(as.numeric(train$pass),lg_train_pred)
#Stepwise selection
logistic_stepwise <- step(logistic, direction = "both", trace=0)
summary(logistic_stepwise)
plot(logistic_stepwise)
#Goodness of Fit
hoslem.test(train_pred, as.numeric(train$pass))
#Outlier diagnostic
plot(logistic_stepwise)

#Refit feature-selected model
train_refit <- predict(logistic_stepwise, newdata=train)
test_refit <- predict(logistic_stepwise, newdata=test)
train_pred <- prediction(train_refit)
test_pred <- prediction(test_refit)
#Training rate (0.7101796)
rate(train$pass, train_pred)
#Testing rate (0.6746411)
rate(test$pass, test_pred)

#############Linear Discriminant Analysis
library(MASS)
library(klaR)
lda_fit <- lda(pass ~ .-G3-type-A, data = train)
lda_train_pred <- predict(lda_fit, newdata = train)
lda_test_pred <- predict(lda_fit, newdata = test)

#Confusion Matrix of training set
table(lda_train_pred$class, train$pass)
#Confusion Matrix of testing set
table(lda_test_pred$class, test$pass)
#get training classification rate of Linear Discriminant Analysis (0.7353293)
rate(train$pass, lda_train_pred$class)
#get testing classification rate of Linear Discriminant Analysis (0.6507177)
rate(test$pass, lda_test_pred$class)

#############Quadratic Discriminant Analysis
qda_fit <- qda(pass ~ .-G3-type-A, data = train)
qda_train_pred <- predict(qda_fit, newdata = train)
qda_test_pred <- predict(qda_fit, newdata = test)

#Confusion Matrix of training set
table(qda_train_pred$class, train$pass)
#Confusion Matrix of testing set
table(qda_test_pred$class, test$pass)
#get training classification rate of Linear Discriminant Analysis (0.794012)
rate(train$pass, qda_train_pred$class)
#get testing classification rate of Linear Discriminant Analysis (0.6363636)
rate(test$pass, qda_test_pred$class)

#Stepwise selection on Linear Discriminant Analysis
newtrain <- train[, c(1:30, 33)]
stepclass(pass ~ ., data = newtrain, method = "lda", 
          maxvar=20, direction ="both", criterion = "AC")

lda_stepwise <- lda(pass ~ age + Medu + Fedu + traveltime + studytime + failures + goout + 
                           Dalc + health + absences, data=train)

lda_stepwise_train_pred <- predict(lda_stepwise, newdata = train)
lda_stepwise_test_pred <- predict(lda_stepwise, newdata = test)
#get training classification rate of Linear Discriminant Analysis (0.6838323)
rate(train$pass, lda_stepwise_train_pred$class)
#get testing classification rate of Linear Discriminant Analysis (0.6602871)
rate(test$pass, lda_stepwise_test_pred$class)
#plot histogram and overlayed density plots
plot(lda_stepwise, dimen=1, type="both")

###################################################################
#Perform PCA on the student dataset
#Apply K Means Clustering on the dataset
###################################################################
#Convert categorical data into numerical data
student1 <- student
student1$school <- as.numeric(student1$school)
student1$sex <- as.numeric(student1$sex)
student1$address <- as.numeric(student1$address)
student1$famsize <- as.numeric(student1$famsize)
student1$Pstatus <- as.numeric(student1$Pstatus)
student1$Mjob <- as.numeric(student1$Mjob)
student1$Fjob <- as.numeric(student1$Fjob)
student1$reason <- as.numeric(student1$reason)
student1$guardian <- as.numeric(student1$guardian)
student1$schoolsup <- as.numeric(student1$schoolsup)
student1$famsup <- as.numeric(student1$famsup)
student1$paid <- as.numeric(student1$paid)
student1$activities <- as.numeric(student1$activities)
student1$nursery <- as.numeric(student1$nursery)
student1$higher <- as.numeric(student1$higher)
student1$internet <- as.numeric(student1$internet)
student1$romantic <- as.numeric(student1$romantic)


#Principle Component Analysis
student_pca <- prcomp(student1[,1:30])
student_pca
#Plot PCA
biplot(student_pca, scale = 0, cex = 0.5, xlim=c(-0.5,0.2), ylim=c(-4,2))
biplot(student_pca, scale = 0, cex = 0.5, xlim=c(-40,10), ylim=c(-5,5))
#Select PCs
get_PVE <- function(pca_out) {
  pca_out$sdev ^ 2 / sum(pca_out$sdev ^ 2)
}
pve <- get_PVE(student_pca)
plot(
  cumsum(pve),
  xlab = "Principal Component",
  ylab = "Cumulative Proportion of Variance Explained",
  ylim = c(0.6, 1),
  type = 'b'
)
summary(student_pca)
#Select top 10 PCs
student_reduce <- as.data.frame(student_pca$x[, 1:10])
student_reduce$pass <- student$pass

#split data
student_reduce_train <- student_reduce[index,]
student_reduce_test <- student_reduce[-index,]

#Logistic Regression
#Get training classification rate of logistic regression (0.6467066)
rate(student_reduce_train$pass, prediction(glm(pass ~ ., data = student_reduce_train, 
                                  family = "binomial")$fitted.values))
#Get testing classification rate of logistic regression (0.6794258)
rate(student_reduce_test$pass, prediction(glm(pass ~ ., data = student_reduce_test, 
                                               family = "binomial")$fitted.values))

#Linear Discriminant Analysis
#get classification rate of Linear Discriminant Analysis (0.645509)
rate(student_reduce_train$pass, predict(lda(pass ~ ., data = student_reduce_train), 
                           newdata = student_reduce_train)$class)
#get classification rate of Linear Discriminant Analysis (0.6889952)
rate(student_reduce_test$pass, predict(lda(pass ~ ., data = student_reduce_test), 
                           newdata = student_reduce_test)$class)
