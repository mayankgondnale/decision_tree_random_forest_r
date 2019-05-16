library(readxl)
#extract data into R
data.white <- read_excel("D:/UIC/Fall 2017/575 Business Analytic Statistics/Project/winequality/White wine.xlsx")

#check number of rows and columns for complete data
nrow(data.white) #4898
ncol(data.white) #12

#check column names
colnames(data.white)

##### DATA PREPROCESSING #####

#rename column names with space to replace the space with underscore
names(data.white)[1] <- "fixed_acidity"
names(data.white)[2] <- "volatile_acidity"
names(data.white)[3] <- "citric_acid"
names(data.white)[4] <- "residual_sugar"
names(data.white)[6] <- "free_sulfur_dioxide"
names(data.white)[7] <- "total_sulfur_dioxide"

#check for nulls
sum(is.na(data.white))
#no nulls found

#find characteristics of variables
#find range for all variables
range <- c()
for (i in 1:(ncol(data.white)-1))
{
  range <- rbind(range, range(data.white[i])[2] - range(data.white[i])[1])
}

#find mean for all variables
mean <- c()
for (i in 1:(ncol(data.white)-1))
{
  mean <- rbind(mean,mean(data.white[[i]]))
}
mean <- round(mean, digits = 2)  

#take column names
colnames <- c()
for (i in 1:(ncol(data.white)-1))
{
  colnames <- rbind(colnames, colnames(data.white)[i])
}

#put characteristics in one dataframe
characteristics <- data.frame(cbind(colnames, mean, range))
colnames(characteristics) <- c("variable", "mean", "range")

#print variable name, its mean and its range
characteristics

#create histograms for each variable to check for outliers
hist(data.white$fixed_acidity)
hist(data.white$volatile_acidity)
hist(data.white$citric_acid)
hist(data.white$residual_sugar)
hist(data.white$chlorides)
hist(data.white$free_sulfur_dioxide)
hist(data.white$total_sulfur_dioxide)
hist(data.white$density)
hist(data.white$pH)
hist(data.white$sulphates)
hist(data.white$alcohol)

#subset the dataset by removing the outliers that were outside 1.5 times the standard deviation
dataout = subset(data.white, data.white$fixed_acidity<(mean(data.white$fixed_acidity)+1.5*sd(data.white$fixed_acidity)))
dataout = subset(data.white, data.white$volatile_acidity<(mean(data.white$volatile_acidity)+1.5*sd(data.white$volatile_acidity)))
dataout = subset(data.white, data.white$citric_acid<(mean(data.white$citric_acid)+1.5*sd(data.white$citric_acid)))
dataout = subset(data.white, data.white$residual_sugar<(mean(data.white$residual_sugar)+1.5*sd(data.white$residual_sugar)))
dataout = subset(data.white, data.white$chlorides<(mean(data.white$chlorides)+1.5*sd(data.white$chlorides)))
dataout = subset(data.white, data.white$free_sulfur_dioxide<(mean(data.white$free_sulfur_dioxide)+1.5*sd(data.white$free_sulfur_dioxide)))
dataout = subset(data.white, data.white$total_sulfur_dioxide<(mean(data.white$total_sulfur_dioxide)+1.5*sd(data.white$total_sulfur_dioxide)))
dataout = subset(data.white, data.white$density<(mean(data.white$density)+1.5*sd(data.white$density)))
dataout = subset(data.white, data.white$pH<(mean(data.white$pH)+1.5*sd(data.white$pH)))
dataout = subset(data.white, data.white$sulphates<(mean(data.white$sulphates)+1.5*sd(data.white$sulphates)))
dataout = subset(data.white, data.white$alcohol<(mean(data.white$alcohol)+1.5*sd(data.white$alcohol)))

#check for rows after removing outliers
nrow(dataout) #4392

#split data into training and testing (80:20)
library(caret)
set.seed(280)
wine_index <- createDataPartition(dataout$quality, p = .8, list = FALSE)
white.train <- dataout[ wine_index, ]
white.test <- dataout[-wine_index, ]

#check number of row in training and testing
nrow(white.train) #3515
nrow(white.test) #877


##### MODELING #####

#LINEAR REGRESSION
set.seed(7272)
lm_fit <- lm(quality ~ ., data = white.train)
wine_pred <- predict(lm_fit, white.test)
lm_fit
summary(lm_fit)

#remove insignificant variable based on the p-values obtained from training data
data_new_train = white.train[,-3]
data_new_train1 = data_new_train[,-4]
data_new_train2 = data_new_train1[,-5]

#remove insignificant variable based on the p-values obtained from testing data
data_new_test = white.test[,-3]
data_new_test1 = data_new_test[,-4]
data_new_test2 = data_new_test1[,-5]

#check number of columns in new train and test data
ncol(data_new_test2)  #9
ncol(data_new_train2)  #9

#fit the Linear Model again
set.seed(8877)
lmfit2 <- train(quality ~ ., data = data_new_train2, method = "lm")
wine_pred2 <- predict(lmfit2, data_new_test2)
lmfit2
summary(lmfit2)

#DECISION TREE
library(tree)
set.seed(1991)
Dtree = tree(quality ~ ., data = white.train)
summary(Dtree)
plot(Dtree)
text(Dtree)

prediction <- predict(Dtree, white.test)
MSEdt <-  mean((white.test$quality - prediction)^2)
MSEdt  #0.5654668


#use cv.tree() to find optimal size
crossv = cv.tree(Dtree)
plot(crossv$size ,crossv$dev ,type='b')
#best size obtained 5


Dtreeprune=prune.tree(Dtree ,best=5)
plot(Dtreeprune)
text(Dtreeprune)
#no change in tree

prune.prediction = predict(Dtreeprune, white.test)
MSEdtprune <- mean((white.test$quality - prune.prediction)^2)
MSEdtprune  #0.5654668
#no change in mse


#RANDOM FOREST
library(randomForest)
set.seed(121)
rf.model <- randomForest(quality ~ ., data = white.train, mtry = 10, ntree = 100, importance = T)
rf.pred <- predict(rf.model, white.test)

MSErf <- mean((white.test$quality - rf.pred)^2)
MSErf  #0.3571055

#finding important variables
importance(rf.model)


######## MODEL OPTIMIZATION ########

#checking rows and columns of data
nrow(dataout)
ncol(dataout) 
colnames(dataout)  

#Subsetting the filtered data for only required columns
#create a new data frame with only 3 independent variables
new_data <-  subset(dataout, select=c("alcohol","volatile_acidity","free_sulfur_dioxide"))
nrow(new_data) #4392
ncol(new_data)  #3
colnames(new_data) 


#scale the data in order to have a more normalized data and create a data frame again
scaled_data <- scale(new_data)
scaled_data <- data.frame(scaled_data)

#add dependent variable into the scaled data frame
final_data <- cbind(scaled_data, quality = dataout$quality)
nrow(final_data)  #4392
ncol(final_data)  #4
colnames(final_data)


#find characteristics of variables
#find range for all variables
nrange <- c()
for (i in 1:(ncol(final_data)-1))
{
  nrange <- rbind(nrange, range(final_data[i])[2] - range(final_data[i])[1])
}
nrange <- round(nrange, digits = 2)

#find mean for all variables
nmean <- c()
for (i in 1:(ncol(final_data)-1))
{
  nmean <- rbind(nmean,mean(final_data[[i]]))
}
nmean <- round(nmean, digits = 2)  

#take column names
ncolnames <- c()
for (i in 1:(ncol(final_data)-1))
{
  ncolnames <- rbind(ncolnames, colnames(final_data)[i])
}

#put characteristics in one dataframe
ncharacteristics <- data.frame(cbind(ncolnames, nmean, nrange))
colnames(ncharacteristics) <- c("nvariable", "nmean", "nrange")

#print variable name, its mean and its range
ncharacteristics


#splitting scaled data into training and testing in the ratio 80:20
library(caret)
set.seed(292)
wine_index_new <- createDataPartition(final_data$quality, p = .8, list = FALSE)
white_train_new <- final_data[ wine_index_new, ]
white_test_new <- final_data[-wine_index_new, ]

nrow(white_train_new)
#3515
nrow(white_test_new)
#877


#DECISION TREE on scaled data
set.seed(2004)
Dtree_new = tree(quality ~ ., data = final_data)
summary(Dtree_new)
plot(Dtree_new)
text(Dtree_new)
#tree has changed

prediction_new <- predict(Dtree_new, white_test_new)
MSE_dt_new <-  mean((white_test_new$quality - prediction_new)^2)
MSE_dt_new  #0.5940833
#MSE has increased


#use cv.tree() to find optimal size
crossv_new = cv.tree(Dtree_new)
plot(crossv_new$size ,crossv_new$dev ,type='b')
#best size obtained 7


Dtreeprune_new=prune.tree(Dtree_new ,best=7)
plot(Dtreeprune_new)
text(Dtreeprune_new)
#no change in tree

prune.prediction_new = predict(Dtreeprune_new, white_test_new)
MSEdtprune_new <- mean((white_test_new$quality - prune.prediction_new)^2)
MSEdtprune_new  #0.5940833
#MSE is same



#RANDOM FOREST on scaled data
library(randomForest)
set.seed(343)
rf_model_new <- randomForest(quality ~ ., data = white_train_new, mtry = 3, ntree = 100, importance = T)
rf_pred_new <- predict(rf_model_new, white_test_new)

MSErf <- mean((white_test_new$quality - rf_pred_new)^2)
MSErf #0.496207
#MSE has increased


#########################