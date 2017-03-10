library(mice)
library(randomForest)
library(Boruta)
library(mlbench)
library(caret)
library(doParallel)

getwd()
setwd("C:/Users/Rasika/Desktop/ML and DM/Project")
getwd()
dataset <- read.csv("train_midterm_data.csv")
dataset_new <- dataset[1:8988,]
head(dataset)
ncol(dataset)
nrow(dataset)
#59381
#note to self: loop in R,goes through each row if iterated through columns.
percentage_na <- function(data,max_col) {
  for(i in 1:max_col)
  {
    if(sum(is.na(data[,i])) > 0)
    {
      ratio <- sum(is.na(data[,i]))/nrow(data) * 100
      print(paste("Column",i,":",ratio))
    }
  }
}

########################################################################
#imputing by median
manage_na <- function(datafra)
{
	for(i in 1:ncol(datafra))
	{
		if(is.numeric(datafra[,i]))
		{
			datafra[is.na(datafra[,i]),i] <- median(datafra[!is.na(datafra[,i]),i])
		}
	}
	datafra
}

#Median imputed dataset
median_data <- manage_na(dataset)
########################################################################

#removed 1 since customer id
percentage_na(dataset,128)
a <- dataset[,-c(1,30,35,36,37,38,48,53,62,70,128)]
b <- dataset[,-c(1,30,35,36,37,38,48,53,62,70)]
ncol(a)
head(a,2)
ncol(b)
percentage_na(a,117)
percentage_na(b,118)

str(a)
#new_train <- mice(a,m=5,maxit=10,meth='pmm',seed=500) 

#summary(new_train)
#ncol(new_train$data)
#complete_data <- complete(new_train,1)
#complete_data

#to make the algorithms run via parallel processing
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

complete_data <- na.omit(b)
nrow(complete_data)
percentage_na(complete_data,117)

#training with boruta
boruta.train <- Boruta(Response ~ .- Product_Info_2, data = complete_data, doTrace=2)
print(boruta.train) 
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
print(boruta.df[which(boruta.df$decision == "Rejected"),])


write.csv(complete_data,"columnnumbers.csv")

#getting the important variables only
new_complete_data <- complete_data[,-c(1,5,6,7,32,54,55,62,64,71,74,76,77,86,89,95,101,108,113)]
ncol(new_complete_data)

#splitting into train and test
new_complete_data_train <- new_complete_data[1:26965,]
new_complete_data_test <- new_complete_data[26966:35953,]
str(new_complete_data )
ncol(new_complete_data)

#######################################################################

# col numbers that are numerical -> 5,9,10,11,12,13,16,18,30,35,36,37,38
# Numerical data set
num_median_data <- median_data[,c(5,9,10,11,12,13,16,18,30,35,36,37,38)]
rest_median_data <- median_data[,-c(5,9,10,11,12,13,16,18,30,35,36,37,38)]

#applying principal component analysis
num_med_pca <- prcomp(num_median_data, center = TRUE, scale. = TRUE)
num_med_pca
summary(num_med_pca)
plot(num_med_pca, type = "l")

## number of components that we'll use -> 4,5
num_pca_select <- as.data.frame(num_med_pca$x)
num_pca_select <- num_pca_select[,1:4]


#######################################################################
#applying principal component analysis
new_complete_data.pca <- prcomp(new_complete_data[,-c(1,99)], center = TRUE, scale. = TRUE)
print(new_complete_data.pca)
summary(new_complete_data.pca)
plot(new_complete_data.pca, type = "l")

#applying linear regression
main_model <- lm(Response  ~ . , data = new_complete_data_train)
summary(main_model)
AIC(main_model)

#predicting linear regression
predictions <- predict(main_model, new_complete_data_test[,1:98])
summary(predictions)

#l <- lm(Response ~ . , data = complete_data)
#summary(l)

#correlation matrix for the new data for further removing multicollinearity
correlationMatrix <- cor(new_complete_data[,-1])
correlationMatrix
highlycorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)
highlycorrelated 

#stepwise regression
ridgec <- lm.ridge (Response ~ .,data = complete_data, lambda = 0.05)
plot(ridgec)
select(ridgec)
names(ridgec)
str(ridgec$coef)

#running random forest
new_complete_data$Response <- as.factor(new_complete_data$Response)
rf_output <- randomForest(as.factor(Response) ~ ., data = new_complete_data_train, ntree = 1000 )
sort(rf_output$importance)
ncol(new_complete_data_train)
varImpPlot(rf_output,n.var = min(40) )
importance(rf_output)
preditions_new <- predict(rf_output, new_complete_data_test)
mean(preditions_new == new_complete_data_test$Response)
rf_output

#outcome
submit <- data.frame(PassengerId = dataset_new$Id, round(Survived = preditions_new))
submit

#control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
#model <- rfe(complete_data[,1:116],complete_data[,117], sizes = c(1:116), rfeControl = control)

library(glmnet)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)

attach(complete_data)

lmout <- lm(Response~ Wt + BMI, data = complete_data)
summary(lmout)
AIC(lmout)
vif(lmout)
VIF(lmout)


########################################################################

#running random forest
set.seed(20)
head(rest_median_data)

# Converting to factor
rest_median_data$Response <- as.factor(rest_median_data$Response)
attach(rest_median_data)
class(Response)

#combining rest of the data with PCA
train_med_data_complete <- cbind(rest_median_data,num_pca_select)

# removing coloumn with ID
train_med_data_complete <- train_med_data_complete[,-1]

#Selecting sample of 10000 observations
sample <- train_med_data_complete[sample(nrow(train_med_data_complete), 10000), ]
sample2 <- train_med_data_complete[10000:20000,]

#random forest
library(randomForest)
attach(sample)
rf_output <- randomForest(as.factor(Response) ~ .
                          , data = sample
                          , ntree = 1000 )

rf_output
rf_output$importance

# predicting and checking accuracy
preditions_new <- predict(rf_output, sample2)
mean(preditions_new == sample2$Response)


# plot error vs no. of tree
plot(rf_output, main = "Error vs No. of Tree")
# variable importance
varImpPlot(rf_output,n.var = 30 )

# random forest with new variables
select_variables <- train_med_data_complete[,c(115,116,117,118,2,39,25,24
                                              ,12,28,7,47,80,15,22,24,65
                                              ,20,31,53,10,53,21,60,5,58
                                              ,3,63,40,48,114)]

sample <- select_variables[sample(nrow(select_variables), 10000), ]
sample2 <- select_variables[10000:20000,]

rf_output <- randomForest(as.factor(Response) ~ .
                          , data = sample
                          , ntree = 1000 )

rf_output



# predicting and checking accuracy
preditions_new <- predict(rf_output, test)
mean(preditions_new == sample2$Response)


#######################################################################

#######################################################################
# lasso regression
library(glmnet)

Product_Info_2_new <- as.numeric(train_med_data_complete$Product_Info_2)
train_med_data_complete$Product_Info_2_new <- Product_Info_2_new
train_med_data_complete_0 <- train_med_data_complete[,-2]
train_med_data_complete_1 <- as.matrix(train_med_data_complete[,-113])

sample_3 <- train_med_data_complete_1[sample(nrow(train_med_data_complete_1), 10000), ]
sample_4 <- train_med_data_complete[10001:20000,]


CV <- cv.glmnet(x = sample_3
                , y = sample_4$Response
                , family = "multinomial"
                , type.measure = "class"
                , alpha = 1
                , nlambda = 100)
plot(CV)

fit <- glmnet(x = sample_3
                , y = sample_4$Response
                , family = "multinomial"
                , alpha = 1
                , lambda = 0.0001)

fit$beta[,1]
coef(fit, s = 0.0001)
plot(fit, xvar="lambda")
######################################################################

test <- read.csv("test.csv")

