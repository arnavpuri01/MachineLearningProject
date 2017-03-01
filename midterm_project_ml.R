library(mice)
library(randomForest)
library(Boruta) 

getwd()
dataset <- read.csv("train_midterm_data.csv")
head(dataset)
ncol(dataset)
nrow(dataset)
#59381
#note to self: loop in R,goes through each row if iterated through columns.
percentage_na <- function(data,max_col)
{
  for(i in 1:max_col)
  {
    if(sum(is.na(data[,i])) > 0)
    {
      ratio <- sum(is.na(data[,i]))/nrow(data) * 100
      print(paste(i,ratio))
    }
  }
}

#removed 1 since customer id
percentage_na(dataset,127)

new_dataset1 <- dataset[,-c(1,30,35,36,37,38,48,53,62,70,128)]
ncol(new_dataset1)
head(new_dataset1,2)

percentage_na(new_dataset1,117)

str(new_dataset1)
new_train <- mice(new_dataset1,m=5,maxit=10,meth='pmm',seed=500) 

summary(new_train)
ncol(new_train$data)
complete_data <- complete(new_train,1)
complete_data

percentage_na(complete_data,117)


