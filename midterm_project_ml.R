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

#removed 1 since customer id
percentage_na(dataset,127)

a <- dataset[,-c(1,30,35,36,37,38,48,53,62,70,128)]
b <- dataset[,-c(1,30,35,36,37,38,48,53,62,70)]
ncol(a)
head(a,2)

percentage_na(a,117)

str(a)
new_train <- mice(a,m=5,maxit=10,meth='pmm',seed=500) 

summary(new_train)
ncol(new_train$data)
complete_data <- complete(new_train,1)
complete_data

percentage_na(complete_data,117)

complete_data <- na.omit(b)

boruta.train <- Boruta(Response ~ ., data = complete_data, doTrace=2, maxRuns = 11)

l <- lm(Response ~ . , data = complete_data)
