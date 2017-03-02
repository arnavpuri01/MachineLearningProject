@@ -1,9 +1,9 @@
library(mice)
library(randomForest)
library(Boruta)
library(ggplot2)
library(gridExtra)


getwd()
dataset <- read.csv("train_midterm_data.csv")
head(dataset)
ncol(dataset)
@@ -27,22 +27,61 @@ percentage_na(dataset,127)
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
#new_train <- mice(a,m=5,maxit=10,meth='pmm',seed=500) 

complete_data <- na.omit(b)
percentage_na(complete_data,117)

complete_data <- na.omit(b)
#boruta.train <- Boruta(Response ~ ., data = complete_data, doTrace=2, maxRuns = 11)

l <- lm(Response ~ ., data = complete_data)

plot(dataset$BMI, dataset$Response)

keywords_sum <- function(data) {
  sums <- as.numeric()
  cols <- grep("Medical_Keyword_", names(data))
  
  for (r in 1:nrow(data)) {
    s <- 0
    for (c in cols) {
      if (data[[c]][r] == 1) {
        s <- s + 1
      }
    }
    sums <- append(sums, s)
  }
  return (sums)
}

complete_data$No_Of_Keywords <- keywords_sum(complete_data)

summary(complete_data$No_Of_Keywords)

moda <- lm(Response ~ No_Of_Keywords,
           data = complete_data) 

modb <- lm(Response ~ No_Of_Keywords + BMI,
          data = complete_data)

modc <- lm(Response ~ No_Of_Keywords +
             BMI + Wt,
           data = complete_data)

summary(moda)
summary(modb)
summary(modc)

head(complete_data$No_Of_Keywords)
length(complete_data$No_Of_Keywords)
complete_data$No_Of_Keywords

boruta.train <- Boruta(Response ~ ., data = complete_data, doTrace=2, maxRuns = 11)
table(complete_data$Response,complete_data$No_Of_Keywords)
p1 <- ggplot(data=complete_data,aes(x=Response,y=No_Of_Keywords)) + geom_jitter()
p2 <- ggplot(data=complete_data,aes(x=Response,y=BMI)) + geom_jitter()
p3 <- ggplot(data=complete_data,aes(x=Response,y=Wt)) + geom_jitter()

l <- lm(Response ~ . , data = complete_data)
grid.arrange(p1,p2,p3,nrow=3,ncol=1)
