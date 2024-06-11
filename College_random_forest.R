library(corrgram)
library(corrplot)
library(caTools)
library(Amelia)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)

#### College data is inside ISLR

str(College)

View(College)

#### changing the name for the simplicity

df <- College

#### EDA time

ggplot(df,aes(Room.Board, Grad.Rate)) + geom_point(position=position_jitter(w=1, h=0),aes(color=Private),alpha=0.5,size=2)

ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black') + theme_bw()

ggplot(df,aes(F.Undergrad)) + geom_histogram((aes(fill=Private)),color='red',alpha=0.5)

#### in the ggplot we see there something off because the graduation rate is going above 100
#### lets find out which one is it

subset(df,Grad.Rate > 100)

#### Getting rid of 118 graduation rate and making it to 100

df['Cazenovia College','Grad.Rate'] <- 100

#### Run subset again just to make sure

subset(df,Grad.Rate > 100)

ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',alpha=0.7) + theme_bw()

#### I think our data is all clean to undergo model transformation
#### train and test data

sample <- sample.split(df$Private,SplitRatio = 0.7)

train <- subset(df,sample == TRUE)

test <- subset(df,sample == FALSE)

#### Making the model now for random forest

rf.model <- randomForest(Private ~ .,train)

print(rf.model$confusion)

#### looking pretty good but do know this just the model and not the predicted with test data so lets do that now

rf.predict <- predict(rf.model,test)

table(rf.predict,test$Private)

#### calculating accuracy

predict.acc <- (58+161) / (58+6+8+161)

print(predict.acc)
