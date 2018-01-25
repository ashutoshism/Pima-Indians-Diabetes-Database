library(readr)
library(caTools)

setwd("E:/ashutosh/Data science/projects/Pima Indians Diabetes Database")
data_main <- read_csv("E:/ashutosh/Data science/projects/Pima Indians Diabetes Database/diabetes.csv")

#train-test split
set.seed(7)
split = sample.split(data_main,SplitRatio = 0.75)
train = subset(data_main,split =="TRUE")
test = subset(data_main,split =="FALSE")

#making log regression model
model = glm(Outcome~.-Insulin-SkinThickness ,train,family = "binomial")

predicted_op = predict(model,newdata = test,type = "response")
x = table(actual =test$Outcome,Predicted =predicted_op> 0.3)
print(x)
#accuracy
print((x[1,1]+x[2,2])/(x[1,1]+x[1,2]+x[2,1]+x[2,2]))

#for finding optimum threshold
library(ROCR)
predicted_op = predict(model,newdata = train,type = "response")
pred = prediction(predicted_op,train$Outcome)
perf = performance(pred,"tpr","fpr")
plot(perf,colorize =T,print.cutoffs.at = seq(0.1,by = 0.1))
