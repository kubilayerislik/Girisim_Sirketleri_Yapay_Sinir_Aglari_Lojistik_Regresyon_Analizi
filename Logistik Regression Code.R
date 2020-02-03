#Prepare Data
data$CategoryF = factor(data$Category)
data$out = relevel(data$CategoryF, ref = "2")
attach(data)

#Create Model
library(nnet)
model = multinom(CategoryF ~ No_Stage+Seed+Stage_A+Stage_B+Stage_C+Stage_D,data = data)
summary(model)

#Predict
predict(model,data, type = "prob")


#Misclassification Error
cm = table(predict(model),data$CategoryF)
print(cm)


