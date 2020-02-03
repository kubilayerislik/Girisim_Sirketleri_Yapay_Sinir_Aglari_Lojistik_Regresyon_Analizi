#Prepare Data
for(i in 1:7) {data[,i] =(data[,i]-min(data[,i]))/(max(data[,i])-min(data[,i])) }
ind = sample(1:nrow(data),567)
train_data = data[ind,]
test_data = data[-ind,]

#Create Model
library(neuralnet)
n = neuralnet(Category~No_Stage+Seed+Stage_A+Stage_B+Stage_C+Stage_D,data = train_data,hidden = c(4,4,4), linear.output = F)
plot(n)

#Create Actual And Predicted Data
output = compute(n,test_data[,-7])
prediction = output$net.result * (max(data1[-ind,7])-min(data1[-ind,7]))+min(data1[-ind,7])
actual = data1[-ind,7]
actual = as.numeric(actual$Category)

#Mean Square Error
MSE = sum((prediction-actual)^2)/nrow(test_data)
table(actual,round(prediction))
MSE

#Actual And Predicted Data Table
output_train = compute(n,train_data[,-7])
prediction_train = output_train$net.result * (max(data1[-ind,7])-min(data1[-ind,7]))+min(data1[-ind,7])
actual_train = data1[ind,7]
actual_train = as.numeric(actual_train$Category)
table(actual_train,round(prediction_train))

