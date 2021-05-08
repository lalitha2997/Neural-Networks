strat_50<- read.csv(file.choose())
View(strat_50)
str(strat_50)
strat_50=strat_50[,-4]
View(strat_50)
##visuvalization##
library(ggplot2)
ggplot(data =strat_50,aes(x=strat_50$R.D.Spend,fill=strat_50$Profit))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data =strat_50,aes(x=strat_50$Administration,fill=strat_50$Profit))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data =strat_50,aes(x=strat_50$Marketing.Spend,fill=strat_50$Profit))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data =strat_50,aes(x=strat_50$Profit,fill=strat_50$Profit))+
  geom_density(alpha=0.9,color='black')
##histograms##
ggplot(data =strat_50,aes(x=strat_50$R.D.Spend,fill=strat_50$Profit))+
  geom_histogram()
##
ggplot(data =strat_50,aes(x=strat_50$Administration,fill=strat_50$Profit))+
  geom_histogram()
##
ggplot(data =strat_50,aes(x=strat_50$Marketing.Spend,fill=strat_50$Profit))+
  geom_histogram()
##
ggplot(data =strat_50,aes(x=strat_50$Profit,fill=strat_50$Profit))+
  geom_histogram()
##
boxplot(strat_50)
plot(strat_50)
summary(strat_50)
str(strat_50)
sum(is.na(strat_50))
#######
#attach(concrete)
#normal_concrete<-scale(concrete)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
start_norm<-as.data.frame(lapply(strat_50[,-4],FUN=normalize))
View(start_norm)
#summary(concrete_norm$strength)
#summary(normal_concrete)
summary(strat_50)
summary(start_norm)
start_norm <- cbind(start_norm,strat_50$Profit)
start_norm
colnames(start_norm)[4] <- "profit"
train<-start_norm[1:39,]
test<-start_norm[40:50,]

# Using multilayered feed forward nueral network
# package nueralnet
# install.packages("neuralnet")
# install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("profit",paste(colnames(start_norm[-9]),collapse ="+"),sep="~")
#concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
start_model <- neuralnet(formula = formula_nn,data =train,linear.output = F)
str(start_model)

plot(start_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(start_model,test[1:4])
str(model_results)
predicted_strength <- model_results$net.result
# predicted_strength
# model_results$neurons
cor(predicted_strength,test$profit)
plot(predicted_strength,test$profit)
model_5<-neuralnet(profit~.,data= start_norm,hidden = 5,linear.output = F)
plot(model_5)
model_5_res<-compute(model_5,test[1:4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,test$profit)
plot(pred_strn_5,test$profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
