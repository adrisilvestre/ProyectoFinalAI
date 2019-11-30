# Load data
# install.packages('datasets')
library (datasets)
data(iris, package="datasets")
ir<- iris[complete.cases(iris),] # keep complete rows
ir
#Test if "ir" is a dataframe
is.data.frame(ir)

#verifY data 
str(ir)
head(ir)
ncol(ir)
nrow(ir)
tail(ir)


# convert to numeric
for(i in 1:4) {
  ir[, i] <- as.numeric(as.character(ir[, i]))
}
head(ir)

# Change Y values to 1's and 0's and  variable class as categorical(factor)
ir$Species <- ifelse(ir$Species == "setosa", 1, ifelse(ir$Species=="versicolor",2,3))
ir$Species <- factor(ir$Species, levels = c(1, 2,3))
ir[1:150,]

# Prep Training and Test data.
set.seed(1)
index <- sample(1:nrow(ir),100 ) #en vez de 200 va el 60 por ciento de la cantidad de datos 0.6 *6 y pico
head(index)
testData<-ir[index, ]
nrow(testData)
head(testData)
trainData<-ir[-index, ]
nrow(trainData)

# Class distribution of train and sample data
table(trainData$Species)
table(testData$Species)


# Build Logistic Model using training data
logitmod <- glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, family ="binomial", trainData)

summary(logitmod)
anova(logitmod)

# fit neural network
library(neuralnet)
nn=neuralnet(logitmod, data=ir, hidden=3, act.fct = "logistic",
             linear.output = FALSE)
nn

# plot neural network
plot(nn)

# predict using test data

pred <- predict(logitmod, newdata = testData, type = "response")
head(pred)

# Recode factors using 50% as probabilistic threshold
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class
class(y_pred)
head(y_pred)
head(y_act)

# Accuracy and contigency analysis
table(y_act, y_pred)
mean(y_pred == y_act)
mean(y_pred != y_act)

#PLOT ROC CURVE- PERFORMANCE
library(ROCR)
pred1 <- prediction(pred, testData$Class)
perf <- performance(pred1,"tpr","fpr")
plot(perf)  

#AREA BAJO LA CURVA ROC
auc <- as.numeric(performance(pred1,"auc")@y.values)
plot(perf,type='o', main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)
