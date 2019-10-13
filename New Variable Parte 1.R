# Load data
# install.packages('mlbench')
library (mlbench)
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]  # keep complete rows
bc
#Test if "bc" is a dataframe
is.data.frame(bc)

#verifY data 
str(bc)
head(bc)
ncol(bc)
nrow(bc)
tail(bc)
#plot(bc$class, col = "green",breaks = 20)

# remove id column
bc <- bc[,-1]
head(bc)

# convert to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
head(bc)

# Change Y values to 1's and 0's and  variable class as categorical(factor)
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
bc[1:10,]

# Prep Training and Test data.
set.seed(1)
index <- sample(1:nrow(bc),683*0.6 ) #en vez de 200 va el 60 por ciento de la cantidad de datos 0.6 *6 y pico
head(index)
testData<-bc[index, ]
nrow(testData)
head(testData)
trainData<-bc[-index, ]
nrow(trainData)

# Class distribution of train and sample data
table(trainData$Class)
table(testData$Class)


# Build Logistic Model using training data
logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion , family = "binomial", trainData)

summary(logitmod)
anova(logitmod)

# fit neural network
library(neuralnet)
nn=neuralnet(logitmod, data=bc, hidden=3, act.fct = "logistic",
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

