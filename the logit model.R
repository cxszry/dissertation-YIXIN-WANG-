
library(readxl)
library(aod)
library(pROC)

# read the required file (for building the logit model)
data <- data.frame(read_excel(path='glm.xlsx',sheet=3))
df <- data
names(df) <- c("CATA","WCTA","TDTA","CLTA","bankruptcy")
# build the logit model and output the result
result <- glm(formula = bankruptcy ~ CATA + WCTA + TDTA + CLTA, data = df, family = binomial)
print(summary(result))
# read the required file (for plotting)
data2 <- data.frame(read_excel(path='glm.xlsx',sheet=6))
rocdata <- data2
names(rocdata) <- c("CATA","WCTA","TDTA","CLTA","bankruptcy","prediction")
# plot the ROC curve
R<-roc(rocdata$bankruptcy, rocdata$prediction)
plot(R)
# conduct the wald test
wald.test(Sigma = vcov(result), b = coef(result), Terms = 1)
wald.test(Sigma = vcov(result), b = coef(result), Terms = 2)
wald.test(Sigma = vcov(result), b = coef(result), Terms = 3)
wald.test(Sigma = vcov(result), b = coef(result), Terms = 4)
# read the required file (test data)
data3 <- data.frame(read_excel(path='glm.xlsx',sheet=4))
test.data <- data3
names(test.data) <- c("CATA","WCTA","TDTA","CLTA","bankruptcy")
# obtain the predictions based on our logit model
probabilities=predict(result,test.data, type = "response")
head(probabilities)
# classify the bankrupt and non-bankrupt companies
predicted.classes <- ifelse(probabilities > 0.5, 1,0)
head(predicted.classes)
# output the predictions
EES <- data.frame(probabilities,predicted.classes)
colnames(EES) <- c('value','forecast result')
write.csv(EES,'prediction2.csv')
