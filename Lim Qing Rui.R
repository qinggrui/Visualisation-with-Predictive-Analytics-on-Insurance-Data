library(data.table)
library(ggcorrplot)
library(ggplot2)
library(nnet)
library(caTools)
library(caret)
library(class)
library(dplyr)
library(rpart)
library(rpart.plot)
library(MLmetrics)


premium = fread("C:/Users/Qing Rui/Desktop/BC2406 Analytics/AY21S1 CBA/AY21S1 CBA/premium2.csv", stringsAsFactors = F)
summary(premium)
View(premium)
anyNA(premium) #no NA values

### Q1 ###

#BMI = weight kg/height m-square
premium$BMI = (premium$Weight/((premium$Height)^2))*10000
View(premium)


### Q2 ###
# No need to convert to factor, models can still be plotted, use linear regression as an example to compare output

premiumfactor= premium #create dataframe for factored variables

premiumfactor$Diabetes = factor(premiumfactor$Diabetes)
class(premiumfactor$Diabetes)

premiumfactor$HighBloodPressure = factor(premiumfactor$HighBloodPressure)
class(premiumfactor$HighBloodPressure)

premiumfactor$Transplant = factor(premiumfactor$Transplant)
class(premiumfactor$Transplant)

premiumfactor$ChronicDisease = factor(premiumfactor$ChronicDisease)
class(premiumfactor$ChronicDisease)

premiumfactor$Allergy = factor(premiumfactor$Allergy)
class(premiumfactor$Allergy)

premiumfactor$CancerInFamily = factor(premiumfactor$CancerInFamily)
class(premiumfactor$CancerInFamily)

premiumfactor$Gender = factor(premiumfactor$Gender)
class(premiumfactor$Gender)


#Linear regression for factorised variables
set.seed(2004)
premiumQ2Fact = lm(Premium ~., data = premiumfactor)
summary(premiumQ2Fact)
premiumQ2Fact

#Linear regression for non-factorised variables
set.seed(2004)
premiumQ2NF = lm(Premium ~., data = premium)
summary(premiumQ2NF)
premiumQ2NF



### Q3 ### do continous chart? left or right skewed?

model.matrix(~0+., data=premium)%>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2) #correlation plot

ggplot(data=premium, aes(x=Age, y = Premium)) +geom_jitter()+facet_grid(Gender~.) +
  labs(title = " Age vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE,color='red',size=1)+
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data=premium, aes(x=BMI, y = Premium)) +geom_jitter()+facet_grid(Gender~.) +
  labs(title = " BMI vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE,color='red',size=1)+
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data=premium, aes(x=Height, y = Premium)) +geom_jitter()+facet_grid(Gender~.) +
  labs(title = "Height vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE,color='red',size=1) +
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data=premium, aes(x=HighBloodPressure, y = Premium)) +geom_jitter()+facet_grid(Gender~HighBloodPressure) +
  labs(title = "HighBloodPressure vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE,color='red',size=1)+
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data=premium, aes(x=CancerInFamily, y = Premium)) +geom_jitter()+facet_grid(Gender~CancerInFamily) +
  labs(title = "CancerInFamily vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE,color='red',size=1) +
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data=premium, aes(x=NumMajorSurgeries, y = Premium)) +geom_jitter()+facet_grid(Gender~.) +
  labs(title = "NumMajorSurgeries vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE, color='red',size=1)+
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data=premium, aes(x=Diabetes, y = Premium)) +geom_jitter()+facet_grid(Gender~Diabetes) +
  labs(title = "Diabetes vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE,color='red',size=1)+
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data=premium, aes(x=ChronicDisease, y = Premium)) +geom_jitter()+facet_grid(Gender~ChronicDisease) +
  labs(title = "ChronicDisease vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE,color='red',size=1)+
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data=premium, aes(x=Transplant, y = Premium)) +geom_jitter()+facet_grid(Gender~Transplant) +
  labs(title = "Transplant vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE,color='red',size=1) +
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data=premium, aes(x=Allergy, y = Premium)) +geom_jitter()+facet_grid(Gender~Allergy) +
  labs(title = "Allergy vs Premium sorted by Gender")+geom_smooth(method = "loess", se = FALSE,color='red',size=1) +
  geom_boxplot(fill="slateblue", alpha=0.2)


### Q4 ###
# Q4a
set.seed(2004)
premiumcart1 <- rpart(Premium ~.,
                  data = premium, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
#plot maximal tree and results
rpart.plot(premiumcart1, nn = T, main = "Premium CART without split")
#pruning sequence and 10-fold CV error
plotcp(premiumcart1, main = 'Subtrees in premiumCart1')
#maximal tree and prune triggers
printcp(premiumcart1)

print(premiumcart1) #printing all nodes in premiumcart1

CVerror.cap <- premiumcart1$cptable[which.min(premiumcart1$cptable[,"xerror"]), "xerror"] +
  premiumcart1$cptable[which.min(premiumcart1$cptable[,"xerror"]), "xstd"] #Obtaining Cv error cap of premiumcart1

#short cut way to find optimal tree
i <- 1; j<- 4
while (premiumcart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
i

cp.opt = ifelse(i > 1, sqrt(premiumcart1$cptable[i,1] * premiumcart1$cptable[i-1,1]), 1)
cp.opt ##Tolerance limit for node at 0.01048363

#set minimum value to prune at 0.01048363
cp1 <- cp.opt

premiumcart2 <- prune(premiumcart1, cp = cp1)
printcp(premiumcart2, digits = 3)
plotcp(premiumcart2) #Shows pruned tree and 10 fold CV error
print(premiumcart2) #Shows error and pruning sequence
rpart.plot(premiumcart2, nn=T, main = "Optimal Tree in premiumcart") #Shows pruned decision trees

cp1.rmse <- sqrt(97526*0.234)
cp1.rmse #151.0665

# Q4b
# To summarize nodes and decision rules
rpart.rules(premiumcart2, nn = T, extra = 4, cover = T)
premiumcart2$variable.importance

# Q4c
#Both not important in determining premium, as they are not in the variables used for the split


# Q4d (use Linear Regression)
set.seed(2004)
train <- sample.split(Y = premium$Premium, SplitRatio = 0.7)
trainset <- subset(premium, train == T)
trainset
testset <- subset(premium, train == F)
testset

# Using CART
set.seed(2004)
premiumcart3 <- rpart(Premium ~.,
                      data = trainset, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
#plot maximal tree and results
rpart.plot(premiumcart3, nn = T, main = "Premium CART without split trainset")
#pruning sequence and 10-fold CV error
plotcp(premiumcart3, main = 'Subtrees in premiumCart2')
#maximal tree and prune triggers
printcp(premiumcart3)

print(premiumcart3) #printing all nodes in premiumcart1

CVerror.cap1 <- premiumcart3$cptable[which.min(premiumcart3$cptable[,"xerror"]), "xerror"] +
  premiumcart3$cptable[which.min(premiumcart3$cptable[,"xerror"]), "xstd"] #Obtaining Cv error cap of premiumcart2

#short cut way to find optimal tree
i <- 1; j<- 4
while (premiumcart3$cptable[i,j] > CVerror.cap1) {
  i <- i + 1
}
i

cp.opt = ifelse(i > 1, sqrt(premiumcart3$cptable[i,1] * premiumcart3$cptable[i-1,1]), 1)
cp.opt ##Tolerance limit for node at 0.01923057

#set minimum value to prune at 0.01923057
cp1 <- cp.opt

premiumcart4 <- prune(premiumcart3, cp = cp1)
printcp(premiumcart4, digits = 3)
plotcp(premiumcart4) #Shows pruned tree and 10 fold CV error
print(premiumcart4) #Shows error and pruning sequence
rpart.plot(premiumcart4, nn=T, main = "Optimal Tree in premiumcart4") #Shows pruned decision trees

# To summarize nodes and decision rules
rpart.rules(premiumcart4, nn = T, extra = 4, cover = T)
premiumcart4$variable.importance

###Testdata fitting onto trainset
testdata= data.frame(testset)
cart.predict = predict(premiumcart4, testdata, type = 'matrix')
mean.results = data.frame(testdata, cart.predict)
summary(mean.results)

# Make predictions on the test data
predicted.classes = premiumcart4 %>% predict(testdata)
# Compute model accuracy rate on test data
mean(predicted.classes == testdata$Premium)
predicted.classes

#Finding RMSE
testset.error = testset$Premium - cart.predict
testset.error

# Testset Errors
RMSE.cart.test = sqrt(mean(testset.error^2))
RMSE.cart.test # CART RMSE is 167.5624
summary(abs(testset.error))


### Linear regression
set.seed(2004)
premiumLR = lm(Premium ~., data = trainset)
#backward stepwise regression to find optimal LR and variables
premiumLR = step(premiumLR, direction='backward', scope=formula(premiumLR), trace=0) 
summary(premiumLR)
premiumLR

#predict using Linear Regression
lr.predict <- predict(premiumLR,newdata=testset)
testset$error <- testset$Premium - lr.predict

# Testset Errors
LR.test.RMSE <- sqrt(mean(testset$error^2))
summary(abs(testset$error))
LR.test.RMSE # Linear Regression RMSE is 200.7566

tb.rmse= data.frame(CART.RMSE = rep(c(RMSE.cart.test)),
                    LinearReg.RMSE = rep(c(LR.test.RMSE))
                    )
tb.rmse

### Q6
CART.MAPE= MAPE(testset$Premium, cart.predict)
CART.MAPE #CART MAPE is 0.09178514
LR.MAPE= MAPE(testset$Premium, lr.predict)
LR.MAPE #Linear Reg MAPE is 0.1217045

MAPE<-c(LR.MAPE,CART.MAPE)
RMSE<-c(LR.test.RMSE, RMSE.cart.test)
varname=c('LinearReg', 'CART')
bcg.matrix<-data.frame(varname,RMSE,MAPE)
ggplot(data=bcg.matrix,aes(x=RMSE,y=MAPE,fill=varname,color=varname))+geom_point(size=5)+
  ggtitle("BCG Comparative Matrix")+
  theme(legend.position="top")+scale_y_reverse()+scale_x_reverse()
