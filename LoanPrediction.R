---
  title: "STAT228_FinalProject"
author: "Veeksha Madhu"
date: "4/24/2022"
output: pdf_document
---

#Loading the data and understanding it

sba <-read.csv("SBAdata.csv", header=TRUE)
sba <- SBAdata  
nrow(sba)
ncol(sba)
head(sba)

?as.factor
#The data has 2102 observations and 35 features - 34 predictors as well as 1 label.  


head(summary(sba))

#The dataset has 9 categorical variables, and 26 numeric or semi-numeric variables. The latter includes binary, continuous, and discreet values. 

#Data Cleaning
#Finding Missing Values

for(i in 1:ncol(sba)){
  if(any(is.na(sba[,i]))){
    print(i)
  }}
#There are missing values in columns 14,21, 22,34
mlist<- list(14, 21, 22, 34)
for(i in mlist){
  for(j in 1:nrow(sba)){
    if(is.na(sba[j,i])){
      bam <- cbind(i,j)
      print(bam)
    }}}

#All values in column 21 are missing, the rest of the columns have one missing data point in row 1147, and 2 missing data points in rows 1256, 1692, and 2102. Since the dataset already has several data points, the removal of those 2 rows would not effect the accuracy of our models.  

#Deleting missing values

sbaclea <- sba[ -c(21) ]
sbaclean <- sbaclea[-c(22, 34), ]


#Imputing missing values
table(sbanum$Default)
table(sbaclean$NewExist)
#Since the mode of the NewExist column is 1, we inpute the value into row 1147 and column 14 which was previouslt NA
sbaclean[1147, 14] <- 1


#Cleaning variables

sbanum <- sbaclean[,c(9:34)]
sapply(sbanum, class)

#Variables with characters: RevLineCr, LowDoc, MIS_Status
#Coding No- 1, Yes - 2, Others - 0

#Variable - RevLineCr

unique(sbaclean$RevLineCr)
for(i in 1:nrow(sbaclean)){
  if(sbaclean[i,19] == "Y"){
    sbaclean[i,19] <- 2}
  else if(sbaclean[i,19] == "N"){
    sbaclean[i,19] <- 1}
  else{
    sbaclean[i,19] <- 0
  }}
unique(sbaclean$RevLineCr)


#Variable - LowDoc

unique(sbaclean$LowDoc)
for(i in 1:nrow(sbaclean)){
  if(sbaclean[i,20] == "Y"){
    sbaclean[i,20] <- 2}
  else if(sbaclean[i,20] == "N"){
    sbaclean[i,20] <- 1}
  else{
    sbaclean[i,20] <- 0
  }}
unique(sbaclean$LowDoc)< sbanum$ChgOffPrinGr 
#hist(sbanum$ChgOffPrinGr, xlim = (0, 100000))

View(sbanum)
sbanum[sbanum$ChgOffPrinGr < 100000]
#hist(sbanum$ChgOffPrinGr < 100000)

#testing for variable distribution 
subsettotest <- sbanum[which(sbanum$Default <= 1),]
sub2 <- sbanum[which(sbanum$Default <= 0),]
hist(subsettotest$ChgOffPrinGr)
mean(subsettotest$ChgOffPrinGr)
mean(sub2$ChgOffPrinGr)

#imbalanced distribution of ChgPrinOfGr - highly skewed and centered on 0 and has high infleunce on dataset - remove 
vifstep(subsettotest, th=10)
subset(sbanum, sbanum$Default )
sbanum$ChgOffPrinGr == 1 
vif()
?hist
#Variable - MIS_Status
#Loan status charged off = CHGOFF - 1
#Paid in full = PIF - 2


unique(sbaclean$MIS_Status)
for(i in 1:nrow(sbaclean)){
  if(sbaclean[i,24] == "CHGOFF"){
    sbaclean[i,24] <- 1}
  else if(sbaclean[i,24] == "P I F"){
    sbaclean[i,24] <- 2}
  else{
    sbaclean[i,24] <- 0
  }}
unique(sbaclean$MIS_Status)


#Data Analyses
#To run initial analyses on the data, the data set must not have any keys or qualitative values. Thus, columns like LoanNr_ChkDgt and Name  are removed. Then, all the values in the columns need to be converted to one uniform data type - integers. 

myvars <- names(sbaclean) %in% c("LoanNr_ChkDgt", "Name", "City", "State", "Bank", "BankState")
sbanum <- sbaclean[!myvars]

for(i in 1:ncol(sbanum)){
  sbanum[,i] <- as.numeric(sbanum[,i])
}
head(sbanum)



cor(sbanum)

## creating dataframe of blanced responses ##

#
#sbazero <- sbanum[ sample( which( sbanum$Default == '0' ) , 700 ) , ]

#Comparing Amount disbursed and Gross amount of loan approved by bank

bplotvars <- names(sbanum) %in% c("DisbursementGross", "GrAppv")
boxplotdata <- sbanum[bplotvars]
ggplot(boxplotdata, aes(x = DisbursementGross, y = GrAppv)) + geom_boxplot()
library("ggplot2")
boxplot(boxplotdata)

#From the plot above, it seems like the amount disbursed is marginally more than the gross amount of loan approved by bank. 


library("vioplot")
vioplot(sbanum$SBA_Appv, sbanum$GrAppv, names=c("SBA's guaranteed amount of approved loan", "Bank approved amount"),
        col=c("yellow","blue"), ylab = "Dollars $")

#This violin plot tells us that banks approve many more smaller loans than larger ones for SBAs.  


boxplot(sbanum$GrAppv ~ sbanum$ApprovalFY, outline = FALSE)

#The above box plot portrays gross amount of loan approved by bank over the years. This graph is not unexpected as it seems to follow the trends of a business cycle, with a notable dip in the early 2000s due to the dot-com bubble and in the late 2000s due to the great recession.  

which(is.na(sbanum), arr.ind=TRUE)
View(sbanum)
#Predictive Techniques

##Logistic Regression for binary classificatin
#check for VIF - multicollinearity

install.packages("usdm")
library("usdm")
#library("car")
vifstep(mydfbalanced, th=10)
as.numeric(sbanum)

#9 variables from the 28 input variables have collinearity problem: 
#Term, DisbursementDate, MIS_Status, xx, ApprovalDate, New, GrAppv, SBA_Appv, daysterm 

#Removing all predictors with VIF scores above threshold of 10, we keep predictors and remove label of "selected" and "Balance Gross" which is highly skewed as well 
#AIC model

vifsubset <- sbanum[ , ! names(sbanum) %in% c("Selected", "ChgOffPrinGr", "Term", "DisbursementDate", "MIS_Status", "xx", "ApprovalDate", "New", "GrAppv", "SBA_Appv", "daysterm", "BalanceGross")]
library(car)

#Term DisbursementDate MIS_Status xx ApprovalDate GrAppv New SBA_Appv daysterm 

vifsubset <- na.omit(vifsubset)
which(is.na(vifsubset), arr.ind=TRUE)
is.na(vifsubset)
vifsubset <- na.omit(vifsubset)

View(vifsubset)
#Logistic Regression 
#for logistic regression, do as.factor for all categorical 

#1 - Default #2 - New exist #3 - urbanRural #4 - RevLineCr #5 - LowDoc  #realestate #recession  

vifsubset$Default <- as.factor(vifsubset$Default)
vifsubset$NewExist <- as.factor(vifsubset$NewExist)
vifsubset$UrbanRural <- as.factor(vifsubset$UrbanRural)
vifsubset$RevLineCr <- as.factor(vifsubset$RevLineCr)
vifsubset$LowDoc <- as.factor(vifsubset$LowDoc)
vifsubset$RealEstate <- as.factor(vifsubset$RealEstate)
vifsubset$Recession <- as.factor(vifsubset$Recession)

View(vifsubset)

#full model
attach(vifsubset)

initmodel <- glm(Default~ ., data = vifsubset,  family=binomial(link="logit"))

summary(initmodel)

#goodness of fit test for model 1 
2651.29 - 212.57
1 - pchisq(2438.72, 20) #0 

#AIC model without interaction terms 
step(initmodel, direction="both", k=2, trace = 0)

aicmodel <- glm(formula = Default ~ NAICS + ApprovalFY + NoEmp + UrbanRural + 
                  RevLineCr + RealEstate + Portion + Recession, family = binomial(link = "logit"), 
                data = vifsubset)
summary(aicmodel)

#goodness of fit 
2651.3 - 1924.4 
1 - pchisq(726.9, 10) #0

#AIC with interaction terms 
step(initmodel, .~.^2, direction="both", k=2, trace = 0)

aicintmodel <- glm(formula = Default ~ Zip + NAICS + ApprovalFY + NoEmp + NewExist + 
                     CreateJob + RetainedJob + FranchiseCode + UrbanRural + RevLineCr + 
                     LowDoc + DisbursementGross + RealEstate + Portion + Recession + 
                     RealEstate:Portion + ApprovalFY:Recession + ApprovalFY:RealEstate + 
                     Zip:LowDoc + RevLineCr:RealEstate + Zip:DisbursementGross + 
                     NewExist:Recession + ApprovalFY:RevLineCr + Zip:RevLineCr + 
                     CreateJob:RetainedJob + ApprovalFY:UrbanRural + NewExist:LowDoc + 
                     NAICS:RealEstate + NewExist:CreateJob + NoEmp:RevLineCr + 
                     NewExist:DisbursementGross + UrbanRural:DisbursementGross + 
                     ApprovalFY:FranchiseCode + FranchiseCode:RevLineCr + CreateJob:RealEstate + 
                     NAICS:CreateJob + ApprovalFY:CreateJob, family = binomial(link = "logit"), 
                   data = vifsubset)

summary(aicintmodel)


#BIC without interaction terms 
nlen <- (length(vifsubset$Default))
step(initmodel, direction="both", k=log(nlen), trace = 0)

bicmodel <- glm(formula = Default ~ NAICS + ApprovalFY + NoEmp + RevLineCr + 
                  RealEstate + Portion + Recession, family = binomial(link = "logit"), 
                data = vifsubset)

summary(bicmodel)



#Bic model with interaction terms 
step(initmodel, .~.^2, direction="both", k=log(nlen), trace = 0)

bicintmodel <- glm(formula = Default ~ NAICS + ApprovalFY + RevLineCr + DisbursementGross + 
                     RealEstate + Portion + Recession + RealEstate:Portion + ApprovalFY:Recession + 
                     ApprovalFY:RealEstate + RevLineCr:RealEstate, family = binomial(link = "logit"), 
                   data = vifsubset)

summary(bicintmodel)





#Cross-validation: calculating accuracy, sensitivity, specificity, precision, aucscore 

#init model 
library("pROC")

n <- dim(vifsubset)[1]
K <- 10 #10-fold CV as an example
n.fold <- floor(n/K)
n.shuffle <- sample(1:n, n, replace=FALSE)
index.fold <- list()
for(i in 1:K) {
  if(i<K) {
    index.fold[[i]] <- n.shuffle[((i-1)*n.fold+1):(i*n.fold)]
  } 
  else {
    index.fold[[i]] <- n.shuffle[((K-1)*n.fold+1):n]
  }
}
index.fold


aucinit <- 0
accinit <- 0
sensinit <- 0
specinit <- 0
precinit <- 0
for(i in 1:K) {
  fit <- glm(Default ~ ., data=vifsubset[-index.fold[[i]],], family=binomial(link="logit"))
  pi.hat <- predict(fit, type="response", data = vifsubset[index.fold[[i]],])
  #plot(roc(final$religion, pi.hat))
  
  Y.hat = ifelse(pi.hat>0.5,1,0)
  coninit <- table(Y.hat,vifsubset[-index.fold[[i]],]$Default)
  
  accinit <- accinit + (1/K)*(coninit[4] + coninit[1]) / nrow(vifsubset)
  sensinit <- sensinit + (1/K)*(coninit[4] / (coninit[4] + coninit[3]))
  specinit <- specinit + (1/K)*(coninit[1] / (coninit[1] + coninit[2]))
  precinit <- precinit + (1/K)*(coninit[4] / (coninit[2] + coninit[4]))
  
  auc1 <-auc(vifsubset[-index.fold[[i]],]$Default, pi.hat) 
  aucinit <- aucinit + (1/K)*auc1
}
accinit #0.8893283
sensinit # 0.9767909 
specinit # 0.9936356
precinit #0.9867191
aucinit # 0.9986509



#aicmod 

aucaic <- 0
accaic <- 0
sensaic <- 0
specaic <- 0
precaic <- 0
for(i in 1:K) {
  aicfit <- glm(formula = Default ~ NAICS + ApprovalFY + NoEmp + UrbanRural + 
                              RevLineCr + RealEstate + Portion + Recession, family = binomial(link = "logit"), data = vifsubset[-index.fold[[i]],])
  pi.hat <- predict(aicfit, type="response", data = vifsubset[index.fold[[i]],])
  #plot(roc(final$religion, pi.hat))
  
  Y.hat = ifelse(pi.hat>0.5,1,0)
  conaic <- table(Y.hat,vifsubset[-index.fold[[i]],]$Default)
  
  accaic <- accaic + (1/K)*(conaic[4] + conaic[1]) / nrow(vifsubset)
  sensaic <- sensaic + (1/K)*(conaic[4] / (conaic[4] + conaic[3]))
  specaic <- specaic + (1/K)*(conaic[1] / (conaic[1] + conaic[2]))
  precaic <- precaic + (1/K)*(conaic[4] / (conaic[2] + conaic[4]))
  
  auc2 <-auc(vifsubset[-index.fold[[i]],]$Default, pi.hat) 
  aucaic <- aucaic + (1/K)*auc2
}
accaic # 0.6945688
sensaic #0.6460144
specaic #0.832619
precaic #0.6515071
aucaic # 0.8349331


#aicmod with interaction 
aucaicint <- 0
accaicint <- 0
sensaicint <- 0
specaicint <- 0
precaicint <- 0
for(i in 1:K) {
  aicintfit<- glm(formula = Default ~ Zip + NAICS + ApprovalFY + NoEmp + NewExist + 
                                    CreateJob + RetainedJob + FranchiseCode + UrbanRural + RevLineCr + 
                                    LowDoc + DisbursementGross + RealEstate + Portion + Recession + 
                                    RealEstate:Portion + ApprovalFY:Recession + ApprovalFY:RealEstate + 
                                    Zip:LowDoc + RevLineCr:RealEstate + Zip:DisbursementGross + 
                                    NewExist:Recession + ApprovalFY:RevLineCr + Zip:RevLineCr + 
                                    CreateJob:RetainedJob + ApprovalFY:UrbanRural + NewExist:LowDoc + 
                                    NAICS:RealEstate + NewExist:CreateJob + NoEmp:RevLineCr + 
                                    NewExist:DisbursementGross + UrbanRural:DisbursementGross + 
                                    ApprovalFY:FranchiseCode + FranchiseCode:RevLineCr + CreateJob:RealEstate + 
                                    NAICS:CreateJob + ApprovalFY:CreateJob, family = binomial(link = "logit"), data = vifsubset[-index.fold[[i]],])
  pi.hat <- predict(aicintfit, type="response", data = vifsubset[index.fold[[i]],])
  #plot(roc(final$religion, pi.hat))
  
  Y.hat = ifelse(pi.hat>0.5,1,0)
  conaicint <- table(Y.hat,vifsubset[-index.fold[[i]],]$Default)
  
  accaicint <- accaicint + (1/K)*(conaicint[4] + conaicint[1]) / nrow(vifsubset)
  sensaicint <- sensaicint + (1/K)*(conaicint[4] / (conaicint[4] + conaicint[3]))
  specaicint <- specaicint + (1/K)*(conaicint[1] / (conaicint[1] + conaicint[2]))
  precaicint <- precaicint + (1/K)*(conaicint[4] / (conaicint[2] + conaicint[4]))
  
  auc1 <-auc(vifsubset[-index.fold[[i]],]$Default, pi.hat) 
  aucaicint <- aucaicint + (1/K)*auc1
}
accaicint #0.7165317
sensaicint #0.6818553
specaicint #0.8514722
precaicint #0.6899229
aucaicint #0.8716609


#bicmod 

aucbic <- 0
accbic <- 0
sensbic <- 0
specbic <- 0
precbic <- 0
for(i in 1:K) {
  bicfit <- glm(formula = Default ~ NAICS + ApprovalFY + NoEmp + RevLineCr + 
                  RealEstate + Portion + Recession, family = binomial(link = "logit"), data = vifsubset[-index.fold[[i]],])
  pi.hat <- predict(bicfit, type="response", data = vifsubset[index.fold[[i]],])
  #plot(roc(final$religion, pi.hat))
  
  Y.hat = ifelse(pi.hat>0.5,1,0)
  conbic <- table(Y.hat,vifsubset[-index.fold[[i]],]$Default)
  
  accbic <- accbic + (1/K)*(conbic[4] + conbic[1]) / nrow(vifsubset)
  sensbic <- sensbic + (1/K)*(conbic[4] / (conbic[4] + conbic[3]))
  specbic <- specbic + (1/K)*(conbic[1] / (conbic[1] + conbic[2]))
  precbic <- precbic + (1/K)*(conbic[4] / (conbic[2] + conbic[4]))
  
  auc1 <-auc(vifsubset[-index.fold[[i]],]$Default, pi.hat) 
  aucbic <- aucbic + (1/K)*auc1
}
accbic #0.6957122
sensbic #0.6557224
specbic #0.82978
precbic #0.6511202
aucbic #0.8343439



#bic model with interaction 
aucbicint <- 0
accbicint <- 0
sensbicint <- 0
specbicint <- 0
precbicint <- 0
for(i in 1:K) {
  bicintfit <- glm(formula = Default ~ NAICS + ApprovalFY + RevLineCr + DisbursementGross + 
                                    RealEstate + Portion + Recession + RealEstate:Portion + ApprovalFY:Recession + 
                                    ApprovalFY:RealEstate + RevLineCr:RealEstate, family = binomial(link = "logit"),  data = vifsubset[-index.fold[[i]],])
  pi.hat <- predict(bicintfit, type="response", data = vifsubset[index.fold[[i]],])
  #plot(roc(final$religion, pi.hat))
  
  Y.hat = ifelse(pi.hat>0.5,1,0)
  conbicint <- table(Y.hat,vifsubset[-index.fold[[i]],]$Default)
  
  accbicint <- accbicint + (1/K)*(conbicint[4] + conbicint[1]) / nrow(vifsubset)
  sensbicint <- sensbicint + (1/K)*(conbicint[4] / (conbicint[4] + conbicint[3]))
  specbicint <- specbicint + (1/K)*(conbicint[1] / (conbicint[1] + conbicint[2]))
  precbicint <- precbicint + (1/K)*(conbicint[4] / (conbicint[2] + conbicint[4]))
  
  auc1 <-auc(vifsubset[-index.fold[[i]],]$Default, pi.hat) 
  aucbicint <- aucbicint + (1/K)*auc1
}
accbicint #0.7034302
sensbicint #0.6648122
specbicint #0.8381169
precbicint #0.6655029
aucbicint #0.8474674




###Random Forest ####

#dividing the dataset into trainind and testing 

n <- dim(vifsubset)
set.seed(1)
id.train <- sample(1:n, 0.8*n, replace = FALSE) #splitting data into training and testing - at least 50% (depends on balaced dataset) 
trainset <- vifsubset[id.train,]
testset <- vifsubset[-id.train,]


#random forest 
install.packages("randomForest")

library("randomForest")

output.forest <- randomForest(Default ~ ., data = trainset, ntree = 100, mtry = 4, na.action = na.roughfix)
?randomForest
plot(output.forest)
importance(output.forest)
varImpPlot(output.forest)
pred.forest <- predict(output.forest, newdata = testset, type = "class")

# View the forest results
print(output.forest) 
table(testset$Default,pred.forest)


#misclassification 
(40+39)/ (246+40+39+95)
#18.8% misclassification rate 

# Importance of each predictor.
print(importance(fit,type = 2)) 




