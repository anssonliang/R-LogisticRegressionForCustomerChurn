# Installing the packages 

install.packages(c("caTools", "MASS", "caret", "plyr","pROC","DMwR","PerformanceAnalytics", "psych"))

# Loading the packages required

package<-(c("caTools", "MASS", "caret", "plyr","pROC","DMwR","PerformanceAnalytics","psych"))
lapply(package, require, character.only = TRUE)

# Read the data

churn<-read.csv("./Churn.csv", header = TRUE)
churn<-na.omit(churn)
#row.has.na <- apply(churn, 1, function(x){any(is.na(x))}) # returns logical vectors denoting NA in a row
#churn <- churn[!row.has.na,]
summary(churn) #Customer ID has too many levels and thus irrelevant for our model
churn<-subset(churn, select = -customerID)
str(churn)

# Check correlation matrix 
churn$Churn <- ifelse(churn$Churn=='No',0,1) # binary format, make reading the pairwise correlation plot easier
dmy <- dummyVars("~ .", data = churn)
churnTrsf <- data.frame(predict(dmy, newdata = churn)) #dummify all factors as 'cor' function only accepts numerical values

## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function(X, dfr = nrow(X) - 2){ # creat correlation matrix with p-values
    R <- cor(X, use = "pairwise.complete.obs")
    above <- row(R) < col(R)
    r2 <- R[above]^2
    Fstat <- r2 * dfr/(1 -r2)
    R[above] <- 1 - pf(Fstat, 1, dfr)
    R[row(R) == col(R)] <- NA
    R
}
## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {  # flatten all combinations from square matrix
    if((class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
    if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
    ut <- upper.tri(m)
    data.frame(i = rownames(m)[row(m)[ut]],
               j = rownames(m)[col(m)[ut]],
               cor = t(m)[ut],
               p = m[ut])
}

corMasterList <- flattenSquareMatrix (cor.prob(churnTrsf)) # data frame of 4 columns(i,j,cor,p)
corList <- corMasterList[order(-abs(corMasterList$cor)),]

head(corList, 50)
selectedSub <- subset(corList, ((cor) > 0.2 & j == 'Churn') ) #create a single vector by filtering abs(cor) > 0.2 against 'Churn'
bestSub <- as.character(selectedSub$i) # save the most correlated variables

pairs.panels(churnTrsf[c(bestSub, 'Churn')])  

plot(churn$TotalCharges,churn$tenure)
cor(churn$TotalCharges,churn$tenure)#0.8258805 Highly correlated
plot(churn$TotalCharges,churn$MonthlyCharges)
cor(churn$TotalCharges,churn$MonthlyCharges)# 0.6510648 Not as high as the previous but evident
plot(churn$tenure,churn$MonthlyCharges)
cor(churn$tenure,churn$MonthlyCharges)# 0.2468618 They are almost uncorrelated
# We choose to stick with TotalCharges as the only variable amongst the 3 of them 

# Creating dummy variables, which are necessary when building the model

churn$gender<-revalue(churn$gender, c("Female"="0", "Male"="1")) 
churn$SeniorCitizen<-as.factor(churn$SeniorCitizen)
churn$Partner<-revalue(churn$Partner, c("No"="0", "Yes"="1"))
churn$Dependents<-revalue(churn$Dependents, c("No"="0", "Yes"="1"))
churn$TotalCharges<-as.integer(churn$TotalCharges)
churn$MonthlyCharges<-as.integer(churn$MonthlyCharges)
churn$PhoneService<-revalue(churn$PhoneService, c("No"="0", "Yes"="1"))
churn$MultipleLines1<-revalue(churn$MultipleLines, c("No"="0", "No phone service"="1","Yes"="0"))
churn$MultipleLines2<-revalue(churn$MultipleLines, c("No"="0", "Yes"="1","No phone service"="0"))#If both 0 then No
churn$InternetService1<-revalue(churn$InternetService, c("No"="0", "Fiber optic"="0","DSL"="1"))#base No
churn$InternetService2<-revalue(churn$InternetService, c("No"="0", "Fiber optic"="1","DSL"="1"))#base No
churn$OnlineSecurity1<-revalue(churn$OnlineSecurity, c("No"="0", "Yes"="0","No internet service"="1"))#base No
churn$OnlineSecurity2<-revalue(churn$OnlineSecurity, c("No"="0", "Yes"="1","No internet service"="0"))#base No
churn$OnlineBackup1<-revalue(churn$OnlineBackup, c("No"="0", "Yes"="0","No internet service"="1"))#base No
churn$OnlineBackup2<-revalue(churn$OnlineBackup, c("No"="0", "Yes"="1","No internet service"="0"))#base No
churn$DeviceProtection1<-revalue(churn$DeviceProtection, c("No"="0", "Yes"="0","No internet service"="1"))#base No
churn$DeviceProtection2<-revalue(churn$DeviceProtection, c("No"="0", "Yes"="1","No internet service"="0"))#base No
churn$TechSupport1<-revalue(churn$TechSupport, c("No"="0", "Yes"="0","No internet service"="1"))#base No
churn$TechSupport2<-revalue(churn$TechSupport, c("No"="0", "Yes"="1","No internet service"="0"))#base No
churn$StreamingTV1<-revalue(churn$StreamingTV, c("No"="0", "Yes"="0","No internet service"="1"))#base No
churn$StreamingTV2<-revalue(churn$StreamingTV, c("No"="0", "Yes"="1","No internet service"="0"))#base No
churn$StreamingMovies1<-revalue(churn$StreamingMovies, c("No"="0", "Yes"="0","No internet service"="1"))#base No
churn$StreamingMovies2<-revalue(churn$StreamingMovies, c("No"="0", "Yes"="1","No internet service"="0"))#base No
churn$Contract1<-revalue(churn$Contract, c("Month-to-month"="0", "One year"="1","Two year"="0"))#Month-to-month
churn$Contract2<-revalue(churn$Contract, c("Month-to-month"="0", "One year"="0","Two year"="1"))#Month-to-month
churn$PaperlessBilling<-revalue(churn$PaperlessBilling, c("No"="0", "Yes"="1"))#Checking the gender to male
churn$PaymentMethod1<-revalue(churn$PaymentMethod, c("Electronic check"="0", "Bank transfer (automatic)"="1","Credit card (automatic)"="0","Mailed check"="0"))#Electronic check
churn$PaymentMethod2<-revalue(churn$PaymentMethod, c("Electronic check"="0", "Bank transfer (automatic)"="0","Credit card (automatic)"="1","Mailed check"="0"))#Electronic check
churn$PaymentMethod3<-revalue(churn$PaymentMethod, c("Electronic check"="0", "Bank transfer (automatic)"="0","Credit card (automatic)"="0","Mailed check"="1"))#Electronic check
churn$Churn<-revalue(churn$Churn, c("No"="0", "Yes"="1"))
summary(churn)

# Removing the old variables from the dataset, since we do not need them anymore
churn<-subset(churn, select = -tenure)
churn<-subset(churn, select = -MultipleLines)
churn<-subset(churn, select = -InternetService)
churn<-subset(churn, select = -OnlineSecurity)
churn<-subset(churn, select = -OnlineBackup)
churn<-subset(churn, select = -DeviceProtection)
churn<-subset(churn, select = -TechSupport)
churn<-subset(churn, select = -StreamingTV)
churn<-subset(churn, select = -StreamingMovies)
churn<-subset(churn, select = -Contract)
churn<-subset(churn, select = -PaymentMethod)
churn<-subset(churn, select = -MonthlyCharges)
summary(churn)

# Splitting data into a training and test set

sample = sample.split(churn, SplitRatio = .80)# 80% of the observations are contained in the training data
train = subset(churn, sample == TRUE)
test = subset(churn, sample == FALSE)

# Building models 

fit1<-glm(Churn~.,data=train,family=binomial(link="logit"))
summary(fit1)

fit2<-glm(Churn~.-MultipleLines1-OnlineSecurity1-OnlineBackup1-DeviceProtection1-TechSupport1-StreamingTV1-StreamingMovies1,
          data=train,family=binomial(link="logit"))
summary(fit2)
prediction2 <- predict(fit2,test,type = 'response')
pred2<-ifelse(prediction2>0.5,1,0)#Probabiliies greater than 0.5 defined as churn 
(confussion2<-table(test$Churn,pred2))#Confusion matrix

# Parameters of the confusion matrix 
((confussion2[2,2])/(confussion2[1,2]+confussion2[2,2]))*100#Specificity
((confussion2[1,1])/(confussion2[2,1]+confussion2[1,1]))*100#Sensitivity 
((confussion2[1,1]+confussion2[2,2])/sum(confussion2))*100#Accuracy 

# New model with only signigicant variables
fit3<-glm(Churn~.-OnlineBackup2-DeviceProtection2-gender-Partner-Dependents-MultipleLines1-OnlineSecurity1-OnlineBackup1-DeviceProtection1-TechSupport1-StreamingTV1-StreamingMovies1,data=train,family="binomial")
summary(fit3)
prediction3<- predict(fit3,test,type = 'response')
pred3<-ifelse(prediction3>0.5,1,0)
(confussion3<-table(test$Churn,pred3))
((confussion3[2,2])/(confussion3[1,2]+confussion3[2,2]))*100#Specificity  
((confussion3[1,1])/(confussion3[2,1]+confussion3[1,1]))*100#Sensitivity
((confussion3[1,1]+confussion3[2,2])/sum(confussion3))*100#Accuracy 

# Testing a random observation to see if model 2 and 3 predict accurately
test[331,] # We choose observation 330, but it can be any observation contained in the testset 
pred2[331]
pred3[331]

# Creating a new observation and predicting its outcome
testset<-rbind(test,c(1,0,1,1,1,1,1400,0,0,1,1,0,1,0,1,0,0,1,0,1,1,0,1,0,1,0,0,0,1) )
predict(fit3,testset["1457",],type = 'response')


