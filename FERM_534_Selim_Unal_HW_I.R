# Set working environment load data
library(readxl)
library(AER)
library(survey)
library(MASS)
library(verification)
library(ROCR)
library(caret)
library(mltools)
library(onehot)
library(data.table)

setwd("/home/selim/Documents/Ozu/FERM 534/HW")
data <-as.data.frame(read_excel("CaseStudy.xls"))

# Explore structure of data
head(data)
str(data)

#Let's check whether data has any missing values
unique(is.na(new_data)) 

# I eliminate "Name, City,State,Bank" columns from data as these are not of interest
# Moreover, ChgOff amount is dependent on our independent variable Status,
# therefore I also do not take it into consideration
new_data <- data[,c("Term","NoEmp","NewExist","UrbanRural","DisbursementGross","Status","GrAppv",
                    "Guarant","RealEstate","Portion","Recession")]
# Change the format of dependent variable
# If it has been charged off then denote it as 1, 0 otherwise
new_data$Status <- as.numeric(ifelse(new_data$Status =="CHGOFF", 1,0 ))

# Check whether independent variables are correlated with each other
cor(new_data)

# I see high correlation between Term and RealEstate
# and among DisbursementGross, GrAppv and Guarant
plot(new_data$RealEstate,new_data$Term)
# This plot shows that all long term loans(>20 years) are backed by real estate, which is normal
# but DisbursementGross, GrAppv and Guarant correlation seems natural as they are all linked to loan amount

#Let's create a model that includes all of the dependent variables
logit_model_full <- glm(Status ~ ., family = binomial(link = "logit"), data = new_data) 
summary(logit_model_full)

# At %5 significance level, I see NewExist and DisbursementGross are redundant with p values 0.98 and 0.46,respectively
# Recession and Portion variables also seem redundant albeit at a lower level with p values 0.28 and 0.06 respectively

#Let's check heteroscedasticity-consistent standard errors for coefficients
coeftest(logit_model_full, vcov. = vcovHC, type="HC1")  

# I again see that NewExist, Recession and Disbursement Gross variables have  high p values and seem redundant
# Portion variable has  p value close to my threshold of 5%, therefore I will keep it for further testing

# Let's plot the residuals of the full model
plot(logit_model_full$residuals)

#I see concentration around zero for most of the residuals but there exists some outliers with residual greater than 100
check=(logit_model_full$residuals[logit_model_full$residuals>100])

#what are these outliers?
new_data[as.numeric(names(check)),]

#When I check these outliers, I see that all dependent variables are around two standard deviations
#around the their respective means. Therefore, I will not go deeper within the context of this homework.

#Measures of fit for the whole model
#pseudo R-square
pseudoR2<-1-logit_model_full$deviance/logit_model_full$null.deviance
pseudoR2

# Homework Part I and Part III
# I develop one model  having significant variables above and an additional one having Portion also
logit_model_I <- glm(Status ~ Term+NoEmp+UrbanRural+GrAppv+Guarant+RealEstate, family = binomial(link = "logit"), data = new_data) 
logit_model_II <- glm(Status ~ Term+NoEmp+UrbanRural+GrAppv+Guarant+RealEstate+Portion, family = binomial(link = "logit"), data = new_data)

# I check whether these two models are significantly different than the intercept only models
# With significantly lower p values, I can confirm that both models are significantly better than 
# intercept only model
waldtest(logit_model_I) 
waldtest(logit_model_II) 
waldtest(logit_model_I, vcov=vcovHC(logit_model, type="HC1")) 
waldtest(logit_model_II, vcov=vcovHC(logit_model, type="HC1")) 

# When I check these two model between each other
# I also see that model with variable Portion in addition 
# is significantly better at significance level 5% with p value 4,43%
anova(logit_model_I, logit_model_II, test='Chisq') 



# I also see that full model with all variables are not significantly better
# than model I select above
anova(logit_model_II, logit_model_full, test='Chisq') 

# Therefore I select model II from now on.
logit_model=logit_model_II

pseudoR2<-1-logit_model$deviance/logit_model$null.deviance
pseudoR2

# Part II
# Let's look at the summary of the model
summary(logit_model)

# I confirm that all variables are significant at %5 level
# Let's create blank data with mean values to predict the effect of 1% change
# around mean values of data

check_data <- data.frame(
  "Term" = round(mean(new_data$Term)), # Discrete value rounded to nearest integer
  "NoEmp" = round(mean(new_data$NoEmp)), # Discrete value rounded to nearest integer
  "UrbanRural" = median(new_data$UrbanRural),
  "Portion" = mean(new_data$Portion),
  "GrAppv" = mean(new_data$GrAppv),
  "RealEstate" = median(new_data$RealEstate)
  )


#As Guarant is dependent on GrAppv and Portion for each loan, it is calculated separately
check_data$Guarant=check_data$GrAppv * check_data$Portion

# Create a temporary dataframe for predictions
temp=check_data
# Now that we have an example average loan, let's see how changing values effect the result
# First, Increase Term by 1 year
temp$Term=check_data$Term+12
predict_0 <- predict(logit_model, newdata = check_data, type = "response")
predict_I <- predict(logit_model, newdata = temp, type = "response")
change=round((predict_I-predict_0) *100) # result in percentage
change
# For an average loan, an increase in term by one year, decreases default risk by 3%.


#Second, Increase NoEmp by 10 
temp=check_data
temp$NoEmp=check_data$NoEmp+10
predict_0 <- predict(logit_model, newdata = check_data, type = "response")
predict_I <- predict(logit_model, newdata = temp, type = "response")
change=round((predict_I-predict_0) *100) # result in percentage
change
# For an average loan, an increase in number of employees by 10, decreases default risk by 1%.

#Third, back loan by real estate
temp=check_data
temp$RealEstate=check_data$RealEstate+1
predict_0 <- predict(logit_model, newdata = check_data, type = "response")
predict_I <- predict(logit_model, newdata = temp, type = "response")
change=round((predict_I-predict_0) *100) # result in percentage
change
# For an average loan, backing loan by real estate, increases default risk by 91%.
# This is calculated based on the model. Please recall earlier observation that 
# long term loans are backed by real estate. So this a categorical value based on
# term of loan. It should not be applied straightforward to our average loan.

#Fourth, change UrbanRural Setting
temp=check_data
temp$UrbanRural=2
predict_0 <- predict(logit_model, newdata = check_data, type = "response")
predict_I <- predict(logit_model, newdata = temp, type = "response")
change=round((predict_I-predict_0) *100) # result in percentage
change
# For an average loan of urban origin, changing origin to rural increases default risk by 6%.
temp=check_data
temp$UrbanRural=0
predict_0 <- predict(logit_model, newdata = check_data, type = "response")
predict_I <- predict(logit_model, newdata = temp, type = "response")
change=round((predict_I-predict_0) *100) # result in percentage
change
# For an average loan of urban origin, changing origin to undefined decreases default risk by 3%.

#Fifth, change Approved Loan Amount
temp=check_data
temp$GrAppv=check_data$GrAppv *1.10
predict_0 <- predict(logit_model, newdata = check_data, type = "response")
predict_I <- predict(logit_model, newdata = temp, type = "response")
change=round((predict_I-predict_0) *100) # result in percentage
change
# For an average loan, increasing loan amount by 10%  rural increases default risk by 1%.


#Sixth, Portion guaranteed by government
temp=check_data
temp$Portion=check_data$portion *1.10
temp$Guarant=check_data$Guarant *1.10
predict_0 <- predict(logit_model, newdata = check_data, type = "response")
predict_I <- predict(logit_model, newdata = temp, type = "response")
change=round((predict_I-predict_0) *100) # result in percentage
change
# For an average loan, increasing government guarantee portion 10% decreases default risk by 1%.

#I will not repeat calculation for Guarant as it is basically same.

# One last remark before continuing, it would be much better to standardize 
# dependent variables to have meaningful results based on deviations in multiples of 
# their standard deviations


# Part IV and V
# Now that we have a model let's try to make predictions on exisitng data
pred<-predict(logit_model, type="response")

#I will select threshold at 0.5 first
pred_logit <-  ifelse(pred>0.5 , 1, 0)

#Let's have confusion matrix of predictions
prop.table(table(true=new_data$Status,pred=pred_logit))

#Summary of the matrix
# For every predicted value of non-default,
# we know correctly for  85,5% =(0.60704091/(0.10275928+0.60704091))  (1)
# That means 14,5 % of our approved loans will default.
# This is the parameter called precision.
# For every non-default loan,
# model correctly guesses 90,1% (0.60704091/(0.60704091+ 0.06660324)) (2)
# as eligible for loan approval.
# This is the parameter called recall.

# ROC Curve is close to left most corner 
roc.plot(new_data$Status, pred)$roc.vol

# Area of ROC Curve is around 88%
roc.area(new_data$Status, pred)

# which are both good indicators of performance.

# Now, how can we increase performance?
# We should increase  parameter (1) precision and increase parameter (2) recall.
# For that let's try different thresholds and plot precision and recall

threshold=seq(0.01,1,0.01)
precisions=c()
recalls=c()

for (x in threshold){
  pred_logit <-  factor(ifelse(pred>x , 1, 0), levels=c(0,1))
  precisions=c(precisions, precision(data=(pred_logit),reference=(as.factor(new_data$Status))))
  recalls=c(recalls, recall(data=pred_logit,reference=as.factor(new_data$Status)))
}

graph_data=data.frame(threshold,precisions,recalls)
ggplot(graph_data, aes(x=threshold)) +                    
  geom_line(aes(y=precisions, colour="precisions")) +  
  geom_line(aes(y=recalls, colour="recalls"))  +
  xlab("Probability Threshold") + ylab("Score") +
  scale_colour_manual("", 
                      breaks = c("precisions", "recalls"),
                      values = c("blue", "red")) +
  scale_x_continuous(breaks=c(0,0.25,0.4,0.5,0.75,1), limits=c(0,1))+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,0.9,1), limits=c(0,1))+
  labs(title="Precision Recall Curve")

# 0.43 seems a better threshold than 0.5 from the graph 
# Let's calculate confusion matrix again
pred_logit <- factor(ifelse(pred>0.43 , 1, 0), levels=c(0,1))
prop.table(table(true=new_data$Status,pred=pred_logit ))

precision(data=(pred_logit),reference=(as.factor(new_data$Status)))
recall(data=pred_logit,reference=as.factor(new_data$Status))

# By changing threshold to 0.43 we have been able to improve precision 
# from 85,5% to 89,7%. This means only 10.3% of of approved loans will default
# compared to 14.5%. We achieved this increase by only slightly decreasing recall rate
# from 90.1% to 89.1%. That means we will approve 1% less of loans (90.1 %- 89.1%) but 
# instead default loans will decrease by 29% (from 14.5% to 10.3%).
# I believe that is a good trade-off.

# I will not perform justification based on selection off two example companies,
# as I believe above justification is much better.





