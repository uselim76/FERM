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
head(data)
str(data)

new_data <- data[,c("Term","NoEmp","NewExist","UrbanRural","DisbursementGross","Status","GrAppv",
                    "Guarant","RealEstate","Portion","Recession")]

head(new_data)
str(new_data)
new_data$Status <- ifelse(new_data$Status =="CHGOFF", 1,0 )

logit_model <- glm(Status ~ ., family = binomial(link = "logit"), data = new_data) # ~. includes all the variables in the data set
coeftest(logit_model, vcov. = vcovHC, type="HC1")                

summary(logit_model)
plot(logit_model$residuals)
check=(logit_model$residuals)
length(check[check>10])
check_data <- new_data[check<100,]


logit_model <- glm(Status ~ ., family = binomial(link = "logit"), data = check_data) # ~. includes all the variables in the data set

waldtest(logit_model) # regTermTest assumes homoscedasticity
waldtest(logit_model, vcov=vcovHC(logit_model, type="HC1")) # heteroscedasticity-robust Wald test.

logit_model2 <- glm(Status ~ Term+NoEmp+UrbanRural+GrAppv+Guarant+RealEstate, family = binomial(link = "logit"), data = check_data)
coeftest(logit_model2, vcov. = vcovHC, type="HC1")   
anova(logit_model2, logit_model, test='Chisq') 

pred_eff_bas<-predict(logit_model2, type="response")
pred_logit <-  ifelse(pred_eff_bas>0.5 , 1, 0)
pred_eff<-predict(logit_model2, type="response")
threshold=seq(0.01,1,0.01)
precisions=c()
recalls=c()

for (x in threshold){
  pred_logit <-  factor(ifelse(pred_eff>x , 1, 0), levels=c(0,1))
  precisions=c(precisions, precision(data=(pred_logit),reference=(as.factor(new_data$Status))))
  recalls=c(recalls, recall(data=pred_logit,reference=as.factor(new_data$Status)))
}


prop.table(table(true=new_data$Status,pred=pred_logit ))

roc.plot(new_data$Status, pred_eff_bas)$roc.vol
roc.area(new_data$Status, pred_eff)
roc.area(new_data$Status, pred_logit)


###############################################################

cat_attributes=c("NewExist","UrbanRural","RealEstate","Recession","TermStruct")
num_attributes=c("Term","NoEmp","DisbursementGross","GrAppv","Guarant","Portion")
new_data$TermStruct = ifelse(data$Term >120, 1,0 )
new_data$UrbanRural =as.factor(new_data$UrbanRural)
new_data$NewExist =as.factor(new_data$NewExist)
new_data$RealEstate =as.factor(new_data$RealEstate)
new_data$Recession =as.factor(new_data$Recession)
data_cat=one_hot(as.data.table(new_data[,cat_attributes]))

data_num=new_data[,num_attributes]
data_num=as.data.frame(scale(data_num))

new_data=cbind(data_num,data_cat)

new_data$Status <- ifelse(data$Status =="CHGOFF", 1,0)

logit_model <- glm(Status ~ ., family = binomial(link="logit"), data = new_data) # ~. includes all the variables in the data set
coeftest(logit_model, vcov. = vcovHC, type="HC1")      
summary(logit_model)
plot(logit_model$residuals)


logit_model1 <- glm(Status ~ Term+NoEmp+UrbanRural_0+GrAppv+Guarant+Portion+RealEstate_0+TermStruct, 
                    family = binomial(link = "logit"), data = new_data)
coeftest(logit_model1, vcov. = vcovHC, type="HC1")   
anova(logit_model1, logit_model, test='Chisq') 
plot(logit_model1$residuals)

pred_eff<-predict(logit_model1, type="response")
pred_logit <-  ifelse(pred_eff>0.45 , 1, 0)

prop.table(table(true=new_data$Status,pred=pred_logit))
roc.plot(new_data$Status, pred_eff)$roc.vol

roc.area(new_data$Status, pred_eff)
roc.area(new_data$Status, pred_logit)

logit_full<- glm(Status ~ ., family = binomial(link = "logit"), data =new_data)
logit_null<- glm(Status ~ 1, family = binomial(link = "logit"), data = new_data)
#based on BIC
MS_BIC<-step(logit_null, scope=list(lower=logit_null, upper=logit_full), trace=F, direction="both", k=log(nrow(new_data)))
coeftest(MS_BIC, vcov. = vcovHC, type="HC1" )

MS_AIC<-step(logit_null, scope=list(lower=logit_null, upper=logit_full), trace=F, direction="forward", k=2)
coeftest(MS_AIC, vcov. = vcovHC, type="HC1" )


pred_eff<-predict(MS_AIC, type="response")
threshold=seq(0.01,1,0.01)
precisions=c()
recalls=c()

for (x in threshold){
  pred_logit <-  factor(ifelse(pred_eff>x , 1, 0), levels=c(0,1))
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



pred_logit <-  ifelse(pred_eff<0.44 , 0, 1)

prop.table(table(pred=pred_logit,true=new_data$Status))
roc.plot(new_data$Status, pred_eff)$roc.vol

roc.area(new_data$Status, pred_eff)
roc.area(new_data$Status, pred_logit)

pred_eff<-predict(MS_AIC, type="link")
pred <- prediction(pred_eff, new_data$Status)

perf <- performance(pred, "prec", "rec")

plot(perf, avg= "threshold", colorize=T, lwd= 3,
     main= "... Precision/Recall graphs ...")
plot(perf, lty=3, col="grey78", add=T)

