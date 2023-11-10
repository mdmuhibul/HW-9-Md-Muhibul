
#Fall 2023
#Homework 9

#Md Muhibul Islam; Mohammed A. Al Muhaymin; Zakaria Sule

library(plyr)
library(dplyr)
library(tidyverse)
library(haven)
attach(acs2021)
summary(acs2021)


load("C:/Program files two/Fall 2023/Econometrics/acs2021_ny_data.RData")
getwd()
levels_n <- read.csv("IND_levels.csv")
names(levels_n) <- c("New_Level","levels_orig")
acs2021$IND <- as.factor(acs2021$IND)
levels_orig <- levels(acs2021$IND) 
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))

acs2021$public_work <- acs2021$IND 
levels_public <- read.csv("publicwork_recode.csv")
names(levels_public) <- c("levels_orig","New_Level")
levels_new_pub <- join(data.frame(levels_orig),data.frame(levels_public))


levels(acs2021$IND) <- levels_new$New_Level
levels(acs2021$public_work) <- levels_new_pub$New_Level
summary(levels(acs2021$IND))
summary(levels_n)
summary(names(levels))
acs2021$public_work_num <- as.numeric(acs2021$public_work == "work for public, stable")
table(acs2021$public_work,acs2021$public_work_num)
print

getwd()
levels_n <- read.csv("IND_levels.csv")
names(levels_n) <- c("New_Level","levels_orig")
acs2021$IND <- as.factor(acs2021$IND)
levels_orig <- levels(acs2021$IND) 
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))

acs2021$public_work <- acs2021$IND 
levels_public <- read.csv("publicwork_recode.csv")
names(levels_public) <- c("levels_orig","New_Level")
levels_new_pub <- join(data.frame(levels_orig),data.frame(levels_public))


levels(acs2021$IND) <- levels_new$New_Level
levels(acs2021$public_work) <- levels_new_pub$New_Level
summary(levels(acs2021$IND))
summary(levels_n)
summary(names(levels))
acs2021$public_work_num <- as.numeric(acs2021$public_work == "work for public, stable")
table(acs2021$public_work,acs2021$public_work_num)
print


#("The subgroup we have chosen to look at is people 25-35, in the labor force, working year round, 
#fulltime.")
#Here we will consider people 25-35, in labor force, working year round,and married.
#We changed the subset because it is giving problems; there was no data 
use_varb <- (acs2021$AGE>=25) & (acs2021$AGE<=35) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 >4) & (acs2021$MARST == 1)
dat_use <- subset(acs2021,use_varb)
summary(dat_use)
#The dat_use has  amount 5944 obs

ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data=dat_use)
summary(ols_out1)
#The P value is very indicating there is some significance, All of the parameters Pr(>|t|) < 0.05

#Lab 7
#From last time:

ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use)
summary(ols_out1)
##H not = Variable has no effect on public work
#H alternative = Variable has some effect on public work
#Consider alpha = 0.05
#R squared is 0.07115 and probability in female  and advanced degree is smaller than the p value. 
#For them, we can reject the null hypothesis

pred_vals_ols1 <- predict(ols_out1, dat_use)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
table(pred = pred_model_ols1, true = dat_use$public_work_num)
#Here we it is not true and predicted to be not true was 2109, predicted to be false but actually true was 647,
#predicted to be true was actually true was 1805, predicted to be true and actually true was 1383

# logit 
model_logit1 <- glm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use, family = binomial
)
summary(model_logit1)

#
pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_use$public_work_num)
#Here we it is not true and predicted to be not true was 3490, predicted to be false but actually 
#true was 1415,
#predicted to be true was actually false was 424, predicted to be true and actually true was 615

#We will try different preditc vals

pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.25)
table(pred = pred_model_logit1, true = dat_use$public_work_num)
#Has less amount that predicted was false and it is actuakky false
#predicted to be false but actually true
#Has more false postive than predict vals > 0.5
#Has more predicted to be true but actually true

pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.75)
table(pred = pred_model_logit1, true = dat_use$public_work_num)
#Has really high predicted to be false and actually false
#Has really predicted false, but actually true
#PUMA
dat_use$PUMA_factor <- as.factor(dat_use$PUMA)

d_pub_work <- data.frame(model.matrix(~ dat_use$public_work_num)) 

d_female <- data.frame(model.matrix(~ dat_use$female))
d_educ_hs <- data.frame(model.matrix(~ dat_use$educ_hs))
d_educ_somecoll <- data.frame(model.matrix(~ dat_use$educ_somecoll))
d_educ_college <- data.frame(model.matrix(~ dat_use$educ_college))
d_educ_advdeg <- data.frame(model.matrix(~ dat_use$educ_advdeg))
d_age <- data.frame(model.matrix(~ dat_use$AGE))
d_PUMA <- data.frame(model.matrix(~ dat_use$PUMA_factor))

#Confirmed sum of it is equal to 0
sum( colSums(d_PUMA) == 0)

#Put Together
dat_for_analysis_sub <- data.frame(
  d_pub_work[,2], # need [] since model.matrix includes intercept term
  d_female[,2],
  d_educ_hs[,2],
  d_educ_somecoll[,2],
  d_educ_college[,2],
  d_educ_advdeg[,2],
  d_age[,2],
  d_PUMA[,2:145] )

#Analysis
names(dat_for_analysis_sub)
names(dat_for_analysis_sub) <- sub("dat_use.","",names(dat_for_analysis_sub)) # drops each repetition of dat_use

names(dat_for_analysis_sub)[1] <- "pub_work"
names(dat_for_analysis_sub)[2] <- "female"
names(dat_for_analysis_sub)[3:6] <- c("HS","SomeColl","College","AdvDeg")
names(dat_for_analysis_sub)[7] <- "Age"

names(dat_for_analysis_sub)

#TraiN data
install.packages("standarize")
require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$pub_work)

#10 percent was used sum of iterations was not zero, so we used 0.35
restrict_1 <- (runif(NN) < 0.35) # use 10% as training data, ordinarily this would be much bigger 
#but start small
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

# again check this below, should be zero
sum( colSums(dat_train) == 0)
#Confirmed it is actually zero

#Check
fmla_sobj <- reformulate( names(dat_for_analysis_sub[2:151]), response = "pub_work")
sobj <- standardize(fmla_sobj, dat_train, family = binomial)
s_dat_test <- predict(sobj, dat_test)

#Final
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$pub_work)

# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$pub_work)
#The coefficcients are more significant in this model than previous model
#i.e education of high school

# Homework 9

use_varb <- (acs2021$AGE>=25) & (acs2021$AGE<=35) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 >4) & (acs2021$MARST == 1) & (RACE = 1)
dat_use <- subset(acs2021,use_varb) # 
summary(dat_use)
#This subset is getting people ages 25 to 35, who are working for some time, married, and white

use_varb2 <- (acs2021$AGE>=25) & (acs2021$AGE<=35) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 >4) & (acs2021$MARST == 1) & (RACE = 2)
dat_use2 <- subset(acs2021,use_varb2)
summary(dat_use2)
#This subset is getting people ages 25 to 35, who are working for some time, married, and African-america


#This is the simple regression
ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data=dat_use2)
summary(ols_out1)

# for now, really simplify the education dummy
dat_use$BA_plus <- dat_use$educ_college + dat_use$educ_advdeg

# Here we are public work file with female, people who have bachelor degree plus, 
#their age, and mixed of female and ba plus, and mix of age and female
model_lpm_v1 <- lm(public_work_num ~ female + BA_plus + AGE + I(female*BA_plus) + I(AGE * female), data = dat_use)
summary(model_lpm_v1)

dat_use_female <- subset(dat_use,as.logical(dat_use$female))
dat_use_male <- subset(dat_use,!(dat_use$female))

##Here the data is splitting two parts -- female and male
model_lpm_v1f <- lm(public_work_num ~ BA_plus + AGE, data = dat_use_female)
summary(model_lpm_v1f)
model_lpm_v1m <- lm(public_work_num ~ BA_plus + AGE, data = dat_use_male)
summary(model_lpm_v1m)

#In both of the regressions age was not significant, but getting education higher bachelor degree was
#highly significant. The p value with female is much lower than male regression model.

#Random Forest
#install.packages('randomForest') # For generating random forest model
#library(randomForest)
require(randomForest)
set.seed(54321)
model_randFor <- randomForest(as.factor(pub_work) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)
# look at confusion matrix for this too
pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$pub_work)

#The Number of trees was 500
#Number of variables to split was 12
#Predicted to be false and actually false was 2296 or 59 percent
#Predicted to be true but actually false was 238 or 0.06 percent
#Predicted to be false, but actually true was 974 or 0.25
#Predicted to be true and actually true was 395 or 10 percent


#Support Vector Machine
#install.packages('e1071') 
#library(e1071)
require(e1071)
# tuned_parameters <- tune.svm(as.factor(pub_work) ~ ., data = sobj$data, gamma = 10^(-3:0), cost = 10^(-2:2)) 
# summary(tuned_parameters)
# figure best parameters and input into next
svm.model <- svm(as.factor(pub_work) ~ ., data = sobj$data, cost = 1, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$pub_work)

#Predicted to be false and actually false was 2197 or 57 percent
#Predicted to be true but actually false was 337 or 0.09 percent
#Predicted to be false, but actually true was 939 or 0.24
#Predicted to be true and actually true was 360 or 9 percent

#Based on this, random forest model was better based in predicted the correct values.  
#Random forest predicted more accurate value than latter model
