install.packages("performance")
library(performance)
library(MASS)
library(dplyr)
library(moments)
library(gvlma)
library(leaps)
library(gplots)
library(car)

##training and test sets
trainIndex <- sort(sample(x=nrow(bank_churn), size =nrow(bank_churn)*0.7))
sample_train <- bank_churn[trainIndex,]
sample_test <- bank_churn[-trainIndex,]

logistic_model <- glm(Exited ~ ., data =sample_train, family = "binomial")
summary(logistic_model)
##impactful variables from the model 1 will be used as the predictor variable in model 2 

##method 1for selecting predictor variables
step(lm(Exited~1, data = cor_df), direction = 'forward', scope = ~ CreditScore+Geography+Age+NumOfProducts+IsActiveMember)

##method 2 for selecting predictor variables
best_subset = regsubsets(Exited ~ ., data = sample_train, nvmax = 5)
reg.summary <- summary(best_subset)
reg.summary

reg.summary$cp
reg.summary$adjr2
reg.summary$bic

which.min(reg.summary$cp)
which.max(reg.summary$adjr2)
which.min(reg.summary$bic)


par(mfrow=c(2,2))
plot(logistic_model)

###checking mulicollinearity
vif_values <- vif(logistic_model)
vif_values

fit_2 <- glm(Exited ~ CreditScore+Gender+Geography+Age+NumOfProducts+IsActiveMember,family= binomial, data =sample_train)

check_model(logistic_model)
summary(fit_2)

par(mfrow=c(2,2))
plot(fit_2)
###checking mulicollinearity
vif_values1 <- vif(fit_2)
vif_values1

AIC(logistic_model,fit_2)
BIC(logistic_model,fit_2)
anova(logistic_model,fit_2)

##both method 1 and 2 suggest that predictors with CreditScore+Gender+Geography+Age+NumOfProducts+IsActiveMember is the best model

fit_3 <- glm(Exited ~ CreditScore+Gender+Geography+Age+NumOfProducts+IsActiveMember,family= quasibinomial, data =sample_train)
summary(fit_3)

