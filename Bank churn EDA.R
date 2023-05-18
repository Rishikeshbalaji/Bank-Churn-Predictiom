library(dplyr)
library(moments)
library(gvlma)
library(leaps)
library(gplots)
library(car)

##1)importing the dataset
bank_churn <- read.csv("C:/Users/Renuka/Desktop/ALy 6000 Jupyter labs/ALY 6010/Churn Modeling.csv", header = TRUE, sep = ",")

##2)taking a glance at the data
head(bank_churn,5)
tail(bank_churn,5)

##3)dataset description
summary(bank_churn)
str(bank_churn)

###4)Data cleaning
###4.1)Rownumber,customerid and surname columns can be dropped as it is will not be used anywhere in the analysis
bank_churn <- bank_churn[,c(-1,-2,-3)]

###4.2)converting the columns as categorical variables
bank_churn$Geography <- as.factor(bank_churn$Geography)
bank_churn$Gender <- as.factor(bank_churn$Gender)
bank_churn$NumOfProducts <- as.factor(bank_churn$NumOfProducts)
bank_churn$HasCrCard <- as.factor(bank_churn$HasCrCard)
bank_churn$Exited <- as.factor(bank_churn$Exited)
bank_churn$IsActiveMember <- as.factor(bank_churn$IsActiveMember)
str(bank_churn)

###Variable of interest is Exited

###5)Univariative analysis

###5.1)box plots
par(mfrow=c(2,2))

boxplot(bank_churn$CreditScore,  
        col = c("red"), 
        main = "CREDIT SCORE")

boxplot(bank_churn$Age,  
        col = c("red"), 
        main = "AGE")

boxplot(bank_churn$Balance,  
        col = c("red"), 
        main = "BALANCE")

boxplot(bank_churn$EstimatedSalary,  
        col = c("red"), 
        main = "SALARY")
###Inferences:
##5.11) It is possible for customers to have low credit score, so considering all the outliers as a valid data
##5.12) Age of customers can be above 60, so considering it as a valid data. It is important to note that most of the customers are below 60 years of age
##5.13) No outliers in Balance and Salary

###5.2)
products <- table(bank_churn$NumOfProducts)
gender <- table(bank_churn$Gender)
geography <- table(bank_churn$Geography)
exited <- table(bank_churn$Exited)

par(mfrow=c(2,2))
barplot(products, 
        main = "Products", 
        xlab = "Product", 
        ylab = "Frequency",
        col="red",
        ylim = c(0,6000))

barplot(gender, 
        main = "Gender", 
        xlab = "Gender", 
        ylab = "Frequency",
        col="red",
        ylim= c(0,6000))

barplot(geography, 
        main = "Geography", 
        xlab = "Geography", 
        ylab = "Frequency",
        col="red",
        ylim = c(0,5000))

barplot(exited, 
        main = "Exited", 
        xlab = "Exited", 
        ylab = "Frequency",
        col="red",
        ylim = c(0,8000))


###6)bivariative analysis

frequency_1 <- table(bank_churn$Exited, bank_churn$Gender)
frequency_2 <- table(bank_churn$Exited, bank_churn$NumOfProducts)
frequency_3 <- table(bank_churn$Exited, bank_churn$Geography)

par(mfrow=c(1,3))
barplot(frequency_1, 
        beside= TRUE,
        ylim = c(0,5000),
        main = "Gender vs Exited",
        xlab = "Exited",
        ylab = "Frequency of customers", col=c("orange", "steelblue"),
        legend =TRUE)

barplot(frequency_2, 
        ylim = c(0,5000),
        main = "Products vs Exited",
        xlab = "Gender",
        ylab = "Frequency of customers",
        legend =TRUE,
        col=c("orange", "steelblue"))



barplot(frequency_3, 
        beside = TRUE,
        ylim = c(0,5000),
        main = "geography vs Exited",
        xlab = "geography",
        ylab = "Frequency of customers",
        legend =TRUE,
        col=c("orange", "steelblue"))

###Inferences
##1)Number of female customers exited is more than the number of male customers
##2)Customers who took all 4 products always exited
##3)France and Germany almost has same number of exited customers


# Create a correlation matrix(1.3)
cor_df <- bank_churn[,c(1,2,3,4,5,6,7,8,9,10,11)]
str(cor_df)

cor_df$Geography <- as.character(cor_df$Geography)
cor_df$Gender <- as.character(cor_df$Gender)
cor_df$NumOfProducts <- as.numeric(cor_df$NumOfProducts)
cor_df$HasCrCard <- as.numeric(cor_df$HasCrCard)
cor_df$IsActiveMember <- as.numeric(cor_df$IsActiveMember)
cor_df$Exited <- as.numeric(cor_df$Exited)

cor_df$Gender[cor_df$Gender == "Male"] <- 1
cor_df$Gender[cor_df$Gender == "Female"] <- 0

cor_df$Geography[cor_df$Geography == "Germany"] <- 1
cor_df$Geography[cor_df$Geography == "France"] <-2 
cor_df$Geography[cor_df$Geography == "Spain"] <- 3

cor_df$Gender <- as.numeric(cor_df$Gender)
cor_df$Geography <- as.numeric(cor_df$Geography)

head(cor_df)
cor_df <- as.numeric[,c(2,3,4,5,7,8,9,11)]

correlation <- round(cor(cor_df),2)
correlation

#heatmap(1.3)

column_names <- colnames(cor_df)

heatmap.2(correlation, trace = "none", col = colorRampPalette(c("white","steelblue"))(100), scale = "none",
          main = "Heatmap",
          cexRow = 0.8, cexCol = 0.8, margins = c(6, 6),
          symm = TRUE, na.rm = TRUE,
          labRow = column_names, labCol = column_names, cellnote = correlation,
          notecol = "black", notecex = 0.8, key.title = NA)
#### Exited has positive correlation only with Balance and Age


###7)Logistic Regression
glm_fit1 <- glm(Exited ~ ., data =bank_churn, family = "binomial")
summary(glm_fit1)
plot(glm_fit1)


glm_fit2 <- glm(Exited ~ Age*Geography, data =bank_churn, family = "binomial")
summary(glm_fit2)


par(mfrow=c(2,2))
plot(glm_fit2)


###8)checking mulicollinearity
vif_values <- vif(glm_fit1)
vif_values

vif_values <- vif(glm_fit2)
vif_values1

##9)checking for outliers
outlierTest(glm_fit1)

crPlots(glm_fit1)

##training and test sets
trainIndex <- sort(sample(x=nrow(bank_churn), size =nrow(bank_churn)*0.7))
sample_train <- bank_churn[trainIndex,]
sample_test <- bank_churn[-trainIndex,]

###forward selection method
step(lm(Exited~1, data = cor_df), direction = 'forward', scope = ~ CreditScore+Geography+Age+NumOfProducts+IsActiveMember)

anova(glm_fit1)

