getwd()
setwd("A:/ASSESMENT/s4b")
dir()

install.packages("dplyr")
install.packages("ltm")
#LOAD THE LIBRARY
library(tidyverse)
library(corrplot)
library(psych)
library(dplyr)
library(ggplot2)
library(gtools)
library(caret)
library(readr)
library(readxl)
library(car)
library(GGally)
library(DataExplorer)
library(explore)
library(ltm)


data<-read_excel("banksv.xlsx")
view(data)

summary(data)
describe(data)

#to Check for NA values
anyNA(data)
sum(is.na(data))

#outputs will be more readablity:
options(scipen = 150)

# converted to factors if it is character
data<- data %>% mutate_if(is.character, as.factor)


#Addressing Data quality issue

#Data quality issue of age
summary(data$age)
boxplot(data$age)
data$age[data$age>100]<- mean(data$age)
data$age[data$age < 5] <- mean(data$age)
boxplot(data$age) # After cleaning data
summary(data$age)

summary(data$job)  
summary(data$marital)

#Data quality issue of default
summary(data$default)
data$default[data$default == 'n' ] <- 'no'
data$default<-as.character(data$default)
data$default<- as.factor(data$default)
summary(data$default)

summary(data$housing)
summary(data$loan)
summary(data$contact)

summary(data$month)
data$month[data$month == 'march' ] <- 'mar'
data$month<-as.character(data$month)
data$month<- as.factor(data$month)
summary(data$month)

summary(data$day_of_week)
summary(data$duration)
summary(data$housing)

summary(data$pdays)
data$pdays[data$pdays == 999] <- 0 #as 999 means not contacted
boxplot(data$pdays)

summary(data$subscribed)
data$subscribed <- relevel(data$subscribed, "yes")

#Data Analysis and Data Visualisation


# plot with target variable:
ggplot(data, aes(x = subscribed)) + geom_bar(colour="black", fill="darkblue") + 
  ggtitle(" Target variable (subscribed)") +
  xlab("Did consumer subscribed (yes/no)") + ylab("Density")

plot_boxplot(data,by="subscribed")



#Relationship between the age and subscribed
chisq.test(data$age,data$subscribed)

boxplot(age~subscribed,
        data=data,
        col=(c("gold","darkgreen")),
        main=" Boxplots for client age with subscribed",
        ylab="Subscribed",
        xlab="Age",
        horizontal = TRUE)


#Relationship between the job and no of people subscribed
chisq.test(data$job, data$subscribed, correct = FALSE)

#ggplot plot of job of all customers
ggplot(data, aes(x = job)) + 
  geom_bar(colour="white", fill="lightblue") +
  ggtitle("Jobs of Overall people") +
  xlab("jobs") + ylab("total number of people") 

#ggplot plot of job with only people who subscribed
people_yes <- data %>% group_by(job) %>% filter(subscribed == 'yes')
view(people_yes)

ggplot(people_yes,aes(x= job))+ geom_bar(colour="white", fill="darkblue") + 
  ggtitle("Jobs of people that subscribed") +
  xlab("Types of Jobs") + ylab("Number of people") + 
  theme( 
    plot.title = element_text(color="black", size=15, face="bold")
  )
#ggplot plot of job
job_tab <- data.frame(table(data$job, data$subscribed))
colnames(job_tab) <- c("job","response","count")
ggplot(data=job_y_tab, aes(x=count,y=reorder(job,count), fill=response))+
  geom_bar(stat = 'identity', position = 'dodge')+
  xlab("Number of customers")+ylab("Job distribution with yes/no to subscribe") +
       ggtitle("Marketing Result of Job Distribution")



#Relationship between the marital status and no of people subscribed

chisq.test(data$marital, data$subscribed, correct = FALSE)

#ggplot plot of  marital status of all customers
ggplot(data, aes(x = marital)) + 
  geom_bar(colour="black", fill="lightblue") +
  ggtitle("Marital status of people") +
  xlab("jobs") + ylab("Number of customers")

#ggplot plot of marital status of all customers and subscribed rate
ggplot(data = data, aes(x = marital, fill = subscribed)) +
  geom_bar() + ggtitle("Marital status of people") +
  xlab("Martial status") + ylab("Number of customers ")




#Relationship between the education and no of people subscribed

chisq.test(data$education, data$subscribed, correct = FALSE)
table(data$education)
edutable <- table(data$education, data$subscribed)
tab <- as.data.frame(prop.table(edutable, 2))
colnames(tab) <-  c("education", "subscribed", "perc")

ggplot(data = tab, aes(x = education, y = perc, fill = subscribed)) + 
  geom_bar(stat = 'identity', position = 'dodge')  +
  xlab("Education") +
  ylab("Percentage") + theme(axis.text.x=element_text(angle = 90, hjust = 0))



#Relationship between the month and no of people subscribed
chisq.test(data$month, data$subscribed, correct = FALSE)

monthtable <- table(data$month, data$subscribed)
tab <- as.data.frame(prop.table(monthtable, 2))
colnames(tab) <-  c("month", "subscribed", "perc")

ggplot(data = tab, aes(x = month, y = perc, fill =subscribed )) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) + 
  xlab("Month")+
  ylab("Percentage")


#Relationship between the default and no of people subscribed
chisq.test(data$default, data$subscribed, correct = FALSE)

def_tab <- data.frame(table(data$default,data$subscribed))
colnames(def_tab) <- c("default","subscribed","number_of_customers")
ggplot(data=def_tab, aes(x=default,y=number_of_customers, fill=subscribed))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3)+ xlab("Default")+
  ylab("No of Customers")

#Relationship between the poutcome and no of people subscribed
chisq.test(data$poutcome, data$subscribed, correct = FALSE)

ggplot(data=data, aes(x=poutcome, fill=subscribed))+
  geom_bar(position = 'stack')+
  labs(X="Number of customers",
       y="No of Customers")
       

# Correlation  

plot_correlation(data, type = 'continuous')# Graphical representation of correlation for continuous variables
plot_correlation(data, type = 'discrete') # Graphical representation of correlation for discrete variables
ggcorr(data, label=TRUE)


#Split data into train and test dataset
set.seed(40387258)
index <- createDataPartition(data$subscribed, p= 0.8, list=FALSE)
train <- data[index,]
test <- data[-index,]

#model1
# To produce a logistic regression
formula <- subscribed ~ age + job + marital + education + poutcome + month
model1 <- glm(formula, data = train, family = "binomial")
summary(model1)

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
logisticPseudoR2s(model1)

vif(model1)
predictions<- predict(model1, test, type = "response")
class_pred <- as.factor(ifelse(predictions > .5, "yes", "no"))
postResample(class_pred, test$subscribed)
confusionMatrix(data=class_pred, test$subscribed)

#Model2
formula2 <- subscribed ~ poutcome + age + job + nr.employed + euribor3m  + emp.var.rate+ cons.price.idx  
model2<- glm(formula2, data = train, family = "binomial"(link="logit"))
summary(model2)


logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
logisticPseudoR2s(model2)

#To get Odds Ratios
exp(model1$coefficients)

#For Confidence intervals
exp(confint(model2))

#Predicted Probabilities
train$predictedProbabilities <- fitted(model2)

head(data.frame(train$predictedProbabilities, train$subscribed))

#Analysing the Residuals
resid(model2)
train$standardisedResiduals <- rstandard(model2)
train$studentisedResiduals <- rstudent(model2)
sum(train$standardisedResiduals > 1.96)

#Examining Influential Cases
train$cook <- cooks.distance(model2)
sum(train$cook > 1)

#Examining leverage
train$leverage <- hatvalues(model2)
sum(train$leverage > 0.0009)

vif(model2)

predictions<- predict(model2, test, type = "response")
class_pred <- as.factor(ifelse(predictions > .5, "yes", "no"))
postResample(class_pred, test$subscribed)
confusionMatrix(data=class_pred, test$subscribed)


#Model3
formula3 <- subscribed ~ age+ poutcome+month+ emp.var.rate +campaign +previous + euribor3m +  cons.conf.idx + nr.employed 
model3 = glm(formula3 ,data = train,family = "binomial")
summary(model3)


logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
logisticPseudoR2s(model3)

#To get Odds Ratios
exp(model3$coefficients)

#For Confidence intervals
exp(confint(model3))

#Predicted Probabilities
train$predictedProbabilities <- fitted(model3)

head(data.frame(train$predictedProbabilities, train$subscribed))

#Analysing the Residuals

train$standardisedResiduals <- rstandard(model3)
train$studentisedResiduals <- rstudent(model3)
sum(train$standardisedResiduals > 1.96)

#Examining Influential Cases
train$cook <- cooks.distance(model3)
sum(train$cook > 1)

#Examining leverage
train$leverage <- hatvalues(model3)
sum(train$leverage > 0.0009)

vif(model3)

predictions<- predict(model3, test, type = "response")
class_pred <- as.factor(ifelse(predictions > .5, "yes", "no"))
postResample(class_pred, test$subscribed)
confusionMatrix(data=class_pred, test$subscribed)


#model4
formula4 <- subscribed ~    month + poutcome+contact + emp.var.rate + cons.price.idx + cons.conf.idx + nr.employed 
model4 = glm(formula4 ,data = train,family = "binomial")
summary(model4)

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
logisticPseudoR2s(model4)

#To get Odds Ratios
exp(model4$coefficients)

#For Confidence intervals
exp(confint(model4))

#Predicted Probabilities
train$predictedProbabilities <- fitted(model4)

head(data.frame(train$predictedProbabilities, train$subscribed))

#Analysing the Residuals
resid(model4)
train$standardisedResiduals <- rstandard(model4)
train$studentisedResiduals <- rstudent(model4)
sum(train$standardisedResiduals > 1.96)
sum(train$standardisedResiduals > 2.58)

 #Examining Influential Cases
train$cook <- cooks.distance(model4)
sum(train$cook > 1)

#Examining leverage
train$leverage <- hatvalues(model4)
sum(train$leverage > 0.0009)

vif(model4)

predictions<- predict(model4, test, type = "response")
class_pred <- as.factor(ifelse(predictions > .5, "yes", "no"))
postResample(class_pred, test$subscribed)
confusionMatrix(data=class_pred, test$subscribed)


