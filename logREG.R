rm(list=ls())
## Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("caTools")
#install.packages("lazyeval")
#install.packages("labeling")
#install.packages("Rcpp")
#install.packages("ggplot2")
#install.packages("RcppRoll")
#install.packages("bindrcpp")
#install.packages("broom")
#install.packages("ddalpha")
#install.packages("DEoptimR")
#install.packages("dimRed")
#install.packages("gower")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(stringr)

#Import the csv files
emp_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey <- read.csv("manager_survey_data.csv",stringsAsFactors = F)
gen_data <- read.csv("general_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

#Data Preparation
#Assigning name "EmployeeID" to col 1 in_time and out_time
colnames(in_time)[1]<-c("EmployeeID")
colnames(out_time)[1]<-c("EmployeeID")

###Checking for duplicates
any(duplicated(emp_survey$EmployeeID))
any(duplicated(in_time$EmployeeID))
any(duplicated(out_time$EmployeeID))
any(duplicated(manager_survey$EmployeeID))
any(duplicated(gen_data$EmployeeID))
#False so there are no duplicate EmployeeIDs

###Missing Value imputations
sapply (gen_data, function (x) sum(is.na(x)))
# 19 NA values in the NumCompaniesWorked . We keep it as 1 as thats the deafult value
# 9 NA values in TotalWorkingYears.We keep equal to YearsAtCompany
gen_data$NumCompaniesWorked[which(is.na(gen_data$NumCompaniesWorked))] <- 1 
gen_data$TotalWorkingYears[which(is.na(gen_data$TotalWorkingYears))] <- gen_data$YearsAtCompany[which(is.na(gen_data$TotalWorkingYears))]
sapply (emp_survey, function (x) sum(is.na(x)))
#25 NA values in EnvironmentSatisfaction. Assign the most common rating 3 to it
#summary(as.factor(emp_survey$EnvironmentSatisfaction))
#20 NA values in JobSatisfaction. Assign the most common rating 4 to it
#summary(as.factor(emp_survey$JobSatisfaction))
#38 NA values in WorkLifeBalance Assign the most common rating 3 to it
#summary(as.factor(emp_survey$WorkLifeBalance))
emp_survey$EnvironmentSatisfaction[which(is.na(emp_survey$EnvironmentSatisfaction))] <- 3
emp_survey$JobSatisfaction[which(is.na(emp_survey$JobSatisfaction))] <- 4
emp_survey$WorkLifeBalance[which(is.na(emp_survey$WorkLifeBalance))] <- 3
sapply (manager_survey, function (x) sum(is.na(x)))
# No NA values in manager_survey
#There are columns in in_time that have all 4410 rows as NA. We can remove them  
# Removing 12 columns that are completely NA in in_time and out_time
in_time <- in_time[,sapply(in_time, function(x) all(is.na(x)) == F)]
out_time <- out_time[,sapply(out_time, function(x) all(is.na(x)) == F)]
###Derived Metric;Counting the number of NAs in "in" and "out" time as 2 new columns in_na_count and out_na_count
in_na_count <- vector(mode="integer",length = 4410)
out_na_count <- vector(mode="integer",length = 4410)

for (j in 1:nrow(in_time)) {in_na_count[j] <- sum(is.na(in_time[j, ]))}
for (j in 1:nrow(out_time)) {out_na_count[j] <- sum(is.na(out_time[j, ]))}
all((in_na_count-out_na_count)==0)
#The in_na_count=out_na_count; we can have one column as leaves_taken
leaves_taken <- vector(mode="integer",length = 4410)
leaves_taken <- in_na_count

#converting to time format and deriving the mean working hours
in_time <- sapply(in_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
in_time<-as.data.frame(in_time)

out_time <- sapply(out_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
out_time<-as.data.frame(out_time)

#removing first column 
in_time <- in_time [ ,-1]
out_time <- out_time [ ,-1]

#calculating the time spent
time_spent<-out_time-in_time

#remove columns with all NA values
time_spent <- time_spent[,colSums(is.na(time_spent))<nrow(time_spent)]

#converting all values to numeric
time_spent<-sapply(time_spent,function(x) as.numeric(x))
time_spent<-as.data.frame(time_spent)

str(time_spent)

#new vector for storing employee id to join with avg work hours
EmployeeID<-seq(from = 1, to = 4410, by = 1)

#aggregating mean of each row #roll up
time_spent$mean_timeatwork<-apply(time_spent,1,mean,na.rm=TRUE)

#creating mean work hours per employeee data frame
mean_timeatwork<-cbind(EmployeeID,time_spent$mean_timeatwork)
mean_timeatwork<-as.data.frame(mean_timeatwork)
colnames(mean_timeatwork)[2] <- "mean_timeatwork"

#creating leave taken per employeee data frame
leaves_taken <-cbind(EmployeeID,leaves_taken)
leaves_taken<-as.data.frame(leaves_taken)
colnames(leaves_taken)[2] <- "leaves_taken"

### Combining all the databases into masterframe
master_frame <- merge(gen_data,mean_timeatwork,by="EmployeeID")
master_frame <- merge(master_frame,leaves_taken,by="EmployeeID")
master_frame <- merge(master_frame,manager_survey,by="EmployeeID")
master_frame <- merge(master_frame,emp_survey,by="EmployeeID")

### Redundant column removal
#The column Over18 has all Yes 
master_frame$Over18 <- NULL  
#The column StandardHours are all 8 
master_frame$StandardHours <- NULL
#The column EmployeeCount is always 1 
master_frame$EmployeeCount <- NULL
# The columns EmployeeID is redundant for modelling
master_frame$EmployeeID <- NULL
####Convert the Attrition ; target variable to 0 and 1
master_frame$Attrition<- ifelse(master_frame$Attrition =="Yes",1,0)

###Writing out master_frame
write.csv(master_frame,file="master_frame.csv")
###EDA for master_frame
###Studying the master_frame
###Considering the following variables as numeric, Eg levels in Education have an order so using the number as is
#Age,DistanceFromHome,Education,MonthlyIncome,NumCompaniesWorked,PercentSalaryHike,TotalWorkingYears,TrainingTimesLastYear,YearsAtCompany,YearsSinceLastPromotion,YearsWithCurrManager,mean_timeatwork,JobInvolvement,PerformanceRating,EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,leaves_taken
#18 variables
###Considering the following variables as categorical
#BusinessTravel,Department,EducationField,Gender,JobLevel,JobRole,MaritalStatus,StockOptionLevel
#8 variables
###Converting to factors
master_frame$Education <- as.factor(master_frame$Education)
master_frame$JobLevel <- as.factor(master_frame$JobLevel)
master_frame$StockOptionLevel <- as.factor(master_frame$StockOptionLevel)
master_frame$JobInvolvement <- as.factor(master_frame$JobInvolvement)
master_frame$PerformanceRating <- as.factor(master_frame$PerformanceRating)
master_frame$EnvironmentSatisfaction <- as.factor(master_frame$EnvironmentSatisfaction)
master_frame$JobSatisfaction <- as.factor(master_frame$JobSatisfaction)
master_frame$WorkLifeBalance <- as.factor(master_frame$WorkLifeBalance)
##### Bar charts for categorical variables 
#master_frame <- read.csv("master_frame.csv",stringsAsFactors = F)
#install.packages("lazyeval")
#install.packages("labeling")
require(cowplot)

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

ggplot(master_frame, aes(x=BusinessTravel,fill=as.factor(Attrition)))+ geom_bar()
          ggplot(master_frame, aes(x=Department,fill=as.factor(Attrition)))+ geom_bar()+bar_theme1
          ggplot(master_frame, aes(x=EducationField,fill=as.factor(Attrition)))+ geom_bar()+bar_theme1
          ggplot(master_frame, aes(x=Gender,fill=as.factor(Attrition)))+ geom_bar()+bar_theme1
          ggplot(master_frame, aes(x=JobLevel,fill=as.factor(Attrition)))+ geom_bar()+bar_theme1
          ggplot(master_frame, aes(x=MaritalStatus,fill=as.factor(Attrition)))+ geom_bar()+bar_theme1
          ggplot(master_frame, aes(x=StockOptionLevel,fill=as.factor(Attrition)))+ geom_bar()+bar_theme1
          ggplot(master_frame, aes(x=JobRole,fill=as.factor(Attrition)))+ geom_bar()+bar_theme1 


ggplot(master_frame, aes(x=BusinessTravel,fill=as.factor(Attrition)))+ geom_bar(position = "fill")
          ggplot(master_frame, aes(x=Department,fill=as.factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1
          ggplot(master_frame, aes(x=EducationField,fill=as.factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1
          ggplot(master_frame, aes(x=Gender,fill=as.factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1
          ggplot(master_frame, aes(x=JobLevel,fill=as.factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1
          ggplot(master_frame, aes(x=MaritalStatus,fill=as.factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1
          ggplot(master_frame, aes(x=StockOptionLevel,fill=as.factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1
          ggplot(master_frame, aes(x=JobRole,fill=as.factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1   



# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")


# Boxplots of numeric variables 
master_frame$Attrition <- as.factor(master_frame$Attrition)
plot_grid(ggplot(master_frame, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master_frame, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_frame, aes(x=Attrition,y=Education, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_frame, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
plot_grid(ggplot(master_frame, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master_frame, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_frame, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_frame, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(master_frame, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master_frame, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_frame, aes(x=Attrition,y=mean_timeatwork, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_frame, aes(x=Attrition,y=JobInvolvement, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
plot_grid(ggplot(master_frame, aes(x=Attrition,y=PerformanceRating, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master_frame, aes(x=Attrition,y=EnvironmentSatisfaction, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_frame, aes(x=Attrition,y=JobSatisfaction, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_frame, aes(x=Attrition,y=WorkLifeBalance, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
ggplot(master_frame, aes(x=Attrition,y=leaves_taken, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none")
# Correlation between numeric variables
library(GGally)
ggpairs(master_frame[, c("Age","DistanceFromHome","Education","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","mean_timeatwork","JobInvolvement","PerformanceRating","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","leaves_taken")])


###Outlier Removal for all Numeric variables
#Age,DistanceFromHome,Education,MonthlyIncome,NumCompaniesWorked,PercentSalaryHike,TotalWorkingYears,TrainingTimesLastYear,YearsAtCompany,YearsSinceLastPromotion,YearsWithCurrManager,mean_timeatwork,JobInvolvement,PerformanceRating,EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,in_na_count,out_na_count
quantile(master_frame$Age,seq(0,1,0.01))
quantile(master_frame$DistanceFromHome,seq(0,1,0.01))
quantile(master_frame$MonthlyIncome,seq(0,1,0.01))
###income has a large spread; not removing any outlier
quantile(master_frame$NumCompaniesWorked,seq(0,1,0.01))
quantile(master_frame$PercentSalaryHike,seq(0,1,0.01))
###not removing any outliers in PercentSalaryHike
quantile(master_frame$TotalWorkingYears,seq(0,1,0.01))
quantile(master_frame$TrainingTimesLastYear,seq(0,1,0.01))
quantile(master_frame$YearsAtCompany,seq(0,1,0.01))
quantile(master_frame$YearsSinceLastPromotion,seq(0,1,0.01))
quantile(master_frame$YearsWithCurrManager,seq(0,1,0.01))
quantile(master_frame$mean_timeatwork,seq(0,1,0.01))
quantile(master_frame$leaves_taken,seq(0,1,0.01))
###Converting to numeric
master_frame$Education <- as.numeric(master_frame$Education)
master_frame$JobInvolvement <- as.numeric(master_frame$JobInvolvement)
master_frame$PerformanceRating <- as.numeric(master_frame$PerformanceRating)
master_frame$EnvironmentSatisfaction <- as.numeric(master_frame$EnvironmentSatisfaction)
master_frame$JobSatisfaction <- as.numeric(master_frame$JobSatisfaction)
master_frame$WorkLifeBalance <- as.numeric(master_frame$WorkLifeBalance)
#### Scaling all numeric variables
master_frame$Age <- scale(master_frame$Age)
master_frame$DistanceFromHome <- scale(master_frame$DistanceFromHome)
master_frame$Education <- scale(master_frame$Education)
master_frame$MonthlyIncome <- scale(master_frame$MonthlyIncome)
master_frame$NumCompaniesWorked <- scale(master_frame$NumCompaniesWorked)
master_frame$PercentSalaryHike <- scale(master_frame$PercentSalaryHike)
master_frame$TotalWorkingYears<- scale(master_frame$TotalWorkingYears)
master_frame$TrainingTimesLastYear <- scale(master_frame$TrainingTimesLastYear)
master_frame$YearsAtCompany <- scale(master_frame$YearsAtCompany)
master_frame$YearsSinceLastPromotion <- scale(master_frame$YearsSinceLastPromotion)
master_frame$YearsWithCurrManager <- scale(master_frame$YearsWithCurrManager)
master_frame$mean_timeatwork <- scale(master_frame$mean_timeatwork)
master_frame$JobInvolvement <- scale(master_frame$JobInvolvement)
master_frame$PerformanceRating <- scale(master_frame$PerformanceRating)
master_frame$EnvironmentSatisfaction <- scale(master_frame$EnvironmentSatisfaction)
master_frame$JobSatisfaction <- scale(master_frame$JobSatisfaction)
master_frame$WorkLifeBalance <- scale(master_frame$WorkLifeBalance)
master_frame$leaves_taken <- scale(master_frame$leaves_taken)
#### Dummy variables for all categoricals
dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = master_frame))
dummy_1 <- dummy_1[,-1]
master_frame <-cbind(master_frame[ ,-3],dummy_1)
dummy_1 <- data.frame(model.matrix( ~Department, data = master_frame))
dummy_1 <- dummy_1[,-1]
master_frame <-cbind(master_frame[ ,-3],dummy_1)
dummy_1 <- data.frame(model.matrix( ~EducationField, data = master_frame))
dummy_1 <- dummy_1[,-1]
master_frame <-cbind(master_frame[ ,-5],dummy_1)
dummy_1 <- data.frame(model.matrix( ~Gender, data = master_frame))
dummy_1 <- dummy_1[,-1]
master_frame <-cbind(master_frame[ ,-5],dummy_1)
dummy_1 <- data.frame(model.matrix( ~JobLevel, data = master_frame))
dummy_1 <- dummy_1[,-1]
master_frame <-cbind(master_frame[ ,-5],dummy_1)
dummy_1 <- data.frame(model.matrix( ~JobRole, data = master_frame))
dummy_1 <- dummy_1[,-1]
master_frame <-cbind(master_frame[ ,-5],dummy_1)
dummy_1 <- data.frame(model.matrix( ~MaritalStatus, data = master_frame))
dummy_1 <- dummy_1[,-1]
master_frame <-cbind(master_frame[ ,-5],dummy_1)
dummy_1 <- data.frame(model.matrix( ~StockOptionLevel, data = master_frame))
dummy_1 <- dummy_1[,-1]
master_frame <-cbind(master_frame[ ,-8],dummy_1)
write.csv(master_frame,file="master_frame_final.csv")
####Final dataset master_frame
#install.packages("caTools")
library(caTools)
set.seed(100)

indices = sample.split(master_frame$Attrition, SplitRatio = 0.7)

train = master_frame[indices,]

test = master_frame[!(indices),]
########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition~., data = train, family = "binomial")
summary(model_1) #AIC: 2165.2
# Stepwise selection
#library("MASS")
model_2<- stepAIC(model_1, direction="both")
##AIC: 2144.6
summary(model_2)
##### Removing multicollinearity through VIF check
#install.packages("Rcpp")
#library("car")
vif(model_2)
##Removing YearsAtCompany vif =4.829 p=0.053525
model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + Education + 
      MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
      TrainingTimesLastYear + YearsSinceLastPromotion + 
      YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
      JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
      BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
      DepartmentSales + EducationFieldMarketing + EducationFieldOther + 
      EducationFieldTechnical.Degree + JobLevel2 + JobRoleLaboratory.Technician + 
      JobRoleManufacturing.Director + JobRoleResearch.Director + 
      JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusMarried + 
      MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
    data = train)
summary(model_3)
##AIC : 2146.3
vif(model_3)
##Removing MaritalStatusMarried vif=2.169 p=0.142321
model_4 <- glm(formula = Attrition ~ Age + DistanceFromHome + Education + 
                 MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldMarketing + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleLaboratory.Technician + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)
summary(model_4)
##AIC : 2146.5
vif(model_4)
###Removing the less significant variables JobRoleLaboratory.Technician  
model_5 <- glm(formula = Attrition ~ Age + DistanceFromHome + Education + 
                 MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldMarketing + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)
summary(model_5)
##AIC : 2146.5
vif(model_5)
###Removing the less significant variables MonthlyIncome
model_6 <- glm(formula = Attrition ~ Age + DistanceFromHome + Education + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldMarketing + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)
summary(model_6)
##AIC : 2146.5
vif(model_6)
###Removing the less significant variables DistanceFromHome
model_7 <- glm(formula = Attrition ~ Age + Education + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldMarketing + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)
summary(model_7)
##AIC : 2146.5
vif(model_7)
###Removing the less significant variables JobRoleResearch.Scientist
model_8 <- glm(formula = Attrition ~ Age + Education + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldMarketing + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 + JobRoleSales.Executive + MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)
summary(model_8)
##AIC : 2146.8
vif(model_8)
###Removing the less significant variables EducationFieldMarketing
model_9 <- glm(formula = Attrition ~ Age + Education + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 + JobRoleSales.Executive + MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)
summary(model_9)
##AIC : 2147.2
vif(model_9)
###Removing the less significant variables EducationFieldTechnical.Degree
model_10 <- glm(formula = Attrition ~ Age + Education + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldOther + JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 + JobRoleSales.Executive + MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)
summary(model_10)
##AIC : 2147.8 
vif(model_10)
###Removing the less significant variables StockOptionLevel1
model_11 <- glm(formula = Attrition ~ Age + Education + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + EducationFieldOther + JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  + JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
                data = train)
summary(model_11)
##AIC : 2148.4
vif(model_11)
###Removing the less significant variables EducationFieldOther
model_12 <- glm(formula = Attrition ~ Age + Education + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  + JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
                data = train)
summary(model_12)
##AIC : 2149.8
vif(model_12)
###Removing the less significant variables JobLevel2
model_13 <- glm(formula = Attrition ~ Age + Education + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  + JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
                data = train)
summary(model_13)
##AIC : 2153.4
vif(model_13)
###Removing the less significant variables Education
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  + JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
                data = train)
summary(model_14)
##AIC : 2157.6
vif(model_14)
###Removing the less significant variables JobRoleResearch.Director
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleManufacturing.Director + JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
                data = train)
summary(model_15)
##AIC : 2161.6
vif(model_15)
###Removing the less significant variables JobRoleSales.Executive 
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleManufacturing.Director + MaritalStatusSingle, family = "binomial", 
                data = train)
summary(model_16)
##AIC : 2164.5
vif(model_16)
###Removing the less significant variables  WorkLifeBalance
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + mean_timeatwork + EnvironmentSatisfaction + 
                  JobSatisfaction + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleManufacturing.Director + MaritalStatusSingle, family = "binomial", 
                data = train)
summary(model_17)
##AIC : 2172.7
vif(model_17)
###This model has all significant variables. It has 15 variables
final_model <- model_17
#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)
                        #test_pred_attrition
#test_actual_attrition   No  Yes
                  #No  1088   22
                  #Yes  162   51
##The 162 predicted 'No' that are actually 'Yes' needs to be reduced
##Model cutoff for Yes needs to be lowered
#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Reference
#Prediction   No  Yes
#No  1056  144
#Yes   54   69
####sensitivity is 0.323; very low
##Cutoff needs to be lowered
#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
####sensitivity is 0.47418; still low
#########################################################################################

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,2]-OUT[,3])<0.005)]
cutoff
#0.169596
# Let's choose a cutoff value of 0.169596 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.169596, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#0.7430083 
sens
#0.7323944
spec
#0.745045
View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
###0.4774394
##the K statistic is 47.74% 
####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#  1      1   133        72      72  33.8    3.38
#  2      2   132        46     118  55.4    2.77
#  3      3   132        26     144  67.6    2.25
#  4      4   133        22     166  77.9    1.95
#  5      5   132        13     179  84.0    1.68
#  6      6   132        14     193  90.6    1.51
#  7      7   133         7     200  93.9    1.34
#  8      8   132         3     203  95.3    1.19
#  9      9   132         7     210  98.6    1.10
#  10     10   132         3     213 100      1   

ggplot(attrition_decile,aes(x=bucket,y=Gain)) + geom_line()
##by the 4th decile, 77.9% of all attrition will be detected
ggplot(attrition_decile,aes(x=bucket,y=Cumlift)) + geom_line()
###by the 3rd decile,lift = 2.25, it catches the attrition 2.25 times more than random model