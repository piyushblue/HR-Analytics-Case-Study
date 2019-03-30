library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(devtools)
library(GGally)
library(ROCR)


intime <-read.csv("in_time.csv", stringsAsFactors = FALSE) #4410 Observations of 262 Variables
outtime <- read.csv ("out_time.csv", stringsAsFactors = FALSE) #4410 Observations of 262 Variables
generaldata <- read.csv("general_data.csv", stringsAsFactors = FALSE) #4410 Observations of 24 Variables
emp_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE) #4410 Observations of 4 Variables
mngr_survey <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE) #4410 Observations of 3 Variables

str(generaldata) # looks good with emp_id as int across all files to start with
str(emp_survey) # looks good with emp_id as int across all files to start with
str(mngr_survey) # looks good with emp_id as int across all files to start with

setdiff(generaldata$EmployeeID,emp_survey$EmployeeID) #matching emp_id for merging
setdiff(generaldata$EmployeeID,mngr_survey$EmployeeID) #matching emp_id for merging
HR_data <- merge(generaldata,emp_survey, by = "EmployeeID", all=F)
HR_data <- merge(HR_data, mngr_survey, by = "EmployeeID", all=F)
View(HR_data)

setdiff(intime$X, outtime$X) #matching emp_id for time difference

# Convert intime, outtime into date format
for(i in 2:ncol(intime)){intime[,i]<- as.POSIXct(intime[,i], format = "%Y-%m-%d %H:%M",na.rm=TRUE)}

str(intime)

for(i in 2:ncol(outtime)){outtime[,i]<- as.POSIXct(outtime[,i], format = "%Y-%m-%d %H:%M",na.rm=TRUE)}

str(outtime)
#outtime - intime
workinghours <- outtime[,-1] - intime[,-1]

View(workinghours)
ncol(workinghours)

#Calculate Average working hours for each row(employee)
for(i in 1:nrow(workinghours)){workinghours[i,262] <- mean(as.numeric(workinghours[i,]), na.rm=TRUE)}
colnames(workinghours)[colnames(workinghours) == 'V262'] <- 'Avg_Hours' # Renaming average column

workinghours <- cbind(workinghours,outtime$X) #coercing Employee ID
colnames(workinghours)[colnames(workinghours) == 'outtime$X'] <- 'EmployeeID' # Renaming column
Avg_hours <- workinghours[,c("EmployeeID","Avg_Hours")]
View(Avg_hours)
#Merging
setdiff(HR_data$EmployeeID,Avg_hours$EmployeeID)
HR_data <- merge(HR_data,Avg_hours, by = "EmployeeID")
View(HR_data)
str(HR_data)
summary(HR_data)

#Check Missing Values
sum(is.na(HR_data))
which(is.na(HR_data))
HR<-na.omit(HR_data)
nrow(HR) # 4300 records
nrow(HR_data) # about 3% of data is removed because of Presence of NA
#So working file nowonwards is "HR"
str(HR)
summary(HR)
write.csv(HR, "HR.csv")
######################## Exploratory Data Analysis 
#-----------------------------------------------------
#Count of Attrition vs non Attrition
ggplot(HR, aes(x=Attrition, fill=Attrition))+ geom_bar()
# About 600 employees have left out of 4300
#-----------------------------------------------------

#Visual Analysis of Various Charateristics and Attrition(count wise)

bar_theme<- theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

plot_grid(ggplot(HR, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme, 
          ggplot(HR, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(HR, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme,
          align = "h")

plot_grid(ggplot(HR, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(HR, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(HR, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme,
          align = "h")

plot_grid(ggplot(HR, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(HR, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(HR, aes(x=NumCompaniesWorked,fill=Attrition))+geom_bar()+bar_theme,
          align = "h") 

#As Observed from the graphs, 
#1.Attrition is Research & Development Department is more (but proportionately it is comparable - As R& D has more people)
#2.Junior level people frequently leave the company as compared to middle and senior levels, 
#3.Bachelor & Master Level people also show high attrition rate comparatively,
#4.Clearly Single people have high attrition rate as compared to Married and Divorced, even MArried shows high attrition as compared to Divorced,
#5.Travel attrtions are proportional - we will get to see more insights in proportional graphs as below)
#6.Gender seems to have no impact on attrition
#7.Majority of the people are in the role of Laboratory Technician, Research Scientist, Sales Executives 
#  followed by HEarth care rep & Manufacturing Dir - followed by others

#-----------------------------------------------------

#Visual Analysis of Various Charateristics and Attrition (% wise)
plot_grid(ggplot(HR, aes(x=Department,fill=Attrition))+ geom_bar(position ="fill")+bar_theme, 
          ggplot(HR, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar(position ="fill")+bar_theme,
          ggplot(HR, aes(x=factor(Education),fill=Attrition))+ geom_bar(position ="fill")+bar_theme,
          align = "h")
          
plot_grid(ggplot(HR, aes(x=Gender,fill=Attrition))+ geom_bar(position ="fill")+bar_theme,
          ggplot(HR, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position ="fill")+bar_theme,
          ggplot(HR, aes(x=JobRole,fill=Attrition))+ geom_bar(position ="fill")+bar_theme,
          align = "h")
          
plot_grid(ggplot(HR, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position ="fill")+bar_theme,
          ggplot(HR, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar(position ="fill")+bar_theme,
          ggplot(HR, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar(position ="fill")+bar_theme,
          align = "h")
#Looking at this graphs gives completely different insights. 
#1. As we see, HR department shows high attrition rate,
#2.Attrition across all the job levels almost same, except lesser at senior most postions
#3. Gender has no impact on Attrition
#4. Single people clearly leave the company
#5. Those who travel frequently - shows higher pertage of Attrition
#6. No impact of job role on attrition
#-----------------------------------------------------

str(HR)
# Employee & Manager Survey Analysis (count wise)
plot_grid(ggplot(HR, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(), 
          ggplot(HR, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar(),
          ggplot(HR, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar(),
          ggplot(HR, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar(),
          ggplot(HR, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(),
          align = "h") 
#1. More employees are satisfied with Job, 
#   workculture(Environment) and Worklife balance - suggests that company takes care of employee well.
#2. Higher count shows job involvement and Earlier conclusion of healthy work situations Higher count shows job involvement could be the reasion.
#3. Most people falls under Performace rating 3 as compared to 4, but none of the employees are in lower rating. 
#   Though Not sure if there is no lower rating or no employee falling under that category.
#4. Attrition seems quite proportionate as per count in various category, though we will get more insights in %wise analysis.

#-----------------------------------------------------

# Employee & Manager Survey Analysis (% wise)
plot_grid(ggplot(HR, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(position ="fill"), 
          ggplot(HR, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar(position ="fill"),
          ggplot(HR, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar(position ="fill"),
          ggplot(HR, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar(position ="fill"),
          ggplot(HR, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(position ="fill"),
          align = "h")

#-----------------------------------------------------

#Average working hours of employee to check the motivation level and visualising attrition pattern
plot_grid(ggplot(HR, aes(HR$Avg_Hours,fill=Attrition))+ geom_histogram(),
          ggplot(HR, aes(HR$Avg_Hours,fill=Attrition))+ geom_histogram(position ="fill"),
          align = "h")
#1. Most people work in the range of 6 to 8 hours.
#2. People doing overtime are in the range of 8 to 11 hours
#3. People doing overtime form greater % of those leaving leaving organisation
#-----------------------------------------------------

# Salary analysis among those who left and not left

plot_grid(ggplot(HR, aes(x=Attrition,y=HR$MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none"),
ggplot(HR, aes(HR$MonthlyIncome,fill=Attrition))+ geom_histogram(),
ggplot(HR, aes(HR$MonthlyIncome,fill=Attrition))+ geom_histogram(position="fill"),align = "h")
# 1. Most people in the range of 20000 - 70000.
# 2. People somewhere at salary 160K/170K have highest % of attrition. May be good performer and better negotiator in market to leave.

#-----------------------------------------------------

# Distance from office analysis among those who left and not left

plot_grid(ggplot(HR, aes(x=Attrition,y=HR$DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none"),
ggplot(HR, aes(HR$DistanceFromHome,fill=Attrition))+ geom_histogram(),align = "h")
#Most people stay close by, but that doesn't seem to be contributing factor for attrition.

#-----------------------------------------------------
# Work Experience and Age analysis to find out attrition pattern

plot_grid(ggplot(HR, aes(x=factor(HR$TotalWorkingYears),fill=Attrition))+ geom_bar(),
ggplot(HR, aes(x=factor(HR$TotalWorkingYears),fill=Attrition))+ geom_bar(position="fill"),align = "h")
#1.There is a spike of people leaving after 1 year, 6 year and 10 years. These are the people to watch out.
#2.We see sharp rise(100%) at Work exp=40. That may be because of Retirement.

##
plot_grid(ggplot(HR, aes(x=factor(HR$Age),fill=Attrition))+ geom_bar(),
ggplot(HR, aes(x=factor(HR$Age),fill=Attrition))+ geom_bar(position="fill"),align = "h")
# 1.After the age of 36-37 years people become relatively stable. Reason could be people are busy stablising their family life, kids education, settling housing and and other loans.
# 2.Though at there is spike after almost every 3 years after that also.
# 3.Intrestingly, there is sudden spike in the last few years as they may gained very valuable experince and other companies might offer them significant postions and benefits.

#-----------------------------------------------------
#Stock Options/salary Hike/YEars in company/Year under current Manager/Year since last promotion- To study as motivation to stay or leave
plot_grid(ggplot(HR, aes(x=factor(HR$StockOptionLevel),fill=Attrition))+ geom_bar(),
ggplot(HR, aes(x=factor(HR$StockOptionLevel),fill=Attrition))+ geom_bar(position="fill"),align = "h")
# %wise attrition looks same. May be employee does not care about maturity of stock option and that's not the motivation to stay or leave.

plot_grid(ggplot(HR, aes(x=factor(HR$PercentSalaryHike),fill=Attrition))+ geom_bar(),
ggplot(HR, aes(x=factor(HR$PercentSalaryHike),fill=Attrition))+ geom_bar(position="fill"),align = "h")
# 1.Most people fall in category of 11 to 14% salary hike. Then count gradually decrease. 
# 2.Although Attrition seems to be prroportional across all the salary hike%, with maximum being at 25% hike. 
#   May be these employees are exceptional performer and can better negotiate their role/Salary in the market and leave.

plot_grid(ggplot(HR, aes(x=factor(HR$YearsAtCompany),fill=Attrition))+ geom_bar(),
ggplot(HR, aes(x=factor(HR$YearsAtCompany),fill=Attrition))+ geom_bar(position="fill"),align = "h")

# 1. Most people in the current company are in range of 0-10 years.
# 2. Most people leave either within 2 year or 5 or 10 years.

plot_grid(ggplot(HR, aes(x=factor(HR$YearsWithCurrManager),fill=Attrition))+ geom_bar(),
ggplot(HR, aes(x=factor(HR$YearsWithCurrManager),fill=Attrition))+ geom_bar(position="fill"),align = "h")
# Does not give any significant insight. This result is quite proportional or previous result
# We also do not know how frequently manager changes and based on what criteria/reason.

plot_grid(ggplot(HR, aes(x=factor(HR$YearsSinceLastPromotion),fill=Attrition))+ geom_bar(),
ggplot(HR, aes(x=factor(HR$YearsSinceLastPromotion),fill=Attrition))+ geom_bar(position="fill"),align = "h")
# 1.People eigther leave quite immidiately after promotion or 
# 2. Or after 6/7 years(may be they waited long enough for promotion but not give and this coulsd be reason for unsatisfaction)


#-----------------------------------------------------
# Correlation between variables
str(HR)
ggpairs(HR[, c("Age", "JobLevel", "PercentSalaryHike","TotalWorkingYears","YearsSinceLastPromotion","JobSatisfaction","PerformanceRating","Avg_Hours")])
# From this matrix, Total working years and age show high coorelation of .681(which is obvious - as the person age, work exp will accumulate)

ggpairs(HR[, c("PercentSalaryHike","JobInvolvement","PerformanceRating","TotalWorkingYears","MonthlyIncome")])
# High correlation between Performance Rating and % Salary Hike - .774

ggpairs(HR[, c("PercentSalaryHike","Education","MonthlyIncome")])
# No corelation

##############################################################

####################Data preparation########################


#1. Correction for Missing Value was already done previously so not required.

#2. Unwanted Columns
# We do not need these columns
#   - Standard Hours - as they are 8 for everyone
#   - Over 18 years - as everyone is above 18years
#   - Employee COunt - it's 1 for every employee id
str(HR)
HR_prep <- HR[,-16]
HR_prep <- HR_prep[,-17]
HR_prep <- HR_prep[,-9]
str(HR_prep) #both the columns deleted

#3.check for blanks
sapply(HR_prep, function(x) length(which(x == "")))
#no blanks

#4.duplicate?
sum(duplicated(HR$EmployeeID)) 

#5. Scaling the data
HR_prep$Age <- scale(HR_prep$Age,center = TRUE, scale = TRUE)
HR_prep$DistanceFromHome <- scale(HR_prep$DistanceFromHome,center = TRUE, scale = TRUE)
HR_prep$Education <- scale(HR_prep$Education,center = TRUE, scale = TRUE)
HR_prep$JobLevel <- scale(HR_prep$JobLevel,center = TRUE, scale = TRUE)
HR_prep$Education <- scale(HR_prep$Education,center = TRUE, scale = TRUE)

ncol(HR_prep)
HR_prep[,14:27] <- sapply(HR_prep[,14:27], function(x) scale(x,center = TRUE, scale = TRUE))
View(HR_prep)
str(HR_prep)

#6. Convert Chr with 2 categories to Numeric 
HR_prep$Attrition <- ifelse(HR_prep$Attrition =="Yes",1,0)
HR_prep$Gender <- ifelse(HR_prep$Gender =="Male",1,0)

#7. Convert Chr to Factors BusinessTravel, Department, EducationField,JobRole, MaritalStatus 
HR_prep$BusinessTravel <- as.factor(HR_prep$BusinessTravel)
HR_prep$Department <- as.factor(HR_prep$Department)
HR_prep$EducationField <- as.factor(HR_prep$EducationField)
HR_prep$JobRole <- as.factor(HR_prep$JobRole)
HR_prep$MaritalStatus <- as.factor(HR_prep$MaritalStatus)
str(HR_prep)
# Now are variables are either numeric or factor.

#----------------Dummy Variables--------

#install.packages("dummies")
library(dummies)
#Conversion of factor to variables to dummy variables
HR_prep <- dummy.data.frame(HR_prep)
str(HR_prep)
View(HR_prep)
HR_prep <- HR_prep[,-1] #dropping employee ID 
summary(HR_prep)
#45 Variables in total

#################### Modelling ##################

#----------------Data split in train and test--------

#splitting the data between train and test
set.seed(100)

indices = sample.split(HR_prep$Attrition, SplitRatio = 0.7)
View(indices) 
HR_train = HR_prep[indices,]
View(HR_train)
HR_test = HR_prep[!(indices),]
View(HR_test)
nrow(HR_train)
nrow(HR_test)


##----------------Logistic regression--------

HRmodel_1 = glm(Attrition ~ ., data = HR_train, family = "binomial")
summary(HRmodel_1) # AIC = 2139.3

HRmodel_2<- stepAIC(HRmodel_1, direction="both")
summary(HRmodel_2) #AIC=2118.11

#Checking multicollinearity
vif(HRmodel_2)


#Though VIF of YEars since last promotion and YEar with current mngr is more than 2, but lets remove variable with high P value first
#Remove EducationFieldTechnicalDEgree
HRmodel_3 <- glm(formula = Attrition ~ Age + `BusinessTravelNon-Travel` + 
                   BusinessTravelTravel_Frequently + `EducationFieldHuman Resources` + 
                   JobLevel + `JobRoleHuman Resources` + JobRoleManager + `JobRoleManufacturing Director` + 
                   `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusDivorced + 
                   MaritalStatusMarried + NumCompaniesWorked + StockOptionLevel + 
                   TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                   WorkLifeBalance + Avg_Hours, 
                 family = "binomial", data = HR_train)

summary(HRmodel_3)
vif(HRmodel_3)

#Removing Stock option level
HRmodel_4 <- glm(formula = Attrition ~ Age + `BusinessTravelNon-Travel` + 
                   BusinessTravelTravel_Frequently + `EducationFieldHuman Resources` + 
                   JobLevel + `JobRoleHuman Resources` + JobRoleManager + `JobRoleManufacturing Director` + 
                   `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusDivorced + 
                   MaritalStatusMarried + NumCompaniesWorked + 
                   TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                   WorkLifeBalance + Avg_Hours, 
                 family = "binomial", data = HR_train)
summary(HRmodel_4)
vif(HRmodel_4)

#Removing JobRoleHuman Resources
HRmodel_5 <- glm(formula = Attrition ~ Age + `BusinessTravelNon-Travel` + 
                   BusinessTravelTravel_Frequently + `EducationFieldHuman Resources` + 
                   JobLevel + JobRoleManager + `JobRoleManufacturing Director` + 
                   `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusDivorced + 
                   MaritalStatusMarried + NumCompaniesWorked +  
                   TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                   WorkLifeBalance + Avg_Hours, 
                 family = "binomial", data = HR_train)
summary(HRmodel_5)

#Removing joblevel
HRmodel_6 <- glm(formula = Attrition ~ Age + `BusinessTravelNon-Travel` + 
                   BusinessTravelTravel_Frequently + `EducationFieldHuman Resources` + 
                   JobRoleManager + `JobRoleManufacturing Director` + 
                   `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusDivorced + 
                   MaritalStatusMarried + NumCompaniesWorked +  
                   TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                   WorkLifeBalance + Avg_Hours, 
                 family = "binomial", data = HR_train)
summary(HRmodel_6)

#remove jobrolemngr
HRmodel_7 <- glm(formula = Attrition ~ Age + `BusinessTravelNon-Travel` + 
                   BusinessTravelTravel_Frequently + `EducationFieldHuman Resources` + 
                   `JobRoleManufacturing Director` + 
                   `JobRoleResearch Director` + `JobRoleSales Executive` + MaritalStatusDivorced + 
                   MaritalStatusMarried + NumCompaniesWorked +  
                   TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                   WorkLifeBalance + Avg_Hours, 
                 family = "binomial", data = HR_train)
summary(HRmodel_7)

#Remove busines non travel
HRmodel_8 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Frequently + `EducationFieldHuman Resources` + 
                   `JobRoleManufacturing Director` + MaritalStatusDivorced + MaritalStatusMarried + 
                   NumCompaniesWorked +  TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                   WorkLifeBalance + Avg_Hours, 
                 family = "binomial", data = HR_train)
summary(HRmodel_8)
vif(HRmodel_8)
#"TotalWorkingYears"has the highest VIF value and seems to have a correlation with "Yearswithcurrmanager", "YearsSinceLastPromotion" & "Age"
#but they all have significantly low p values hence we will proceed with this as our final model.


#############Model Evaluation#######
View(HR_test)
HR_predict <- predict(HRmodel_8, type = 'response', newdata = HR_test[,-2])
summary(HR_predict)
View(HR_predict)

HR_test$Attrition_predict <- HR_predict
View(HR_test)

# keeping probability cutoff of 50%.
test_pred_attrition.5 <- factor(ifelse(HR_predict >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(HR_test$Attrition ==1,"Yes","No"))

HRtest_accuracy.5 <- confusionMatrix(test_pred_attrition.5, test_actual_attrition, positive = "Yes")
HRtest_accuracy.5
#Confusion Matrix
#            Reference
#Prediction   No  Yes
#No         1043  149
#Yes          38   60
#Accurancy 85.5%
#Sensitivity : 0.28708- quite low sensitivity of model      
#Specificity : 0.96485 - which is already a good value as we are intesrested in those who may leave company


#Trying with 40% probability cutoff
test_pred_attrition.4 <- factor(ifelse(HR_predict >= 0.40, "Yes", "No"))
HRtest_accuracy.4 <- confusionMatrix(test_pred_attrition.4, test_actual_attrition, positive = "Yes")
HRtest_accuracy.4
#Accurancy 85.58% (Improved from .5)
#Sensitivity : 0.39713- (low sensitivity but improved from .5)     
#Specificity : 0.94450 -(Good but decreased by 2% from cutoff of .5)

# finding optimum value of probability 


# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(HR_predict >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.80,length=100)
s
OUT = matrix(0,100,3)
OUT
for(i in 1:100){OUT[i,] = perform_fn(s[i])} 
OUT

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,round(seq(0,1,length=10),digit=1),round(seq(0,1,length=10),digit=1),cex.lab=1.5)
axis(2,round(seq(0,1,length=10),digit=1),round(seq(0,1,length=10),digit=1),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff # 0.1855556 - point where graph meets

#Choosing cutoff value = .1855
test_cutoff_attrition <- factor(ifelse(HR_predict >=0.1855, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final
#Confusion Matrix
#            Reference
#Prediction  No   Yes
#No          825  51
#Yes         256  158

accuracy <- conf_final$overall[1]    #.762
senstivity <- conf_final$byClass[1]  #0.7560
specificity <- conf_final$byClass[2] #0.7632

##################################################################################################
### KS -statistic - Test Data ######

cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

library(ROCR)
#on testing  data
pred_attrition<- prediction(cutoff_attrition, actual_attrition)
performance_measures_test<- performance(pred_attrition, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
ks_table_test
max(ks_table_test) #.5191 which is more than .4 ->suggests that model is good and usable

#########################################
#Gain and Lift chart

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

Attrition_decile <- lift(actual_attrition, cutoff_attrition, groups = 10)
Attrition_decile

# A tibble: 10 x 6
#bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>         <dbl>  <dbl> <dbl>   <dbl>
#    1   129        49      49  23.4    2.34
#    2   129        43      92  44.0    2.20
#    3   129        53     145  69.4    2.31 ##### Optimum Decile #####
#    4   129        17     162  77.5    1.94
#    5   129         6     168  80.4    1.61
#    6   129         6     174  83.3    1.39
#    7   129         9     183  87.6    1.25
#    8   129         6     189  90.4    1.13
#    9   129         7     196  93.8    1.04
#   10   129        13     209 100      1   

#plot the lift chart 
plot(Attrition_decile$Cumlift, type="o", lwd=2, col="red",
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

#Plot Gain Chart 
plot((Attrition_decile$Gain)/100, type="o", col="red", lwd = 2,
     xlim = c(0,10),
     ylim = c(0,1),
     main = "Gain Chart",
     xlab = "Decile",
     ylab = "Gain %")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

#####################################################################