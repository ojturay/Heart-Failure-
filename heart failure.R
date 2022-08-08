library(tidyverse)
library(ggcorrplot)
library(caret)
library(ggthemes)
library(vip)
library(survminer)
library(survival)
library(gtsummary)
library(pROC)
library(AICcmodavg)

heartfailure <- read.csv("...\Desktop\heart failure project\heartfailure.cs")


###counting the total number of observations in the datatset 
count(heartfailure)

###finding the number of missing values in the datatset 
sum(is.na(heartfailure)) ##no missing values in the dataset 

##obtaining a data summary  and structure of the dataset 
str(heartfailure)
summary(heartfailure)


##it can be seen that some columns here like anaemia,
##sex, diabetes smoking can be viewed as categorical
##variables. those variables will be converted to factor
## and analysed using a barchart
##the other columns which are continuous in nature will be
##explored using a histogram and a boxplot

#Analysis of the the individual variables 

##Analysis of Age
ggplot(heartfailure, aes(x=age)) +
  geom_boxplot() + ggtitle('Distribution of Age') +
  theme_economist() 
#from the boxplot the median age is 60yrs 

#calculating the binwidth using the Freedman-Diaconis rule
ag <- hist(heartfailure$age, breaks='FD'); #use breaks to indicate classes
ggplot(heartfailure, aes(x=age)) + geom_vline(xintercept =65, linetype = 'dashed') +
  geom_histogram(breaks = ag$breaks) + theme_economist() +
  ggtitle('Distribution of Age')

ggplot(heartfailure, aes(x=age)) + geom_density() + 
  theme_economist() + ggtitle('Denisty Distribution of Age')


heartfailure$agegroup[heartfailure$age >= 40 & heartfailure$age <=45] <- '43'
heartfailure$agegroup[heartfailure$age >= 46 & heartfailure$age <=50] <- '48'
heartfailure$agegroup[heartfailure$age >= 51 & heartfailure$age <=55] <- '53'
heartfailure$agegroup[heartfailure$age >= 56 & heartfailure$age <=60] <- '58'
heartfailure$agegroup[heartfailure$age >= 61 & heartfailure$age <=65] <- '63'
heartfailure$agegroup[heartfailure$age >= 66 & heartfailure$age <=70] <- '68'
heartfailure$agegroup[heartfailure$age >= 71 & heartfailure$age <=75] <- '73'
heartfailure$agegroup[heartfailure$age >= 76 & heartfailure$age <=80] <- '78'
heartfailure$agegroup[heartfailure$age >= 81 & heartfailure$age <=85] <- '83'
heartfailure$agegroup[heartfailure$age >= 86 & heartfailure$age <=90] <- '88'
heartfailure$agegroup[heartfailure$age >= 91 & heartfailure$age <=95] <- '93'

heartfailure$agegroup = factor(heartfailure$agegroup, 
                               levels = c('43','48',
                                          '53','58',
                                          '63','68',
                                          '73','78',
                                          '83','88',
                                          '93'))
table(heartfailure$agegroup) #shows the distribution amongst the agegroups


#Analysis of Anaemia  
#creating a table to differentiate the patients with and without anaemia
# 0 = 'Non-Anaemic', 1 = 'Anaemic'
heartfailure$anaemic <- ifelse(heartfailure$anaemia == 0,'Non-Anaemic','Anaemic')
table(heartfailure$anaemic) # 170patients (57%) did not have anaemia and
# 129patients (43%) had anaemia

ggplot(heartfailure, aes(x=anaemic)) + 
  geom_bar() + theme_economist() + labs(x='Anaemic Status') +
  ggtitle('Distribution of Anaemic Status')

  
#Analysis of Creatinine_Phosphokinase
ggplot(heartfailure, aes(x=creatinine_phosphokinase)) +
  geom_boxplot() + theme_economist() +
  labs(x='Creatinine Phosphokinase') +
  ggtitle('Boxplot of Creatinine_Phosphokinase levels')
#boxplot shows that there are a lot of outliers and the data is heavily skewed

cp <- hist(log(heartfailure$creatinine_phosphokinase), breaks='FD')
ggplot(heartfailure, aes(x=log(creatinine_phosphokinase))) +
  geom_histogram(breaks = cp$breaks) + theme_economist() + 
  labs(x='Creatinine Phosphokinase') +
  ggtitle('Histogram of Transformed Creatinine_phosphokinase levels')

ggplot(heartfailure, aes(x=log(creatinine_phosphokinase))) +
  geom_density() + theme_economist() +
  labs(x='Creatinine Phosphokinase') +
  ggtitle('Density Distribuiton of Transformed Creatinine_phosphokinase levels')


#Analysis of Diabetes 
#0 = 'Non-Diabetic', 1 = 'Diabetic'
heartfailure$diabetic <- ifelse(heartfailure$diabetes == 0, 'Non-Diabetic', 
                                'Diabetes')
table(heartfailure$diabetic) #174patients (58%) did not have diabetes
#125patients (42%) had diabetes 

ggplot(heartfailure, aes(x=diabetic)) +
  geom_bar() + theme_economist() + labs(x='Diabetic Status')
  ggtitle('Diabetic Status Distribution')

  
#Analysis Of Ejection Fraction 
ggplot(heartfailure, aes(x=ejection_fraction)) +
  geom_boxplot() + theme_economist() +
  labs(x='Ejection Fraction') +
  ggtitle('Boxplot of Ejection Fraction Levels')

ej <- hist(heartfailure$ejection_fraction, breaks='FD')
ggplot(heartfailure, aes(x=ejection_fraction)) +
  geom_histogram(breaks = ej$breaks) + theme_economist() +
  labs(x='Ejection Fraction') +
  ggtitle('Histogram of Ejection Fraction Distribution')


ggplot(heartfailure, aes(x=ejection_fraction)) +
  geom_density() + theme_economist() + 
  labs(x='Ejection Fraction') +
  ggtitle(' Ejection Fraction Density Distribution')


#Analysis of Blood Pressure
#0 = 'Non-Hypertensive', 1 ='Hypertensive' 
heartfailure$hypertensive <- ifelse(heartfailure$high_blood_pressure == 0, 
                                    'Non-Hypertensive', 'Hypertensive')
table(heartfailure$hypertensive)
# 194patients (65%) did not have high blood pressure
# 105patients (35%) had high blood pressure 

ggplot(heartfailure, aes(x=hypertensive)) +
  geom_bar() + theme_economist() + labs(x='Blood Pressure Status') +
  ggtitle(' Blood Pressure Status Distribution')


#Analysis of Platelet Count
ggplot(heartfailure, aes(x=platelets)) +
  geom_boxplot() + theme_economist()+
  labs(x='Platelets') +
  ggtitle('Boxplot of Platelets Count')
#it is observed from the boxplot that there are some outliers 
#for some participants but not enough to skew the data

pl <- hist(heartfailure$platelets, breaks='FD')
ggplot(heartfailure, aes(x=platelets)) +
  geom_histogram(breaks = pl$breaks) + theme_economist() +
  labs(x='Platelets') +
  ggtitle('Histogram of Platelet Count Distribution')

ggplot(heartfailure, aes(x=platelets)) +
  geom_density() + theme_economist() +
  labs(x='Platelets') +
  ggtitle(' Platelets Count Density Distribution')


#Analysis of Serum Creatinine
ggplot(heartfailure, aes(x=serum_creatinine)) +
  geom_boxplot() + theme_economist() +
  labs(x='Serum Creatinine') +
  ggtitle('Boxplot of Serum Creatinine Levels')
##the presence of an adequate number of outliers makes the 
#data to be skewed. thus a transformation is needed

sc <- hist(log(heartfailure$serum_creatinine), breaks='FD')
ggplot(heartfailure, aes(x=serum_creatinine)) +
  geom_histogram(breaks = sc$breaks) + theme_economist() + 
  labs(x='log Sreum Creatinine') +
  ggtitle('Histogram of Transformed Serum Creatinine Levels')

ggplot(heartfailure, aes(x=log(serum_creatinine))) +
  geom_density() + theme_economist() +
  labs(x='Serum Creatinine') +
  ggtitle('Transformed Serum Creatinine Levels Density Distrinution')

#Analysis of  Serum Sodium
ggplot(heartfailure, aes(x=serum_sodium)) +
  geom_boxplot() + theme_economist() +
  labs(x='Serum Sodium') +
  ggtitle('Boxplot of Serum Sodium Levels')

ss <- hist(heartfailure$serum_sodium, breaks='FD')
ggplot(heartfailure, aes(x=serum_sodium)) +
  geom_histogram(breaks = ss$breaks) + theme_economist() +
  labs(x='Serum Sodium') +
  ggtitle('Histogram of Serum Sodium Levels')

ggplot(heartfailure, aes(x=serum_sodium)) +
  geom_density() + theme_economist() +
  labs(x='Serum Sodium') +
  ggtitle('Serum Sodium Levels Density Distribution')


#Analysis of Sex 
#0 = Female, 1 = Male
heartfailure$sexc <- ifelse(heartfailure$sex == 0, 'Female', 'Male')
table(heartfailure$sexc)
# 105patients (35%) were Females 
# 194patients (65%) were Males

ggplot(heartfailure, aes(x=sexc)) +
  geom_bar() + theme_economist() + labs(x='Sex') +
  ggtitle('Distribution of Sex')


#Analysis of Smoking 
#0 = non-smoker, 1 = smoker
heartfailure$smoker <- ifelse(heartfailure$smoking == 0, 'Non-Smoker', 
                                'Smoker')
table(heartfailure$smoker)
#203patients (68%) were non-smokers
#96patients (32%) were smokers 

ggplot(heartfailure, aes(x=smoker)) +
  geom_bar() + theme_economist() + labs(x='Smoking Status')
  ggtitle('Distribution of Smoking Status')


#Analysis of Death 
#0 =Patient did not die , 1 = Patient died 
heartfailure$DEATH <- ifelse(heartfailure$DEATH_EVENT ==1, 'Dead',
                                   'Not Dead')  
table(heartfailure$DEATH)
#203patients (68%) did NOT die
#96patients (32%) died


#MULTIVARIATE ANALYIS 
#visualizing the features with and how they relate to death event
#as it is the outcome feature

#Analysis of Death_Event and Age
#a bar chart to show the number of patients alive or death from each age group
ggplot(heartfailure, aes(x=agegroup)) + 
  geom_bar() + facet_grid(~DEATH) + theme_economist()+
  ggtitle('Age Group and Event')
#most death came from the 56-60 age group


#Analysis of Anaemia and Death_Event
anade<-table(heartfailure$anaemic, heartfailure$DEATH)
anade
# 35.6% of anaemic patients died and 29.4% on non-anaemic patients also died

ggplot(heartfailure, aes(x=anaemic)) + 
  geom_bar() + facet_wrap(~DEATH) + theme_economist() +
  ggtitle('Anaemia and Death Event')


#Analysis of Death_Event and Creatinine_phosphokinase Levels
ggplot(heartfailure, aes(x=log(creatinine_phosphokinase))) + 
  geom_boxplot() + facet_grid(~DEATH) +
  theme_economist() + labs(x='Creatinine Phosphokinase') +
  ggtitle('Creatinine Phosphokinase and Death Event')
#median of creatinine_phosphokinase levels seems to be overlap from the
#boxplots which might indicate little to no effect in its levels 

ggplot(heartfailure, aes(x=log(creatinine_phosphokinase))) + 
  geom_histogram(breaks=cp$breaks) + facet_grid(~DEATH) +
  theme_economist() + labs(x='Creatinine Phosphokinase') +
  ggtitle('Creatinine Phosphokinase and Death Event')


#Analysis of Death_Event and Diabetic Status
diab<-table(heartfailure$diabetic, heartfailure$DEATH)
diab
#32.1% of non-diabetic patients died and 32% of diabetic patients died

ggplot(heartfailure, aes(x=diabetic)) + 
  geom_bar() + facet_wrap(~DEATH) +theme_economist()+
  labs(x='Diabetic Status') +
  ggtitle('Diabetic Staus and Death Event')


#Analysis of Death_Event and Blood pressure 
hbp<-table(heartfailure$hypertensive, heartfailure$DEATH)
hbp
#29.4% of non-hypertensive patients died and 37.1% of hypertensive patients died 

ggplot(heartfailure, aes(x=hypertensive)) + 
  geom_bar() + facet_wrap(~DEATH) +theme_economist() +
  labs(x='Blood Pressure') +
  ggtitle('Blood Pressure and Death Event')


#Analysis of  Death_Event and Smoking Status
smo<-table(heartfailure$smoker, heartfailure$DEATH)
smo
#32.5% of non-smokers died and 31.3% of smokers died 

ggplot(heartfailure, aes(x=smoker)) + 
  geom_bar() + facet_wrap(~DEATH) + theme_economist() +
  labs(x='Smoking Status') +
  ggtitle('Death Event and Smoking Status')


#Analysis of Death_Event and Sex 
gender<-table(heartfailure$sexc, heartfailure$DEATH)
gender
#32.4% of females died and 32% of males died

ggplot(heartfailure, aes(x=sexc)) + 
  geom_bar() + facet_wrap(~DEATH) + theme_economist()+
  labs(x='Sex') + ggtitle('Death Event and Sex')


#Analysis of Death_Event and Ejection_Fraction
ggplot(heartfailure, aes(x=ejection_fraction)) + 
  geom_boxplot() + facet_grid(~DEATH) +
  theme_economist() + labs(x='Ejection Fraction') +
  ggtitle('Death Event and Ejection Fraction')
#the median ejection fraction is lower in the dead which is logical since the 
#heart will not be capable of pumping more blood at lower ejection fractions.
#from the boxplots, people with higher ejection fractions survived more than 
#than those with lower ejection fractions 

ggplot(heartfailure, aes(x=ejection_fraction)) + 
  geom_histogram(breaks=ej$breaks) + facet_grid(~DEATH) +
  theme_economist() + labs(x='Ejection fraction') +
  ggtitle('Death Event and Ejection Fraction')


#Analysis of Death_Event and Serum_Creatinine
ggplot(heartfailure, aes(x=log(serum_creatinine))) + 
  geom_histogram(breaks=sc$breaks) + facet_grid(~DEATH_EVENT) +
  theme_economist() + labs(x='Serum Creatinine') +
  ggtitle('Death Event and Serum Creatinine')

ggplot(heartfailure, aes(x=log(serum_creatinine))) + 
  geom_boxplot() + facet_grid(~DEATH_EVENT) +
  theme_economist() + labs(x='Serun Creatinine') +
  ggtitle('Death Event and Serum Creatinine')
#as serum_creatinine increases the survival chances reduces as seen by a higher
#median value in the dead indicating a positive correlation

#Analysis of Death_Event and Serum Sodium
ggplot(heartfailure, aes(x=serum_sodium)) + 
  geom_boxplot() + facet_grid(~DEATH) +
  labs(x='Serum Sodium') + theme_economist() +
  ggtitle('Death Event and Serum Sodium')
#from the boxplots, the dead had a lower median serum sodium levels thus having
#a negative correlation with death

ggplot(heartfailure, aes(x=serum_sodium)) + 
  geom_histogram(breaks=ss$breaks) + facet_grid(~DEATH) +
  labs(x='Serum Sodium') + theme_economist() +
  ggtitle('Death Event and Serum Sodium')


#Analysis of Death_Event and Platelets
ggplot(heartfailure, aes(x=platelets)) + 
  geom_boxplot() + facet_grid(~DEATH) +
  labs(x='Platelets') + theme_economist() +
  ggtitle('Death Event and Platelet Count')
#median values in both groups seems to overlap 

ggplot(heartfailure, aes(x=platelets)) + 
  geom_histogram(breaks=pl$breaks) + facet_grid(~DEATH) +
  labs(x='Platelets') + theme_economist() +
  ggtitle('Death Event and Platelet Count')



#Identifying the Correlation between the variables 
heartcorr <- subset(heartfailure, select = -(12))
head(heartcorr)

#calculation the correlation matrix 
cor <- round(cor(heartcorr),1)
cor

#visualizing the correlation matrix 
ggcorrplot(cor, lab=TRUE, type = 'lower', tl.col = 'black',
           tl.srt=90) + theme_economist() 

#from the correlation matrix, we observe that death_event has no correlation 
#with diabetic status, platelets level, sex and smoking status. 


model <- glm(DEATH_EVENT ~ ., data=subset(heartfailure, selec =-(time)), family = binomial())
summary(model)

model1 <- glm(DEATH_EVENT ~ age + creatinine_phosphokinase + ejection_fraction + 
                serum_creatinine + serum_sodium,
               data=heartfailure, family = binomial)
summary(model1)

#using backward stepwise method
variableselect <- step(model, direction = 'backward', trace = 0)
summary(variableselect)
#variables that are significant are age, ejection fraction,
#creatinine phosphokinase.
model2 <- glm(DEATH_EVENT ~ age + serum_creatinine + ejection_fraction,
                           data=heartfailure, family = binomial)
summary(model2)

#variable selection 
vip(variableselect, num_features =length(coef(variableselect)))
#variables with importance greater than 1 for the  model were age, ejection fraction,
#serum creatinine, creatinine phosphokinase and serum sodium

#from the initial model 
model3 <- glm(DEATH_EVENT ~ age + ejection_fraction + creatinine_phosphokinase  +
           serum_creatinine, data= heartfailure, family=binomial)
summary(model)
 
#model selection based on their AIC score 
models <- list( model, model1, model2, model3)
aictab(cand.set = models)

#model 2 with variables age, ejection fraction and serum creatinine had the 
#lowest AIC thus will be used 
#these same variables also had high correlation index from correlation index, 
#indicating they're the variables needed for an effective model


#splitting data into Training and Testing Set to Test the model (80-20 split)
set.seed(100)
trainindex <- createDataPartition(heartfailure$DEATH,
                                         p=0.8, list=FALSE, times=1)

trainheartfailure <- heartfailure[trainindex,] #training dataset
testheartfailure <- heartfailure[-trainindex,] #testing dataset


addmargins(table(trainheartfailure$DEATH)) # training dataset has 
#240 patients 
addmargins(table(testheartfailure$DEATH)) #testing dataset has 
#59 patients 

#training the model 
model4 <- glm(DEATH_EVENT ~ age +  ejection_fraction  + serum_creatinine, 
              data= trainheartfailure, family='binomial')
coef(model4)

#testing the training model to obtain predicted values  
predtest <- predict(model4, testheartfailure[,-13]) #DEATH_EVENT variable is 
                                      #omitted as is it the response variable
predtest  <- ifelse(predtest >0.5,1,0) #group the response variable into 0,1 as events 

#create a confusion matrix to compare the death_even in the test data and the 
#response from using the model coefficients from the training data
confusionMatrix(as.factor(testheartfailure$DEATH_EVENT), as.factor(predtest))

#from the confusion matrix, model3 (which was used to train the data) has an 
#accuracy of 76.27% (95% C.I 63.41%, 86.38%) in predicting whether a patient 
#is going to have the Death_Event or not
#true positive(sensitivity) 75.00% and true negative of(specificity) 
#of 85.71%

#testing dataset AUC 
testroccurve <- roc(as.factor(predtest),testheartfailure$DEATH); testroccurve
plot(testroccurve, main = 'AUC of the Test Dataset ROC')
#AUC for the testing dataset in 70.71%


#training dataset AUC 
predtrain <- predict(model4, trainheartfailure[,-13])
predtrain <- ifelse(predtrain >0.5,1,0)
trainroccurve <- roc(as.factor(predtrain), trainheartfailure$DEATH); trainroccurve
plot(trainroccurve, main = 'AUC of the Training Dataset ROC')
#AUC for the training dataset in 76.21%

#Survival Analysis
#testing the Proportional Hazard assumption and assessing the the features
#to see how it fits the survival curves and their respected Hazard Ratio
#use right censoring rule, the event occurred after a specific date

#categorising the event, age, ejection fraction and serum creatinine 
heartfailure$EVENT <- ifelse(heartfailure$DEATH_EVENT == 1, 'Event',
                                   'Censor')

heartfailure$efraction <- ifelse(heartfailure$ejection_fraction >= 41  &
                                   heartfailure$ejection_fraction <= 75, 
                                 'Ejection Normal', 'Ejection Abnormal')

heartfailure$serumcreat <- ifelse(heartfailure$serum_creatinine >= 0.74 & 
                                    heartfailure$serum_creatinine <= 1.35 ,
                                  'Creatinine Normal', 'Creatinine Abnormal')

heartfailure$agegp <- ifelse(heartfailure$age <65, 'Age <65', 'Age>=65')
table(heartfailure$agegp)

###Assessing the Proportional Hazard 
samodel <- coxph(Surv(time,DEATH_EVENT) ~ efraction + agegp + serumcreat,
              data = heartfailure)
ccox <- cox.zph(samodel)
ccox


#Assessing the Hazard Ratio
ggforest(samodel)



#Kaplan Meier (KM) Curve 
#KM curve a visual representation of the estimated non-parametric survival 
#function that shows the probability of an event at a respective time interval.
#It requires no assumptions regarding the underlying distribution of the data

#kaplan meier curves for the various variables 
survivalefraction <- survfit(Surv(time,DEATH_EVENT) ~ heartfailure$efraction, 
                       data=heartfailure)
survivalserumcreat <- survfit(Surv(time,DEATH_EVENT) ~ heartfailure$serumcreat, 
                              data=heartfailure)
survivalagegp <-survfit(Surv(time,DEATH_EVENT) ~ heartfailure$agegp, 
                             data=heartfailure)

splots <- list()
splots[[1]] <- ggsurvplot(survivalefraction, data=heartfailure, 
                          xlab='Time in Days', ggtheme = theme_economist())
splots[[2]] <- ggsurvplot(survivalserumcreat, data=heartfailure, 
                          xlab='Time in Days', ggtheme = theme_economist())
splots[[3]] <- ggsurvplot(survivalagegp, data=heartfailure, 
                          xlab='Time in Days', ggtheme = theme_economist())


arrange_ggsurvplots(splots, print = TRUE, nrow=3, ncol = 2)


#combining all the variables into two groups; high risk and low risk 
#low risk group : creatinine normal, ejection normal, age<65
#high risk group:  creatinine abnormal, ejection abnormal, age<65


heartfailure$riskgroup <- ifelse(heartfailure$agegp == 'Age <65' & 
                                   heartfailure$efraction == 'Ejection Normal' & 
                                   heartfailure$serumcreat == 'Creatinine Normal',
                                  'Low Risk', 'High Risk')
survivalrisk <- survfit(Surv(time, DEATH_EVENT) ~ riskgroup, data=heartfailure)
riskcurve <- ggsurvplot(survivalrisk, data=heartfailure, risk.table=TRUE,
                        break.time.by=30, ggtheme = theme_economist(),
                        xlab='Time in Days')
riskcurve


#log rank p values for groups 
survdiff(Surv(time, DEATH_EVENT) ~ riskgroup, data=heartfailure)
#a signiicant pvalue at 0.05 which is consistent with our model

#Cox Proportion Model and Hazard Ratio
coxph(Surv(time, DEATH_EVENT) ~ riskgroup, data=heartfailure) %>%
  tbl_regression(exp=TRUE)

#Sumary
#it can be seen that p values from the cox model is consistent with result that 
#Serum Creatinine, Ejection Fraction and Age are the most important variables in patients with heart disease 