# Course Project
library(tidyverse)
library(knitr)
library(tidyverse) 
library(randomForest)    # for random forest
library(caret)           # for model tuning
library(earth)           # for MARS
library(rpart)           # for decision tree
library(e1071)           # for SVM
library(gbm)             # for gbm

dfFull <- as_tibble(read.csv("heart.csv"))
# View(dfFull)
#dfFull$index <- 1:nrow(dfFull)
str(dfFull)

# ---------------- Data Summary ---------------------

# Create numeric tibble
fullNumeric <- dplyr::select(dfFull, where(is.numeric))
# View(fullNumeric)

# Create factor tibble
fullFactor <- dplyr::select(dfFull, !where(is.numeric)) #create subset only including factors
fullFactor[, names(fullFactor)] <- lapply(fullFactor[, names(fullFactor)], factor) #make variables factors for structure test
# View(fullFactor)


# Check the structure of the factor tibble - single level factors will not work
# for the mode/count function
str(fullFactor) #check structure

# Create quantile functions
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
# Q1 takes a numeric input vector, x, and returns the first quantile of non-missing data.
# When data is missing, the function skips that row.

Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}
# Q3 takes a numeric input vector, x, and returns the third quantile of non-missing data.
# When data is missing, the function skips that row.

# Create summary function
myNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

# Apply summary function to every variable in the numeric tibble
numericSummary <- fullNumeric %>%
  dplyr::summarize(across(.cols = everything(), .fns = myNumericSummary))
# View(numericSummary)

# Add labels to the summary stats
numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)
# View(numericSummary)
# glimpse(numericSummary)

# Pivot the summary statistics and add missing value statistics
numericSummaryFinal <- numericSummary %>%
  pivot_longer("Age":"HeartDisease", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())

# Produce the numeric summary report
options(digits=3)
options(scipen=99)
numericSummaryFinal %>% kable()

# # Start writing to an output file
# sink('full-numeric-analysis-output.txt')
# numericSummaryFinal %>% kable()
# # Stop writing to the file
# sink()

# Create function to find the modes
getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}

# Create function to calculate the frequency for the modes
getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

# Combine into one function
myFactorSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)),round(getmodesCnt(x, type = 1)/getmodesCnt(x, type = 2),2),
    getmodes(x, type = 1), getmodesCnt(x, type = 1),getmodes(x, type = 2), getmodesCnt(x, type = 2),
    getmodes(x, type = -1), getmodesCnt(x, type = -1))
}
# Apply summary function to every variable in the factor tibble
factorSummary <- fullFactor %>%
  dplyr::summarize(across(.cols = everything(), .fns = myFactorSummary))
# View(factorSummary)

# Add labels to the summary stats
factorSummary <-cbind(
  stat=c("n","unique","missing","freqRatio","1st mode","1st mode freq","2nd mode","2nd mode freq","least common","least common freq"),
  factorSummary)
# View(factorSummary)
# glimpse(factorSummary)

# Pivot the summary statistics and add missing value statistics
factorSummaryFinal <- factorSummary %>%
  pivot_longer("Sex":"ST_Slope", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*as.numeric(unlist(missing))/as.numeric(unlist(n)),
         unique_pct = 100*as.numeric(unlist(unique))/as.numeric(unlist(n))) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, freqRatio, everything())

# Produce the factor summary report
options(digits=3)
options(scipen=99)
factorSummaryFinal %>% kable()

# # Start writing to an output file
# sink('full-factor-analysis-output.txt')
# factorSummaryFinal %>% kable()
# # Stop writing to the file
# sink()

#-------------------- Distribution of Data ---------------
# Factor data
library(Hmisc)
hist.data.frame(fullFactor)

# Numeric data
library(ggplot2)
numeric_long <- subset(fullNumeric, select = -HeartDisease) %>%                          # Apply pivot_longer function
  pivot_longer(colnames(subset(fullNumeric, select = -HeartDisease))) %>% 
  as.data.frame()
head(numeric_long)                                # Print head of long data

ggp1 <- ggplot(numeric_long, aes(x = value)) +    # Draw each column as histogram
  geom_histogram() + 
  facet_wrap(~ name, scales = "free") +
  theme_update()
ggp1

ggplot(fullNumeric, aes(x=Cholesterol)) + geom_histogram()
sum(fullNumeric$Cholesterol == 0)
# Distribution of response variable
ggplot(fullNumeric, aes(x=HeartDisease)) + geom_histogram(binwidth=0.5) +
  theme_bw()

# Boxplots
# boxplot(data=dfFull, RestingBP ~ Sex,           # boxplot of Sepal.Length by Species 
#         main = "Resting Blood Pressure by Sex",      # main plot title
#         xlab = "Sex",                            # x-axis label   
#         ylab = "Heart Disease")                  # y-axis label  

# Boxplots by heart disease of all numeric features
library(reshape2)
x <- subset(fullNumeric, select = -c(FastingBS, Oldpeak))
x$HeartDisease <- as.factor(x$HeartDisease)
fullNumeric2<- melt(x)
ggplot(fullNumeric2,aes(x=variable, y=value, fill=HeartDisease)) + geom_boxplot()


#------ Data Cleaning ---------
dfFull$HeartDisease <- as.factor(dfFull$HeartDisease)
dfFull$Sex <- as.factor(dfFull$Sex)
dfFull$ChestPainType <- as.factor(dfFull$ChestPainType)
dfFull$RestingECG <- as.factor(dfFull$RestingECG)
dfFull$ExerciseAngina <- as.factor(dfFull$ExerciseAngina)
dfFull$ST_Slope <- as.factor(dfFull$ST_Slope)

str(dfFull)
#------ Data Imputation ---------

# Mode imputation of RestingBP -> one missing value
val <- unique(dfFull$RestingBP[!dfFull$RestingBP == 0]) # non-missing values
my_mode <- val[which.max(tabulate(match(dfFull$RestingBP, val)))]   # Mode of non-missing values
dfFull$RestingBP[dfFull$RestingBP == 0] <- my_mode # mode imputation

# Use mice imputation for Cholesterol -> many 0s
library(mice)

dfPMM.imp <- dfFull
missing <- dfFull$Cholesterol == 0
dfPMM.imp <- dfPMM.imp %>%mutate(missing = missing)
dfPMM.imp[missing,"Cholesterol"] <- mice.impute.pmm(dfPMM.imp$Cholesterol,!dfPMM.imp$missing,dfPMM.imp$FastingBS)
dfFull$Cholesterol <- dfPMM.imp$Cholesterol
hist(dfFull$Cholesterol)


#------ Feature Engineering ------
# Create a variable indicating if their max heart rate exceeded 90% of their 
# recommended max heart rate = 220-age
HRGreater90 <- dfFull$MaxHR > (220 - dfFull$Age)
dfFull <- dfFull %>%mutate(HRGreater90 = HRGreater90)
dfFull$HRGreater90[dfFull$HRGreater90 == FALSE] <- 0 # change from false to 0, true to 1
dfFull$HRGreater90 <- as.factor(dfFull$HRGreater90)

#------ Data Splitting ---------
sample_size = floor(0.8*nrow(dfFull))
set.seed(156)
picked = sample(seq_len(nrow(dfFull)),size = sample_size)
train_Full = dfFull[picked,]
test_Full = dfFull[-picked,]

str(train_Full)
str(test_Full)


#-------------- Modelling ----------------

#------------- Logistic Regression ---------------
# control for 5-fold cross validation 
control <- trainControl(method='repeatedcv', 
                        number=5, 
                        repeats=3)
# set seed to replicate the results
set.seed(123)
# glm model using caret
glm_model <- train(HeartDisease~., 
                    data=train_Full, 
                    method='glm', 
                    tuneLength = 10,
                    trControl=control)

glm_model
# predict 
preds.glm = predict(glm_model,test_Full)
# confusion matrix to check the accuracy
confusionMatrix(preds.glm, test_Full$HeartDisease)

#------------- AdaBoost Tree Ensemble ---------------
library(fastAdaboost)
# control for 5-fold cross validation 
adaCtrl = trainControl(
  method = "repeatedcv",
  repeats = 5
)

adaGrid = expand.grid(
  nIter = c(5, 10, 15),
  method = c("Adaboost.M1")
)
# Adaboost modelling
adaboost.fit = train(
  HeartDisease ~ .-index,
  data = train_Full,
  method = 'adaboost',
  trControl = adaCtrl,
  tuneGrid = adaGrid
)
adaboost.fit
# predict
preds.ADA = predict(adaboost.fit,test_Full)
# confusion matrix to check the accuracy
confusionMatrix(preds.ADA, test_Full$HeartDisease)


#------------- Random Forest ---------------
# control for 5 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=5, 
                        repeats=3)
# set seed to replicate the results
set.seed(123)
# random forest modelling
rf_default <- train(HeartDisease~., 
                    data=train_Full, 
                    method='rf', 
                    tuneLength = 10,
                    trControl=control)
rf_default
# predict
preds.rf = predict(rf_default,test_Full)
# confusion matrix to check the accuracy
confusionMatrix(preds.rf, test_Full$HeartDisease)


#------------- MARS ---------------
# control for 5 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=5, 
                        repeats=3)
# set seed to replicate the results
set.seed(123)
# MARS modelling
mars_model <- train(HeartDisease~., 
                    data=train_Full, 
                    method='earth', 
                    tuneLength = 10,
                    trControl=control)
mars_model
# predict
preds.mars = predict(mars_model,test_Full)
# confusion matrix to check the accuracy
confusionMatrix(preds.mars, test_Full$HeartDisease)

### MARS modelling using earth package 
marsFit <- earth(HeartDisease ~.,
                 data=train_Full,
                 degree=3,nk=50,pmethod="cv",nfold=5,ncross=5)

predMARS <- predict(marsFit,test_Full, type = "class")

predMARS <- as.factor(predMARS)
confusionMatrix(predMARS, test_Full$HeartDisease)


#------------- SVM ---------------
# Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=5, repeats=5)
# SVM modelling
svmFit <- train(HeartDisease ~., data = train_Full, method = "svmRadial",
                trControl = train_control,  preProcess = c("center","scale"), tuneLength = 10)
# predict
svmPred <- predict(svmFit,test_Full)
# confusion matrix to check the accuracy
confusionMatrix(svmPred, test_Full$HeartDisease)






