#***********************************************************************
#*
#*                GARMENT EMPLOYEE PRODUCTIVITY PREDICTION MODEL
#*                USING ADVANCED DECISION MAKING
#*                
#************************************************************************
# setting working directory

setwd(dirname(file.choose()))
getwd()

# Reading the UCI Dataset and Exploring it

Garment <- read.csv("Garment_input.csv", stringsAsFactors = FALSE)

#Structure of Dataframe
str(Garment)    #could see date is in str type

#Sample of Dataset
head(Garment)  #could see department has problem

#Feature types
summary(Garment)

#List the column names
col = colnames(Garment)
print(col)

#Converting date column to date object

a <- as.Date(Garment$date,format="%d/%m/%Y") # Produces NA when format is not "%d-%m-%Y"
b <- as.Date(Garment$date,format="%d-%m-%Y") # Produces NA when format is not "%d/%m/%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
Garment$date <- a # Put it back in your dataframe

#Missig value imputataion - Only WIP
apply(Garment, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(Garment, col = c("red", "blue"), legend = FALSE)

#Separate Categorical and Continous variables
G_con = Garment[ ,!sapply(Garment, is.character)]
G_cat = Garment[ ,sapply(Garment, is.character)]


#**************************************************************************
#*
#*Data Cleaning
#*
#*****************************************************************************
print('Department field Cleaning')
print(unique(G_cat$department))

G_cat$department <- trimws(G_cat$department)
print(unique(G_cat$department))

G_cat$department <- na.omit(G_cat$department)

library("stringr")
G_cat$department = str_replace_all(G_cat$department,'sweing','sewing')
#Installed stringr Package for using str_replace_all ; whereas str_replace will only
#do the first occurance

print(unique(G_cat$department))


##WIP Column missing value handling
#To replace missing values in R with the minimum, you can use the tidyverse package. 
#Firstly, you use the mutate() function to specify the column in which you want to
#replace the missing values. Secondly, you call the replace() function to identify 
#the NA's and to substitute them with the column lowest value.
library(dplyr)
library(zoo) #advanced date package
library(imputeTS)
G_con$wip[is.na(G_con$wip)] <- min(G_con$wip, na.rm = T)
#After removal
apply(G, MARGIN = 2, FUN = function(x) sum(is.na(x)))
missmap(G_con, col = c("red", "blue"), legend = FALSE)



#Creating Month name as Separate Field
#library(lubridate)
G_con$month <- months(as.Date(G_con$date)) 

#**************************************************************************
#*
#Univariate Analysis
#*
#******************************************************************************

#Pie chart of Quarter Field
pie(table(G_cat$quarter), labels = paste(round(prop.table(table(G_cat$quarter))*100), "%", sep = ""), 
    col = heat.colors(5), main = "Quarter Pie chart")
legend("topright", legend = c("Quarter1", "Quarter2", "Quarter3", "Quarter4", "Quarter5"), 
       fill = heat.colors(5), title = "Categories", cex = 0.5)

#print(unique(G_cat$quarter))


#Pie chart of department field
pie(table(G_cat$department), labels = paste(round(prop.table(table(G_cat$department))*100), "%", sep = ""), 
    col = c("lightblue","mistyrose"), main = "department Pie chart")
legend("topleft", legend = c("sewing", "finishing"),
       fill = c("lightblue", "mistyrose"))

#print(unique(Gcat$department))

#Pie chart of day field
#print(unique(G_cat$day))

pie(table(G_cat$day), labels = paste(round(prop.table(table(G_cat$day))*100), "%", sep = ""), 
    col = heat.colors(6), main = "day Pie chart")
legend("topleft", legend = c("Thursday", "Saturday","Sunday","Monday","Tuesday","Wednesday"),
       fill = heat.colors(6), title = "Categories", cex = 0.5)


# Stacked Bar Plot with Colors and Legend
counts <- table(G_cat$department,G_cat$day)
barplot(counts, main="Department vs day",
        xlab="Number of Departments", col=c("darkblue","red"),
        legend = rownames(counts))


counts <- table(G_cat$department,G_cat$quarter)
barplot(counts, main="quarter vs Department",
        xlab="Number of Departments", col=c("pink","lightblue"),
        legend = rownames(counts))

#Histogram for continous variables
library(Hmisc)
hist.data.frame(G_con[c(-1,-2,-3,-4,-5,-6,-7)])
hist.data.frame(G_con[c(-5,-6)])
hist.data.frame(G_con[c(2,3,4,5,6)])

# boxplot all the variables excluding the first two text variables
boxplot(G_con[c(2,3,4)])
boxplot(G_con[c(5,6,7,8)])
boxplot(G_con[c(9,10,11)])

#Targeted vs actual
boxplot(G_con[c(3,12)])
par(mar=c(1, 1, 1, 1))

#*************************************************************************
#*
#*Correlation
#*
#*************************************************************************

#Correlation between variables - Corrgram
library(corrgram)
# corrgram works best with Pearson correlation
corrgram(G_con, order=TRUE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Actual Productivity vs all",
         col = colorRampPalette(c("#8073AC", "red", "navy")))

#From the above heatmap we can see that there is very high correlation between smv and no_of_workers.
#over_time & smv as well as over_time and no_of)workers are correlated  

#Handling variables which are cont but actually cat
stylechn <- table(G_con$no_of_style_change)
stylechn

#Adding style into cat dataframe and greating new
# Categorical - 'quarter', 'department', 'day', 'team','no_of_style_change','month'

str(Garment)
#we could see team and no of style change is int.
#so we need to convert as str to make it as cate

G_con$team <- as.character(G_con$team)
G_con$no_of_style_change <- as.character(G_con$no_of_style_change)

#Dropping date field before encoding
#G_con = subset(G_con, select = -c(date) )

#One hot Encoding for categorical data
#Method 1: one_hot in mltools package
library(caret)
dummy <- dummyVars(" ~ .", data=G_cat)
G_catenc <- data.frame(predict(dummy, newdata = G_cat)) 

G_connew = subset(G_con, select = -c(month,team,no_of_style_change) )

boxplot(G_connew[1:3])
boxplot(G_connew[4:6])
boxplot(G_connew[7:8])
boxplot(G_connew[10:11])
str(G_connew)

#MErging dataframes
#drop(Garment_f)
#Garment_f <- cbind(G_connew,G_catenc)
#Garment_f$team <- as.character(Garment_f$team)
#Garment_f$no_of_style_change <- as.character(Garment_f$no_of_style_change)

str(Garment_f)
boxplot(Garment_f)

G_catenc1 <- cbind(G_con[1])
G_catenc2 <- cbind(G_con[9])
G_catnew <- cbind(G_catenc,G_catenc1,G_catenc2)


#Scaling
#cols_to_scale  = ['smv', 'wip', 'over_time', 'incentive', 'no_of_workers', 'idle_time', 'idle_men']
# min-max scaling
Garment.mm <- apply(G_connew[], MARGIN = 2, FUN = function(x) (x - min(x))/diff(range(x)))


# 2 --> z-score

Garment.z1 <- apply(Garment.mm, MARGIN = 2, FUN = function(x) (x - mean(x))/sd(x))
Garment.z2 <- apply(Garment.mm, MARGIN = 2, FUN = function(x) (x - mean(x))/(2*sd(x)))

boxplot (Garment.z1, main = "Z-score, 1 sd")
boxplot (Garment.z2, main = "Z-score, 2 sd")

replace_outlier <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}


# Replacing extreme values with percentile
Garment.z3 = replace_outlier(Garment.mm)
# soft max scaling - iterative

library(DMwR2)
help(SoftMax)

Garment.sm <- apply(Garment.z1, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 1")

Garment.sm <- apply(Garment.z1, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 2")

Garment.sm <- apply(Garment.z1, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 3")

Garment.sm <- apply(Garment.z1, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 9")

Garment.sm <- apply(Garment.z1, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 10")

#Using the Z2
Garment.sm <- apply(Garment.z2, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 1")

Garment.sm <- apply(Garment.z2, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 2")

Garment.sm <- apply(Garment.z2, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 3")

Garment.sm <- apply(Garment.z2, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 9")

Garment.sm <- apply(Garment.z2, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (Garment.sm, main = "Soft Max, lambda = 10")


#MErging dataframes

Garment_f <- cbind(Garment.z2,G_catnew)

# Test dependent variable for normality
# graphically
qqnorm(Garment_f$actual_productivity, xlab = "Actual Prod Graph" )
qqline(Garment_f$actual_productivity, col = 2) ## red color


#Not normal
# Optionally use Tukey transform to find a transformation
library(rcompanion)
library(nortest)
Garment_f$actual_productivity_1 <- transformTukey(Garment_f$actual_productivity)
plotNormalHistogram(Garment_f$actual_productivity_1, main = "Histogram", xlab = "actual")

# Plot original and transformed values
plot(Garment_f$actual_productivity, Garment_f$actual_productivity_1, main = "Scatterplot", xlab = "act", ylab = "act_1")

# Test transformed dependent variable for normality
# graphically
qqnorm(Garment_f$actual_productivity_1, xlab = "Theoretical Quantiles: act1" )
qqline(Garment_f$actual_productivity_1, col = 2) ## red color


# Lilliefors (Kolmogorov-Smirnov) normality test
lillie.test(Garment_f$actual_productivity_1)



# check the distribution of target variable
boxplot(Garment_f$actual_productivity_1, Garment_f.tr$actual_productivity_1, Garment_f.te$actual_productivity_1,
        names = c("garment all", "garment train", "garment test"))
#*********************************************************************************
#*
# Single Linear Regression - Model 1
#
#********************************************************************************
# split data set
Garment_f.tr <- Garment_f[1:354, ]     # 70%
Garment_f.te <- Garment_f[355:506, ]   # 30%

cor(Garment_f.tr$targeted_productivity, Garment_f.tr$actual_productivity, method = "pearson")

plot(Garment_f.tr$targeted_productivity, Garment_f.tr$actual_productivity, main="Scatterplot",
     xlab = "lower status", ylab = "transform median value")

model1 <- lm( targeted_productivity ~ actual_productivity , data = Garment_f.tr)
             
# Add regression line to scatter plot
plot(Garment_f.tr$targeted_productivity, Garment_f.tr$actual_productivity, main="Scatterplot",
     xlab = "lower status", ylab = "transform median value")
abline(model1, col="red")

# Summarise the model
summary(model1)

Garment_f.tr2 <- Garment_f.tr[c("actual_productivity")]
Garment_f.pr <- predict.lm(model1, Garment_f.tr2, se.fit=TRUE)



# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}


# RMSE
error <-Garment_f.te$actual_productivity - Garment_f.pr$fit
rmse(error)
# MAE
mae(error)
# R-squared
Garment_f.te2 <- Garment_f.te[c("actual_productivity")]
resid.mod1 <- lm(Garment_f.te2$actual_productivity ~ Garment_f.pr$fit)
summary(resid.mod1)


#************************************************************************************
#
## Factor analysis
#
#******************************************************************************
# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(G_connew))

# Determine Number of Factors to Extract
library(nFactors)

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(G_connew))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")


# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(GPArotation)

# principal() uses a data frame or matrix of correlations
fit <- principal(G_connew, nfactors=6, rotate="varimax")
fit

# weed out further variables after first factor analysis
myvars <- names(G_connew) %in% c("idle_time",
                                    "idle_men","incentive" )
G_connew1 <- G_connew[!myvars]
str(G_connew1)
rm(myvars)

library(psych)
KMO(cor(G_connew1))

# get eigenvalues
ev <- eigen(cor(G_connew1))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

# Varimax RLotated Principal Components
# retaining 'nFactors' components
fit <- principal(G_connew1, nfactors=3, rotate="varimax")
fit


#**********************************************************************************

#Multiple Linear Regression - Model2

#***************************************************************************

# Model 2 - Multiple Regression
# model with all variables
# split data set
Garment_a.tr <- G_connew1[1:354, ]     # 70%
Garment_a.te <- G_connew1[355:506, ]   # 30%

model.mr <- lm(Garment_a.tr$targeted_productivity ~ Garment_a.tr$smv + Garment_a.tr$wip
             + Garment_a.tr$over_time + Garment_a.tr$actual_productivity + Garment_a.tr$no_of_workers)
summary(model.mr)

Garment_a.tr2 <- Garment_a.tr[c("smv","wip","over_time","actual_productivity","no_of_workers")]
Garment_a.pr <- predict.lm(model.mr, Garment_a.tr2, se.fit=TRUE)


# RMSE
error <-Garment_a.te$actual_productivity - Garment_a.pr$fit
rmse(error)
# MAE
mae(error)
# R-squared
Garment_a.te2 <- Garment_a.te[c("smv","wip","over_time","actual_productivity","no_of_workers")]
resid.mod2 <- lm(Garment_a.te2$actual_productivity ~ Garment_a.pr$fit)
summary(resid.mod2)

#******************************************************************************

#Classification Algorithm - KNN - Model1 
  
#******************************************************************************
#Garmenbkp <- Garment
#Garment1bkp <- Garmenbkp # Garmenbkp used
#Ga <- Garment 
#*drop(Garmenbkp)

str(Ga)
Ga$department <- trimws(Ga$department)
print(unique(Ga$department))

Ga$department <- na.omit(Ga$department)
library("stringr")
Ga$department = str_replace_all(Ga$department,'sweing','sewing')
#Installed stringr Package for using str_replace_all ; whereas str_replace will only
#do the first occurance

print(unique(Ga$department))

#Making Ga with scaled n good ones
Ga1 <- Ga[3]
Ga1 <- cbind(Ga[3],G_connew[1:9])
#Ga <- Ga[-1]
#Ga <- Ga[-1]


str(Ga1)
# table of dept
table(Ga1$department)

# Indicating all possible values and levels for dept
Ga1$department <- factor(Ga1$department, levels = c("sewing", "finishing"),
                         labels = c("s", "f"))

# table or proportions with more informative labels
round(prop.table(table(Ga1$department)) * 100, digits = 1)

# summarize three numeric features
summary(Ga1[c("targeted_productivity", "actual_productivity", "smv")])
# note contrasts in scale


Ga1_mm <- as.data.frame(apply(Ga1[2:10], MARGIN = 2, FUN = function(x)
  (x - min(x))/diff(range(x))))

# confirm that normalization worked
summary(Ga1$targeted_productivity)

# inspect using boxplots
boxplot (Ga1, main = "MinMax")

# create training (80%) and test data (20%) (data already in random order)
missmap(Ga1, col = c("red", "blue"), legend = FALSE)

Gar_train <- Ga1_mm[1:354, ]
Gar_test <- Ga1_mm[355:506, ]

# create labels (from first column) for training and test data
Gar_train_labels <- Ga1[1:354, 1]
Gar_test_labels <- Ga1[355:506, 1]

# training a model on the data

# load the "class" library
library(class)
# look at help for class and knn

# perform kNN, use k=21 as starting point because SQR(455)
missmap(Gar_test, col = c("red", "blue"), legend = FALSE)
missmap(Gar_train, col = c("red", "blue"), legend = FALSE)
Gar_test_pred <- knn(train = Gar_train, test = Gar_test,
                      cl = Gar_train_labels, k=1)

# inspect results for 114 (20%) test observations
Gar_test_pred


# evaluating model performance

# load the "gmodels" library
library(gmodels)
# look at help for gmodels and CrossTable

# Create the cross tabulation of predicted vs. actual
CrossTable(x = Gar_test_labels, y = Gar_test_pred, prop.chisq=FALSE)
# Inspect FP (0) and FN (2)

# improving model performance

# create normalization functions for SoftMax
#library(DMwR)
library(DMwR2)
Gar_sm <- as.data.frame(apply(Ga1[2:10], MARGIN = 2, FUN = function(x)
  (SoftMax(x,lambda = 3, mean(x), sd(x)))))

# confirm that the transformation was applied correctly
summary(Gar_sm$targeted_productivity)

# Inspect using boxplots
boxplot (Gar_sm, main = "Soft Max, lambda = 3")

# create training and test datasets
Gar_train1 <- Gar_sm[1:354, ]
Gar_test1 <- Gar_sm[355:506, ]

# re-classify test cases
Gar1_test_pred <- knn(train = Gar_train1, test = Gar_test1,
                      cl = Gar_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = Gar_test_labels, y = Gar_test_pred, prop.chisq=FALSE)

# try several different values of k (odd values) reverting to original normalisation

Gar_train <- Ga1_mm[1:354, ]
Gar_test <- Ga1_mm[355:506, ]

Gar_test_pred <- knn(train = Gar_train, test = Gar_test, cl = Gar_train_labels, k=1)
CrossTable(x = Gar_test_labels, y = Gar_test_pred, prop.chisq=FALSE)

Gar_test_pred <- knn(train = Gar_train, test = Gar_test, cl = Gar_train_labels, k=5)
CrossTable(x = Gar_test_labels, y = Gar_test_pred, prop.chisq=FALSE)

Gar_test_pred <- knn(train = Gar_train, test = Gar_test, cl = Gar_train_labels, k=11)
CrossTable(x = Gar_test_labels, y = Gar_test_pred, prop.chisq=FALSE)

Gar_test_pred <- knn(train = Gar_train, test = Gar_test, cl = Gar_train_labels, k=15)
CrossTable(x = Gar_test_labels, y = Gar_test_pred, prop.chisq=FALSE)

Gar_test_pred <- knn(train = Gar_train, test = Gar_test, cl = Gar_train_labels, k=21)
CrossTable(x = Gar_test_labels, y = Gar_test_pred, prop.chisq=FALSE)

Gar_test_pred <- knn(train = Gar_train, test = Gar_test, cl = Gar_train_labels, k=25)
CrossTable(x = Gar_test_labels, y = Gar_test_pred, prop.chisq=FALSE)

Gar_test_pred <- knn(train = Gar_train, test = Gar_test, cl = Gar_train_labels, k=27)
CrossTable(x = Gar_test_labels, y = Gar_test_pred, prop.chisq=FALSE)


# evaluate qualitatively, PCC, FN vs FP, k=1 may overfit and not be good predictor

# return to 'best' model
Gar_test_pred <- knn(train = Gar_train, test = Gar_test, cl = Gar_train_labels, k=1)


# more evaluation statistics

library(caret)
confusionMatrix(Gar_test_pred, Gar_test_labels)

#****************************************************************************

#Random Forest Classifier - Model 2

#******************************************************************************
set.seed(8)
library(caret)
fitControl = trainControl(method="cv", number=10)
install.packages("caret")
Gar_trainrf <- Ga1[1:354, ]
Gar_testrf <- Ga1[355:506, ]

model.rf = train(department ~ .,
                 data = Gar_trainrf,
                 method = "rf",
               trControl = fitControl)
model.rf
plot(model.rf)
#confusionMatrix
preds.rf <- predict(model.rf, Gar_testrf)
confusionMatrix(preds.rf, Gar_testrf$department, mode = "everything")

#****************************************************************************

#Logistic Regression Classifier - Model 3

#************************************************************************************
Gar_trainlr <- Ga1[1:354, ]
Gar_testlr <- Ga1[355:506, ]

model.lr = train(department ~ .,
                 data = Gar_trainlr,
                 method = "glm",
                 trControl = fitControl)
model.lr
#confusionMatrix
preds.lr <- predict(model.lr, Gar_testlr)
confusionMatrix(preds.lr, Gar_testlr$department, mode = "everything")


#**************************************************************************

#SVM Classifier - Model 4

#**************************************************************************************
library(e1071)
library(caTools)
model.svm = svm(formula = department ~ .,
                 data = Gar_trainlr[1:6],
                 type = 'C-classification',
                 kernel = 'linear')
y_pred = predict(model.svm, newdata = Gar_testlr[1:6])
y_train_pred = predict(model.svm, newdata = Gar_trainlr)

confusionMatrix(y_pred, Gar_testlr$department, mode = "everything")

#**********************************************************************************
