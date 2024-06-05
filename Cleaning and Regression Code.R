################################################################################



#storing the original dataset 
original = read.csv("https://raw.githubusercontent.com/NikolaosProjects/UFC-Bout-Finish-Examination/main/Dataset.csv")

#copying the dataset into a new variable which is going to be manipulated
manip = original



################################################################################



### BEGINNING THE MANIPULATION OF THE DATASET ###



################################################################################
#### 1)going through the columns in the original dataset and removing all ####
#### that are not of interest. ####
################################################################################

#creating empty variable for storing the indexes of the unwanted columns
unwanted_cols = c()

#saving names of all columns in starting dataset
col_names = names(manip)

#defining vector containing only the names of the colums we want to keep
wanted_col_names = c('date', 'no_of_rounds', 'weight_class', 'gender', 'reach_dif', 'age_dif',
                     'total_fight_time_secs', 'total_title_bout_dif', 'finish')

#setting index 0
k = 0

#creating vector with the indices of the columns we don't want
for (i in 1:length(names(manip))){
  if ((col_names[i] %in% wanted_col_names) == FALSE){
    k = k + 1
    unwanted_cols[k] = i
  }
}                

#removing the above columns we don't want from the dataset
manip = manip[,-unwanted_cols]




################################################################################
#### 2) removing all missing values from the dataset ####
################################################################################

manip = na.omit(manip)

#now checking types of finishes to each bout

fin = manip[, "finish"]

unwanted_row = c()

k = 0

for ( i in 1:length(fin)){
  if (!(fin[i] %in% c("SUB", "KO/TKO"))){
    k = k + 1
    unwanted_row[k] = i
  }
}

manip <- manip[-unwanted_row,]


#turning the above column into binary, with 1 indicating finish by KO/TKO, 0 indicating
#finish by SUB

fin = manip[, "finish"]

k = 0

finish_type = c()

for ( i in 1:length(fin)){
  if (fin[i] == "KO/TKO"){
    finish_type[i] = "KO/TKO"
  }
  else{
    finish_type[i] = "SUB"
  }
}

manip[, "FinishType"] <- finish_type 




################################################################################
#### 3) only keeping data from 2013 and later####
################################################################################

#extracting the year from the full dates in the dataset
dates = as.POSIXct(manip[,1], format = "%m/%d/%Y")
years = format(dates, format = "%Y")

# placing a new column in the dataset indicating just the year the bout was contested.
manip[, "Year"] <- years

#defining the years we want
wanted_years = c('2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021')

#defining empty vector for storing the indices of the rows containing unwanted years
unwanted_rows = c()

#setting index 0 
k = 0

#looping through the date values in the dataset and creating vector with the indeces
# of the years we don't want.
for (i in 1:length(years)){
  if (((years[i] %in% wanted_years)) == FALSE){
    k = k + 1
    unwanted_rows[k] = i
  } 
}

#removing the rows containing data corresponding to dates before the year 2013
manip = manip[-unwanted_rows, ]





################################################################################
#### 4) only keeping bouts contested over 3 rounds (no 5 round contests). ####
################################################################################

#extracting column containing how many rounds the bout was scheduled for.
round_col = manip[,"no_of_rounds"]

#defining empty vector for storing the indices of the rows corresponding to a 5 round bout.
unwanted_row = c()

#setting index 0
k = 0

#looping over the columns and noting the indices of bouts which were scheduled for 5 rounds
for (i in 1:length(round_col)){
  if (round_col[i] > 3){
    k = k + 1
    unwanted_row[k] = i
  }
}

#removing the rows corresponding to 5 round bouts from the dataset
manip = manip[-unwanted_row,]

#removing all 3 round bouts that lasted more than 15 minutes (extreme outliers) from the dataset#

#saving the colummn containing the number of seconds a contest lasted
duration = manip[,"total_fight_time_secs"]
#creating empty vector for storing the indices of such entries.
unwanted_row = c()

#setting index 0
k = 0

#looping over the time values and recording which of the 3 round bouts lasted for more or equal to 15 minutes (900 seconds).
for (i in 1:length(duration)){
  if (duration[i] >= 900){
    k = k + 1
    unwanted_row[k] = i
  }
}

#removing rows corresponding to such bouts from the dataset.
manip = manip[-unwanted_row,]





################################################################################
#### 5) Only keeping the data for males ####
################################################################################

#saving the column indicating whether the fight was contested amongst men or
#women (these are the only gender categories in the organization)
dataset_gender = manip[,"gender"]
#creating empty vector for storing the indices corresponding to a fight amongst female competitors.
unwanted_row = c()

#setting index 0
k = 0

#looping over the column indicating the gender of the contenders and storing the indexes of female bouts.
for (i in 1:length(dataset_gender)){
  if (dataset_gender[i] == "FEMALE"){
    k = k + 1
    unwanted_row[k] =  i
  }
}

#removing the data for female bouts from the dataset.
manip = manip[-unwanted_row,]




################################################################################
#### 6) Removing the catchweight weightclass ####
################################################################################
#saving the column indicating the weightclass of the bout in a new vector
weight_class_by_name_col = manip[,"weight_class"]
#creating new vector for storing the indices of the unwanted weightclasses
unwanted_row = c()

#setting index 0
k = 0

#looping over the weightlasses for each bout, recording the weightclass' name
for (i in 1:length(weight_class_by_name_col)){
  if (weight_class_by_name_col[i] == "Catch Weight"){
    k = k + 1
    unwanted_row[k] = i
  }
}

#removing the rows corresponding to bouts not contested in one of the standard weightclasses.
manip = manip[-unwanted_row,]





################################################################################
#### 7) Determining the difference in title bout experience between the two competitors 
################################################################################

title_bouts_dif = manip[,"total_title_bout_dif"]

#adding the new column of the vector described above to our dataframe.
manip[,"TitleFightDiff"] <- abs(title_bouts_dif)




################################################################################
#### 8) Taking the absolute values of the reach and age differences.####
#################################################################################

#saving the values indicating the reach and age difference between fighter1 and fighter2.
reach = manip[,"reach_dif"]
age = manip[,"age_dif"]

#defining vector meant for storing the absolute values of the above.
abs_reach_difference = c()
abs_age_difference = c()


#looping over the reach and age difference values and assigning for each bout what
#the absolute difference in these two quantities are between each pair of contestants.
for (i in 1:length(reach)){
  abs_reach_difference[i] = abs(reach[i])
  abs_age_difference[i] = abs(age[i])
}

#adding the new column containing the values described above to our dataset.
manip[,"Reach Difference"] <- abs_reach_difference
manip[,"Age Difference"] <- abs_age_difference




################################################################################
#### 9) Removing unwanted columns of data before rearranging the dataset. ####
################################################################################

manip <- manip[,-c(1, 3, 4, 5, 6, 7, 8)]




################################################################################
#### 10) Renaming the columns as to make the dataset better looking and easier to work ####
#### with. ####
################################################################################

names(manip)[1] <- "WeightClass"
names(manip)[2] <- "ContestDuration"
names(manip)[3] <- "FinishType"
names(manip)[4] <- "Year"
names(manip)[5] <- "TitleExperienceDif"
names(manip)[6] <- "ReachDifference"
names(manip)[7] <- "AgeDifference"


#renaming the dataset, and changing the order of the columns in order to have
#the response in the first column, the numeric (continuous) variables in the next 3
#columns, and the categorical variables in the last 3 columns
dataset <- manip[, c(2, 7, 6, 5, 4, 3, 1)]

#making the indexing of each row to increase from 1 to the total number of rows
rownames(dataset) <- 1:nrow(dataset)

################################################################################
######################## EXPLORATORY DATA ANALYSIS #############################
################################################################################

# 1) Plotting the Distributions of each variable (including the response)

#fight duration (RESPONSE VARIABLE)
plot(density(dataset$ContestDuration), xlab = "ContestDuration", main = "PDF of ContestDuration")

#age difference
plot(density(dataset$AgeDifference), xlab = "AgeDifference", main = "PDF of AgeDifference")

#reach difference
plot(density(dataset$ReachDifference), xlab = "ReachDifference", main = "PDF of ReachDifference")

#title fight number difference
plot(density(dataset$TitleExperienceDif), xlab = "TitleExperienceDif", main = "PDF of TitleExperienceDif")

#weight class
barplot(prop.table(table(dataset$WeightClass)), xlab = "WeightClass", ylab = "Count", main = "PMF of WeightClass")

#finish type
barplot(prop.table(table(dataset$FinishType)), xlab = "FinishType", ylab = "Count", main = "PMF of FinishBinary")

#year
barplot(prop.table(table(dataset$Year)), xlab = "Year", ylab = "Count", main = "PMF of YEAR")

#2) Plotting each variable vs the response as to observe the results
#
#plotting the age difference in relation to fight duration plot
y = dataset[,"ContestDuration"]
x = dataset[,"AgeDifference"]
plot(x, y, main = "Duration vs Age difference",xlab = 'Age Difference (Years)', ylab = 'Duration (s)')
#
#plotting the reach difference in relation to fight duration plot
x = dataset[,"ReachDifference"]
plot(x, y, main = "Duration vs Reach Difference" ,xlab = 'Reach Difference (cm)', ylab = 'Duration (s)')
#
#plotting the title fight number difference in relation to fight duration plot
x = dataset[,"TitleExperienceDif"]
plot(x, y, main = "Duration vs Title Bouts Difference" ,xlab = 'Title bouts Difference', ylab = 'Duration (s)')

#plotting the boxplots of the Fight duration for each weighclass
x = dataset[,"WeightClass"]
x <- factor(x, levels = c("Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight", "Middleweight", "Light Heavyweight", "Heavyweight"))
boxplot(y ~ x, main = "Duration vs Weightclass",xlab = "Weightclass", ylab = "Duration (s)")
#
#plotting the boxplots of fight duration vs Finish catecory.
x = dataset[,"FinishType"]
boxplot(y ~ x, main = "Duration vs Finish Type",xlab = "Finish", ylab = "Duration (s)")
#
#plotting the boxplots of fight duration vs Year.
x = dataset[,"Year"]
boxplot(y ~ x, main = "Duration vs Year",xlab = "Year", ylab = "Duration (s)")

#3) Fitting 6 models of each predictor vs response in order to observe the
#p-value of each predictor

#evaluate the p-values of each of the models corresponding to each of the predictors used
summary(lm(formula = ContestDuration ~ AgeDifference, data = dataset))$coefficients[,4]
summary(lm(formula = ContestDuration ~ ReachDifference, data = dataset))$coefficients[,4]
summary(lm(formula = ContestDuration ~ TitleExperienceDif, data = dataset))$coefficients[,4]
summary(lm(formula = ContestDuration ~ WeightClass, data = dataset))$coefficients[,4]
summary(lm(formula = ContestDuration ~ FinishType, data = dataset))$coefficients[,4]
summary(lm(formula = ContestDuration ~ Year, data = dataset))$coefficients[,4]

# 4) Fitting one model with all the predictors included vs the response (full model)
fullmodel <- lm(formula = ContestDuration ~ AgeDifference + ReachDifference + TitleExperienceDif + WeightClass + FinishType + Year, data = dataset)
summary(fullmodel)

################################################################################
######### INITIAL MODEL REGRESSION CONDITIONS AND ASSUMPTIONS CHECK ############
################################################################################

#importing relevant packages
require(lattice)
require(ggplot2)
require(gridExtra)
require(grid)
require(tidyverse)
library(car)

#modifying the dataset as to assign numbers to each of the categories of the 3 categorical variables
#this will allow for the creation of pairwise scatterplots between the continuous and discrete (categorical)
#variables in the next step.
datasetnumeric = dataset

datasetnumeric$WeightClass <- as.numeric(factor(dataset$WeightClass))
datasetnumeric$Year <- as.numeric(factor(dataset$Year))
datasetnumeric$FinishType <- as.numeric(factor(dataset$FinishType))

                    ### CHECKING REGRESSION CONDITIONS ###

### CONDITION 2 ###

#plotting pairwise predictor scatterplots for the full model
pairs(datasetnumeric[,2:7], main = "Pairwise Predictors Scatterplots")

### CONDITION 1 ###

#plotting the response vs fitted values (Y_hat) scatterplot
plot(dataset$ContestDuration ~ fitted(fullmodel), xlab = expression(hat(Y)), ylab = "Y", main = expression("Y vs "~hat(Y)))

#line through the scatterplot (solid line)
lines(lowess(fitted(fullmodel), dataset$ContestDuration))

#line of identity function (dotted line)
abline(a = 0, b = 1, lty = 2)

                    ### CHECKING REGRESSION ASSUMPTIONS ###

#extracting the residuals of the fitted full model
residuals <- resid(fullmodel)

#(LINEARITY, HOMOSKEDASTICITY CHECK) plotting the scatterplot of the residuals vs fitted values
plot(residuals ~ fitted(fullmodel), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")

#plotting the residuals vs each predictor
plot1 <- ggplot(dataset, aes(x = Year, y = residuals)) +
  geom_boxplot()
plot2 <- ggplot(dataset, aes(x = ReachDifference, y = residuals), group = expo) +
  geom_point()
plot3 <- ggplot(dataset, aes(x = TitleExperienceDif, y = residuals), group = expo) +
  geom_point()
plot4 <- ggplot(dataset, aes(x = AgeDifference, y = residuals), group = expo) +
  geom_point()
plot5 <- ggplot(dataset, aes(x = WeightClass, y = residuals), group = expo) +
  geom_boxplot()
plot6 <- ggplot(dataset, aes(x = FinishType, y = residuals), group = expo) +
  geom_boxplot()

#placing the above plots in one grid
gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=2)#, main = "Residuals vs Predictors Scatterplots")

#Checking distribution of residuals
plot(density(residuals), main = "PDF of Residuals")

#(NORMALITY CHECK) plotting the normal QQ plot for our fitted model

#scatterplot of normal residuals
qqnorm(residuals, main = "Normal QQ-Plot")
#line through the above
qqline(residuals)

################################################################################
############ MANIPULATING THE MODEL IN ORDER TO IMPROVE IT #####################
################################################################################

#conducting BOX-COX transformation in order to improve normality
#note that we will only power transform continuous numeric variables,
#as raising a categorical variable to a power is not something that makes
#mathematical sense

#adding 10^(-100) to each predictor containing 0 values, as to eliminate the error given by R without 
#affecting the resulting powers (we need nonzero values. By adding such a minsicule number we effectivelly
#get nonzero values, witout practically changning each of the values in the dataset)
powers <- summary(car::powerTransform(as.matrix(cbind(dataset$ContestDuration, dataset$AgeDifference + 10^(-100), dataset$ReachDifference + 10^(-100)))))

#constructing the transformed dataset, with the predictors raised to the appropriate powers given by the BOX-COX transformation
dataset2 <- mutate(dataset, ContestDuration = ContestDuration^(0.5), AgeDifference = AgeDifference^(0.06), ReachDifference = ReachDifference^(0.04), TitleExperienceDif = TitleExperienceDif, Year = Year, FinishType = FinishType, WeightClass = WeightClass)

#Fitting the model with the transformed dataset
fullmodel2 <- lm(formula = ContestDuration ~ AgeDifference + ReachDifference + TitleExperienceDif + WeightClass + FinishType + Year, data = dataset2)

                  ### CHECKING REGRESSION CONDITIONS ###

### CONDITION 2 ###

#assigning a numerical value to each of the categories of each categorical
#predictor
datasetnumeric2 = dataset2

datasetnumeric2$WeightClass <- as.numeric(factor(dataset2$WeightClass))
datasetnumeric2$Year <- as.numeric(factor(dataset2$Year))
datasetnumeric2$FinishType <- as.numeric(factor(dataset2$FinishType))

#plotting pairwise predictor scatterplots for the full model
pairs(datasetnumeric2[,2:7], main = "BOX-COX: Paiwise Predictors Scatterplot")

### CONDITION 1 ###

#plotting the response vs fitted values (Y_hat) scatterplot
plot(dataset2$ContestDuration ~ fitted(fullmodel2), xlab = expression(hat(Y)), ylab = "Y", main = expression("BOX-COX: Y vs "~hat(Y)))

#line through the scatterplot (solid line)
lines(lowess(fitted(fullmodel2), dataset2$ContestDuration))

#line of identity function (dotted line)
abline(a = 0, b = 1, lty = 2)

                   ### CHECKING REGRESSION ASSUMPTIONS ###

#extracting the residuals of the fitted full model
residuals2 <- resid(fullmodel2)

#(LINEARITY, HOMOSKEDASTICITY CHECK) plotting the scatterplot of the residuals vs fitted values
plot(residuals2 ~ fitted(fullmodel2), xlab = "Fitted Values", ylab = "Residuals", main = "BOX-COX: Residuals vs Fitted Values")

#plotting the residuals vs each predictor
plot1 <- ggplot(dataset2, aes(x = Year, y = residuals2)) +
  geom_boxplot()
plot2 <- ggplot(dataset2, aes(x = ReachDifference, y = residuals2), group = expo) +
  geom_point()
plot3 <- ggplot(dataset2, aes(x = TitleExperienceDif, y = residuals2), group = expo) +
  geom_point()
plot4 <- ggplot(dataset2, aes(x = AgeDifference, y = residuals2), group = expo) +
  geom_point()
plot5 <- ggplot(dataset2, aes(x = WeightClass, y = residuals2), group = expo) +
  geom_boxplot()
plot6 <- ggplot(dataset2, aes(x = FinishType, y = residuals2), group = expo) +
  geom_boxplot()

#placing the above plots in one grid
gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=2)#, main = "BOX-COX: Residuals vs Predictors Scatterplot")

#Checking distribution of residuals
plot(density(residuals2), main = "BOX-COX: PDF of Residuals")

#(NORMALITY CHECK) plotting the normal QQ plot for our fitted model
#scatterplot of normal residuals
qqnorm(residuals2, main = "BOX-COX: Normal QQ-Plot")
#line through the above
qqline(residuals2)

################################################################################
#######################ATTEMPTING TO REDUCE THE MODEL###########################
################################################################################

summary(fullmodel2)

#our pool of variables available for removal is constituted of the variables:
#AgeDifference, TitleExperienceDifference, as they are the only ones that seem to
#have insignificant effects on the model.

#constructing a dataset which does not include the variable we wish to remove
reduceddataset <- subset(dataset2, select = -AgeDifference)

#We fit the model with the 5 remaining predictors 
reducedmodel <- lm(formula = ContestDuration ~ ReachDifference + WeightClass + TitleExperienceDif + FinishType + Year, data = reduceddataset)

#conduct and F-test in order to observe the results
anova(reducedmodel, fullmodel2)

#the F-statistic indicates that the variable can be removed. We thus remove the 
#variable and fit a new model with 5 instead the of 6 original predictors

### CHECKING REGRESSION CONDITIONS ###

### CONDITION 2 ###

#assigning a numerical value to each of the categories of each categorical predictor
datasetnumeric3 = reduceddataset

datasetnumeric3$WeightClass <- as.numeric(factor(reduceddataset$WeightClass))
datasetnumeric3$Year <- as.numeric(factor(reduceddataset$Year))
datasetnumeric3$FinishType <- as.numeric(factor(reduceddataset$FinishType))

#plotting pairwise predictor scatterplots for the full model
pairs(datasetnumeric3[,2:6], main = "REDUCED MODEL 1: Paiwise Predictors Scatterplot")

### CONDITION 1 ###

#plotting the response vs fitted values (Y_hat) scatterplot
plot(reduceddataset$ContestDuration ~ fitted(reducedmodel), xlab = expression(hat(Y)), ylab = "Y", main = expression("REDUCED MODEL 1: Y vs "~hat(Y)))

#line through the scatterplot (solid line)
lines(lowess(fitted(reducedmodel), reduceddataset$ContestDuration))

#line of identity function (dotted line)
abline(a = 0, b = 1, lty = 2)

### CHECKING REGRESSION ASSUMPTIONS ###

#extracting the residuals of the fitted full model
residuals3 <- resid(reducedmodel)

#(LINEARITY, HOMOSKEDASTICITY CHECK) plotting the scatterplot of the residuals vs fitted values
plot(residuals3 ~ fitted(reducedmodel), xlab = "Fitted Values", ylab = "Residuals", main = "REDUCED MODEL 1: Residuals vs Fitted Values")

#plotting the residuals vs each predictor
plot1 <- ggplot(reduceddataset, aes(x = Year, y = residuals3)) +
  geom_boxplot()
plot2 <- ggplot(reduceddataset, aes(x = ReachDifference, y = residuals3), group = expo) +
  geom_point()
plot3 <- ggplot(reduceddataset, aes(x = TitleExperienceDif, y = residuals3), group = expo) +
  geom_point()
plot5 <- ggplot(reduceddataset, aes(x = WeightClass, y = residuals3), group = expo) +
  geom_boxplot()
plot6 <- ggplot(reduceddataset, aes(x = FinishType, y = residuals3), group = expo) +
  geom_boxplot()

#placing the above plots in one grid
gridExtra::grid.arrange(plot1, plot2, plot3, plot5, plot6, nrow=2)

#Checking distribution of residuals
plot(density(residuals3), main = "REDUCED MODEL 1: PDF of Residuals")

#(NORMALITY CHECK) plotting the normal QQ plot for our fitted model
#scatterplot of normal residuals
qqnorm(residuals3, main = "REDUCED MODEL 1: Normal QQ-Plot")
#line through the above
qqline(residuals3)

################################################################################
##########################REMOVING SECOND PREDICTOR#############################
################################################################################

#we now further remove the ReachDifference variable
reduceddataset1 <- subset(reduceddataset, select = -ReachDifference)

#We fit the model with the 4 remaining predictors
reducedmodel1 <- lm(formula = ContestDuration ~ WeightClass + TitleExperienceDif + FinishType + Year, data = reduceddataset1)

#conduct an F-test in order to observe the results
anova(reducedmodel1, reducedmodel)

#the F-statistic indicates that the variable cannot be removed. We keep the model
#with the 5 predictors.

################################################################################
######IDENTIFYING INFLUENCIAL OBSERVATIONS, LEVERAGE POINTS AND OUTLIERS########
################################################################################



#computing the n and p for our reduced (final) model
n <- length(reduceddataset$ContestDuration)
p <- length(coef(reducedmodel))-1


#LEVERAGE POINTS


#computing hii
h <- hatvalues(reducedmodel)

#determining cutoff threshold
hcut <- 2*(p + 1)/n

#identify which observations are leverage points, given the above
Lp <- which(h > hcut)

#printing the points that are classified as leverage points
length(Lp)


#OUTLIERS


#computing r (standardized residuals)
r <- rstandard(reducedmodel)

#determining which points are outliers given a cutoff of [-4, 4]
#because we have a dataset of more than 1000 observations (large)
OL <- which(r < -4 | r > 4)

#printing the points which are classified as outliers
length(OL)


#INFLUENTIAL OBSERVATIONS


# cooks distance

CD <- cooks.distance(reducedmodel)

#determining the cutoff for cooks distance
CDcut <- qf(0.5, p + 1, n - p - 1)

#comparing cooks distance to cutoff
which(CD > CDcut)


# DFFITS


DFS <- dffits(reducedmodel)

#determining cutoff
DFScut <- 2*sqrt((p + 1)/n)

#comparing DFFITS to cutoff value
W <- which(abs(DFS) > DFScut)


#DFBETAS


#computing DFBETAS
DFB <- dfbetas(reducedmodel)

#determining cutoff value for DFBETAS
DFBcut <- 2/sqrt(n)

#comparing DFBETAS to cutoff value (5 predictors => 6 such comparisons)
W1 <- which(abs(DFB[,1]) > DFBcut)
W2 <- which(abs(DFB[,2]) > DFBcut)
W3 <- which(abs(DFB[,3]) > DFBcut)
W4 <- which(abs(DFB[,4]) > DFBcut)
W5 <- which(abs(DFB[,5]) > DFBcut)
W6 <- which(abs(DFB[,6]) > DFBcut)

#saving all points which were evaluated as INFLUENTIAL OBSERVATIONS
IO <- unique(c(W, W1, W2, W3, W4, W5, W6))

#printing the number of points that are classified as infuential
length(IO)

#printing the number of influential points that are the same as the 
#leverage points found earlier
k = 0

for (i in 1:length(IO)){
  for (j in 1:length(Lp)){
    if (Lp[j] == IO[i]){
      k = k + 1
    }
  }
}

k

#plotting these observations for the response vs each predictor.
plot(reduceddataset[, 1] ~ reduceddataset[, 2], main = "ContestDuration vs ReachDifference", xlab = "ReachDifference", ylab = "ContestDuration")
points(reduceddataset[Lp, 1] ~ reduceddataset[Lp, 2], col = 'red') #leverage points
points(reduceddataset[OL, 1] ~ reduceddataset[OL, 2], col = 'green') #outliers
points(reduceddataset[W, 1] ~ reduceddataset[W, 2], col = 'blue') #influential observations

plot(reduceddataset[, 1] ~ reduceddataset[, 3], main = "ContestDuration vs TitleExperienceDif", xlab = "", ylab = "ContestDuration")
points(reduceddataset[Lp, 1] ~ reduceddataset[Lp, 3], col = 'red')
points(reduceddataset[OL, 1] ~ reduceddataset[OL, 3], col = 'green')
points(reduceddataset[W, 1] ~ reduceddataset[W, 3], col = 'blue')

plot(reduceddataset[, 1] ~ as.factor(reduceddataset[, 4]), main = "ContestDuration vs Year", xlab = "Year", ylab = "ContestDuration")
points(reduceddataset[Lp, 1] ~ as.factor(reduceddataset[Lp, 4]), col = 'red')
points(reduceddataset[OL, 1] ~ as.factor(reduceddataset[OL, 4]), col = 'green')
points(reduceddataset[W, 1] ~ as.factor(reduceddataset[W, 4]), col = 'blue')

plot(reduceddataset[, 1] ~ as.factor(reduceddataset[, 5]), main = "ContestDuration vs FinishType", xlab = "FinishType", ylab = "ContestDuration")
points(reduceddataset[Lp, 1] ~ as.factor(reduceddataset[Lp, 5]), col = 'red')
points(reduceddataset[OL, 1] ~ as.factor(reduceddataset[OL, 5]), col = 'green')
points(reduceddataset[W, 1] ~ as.factor(reduceddataset[W, 5]), col = 'blue')

plot(reduceddataset[, 1] ~ as.factor(reduceddataset[, 6]), main = "ContestDuration vs Weightclass", xlab = "Weightclass", ylab = "ContestDuration")
points(reduceddataset[Lp, 1] ~ as.factor(reduceddataset[Lp, 6]), col = 'red')
points(reduceddataset[OL, 1] ~ as.factor(reduceddataset[OL, 6]), col = 'green')
points(reduceddataset[W, 1] ~ as.factor(reduceddataset[W, 6]), col = 'blue')

################################################################################
##########################CHECKING FOR MULTICOLINEARITY#########################
################################################################################

#checking the colinearity of our reduced model
vif(reducedmodel)

################################################################################
###################CREATING OUTPUT AND CONFIDENCE INTERVALS#####################
################################################################################

#coefficient values
summary(reducedmodel)$coefficients

#95% CIs for the coefficients
confint(reducedmodel, level = 0.95)