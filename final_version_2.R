# Set up the directory path
setwd(dir = "E:/Alok-Cy Tech/Semester 3/Datamining approach/Exam")

getwd()


###############*********Exercise 1 "Analyzing variables" **********################
###############*********Question 1**********################

# Read the data from the titanic.csv file. We call Titanic the obtained data frame.

titanic<- read.csv("titanic.csv",sep = ',',stringsAsFactors = F)

head((titanic),10)

str(titanic)
###############*********Question 2**********###############
# Represent by a pie chart the proportion of Titanic survivors/non survivors.
prop.table(table(titanic$Survived))


table(titanic$Survived) # BY using "table" function we count the number of  0, 1.
# Here 0 means "non survivors" and 1 means "survivors"
# Plotting With Pie

slices<-c(342, 549)
lbls <-c("Survivors", "Non survivors")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"% ",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="The proportion of Titanic survivors/non survivors")

###############**********Question 3*********################
#  Represent by a barplot the influence of the value of the Pclass variable on that of Survived.

table(titanic$Pclass)

prop.table(
  table(titanic$Pclass))

counts <- table(titanic$Survived, titanic$Pclass )
barplot(counts, ylab="No Of passengers", main="The Pclass variable that of Survived", 
        names.arg=c("1st class", "2nd class", "3rd cass"),
        col=c("darkblue","red"),border="white",
        legend = c("Survivors", "Non Survivors"), beside = TRUE)


###############*********Question 4**********################

#Represent by a bar plot the influence of the value of the Sex variable on that of Survived.

table(titanic$Sex)

counts2 <- table(titanic$Survived, titanic$Sex )
barplot(counts2, ylab="No Of passengers", main="The Sex variable on that of Survived", 
        names.arg=c("Female", "Male"),
        col=c("green","red"),border="white",
        legend = c("Survivors", "Non Survivors"), beside = TRUE)


###############*********Question 5.(a)**********################
# Create a new data frame Passengers Without Age containing passengers for whom we don't a value the Age variable.

# Count the missing value as TRUE of Age variable

missing.age <- is.na(titanic$Age)

table(missing.age)


newdata <- na.omit(titanic$Age)

newdata

# This is the function which one create where age value are missing.


new_DF <- subset.data.frame(titanic, is.na(titanic$Age))
head((new_DF),10)
str(new_DF)

#Print the top 20 date of the data frame
head((new_DF),20)


###############*********Question 5.(b)**********################
#Find passengers having non age value and for whom the Parch variable is equal to 0. We will suppose that these passengers are children.

new_DF$Parch

children<- subset.data.frame(new_DF,new_DF$Parch==0)
children

table(children)

head(children)

###############*********Question 5.(c)**********################
#Remove passengers having no age and for whom Parch != 0.

parch_0 <- subset.data.frame(children, children$Parch==0)

head(parch_0)


###############********Question 5.(d)***********################
#Add a new variable AgeGroup having two possible values: Child and Adult. We will consider that a passenger is an adult if he/she is at least 17 years old.

extractAgeGroup <-function(age)
{
  if (is.na(age)) return ("NULL") 
  else if (age <= 17) return("Child") 
  else return ("Adult")
}

AgeGroups <- NULL

for(i in 1:nrow(titanic) )
{
  AgeGroups <- c(AgeGroups, 
                 extractAgeGroup(titanic[i,"Age"]))
  
}

titanic$AgeGroup = as.factor(AgeGroups)

table(titanic$AgeGroup)
str(titanic)

###############**********Question 5.(e)*********################
# Represent by a bar plot the influence of the value of the AgeGroup variable on that of Survived.

table(titanic$AgeGroup)

counts5 <- table(titanic$Survived, titanic$AgeGroup)

barplot(counts5, ylab="No Of passengers", main="The AgeGroup variable on that of Survived", 
        names.arg=c("Adult", "Child", "NULL"),
        col=c("darkblue","red"),border="white",
        legend = c("Survivors", "Non Survivors"), beside = TRUE)


###############*********Question 6 **********################
# Add a new variable Family to this dataframe having two possible values : Alone and WithAFamily

extractFamily <- function(sibsp, parch)
{
  if (sibsp+parch==0) return("Alone ")
  else return("WithAFamily")
}

#Adding the variable Family

Family <- NULL
for(i in 1:nrow(titanic))
{
  Family <- c(Family, extractFamily(titanic[i,"SibSp"],titanic[i,"Parch"]))
}
titanic$Family = as.factor(Family)
table(titanic$Family)
str(titanic)

###############*********Question 7 **********################
#Represent by a barplot the influence of the value of the Family variable on that of Survived
counts6 <- table(titanic$Survived, titanic$Family)
counts6
barplot(counts6, ylab="No Of passengers", main="the Family variable on that of Survived", 
        names.arg=c("Alone","With A Family"),
        col=c("darkblue","red"),border="white",
        legend = c("Survivors", "Non Survivors"), beside = TRUE)

###############*********Question 8 **********################
#Add a new variable Cabin Group having as value the first letter of the value of the variable Cabin if it is not empty, and "X" otherwise

titanic$CabinGroup <- substr(titanic$Cabin, 1, 1)

for(i in 1:nrow(titanic))
{
  if (titanic[i,"CabinGroup"]=="") titanic[i,"CabinGroup"]="X"
}

titanic$CabinGoup <- as.factor(titanic$CabinGroup)

titanic$CabinGroup

table(titanic$CabinGroup)
str(titanic)

###############*********Question 9 **********################
#Represent by a barplot the influence of the value of the Cabin Group variable on that of Survived.

counts7 <- table(titanic$Survived, titanic$CabinGroup)
counts7

barplot(counts7, ylab="No Of passengers", main="The Cabin Group variable on that of Survived", 
        names.arg=c("A","B","C","D","E","F","G","T","X"),
        col=c("darkblue","red"),border="white",
        legend = c("Survivors", "Non Survivors"), beside = TRUE)

###############*********Question 10 **********################

#Represent by a barplot the influence of the value of the Embarked variable on that of Survived.

table(titanic$Embarked)

counts10 <- table(titanic$Survived, titanic$Embarked)

counts10

barplot(counts10, ylab="No Of passengers", main="The Embarked variable on that of Survived", 
        names.arg=c("NULL","C", "Q", "S"),
        col=c("purple","green"),border="white",
        legend = c("Survivors", "Non Survivors"), beside = TRUE)
#############################################################################################################################

###############*********Exercise 2 "Building and analysing random forests" **********################

###############**********some extra function for random forest****************##############
###############*#Functions to define the variables Title****************##############

extractTitle <- function(sex, name)
{
  name = as.character(name)
  
  if (sex=="male")
  {
    if (length(grep("Mr.",name))>0)
    {
      return ("Mr")
    }
    else
    {
      return ("Other-M")
    }
  }
  else
  {
    if (length(grep("Miss",name))>0)
    {
      return ("Miss")
    }
    else if (length(grep("Mrs.",name))>0)
    {
      return ("Mrs")
    }
    else
    {
      return ("Other-F")
    }
  }
}

#Adding the variable Title
titles <- NULL
for(i in 1:nrow(titanic))
{
  titles <- c(titles, extractTitle(titanic[i,"Sex"], titanic[i,"Name"])) 
}
titanic$Title = as.factor(titles)


str(titanic)

###############**************************##############

titanic$Embarked <- as.factor(titanic$Embarked)

titanic$Survived <- as.factor(titanic$Survived)

titanic$Pclass <- as.factor(titanic$Pclass)

titanic$Sex <- as.factor(titanic$Sex)

titanic$AgeGroup <- as.factor(titanic$AgeGroup)

titanic$Family <- as.factor(titanic$Family)

titanic$CabinGroup <- as.factor(titanic$CabinGroup)

###############*********Question 1**********################
#1. Split your dataset into a training set and a test set.


devide<- sample(2, nrow(titanic), replace = T, prob = c(0.8, 0.2))

table(devide)

length(devide)

## For checking the ration of split data.
table(devide)/length(devide)

dataset.train<- titanic[devide==1, ]
dataset.test<- titanic[devide==2, ]
###############*********Question 2**********################
#Create a random forest using the default values of its parameters
library(randomForest)

# here we created this as by default parameter
formula= Survived~Pclass+Sex+AgeGroup+Family+Fare+CabinGroup+Title+Embarked


rf<-randomForest(formula ,data=dataset.train)
rf
###############*********Question 3**********################
#Evaluate the quality of your classifier using two measures.

# Prediction the data set
pred<-predict(rf, newdata = dataset.test)

pred

conf_test<-table(pred, dataset.test$Survived)
conf_test

## first Measures-- classifier of accuracy   
acc<-sum(diag(conf_test))/sum(conf_test)

acc


###2nd measure -- Out of Beg error 

conf_OOB<- rf$confusion

OOB_acc<- sum(diag (conf_OOB))/sum(conf_OOB)
OOB_acc

OOB_err<- 1- OOB_acc

OOB_err

##
###############*********Question 4**********################
# What do you notice when you create and evaluate your RF several times? How can we explain that? How can we solve this "problem"?
library(randomForest)
set.seed(100)
# here we created this as by default parameter
formula_sev= Survived~Pclass+Sex+AgeGroup+Family+Fare+CabinGroup+Title+Embarked

rf_sev<-randomForest(formula_sev ,data=dataset.train)
rf_sev

# Prediction the data set
pred_sev<-predict(rf_sev, newdata = dataset.test)

pred_sev

conf_test_sev<-table(pred_sev, dataset.test$Survived)
conf_test_sev

##Accuracy  
acc_sev<-sum(diag(conf_test_sev))/sum(conf_test_sev)

acc_sev


###############*********Question 5**********################
#Try and compare many values of the RF parameters

## first 
library(randomForest)

rf_2<-randomForest(formula ,data=dataset.train, ntree=300, mtry=4, importance=T)
rf_2

pred_2<-predict(rf_2, newdata = dataset.test)



conf_test_2<-table(pred_2, dataset.test$Survived)


acc_2<-sum(diag(conf_test_2))/sum(conf_test_2)

acc_2

##Second
library(randomForest)

rf_3<-randomForest(formula ,data=dataset.train,ntree=200, mtry=2)
rf_3
pred_3<-predict(rf_3, newdata = dataset.test)



conf_test_3<-table(pred_3, dataset.test$Survived)
conf_test_3

acc_3<-sum(diag(conf_test_3))/sum(conf_test_3)

acc_3

##Third
library(randomForest)

rf_4<-randomForest(formula, data=dataset.train, ntree=100, mtry=6, importance=F,
                   maxnodes = NULL)
rf_4

pred_4<-predict(rf_4, newdata = dataset.test)


conf_test_4<-table(pred_4, dataset.test$Survived)


acc_4<-sum(diag(conf_test_4))/sum(conf_test_4)

acc_4
## Fouth 
library(randomForest)

rf_5<-randomForest(formula, data=dataset.train, ntree=500, mtry=4, importance=T,
                   maxnodes = NULL)
rf_5
pred_5<-predict(rf_5, newdata = dataset.test)


conf_test_5<-table(pred_5, dataset.test$Survived)

acc_5<-sum(diag(conf_test_5))/sum(conf_test_5)

acc_5


###############*********Question 6**********################
# For the best choice of the parameters give and plot the values of the variables' importance.
#In first one, Here I have used "ntree = 300,    mtry = 4, importance = T" and we got the best choice 
# So I am taking rf_2 is my main objective for this answer


rf_2$importance

rf_2$importance[,1]

varImpPlot(rf_2, sort=TRUE, n.var=min(30, nrow(rf_2$importance)),
           type=NULL, class=NULL, scale=TRUE, 
           main=deparse(substitute("Random Forest MDA and MDG comparision"))) 

###############*********Question 2.1 "Extracting and analyzing rules"**********################

###############*********Question 1**********################
#Install and load the library in Trees.

#install.packages("tree")

library(tree)



###############*********Question 2**********################
#Use the functions of this library to extract and analyze the rules contained in your random forest.


formula
tree(rf, data = dataset.train)

model= tree(rf, data = dataset.train)
plot(model)
text(model)

# anoather function of tree 

getTree(rf, k=1, labelVar=FALSE)

getTree(rf, k=2, labelVar=FALSE)


# Here:
#rf- a randomForest object.
#k which- tree to extract
#labelVar-Should better labels be used for splitting variables and predicted class

#### By using rpart and rpart.plot library 

library(rpart)
library(rpart.plot)
fit <- rpart(rf, data = dataset.train, method = 'class')
rpart.plot(fit, extra = 106, main="Dcision Tree")

#############################################################################################################################
###############*********Exercise 3 "Building and analysing a Boosting classifier"**********################

###############*********Question 1**********################

# Create a boosting classifier having 5 weak classifiers.

# default boosting classifier 

library(adabag) 

# here we mfianl is 5 means the no of weak classifier 
c1_Boosting <-
  boosting(formula,data = dataset.train, mfinal=5)

c1_Boosting


###############*********Question 2**********################
#Evaluate the quality of your classifier.

c1_Boosting$trees
##
c1_Boosting$trees[[1]]
# for ploting the boosting classifer 
plot(c1_Boosting$trees[[1]])
text(c1_Boosting$trees[[1]])

##
c1_Boosting$trees[[2]]

plot(c1_Boosting$trees[[2]])
text(c1_Boosting$trees[[2]])

##
c1_Boosting$trees[[4]]

plot(c1_Boosting$trees[[4]])
text(c1_Boosting$trees[[4]])
##
c1_Boosting$trees[[5]]

plot(c1_Boosting$trees[[5]])
text(c1_Boosting$trees[[5]])

##Applying boosting classifier on test set

c3_Boosting <-
  boosting(formula,data = dataset.test, mfinal=5)

c3_Boosting

c3_Boosting$weights

c3_Boosting$trees[[1]]
# for plotting the boosting classifier 
plot(c3_Boosting$trees[[1]])
text(c3_Boosting$trees[[1]])

# 

# For predict the boosting classifier

pred<-predict(c1_Boosting, newdata = dataset.test)
pred
str(pred)
# confusion matrix
myconf_boost<-pred$confusion

myconf_boost

# Find the accuracy of boosting classifier 
acc_boosting<-sum(diag(myconf_boost))/sum(myconf_boost)

acc_boosting



pred$class
# find accuracusing pred$class
myconf_boost_2<-table(pred$class, dataset.test$Survived)
myconf_boost_2

acc_boosting_2<-sum(diag(myconf_boost_2))/sum(myconf_boost_2)

acc_boosting_2

###############*********Question 3**********################
#Give the weight of each weak classifier and analyse its contribution to the final result.

c1_Boosting$weights

pred$prob
