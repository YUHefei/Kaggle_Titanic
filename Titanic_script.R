#----------------------------------------------------------------------------------------------------------------------------#
#                                     Titanic Machine Learning from Disaster                                                 #
#                                     Train Data: train.csv                                                                  #
#                                     Test Data: test.csv                                                                    #
#                                     Author : Yu hefei, Zheng Siya,Guo yang, Wang longling                                  #
#                                     Date : Aug ,2017                                                                       #
#----------------------------------------------------------------------------------------------------------------------------#



#------------------------------------------------------install package-------------------------------------------------------------------#
# Visualization package: "ggplot2","ggthemes","glmnet","scales","plyr","MASS","stringr","InformationValue","MLmetrics","corrplot"        #
# decision tree package: "rpart","randomForest","dplyr","e1071","Amelia","party","gbm","class","caret","lattice"                         #
# other package: "readr","C50","ipred","kernlab","klaR"                                                                                  #
#----------------------------------------------------------------------------------------------------------------------------------------#
packages <- c("readr","C50","ipred","kernlab","klaR",
              "ggplot2","ggthemes","glmnet","scales","plyr","MASS","stringr","InformationValue","MLmetrics","corrplot",
              "rpart","randomForest","dplyr","e1071","Amelia","party","gbm","class","caret","lattice" )
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}


install.packages("car",dependencies = TRUE)
install.packages("caret",dependencies = TRUE)
#----------------------------------------------------------------load library--------------------------------------------------------#
library(caret) # Streamline the process of creating random variable. 
library(readr) # File read / write
library(ggplot2) # Data visualization
library(ggthemes) # Data visualization
library(scales) # Data visualization
library(plyr) # Tools for splitting, applying and combining data
library(stringr) # String manipulation
library(InformationValue) # IV / WOE calculation
library(MLmetrics) # Mache learning metrics.e.g. Recall, Precision, Accuracy, AUC
library(rpart) # Decision tree utils
library(randomForest) # Random Forest
library(dplyr) # Data manipulation
library(e1071) # SVM
library(Amelia) # Missing value utils
library(party) # Conditional inference trees
library(gbm) # AdaBoost
library(class) # KNN
library(corrplot)
library(car)


#------------------------------------------------------------- Set working directory----------------------------------------------------#
setwd("/Users/mdt000mbp/Desktop/My Doc/AS/Group Assignment")

#------------------------------------------------------------- Import data-------------------------------------------------------------#
train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#----------------------------------------------------Exploring and preparing the data-------------------------------------------------#
#label the two data sets
train$IsTrainSet <-TRUE
test$IsTrainSet <-FALSE

#Preview the data frame
dim(train)
dim(test)
names(train)
names(test)

test$Survived <- NA 

#combing the data to facilitate future manipulation and prediction
full<- rbind(train, test)

summary(test)
summary(train)
summary(full)

full$Survived <- factor(full$Survived)

# Impact of Pclass
ggplot(data = full[1:nrow(train),], mapping = aes(x = Pclass, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Pclass') + 
  ylab('Count') + 
  ggtitle('How Pclass impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")


# Impact of title
# Extract title
full$Title <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
data.frame(table(full$Title))

full$Title <- as.character(full$Title)    
full$Title[full$Title %in% c(' Mme', ' Mlle')] <- 'Mlle'
full$Title[full$Title %in% c(' Capt', ' Don', ' Major', ' Sir')] <- 'Sir'
full$Title[full$Title %in% c(' Dona', ' Lady', ' the Countess', ' Jonkheer')] <- 'Lady'
#full$Title[full$Title %in% c(' Col', ' Ms')] <- 'Rare'
full$Title <- factor(full$Title)    

ggplot(data = full[1:nrow(train),], mapping = aes(x = Title, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='stack') + 
  xlab('Title') + 
  ylab('Count') + 
  ggtitle('How title impact survivor') + 
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("Not Survived", "Survived")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#Transfer title categories to numeric values
x=full$Title==" Col"
full$tcode[x]<- "1" 
full$tcode[full$Title==" Dr"]<-"2"
full$tcode[full$Title==" Master"]<-"3"
full$tcode[full$Title==" Miss"]<-"4" 
full$tcode[full$Title==" Mr"]<-"5" 
full$tcode[full$Title==" Mrs"]<-"6" 
full$tcode[full$Title==" Ms"]<-"7"
full$tcode[full$Title==" Rev"]<-"8" 
full$tcode[full$Title=="Lady"]<-"9" 
full$tcode[full$Title=="Mlle"]<-"10" 
full$tcode[full$Title=="Sir"]<-"11" 
full$tcode<-as.numeric(full$tcode) 

#Impact of sex
full$Sex <- as.factor(full$Sex)
ggplot(data = full[1:nrow(train),], mapping = aes(x = Sex, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Sex') + 
  ylab('Count') + 
  ggtitle('How Sex impact survivo') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#Transfer gender categories to numeric values
full$scode[full$Sex=="male"]<-"1" 
full$scode[full$Sex=="female"]<-"0" 
full$scode<-as.numeric(full$scode) 

#Impact of gender and class combination
full$PclassAndSex = 1
full[full$Sex=="female",]$PclassAndSex <-sapply(full[full$Sex=="female",]$Pclass,function(x) if(x==1) as.character("female_high_class") else if(x==2) as.character("female_middle_class") else as.character("female_low_class"))
full[full$Sex=="male",]$PclassAndSex <-sapply(full[full$Sex=="male",]$Pclass,function(x) if(x==1) as.character("male_high_class") else if(x==2) as.character("male_middle_class")else as.character("male_low_class")) 
full$PclassAndSex<-as.factor(full$PclassAndSex)

#Transfer gender and class combination categories to numeric values
full$pcode[full$PclassAndSex=="male_low_class"]<-"1" 
full$pcode[full$PclassAndSex=="male_high_class"]<-"2" 
full$pcode[full$PclassAndSex=="female_low_class"]<-"3" 
full$pcode[full$PclassAndSex=="female_high_class"]<-"4" 
full$pcode[full$PclassAndSex=="female_middle_class"]<-"5" 
full$pcode[full$PclassAndSex=="male_middle_class"]<-"6" 
full$pcode<-as.numeric(full$pcode) 

#Impact of siblings 
ggplot(data = full[1:nrow(train),], mapping = aes(x = SibSp, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "How SibSp impact survivor", x = "Sibsp", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#Impact of parents
ggplot(data = full[1:nrow(train),], mapping = aes(x = Parch, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "How Parch impact survivor", x = "Parch", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#Imapct of family size 
full$FamilySize <- full$SibSp + full$Parch + 1
ggplot(data = full[1:nrow(train),], mapping = aes(x = FamilySize, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('FamilySize') + 
  ylab('Count') + 
  ggtitle('How FamilySize impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#Impact of cabin
#  (Divide by Cabin category)
full$Cabin <- sapply(full$Cabin, function(x) str_sub(x, start = 1, end = 1))
full$Survived <- factor(full$Survived)
ggplot(full[1:nrow(train), ], mapping = aes(x = as.factor(sapply(full$Cabin[1:nrow(train)], function(x) str_sub(x, start = 1, end = 1))), y = ..count.., fill = Survived)) +  geom_bar(stat = 'count', position='dodge') +   xlab('Cabin') +  ylab('Count') +  ggtitle('How cabin impact survivor') +  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) +   theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
full$Cabin <- factor(full$Cabin)

#Transfer catorical value to numeric value
full$ccode[full$Cabin==""]<-"1" 
full$ccode[full$Cabin=="A"]<-"2" 
full$ccode[full$Cabin=="B"]<-"3" 
full$ccode[full$Cabin=="C"]<-"4" 
full$ccode[full$Cabin=="D"]<-"5" 
full$ccode[full$Cabin=="E"]<-"6" 
full$ccode[full$Cabin=="F"]<-"7" 
full$ccode[full$Cabin=="G"]<-"8" 
full$ccode[full$Cabin=="T"]<-"9" 
full$ccode<-as.numeric(full$ccode) 

#Impact of small families(if a person satisfy both two conditions: 1, family size <=1; 2, Surname frequency <=2, he will be considered in small family group)
# Extract Surname
full$Surname <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
data.frame(table(full$Surname))
full$Surname <- factor(full$Surname) 


#Impact of surname frequency
full$FamilyID <- paste(as.character(full$FamilySize),full$Surname,sep=" ")
SurnameFreq <- data.frame(table(full$Surname))
full$Surnamefreq <- SurnameFreq[(full$Surname %in% SurnameFreq$Var1),"Freq"]
ggplot(full[1:nrow(train), ], mapping = aes(x = as.factor(sapply(full$Surnamefreq[1:nrow(train)], function(x) str_sub(x, start = 1, end = 1))), y = ..count.., fill = Survived)) +  geom_bar(stat = 'count', position='dodge') +   xlab('SurnameFreq') +  ylab('Count') +  ggtitle('How surname frequency impact survivor') +  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) +   theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")


# Impact of Family ID(If(Familysize<2 and SurnameFre<2, then FamilyID=small, otherwise FamilyID=Big))
full$FamilyID <- paste(as.character(full$FamilySize),full$Surname,sep="")
full$FamilyID[full$FamilySize < 2] <- 'Small'
famIDs <- data.frame(table(full$FamilyID))
famIDs <- famIDs[famIDs$Freq < 2,]
full$FamilyID[!full$FamilyID %in% famIDs$Var1] <- 'Big'
full$FamilyID[full$FamilyID %in% famIDs$Var1] <- 'Small'

full$FamilyID <- factor(full$FamilyID)
ggplot(full[1:nrow(train), ], mapping = aes(x = as.factor(sapply(full$FamilyID[1:nrow(train)], function(x) str_sub(x, start = 1, end = 1))), y = ..count.., fill = Survived)) +  geom_bar(stat = 'count', position='dodge') +   xlab('FamilyID') +  ylab('Count') +  ggtitle('How familyID impact survivor') +  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) +   theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") 
#Transfer catorical value to numeric value
full$fcode[full$FamilyID=="Big"]<-"1" 
full$fcode[full$FamilyID=="Small"]<-"2" 
full$fcode<-as.numeric(full$fcode) 


#Impact of ticket number
ticket.count <- aggregate(full$Ticket, by = list(full$Ticket), function(x) sum(!is.na(x)))
full$TicketCount <- apply(full, 1, function(x) ticket.count[which(ticket.count[, 1] == x['Ticket']), 2])
full$TicketCount <- factor(sapply(full$TicketCount, function(x) ifelse(x > 1, 'Share', 'Unique')))

ggplot(data = full[1:nrow(train),], mapping = aes(x = TicketCount, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('TicketCount') + 
  ylab('Count') + 
  ggtitle('How TicketCount impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
WOETable(X=full$TicketCount[1:nrow(train)], Y=full$Survived[1:nrow(train)])
IV(X=full$TicketCount[1:nrow(train)], Y=full$Survived[1:nrow(train)])

#Transfer catorical value to numeric value
full$ttcode[full$TicketCount=="Unique"]<-"1" 
full$ttcode[full$TicketCount=="Share"]<-"2" 
full$ttcode<-as.numeric(full$ttcode) 

#Predict fare and impact
full[is.na(full$Fare),"Fare"]
boxplot(full$Fare)
boxplot.stats(full$Fare)
upper.whisker<-boxplot.stats(full$Fare)$stats[5]
outlier.filter<-full$Fare<upper.whisker
full.outlier<-full[outlier.filter,]
class.filter<-full.outlier$Pclass == 3
full.class<-full.outlier[class.filter,]
embark.filter<-full.class$Embarked =="S"
full.class[embark.filter,]
fare.equation="Fare~ Sex + SibSp + Parch + FamilySize + Cabin + FamilyID"
fare.model<-lm( formula = fare.equation, data = full.class[embark.filter,])
fare.row<-full[is.na(full$Fare),c("Sex","SibSp","Parch","FamilySize", "Cabin","FamilyID")]
fare.prediction<-predict(fare.model,newdata = fare.row)
full[is.na(full$Fare),"Fare"]<- fare.prediction

ggplot(data = full[(!is.na(full$Fare)) & row(as.matrix(full[, 'Fare'])) <= 891, ], aes(x = Fare, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=10)  + 
  labs(title = "How Fare impact survivor", x = "Fare", y = "Count", fill = "Survived")


#Impact of embarked
tapply(full$Embarked, full$Pclass, median, na.rm=TRUE)
full[c(62, 830), 'Embarked']  #S is the most frequent
full$Embarked[c(62, 830)] <- 'S'  #Change the two blank Value to S
ggplot(full[1:891,], aes(Embarked, fill = factor(Survived))) + 
  geom_bar(stat = "count") +
  labs(title = "How Embarked impacts survival")

full$ecode[full$Embarked=="C"]<-"1" 
full$ecode[full$Embarked=="Q"]<-"2" 
full$ecode[full$Embarked=="S"]<-"3" 
full$ecode<-as.numeric(full$ecode)

#Categorical casting 
full$Pclass<-as.numeric(full$Pclass)
full$Sex<-as.factor(full$Sex)
full$Embarked<-as.factor(full$Embarked)
full$TicketCount<-as.factor(full$TicketCount)
full$Cabin<-as.factor(full$Cabin)

#Predict age
age.model <- randomForest(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + Cabin, data=full[!is.na(full$Age),], method="anova")
full$Age[is.na(full$Age)] <- predict(age.model, full[is.na(full$Age),])
#Impact of age
ggplot(full[1:891,], aes(Age, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=2)  + 
  labs(title = "How Age impact survivor", x = "Age", y = "Count", fill = "Survived")

#Split age
full$Splitage[full$Age < 5] <- 'Weak'
full$Splitage[full$Age >= 5 & full$Age < 25] <- 'Strong'
full$Splitage[full$Age >= 25& full$Age < 47] <- 'Middle'
full$Splitage[full$Age >= 47] <- 'Old'
table(full$Splitage, full$Survived)
full$Splitage  <- factor(full$Splitage)

#Impact of mother 
full$Mother <- 'NotMother'
full$Mother[full$Sex =='female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
table(full$Mother, full$Survived)
full$Mother <- factor(full$Mother)
full$mcode[full$Mother=="NotMother"]<-"1" 
full$mcode[full$Mother=="Mother"]<-"2" 
full$mcode<-as.numeric(full$mcode)

#Split dataset back out into train and test
train<-full[full$IsTrainSet==TRUE,]
test<-full[full$IsTrainSet==FALSE,]



#Check the correlation between the attributes. Values above 0.75 or below -0.75 are indicative of high positive or high negative correlation. 
str(full)
full$SibSp=as.numeric(full$SibSp)
full$Parch=as.numeric(full$Parch)
full$Surnamefreq=as.numeric(full$Surnamefreq)
cor(full[c(3,6,7,8,10,15,16,18,19,20,24,26,27,30)])
str(full[c(3,6,7,8,10,15,16,18,19,20,24,26,27,30)])
#From the above results, variables are not highly correlated in this dataset.

#------------------------------------------------Train models on the data and eveluate model performance---------------------------------------#
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

# We????ll try some linear and non-linear algorithms:
# Linear Algorithms: Logistic Regression (LG), Linear Discriminate Analysis (LDA) and Regularized Logistic Regression (GLMNET).
# Non-Linear Algorithms: k-Nearest Neighbors (KNN), Classification and Regression Trees (CART), Naive Bayes (NB) and Support Vector Machines with Radial Basis Functions (SVM).

# LG
set.seed(7)
fit.glm <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="glm", metric=metric, trControl=trainControl)
# LDA
set.seed(7)
fit.lda <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="lda", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="glmnet", metric=metric,
                    trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="rpart", metric=metric,
                  trControl=trainControl)
# Naive Bayes
set.seed(7)
fit.nb <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="nb", metric=metric, trControl=trainControl)

# SVM
set.seed(7)
fit.svm <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="svmRadial", metric=metric,
                 trControl=trainControl)
#Compare algorithms
results <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn,
                          CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(results)

#Accuracy 
#           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#LG     0.6966292 0.7759051 0.8000000 0.7991295 0.8291511 0.8863636    0
#LDA    0.7078652 0.7865169 0.7977528 0.7965077 0.8263733 0.8636364    0
#GLMNET 0.7000000 0.7871099 0.8089888 0.8017472 0.8314607 0.8863636    0
#KNN    0.6404494 0.6901813 0.7401047 0.7328317 0.7771536 0.8089888    0
#CART   0.7303371 0.7865169 0.8044944 0.8058545 0.8398876 0.8876404    0
#NB     0.7159091 0.7422909 0.7696629 0.7732945 0.8089888 0.8314607    0
#SVM    0.7303371 0.8022472 0.8202247 0.8193296 0.8426966 0.8988764    0

#Kappa 
#            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#LG     0.3726542 0.5179637 0.5799811 0.5713027 0.6294404 0.7522523    0
#LDA    0.3589041 0.5361290 0.5662383 0.5642002 0.6185762 0.6993166    0
#GLMNET 0.3450135 0.5354032 0.5908467 0.5762058 0.6392331 0.7522523    0
#KNN    0.1932011 0.3573118 0.4414709 0.4255634 0.5106113 0.5931702    0
#CART   0.4157549 0.5237067 0.5648993 0.5770820 0.6506553 0.7620321    0
#NB     0.3475682 0.4097461 0.4721113 0.4794834 0.5635997 0.6240496    0
#SVM    0.4352195 0.5800645 0.6170892 0.6144639 0.6658990 0.7870247    0

#Plot the results
dotplot(results)


# Apply a Box-Cox transform to flatten out the distribution.
# Compare algorithms
# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# LG
set.seed(7)
fit.glm <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="glm", metric=metric, preProc=c("BoxCox"),
                 trControl=trainControl)
# LDA
set.seed(7)
fit.lda <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="lda", metric=metric, preProc=c("BoxCox"),
                 trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="glmnet", metric=metric,
                    preProc=c("BoxCox"), trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="knn", metric=metric, preProc=c("BoxCox"),
                 trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="rpart", metric=metric,
                  preProc=c("BoxCox"), trControl=trainControl)
# Naive Bayes
set.seed(7)
fit.nb <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="nb", metric=metric, preProc=c("BoxCox"),
                trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="svmRadial", metric=metric,
                 preProc=c("BoxCox"), trControl=trainControl)
# Compare algorithms
transformResults <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn,
                                   CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(transformResults)
# Accuracy 
#           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#LG     0.7000000 0.7780899 0.8089888 0.7980056 0.8217228 0.8651685    0
#LDA    0.6853933 0.7777778 0.7977528 0.7946558 0.8179463 0.8651685    0
#GLMNET 0.7159091 0.7865169 0.7988764 0.7987549 0.8197140 0.8636364    0
#KNN    0.6179775 0.6713802 0.7022472 0.7100134 0.7528090 0.8111111    0
#CART   0.7303371 0.7865169 0.8044944 0.8058545 0.8398876 0.8876404    0
#NB     0.6818182 0.7303371 0.7640449 0.7586704 0.7932201 0.8333333    0
#SVM    0.7303371 0.8005618 0.8323970 0.8249226 0.8539326 0.8988764    0

#Kappa 
#           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#LG     0.3378747 0.5156318 0.5931702 0.5687184 0.6181702 0.7111952    0
#LDA    0.3378747 0.5250267 0.5637499 0.5603598 0.6123580 0.7176097    0
#GLMNET 0.3840985 0.5401142 0.5756089 0.5679661 0.6176673 0.6993166    0
#KNN    0.1528555 0.2879387 0.3577639 0.3736423 0.4774990 0.5876011    0
#CART   0.4157549 0.5237067 0.5648993 0.5770820 0.6506553 0.7620321    0
#NB     0.2820513 0.3712645 0.4704536 0.4519852 0.5250975 0.6361186    0
#SVM    0.4414226 0.5770368 0.6363686 0.6248568 0.6853413 0.7870247    0

#Plot the results 
dotplot(results)


# We can have a look at 4 ensemble methods:
#   
# Bagging: Bagged CART (BAG) and Random Forest (RF).
# Boosting: Stochastic Gradient Boosting (GBM) and C5.0 (C50).

# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# Bagged CART
set.seed(7)
fit.treebag <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="treebag", metric=metric,preProc=c("BoxCox"),
                     trControl=trainControl)
# Random Forest
set.seed(7)
fit.rf <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="rf", metric=metric, preProc=c("BoxCox"),
                trControl=trainControl)
# Stochastic Gradient Boosting
set.seed(7)
fit.gbm <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="gbm", metric=metric, preProc=c("BoxCox"),
                 trControl=trainControl, verbose=FALSE)
# C5.0
set.seed(7)
fit.c50 <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="C5.0", metric=metric, preProc=c("BoxCox"),
                 trControl=trainControl)
#Compare results
ensembleResults <- resamples(list(BAG=fit.treebag, RF=fit.rf, GBM=fit.gbm, C50=fit.c50))
summary(ensembleResults)

# Accuracy 
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#BAG 0.7303371 0.7759051 0.7988764 0.7988055 0.8179463 0.8977273    0
#RF  0.7078652 0.8022472 0.8314607 0.8234623 0.8444444 0.8977273    0
#GBM 0.7415730 0.8089888 0.8314607 0.8271953 0.8498787 0.8988764    0
#C50 0.7303371 0.8095194 0.8314607 0.8268292 0.8539326 0.8988764    0

#Kappa 
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#BAG 0.4020157 0.5378502 0.5701426 0.5700183 0.6019531 0.7807309    0
#RF  0.3670678 0.5771472 0.6327373 0.6162738 0.6648865 0.7757644    0
#GBM 0.4575342 0.5824701 0.6369323 0.6284945 0.6779905 0.7870247    0
#C50 0.4288770 0.5900923 0.6357419 0.6273962 0.6835022 0.7846195    0
#Plot
dotplot(ensembleResults)

#Tuning SVM model 

#The SVM implementation has two parameters that we can play with.  
#The sigma: a smoothing term, 
#The C: cost constraint.

# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 10, by=1))
fit.svm <- train(Survived~., data=train[c(2,3,6,7,8,10,15,16,18,19,20,24,26,27,30)], method="svmRadial", metric=metric, tuneGrid=grid,
                 preProc=c("BoxCox"), trControl=trainControl)
print(fit.svm)
# Support Vector Machines with Radial Basis Function Kernel 

#891 samples
#14 predictor
#2 classes: '0', '1' 

#Pre-processing: Box-Cox transformation (7) 
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 802, 801, 802, 802, 803, 802, ... 
#Resampling results across tuning parameters:

#  sigma  C   Accuracy   Kappa    
#0.025   1  0.8335329  0.6433075
#0.025   2  0.8324093  0.6404208
#0.025   3  0.8331668  0.6419237
#0.025   4  0.8312856  0.6372897
#0.025   5  0.8309069  0.6359743
#0.025   6  0.8282935  0.6304750
#0.025   7  0.8286766  0.6314225
#0.025   8  0.8279190  0.6295103
#0.025   9  0.8267912  0.6269989
#0.025  10  0.8249311  0.6228956
#0.050   1  0.8305450  0.6362687
#0.050   2  0.8275320  0.6293711
#0.050   3  0.8230542  0.6195774
#0.050   4  0.8193088  0.6103811
#0.050   5  0.8200580  0.6116951
#0.050   6  0.8204493  0.6115382
#0.050   7  0.8234498  0.6175531
#0.050   8  0.8238244  0.6180519
#0.050   9  0.8264419  0.6238688
#0.050  10  0.8253225  0.6214785
#0.100   1  0.8215434  0.6166546
#0.100   2  0.8212025  0.6130388
#0.100   3  0.8223177  0.6152577
#0.100   4  0.8163336  0.6031798
#0.100   5  0.8129669  0.5954187
#0.100   6  0.8114813  0.5916998
#0.100   7  0.8084765  0.5850388
#0.100   8  0.8096002  0.5877959
#0.100   9  0.8084891  0.5860016
#0.100  10  0.8114855  0.5929343
#0.150   1  0.8192962  0.6106993
#0.150   2  0.8140904  0.5991889
#0.150   3  0.8107237  0.5918708
#0.150   4  0.8077358  0.5851296
#0.150   5  0.8084890  0.5875903
#0.150   6  0.8118557  0.5956381
#0.150   7  0.8118558  0.5962959
#0.150   8  0.8099873  0.5922232
#0.150   9  0.8092424  0.5904366
#0.150  10  0.8099874  0.5919317

#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were sigma = 0.025 and C = 1.
plot(fit.svm)


#Predict the test dataset(exclude cabbin and ttcode)
model<-svm(Survived~.,data=train[c(2,3,6,7,8,10,15,16,18,20,24,26,27,30)])
testData<-test[c(3,6,7,8,10,15,16,18,20,24,26,27,30)]
preprocessParams <- preProcess(testData, method=c("BoxCox"))
testData <- predict(preprocessParams, testData)
predictions <- predict(model, testData, type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = predictions)
write.csv(submit, file = "Submission.csv", row.names = FALSE)
#------------------------------------------------------------------  end ------------------------------------------------------------------#


