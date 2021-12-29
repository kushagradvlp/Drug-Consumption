library(car)
require(plm)
library("formattable")
library(stargazer)
# install.packages("corrplot")
# install.packages("car")
library(corrplot)
library(car)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(GGally)
library(ggplot2)
library(MASS)
library(klaR)
library(tibble)
library(cvms)
library(gbm)

consumption = read.csv("Drug_Consumption.csv") 
attach(consumption)


consumption_numeric <- consumption[c('Nscore', 
                            'Escore', 
                            'Oscore', 
                            'AScore', 
                            'Cscore', 
                            'Impulsive', 
                            'SS')] 
consumption_categorical<-consumption[,c('Age',
                                  'Gender',
                                  'Education',
                                  'Country',
                                  'Ethnicity',
                                  'Choc',
                                  'Caff',
                                  'Alcohol',
                                  'Nicotine')]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#   
#   bar graph for the categorical columns 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  

par(mfrow=c(3,3)) 
#axis(1, at = 0:1) 
for(i in 1:ncol(consumption_categorical)){ 
  counts<-table((consumption_categorical)[i]) 
  print(counts)
  barplot(counts,xlab=names(consumption_categorical)[i],col="light green",label=TRUE,plot = TRUE) 
} 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#   
#   histogram for the continuous columns 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  

#windows(width=50,height=50) 
par(mfrow=c(2,4)) 

for(i in 1:ncol(consumption_numeric)){ 
  hist(consumption_numeric[[i]] ,main=names(consumption_numeric)[i],xlab=names(consumption_numeric)[i],col="#E0FFFF",label=TRUE,plot = TRUE) 
} 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#    Boxplot for continuous variables 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
par(mfrow=c(2,4)) 
for(i in 1:7){ 
  boxplot((consumption_numeric)[i], xlab=names(consumption_numeric)[i], col = 'light blue')
} 

# Remove rows where Semer is not 0 as its an fictitional drug 
consumption<-consumption[!grepl('CL1|CL2|CL3|CL4|CL5|CL6',consumption$Semer),]

# Remove Semer and ID column
consumption = subset(consumption, select = -c(Semer,ID))

attach(consumption)

outliers_nscore=boxplot(consumption_numeric$Nscore,plot=FALSE)$out
outliers_escore=boxplot(consumption_numeric$Escore,plot=FALSE)$out
outliers_oscore=boxplot(consumption_numeric$Oscore,plot=FALSE)$out
outliers_ascore=boxplot(consumption_numeric$AScore,plot=FALSE)$out
outliers_cscore=boxplot(consumption_numeric$Cscore,plot=FALSE)$out

consumption_numeric<-consumption_numeric[-which(consumption_numeric$Nscore %in% outliers_nscore),]
consumption_numeric<-consumption_numeric[-which(consumption_numeric$Escore %in% outliers_escore),]
consumption_numeric<-consumption_numeric[-which(consumption_numeric$Oscore %in% outliers_oscore),]
consumption_numeric<-consumption_numeric[-which(consumption_numeric$AScore %in% outliers_ascore),]
consumption_numeric<-consumption_numeric[-which(consumption_numeric$Cscore %in% outliers_cscore),]


consumption<-consumption[-which(consumption$Nscore %in% outliers_nscore),]
consumption<-consumption[-which(consumption$Escore %in% outliers_escore),]
consumption<-consumption[-which(consumption$Oscore %in% outliers_oscore),]
consumption<-consumption[-which(consumption$AScore %in% outliers_ascore),]
consumption<-consumption[-which(consumption$Cscore %in% outliers_cscore),]
attach(consumption)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Correlation Matrix for continuous variables 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

par(mfrow=c(1,1)) 
cor_mat = cor(consumption_numeric) 
corrplot(cor_mat, method = 'number') 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Tree Based Methods 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


factorCols <- c('Age',
               'Gender',
               'Education',
               'Country',
               'Ethnicity',
               'Choc',
               'Caff',
               'Alcohol',
               'Nicotine')

for (factorCol in factorCols) {   
  consumption[, factorCol] <- as.factor(consumption[, factorCol])   
}

response_variables<-c('Amphet',
                      'Amyl',
                      'Benzos',
                      'Cannabis',
                      'Coke',
                      'Crack',
                      'Ecstasy',
                      'Heroin',
                      'Ketamine',
                      'Legalh',
                       'LSD',
                       'Meth',
                       'Mushrooms',
                        'VSA')

for (l in response_variables) {   
  consumption[, l] <- as.numeric(factor(consumption[, l]))   
  consumption[, l] <- as.factor(consumption[, l])   
}

## Decision Tree
##Check optimal CP
mytree=rpart(Amphet~Age+Gender+Education+Country+Ethnicity+Nscore+
               Escore+Oscore+AScore+Cscore+Impulsive+SS+Caff+Nicotine+Choc,control=rpart.control(cp=0.00001))
summary(mytree)

printcp(mytree)
plotcp(mytree)
opt_cp=mytree$cptable[which.min(mytree$cptable[,"xerror"]),"CP"]


#rpart.plot(mytree,box.palette = 'blue')
#rpart.plot(mytree)


##To choose best value of n trees using do.trace command

myforest=randomForest(Amphet~Age+Gender+Education+Country+Ethnicity+Nscore+
                        Escore+Oscore+AScore+Cscore+Impulsive+SS+Caff+Nicotine+Alcohol, ntree=10000, 
                      data=consumption, importance=TRUE, na.action = na.omit,do.trace=500)
print(myforest)

#data(consumption) ## 70% of the sample size
smp_size <- floor(0.75 * nrow(consumption))## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(consumption)), size = smp_size)


consumption_train <- consumption[train_ind, ]
consumption_test <- consumption[-train_ind, ]


result<-function(actual,predicted)
{
  cm = as.matrix(table(Actual = actual, Predicted = predicted))
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  accuracy = sum(diag) / n 
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  return(f1)
}


#Decision Tree

decision_tree<-function(response_variable)
{
model_tree=rpart(response_variable~Age+Gender+Education+Country+Ethnicity+Nscore+
               Escore+Oscore+Cscore+Impulsive+SS+Caff+Nicotine+Alcohol+AScore+Choc,control=rpart.control(cp=opt_cp),data=consumption_train)
predicted=predict(model_tree,consumption_test,type='class')
actual=consumption_test$response_variable

#conf_mat <- confusion_matrix(targets = actual,predictions = predicted)
#plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]], add_sums = TRUE)
return(predicted)
}

actual_dt_Amphet<-consumption_test$Amphet
actual_dt_Amyl<-consumption_test$Amyl
actual_dt_Benzos<-consumption_test$Benzos
actual_dt_Cannabis<-consumption_test$Cannabis
actual_dt_Coke<-consumption_test$Coke
actual_dt_Crack<-consumption_test$Crack
actual_dt_Ecstasy<-consumption_test$Ecstasy
actual_dt_Heroin<-consumption_test$Heroin
actual_dt_Ketamine<-consumption_test$Ketamine
actual_dt_Legalh<-consumption_test$Legalh
actual_dt_LSD<-consumption_test$LSD
actual_dt_Meth<-consumption_test$Meth
actual_dt_Mushrooms<-consumption_test$Mushrooms
actual_dt_VSA<-consumption_test$VSA


predictions_dt_Amphet<-decision_tree(consumption_train$Amphet)
predictions_dt_Amyl<-decision_tree(consumption_train$Amyl)
predictions_dt_Benzos<-decision_tree(consumption_train$Benzos)
predictions_dt_Cannabis<-decision_tree(consumption_train$Cannabis)
predictions_dt_Coke<-decision_tree(consumption_train$Coke)
predictions_dt_Crack<-decision_tree(consumption_train$Crack)
predictions_dt_Ecstasy<-decision_tree(consumption_train$Ecstasy)
predictions_dt_Heroin<-decision_tree(consumption_train$Heroin)
predictions_dt_Ketamine<-decision_tree(consumption_train$Ketamine)
predictions_dt_Legalh<-decision_tree(consumption_train$Legalh)
predictions_dt_LSD<-decision_tree(consumption_train$LSD)
predictions_dt_Meth<-decision_tree(consumption_train$Meth)
predictions_dt_Mushrooms<-decision_tree(consumption_train$Mushrooms)
predictions_dt_VSA<-decision_tree(consumption_train$VSA)

result_tree_Amphet=result(actual= actual_dt_Amphet,predicted = predictions_dt_Amphet)
result_tree_Amyl=result(actual= actual_dt_Amyl,predicted = predictions_dt_Amyl)
result_tree_Benzos=result(actual= actual_dt_Benzos,predicted = predictions_dt_Benzos)
result_tree_Cannabis=result(actual= actual_dt_Cannabis,predicted = predictions_dt_Cannabis)
result_tree_Coke=result(actual= actual_dt_Coke,predicted = predictions_dt_Coke)
result_tree_Crack=result(actual= actual_dt_Crack,predicted = predictions_dt_Crack)
result_tree_Ecstasy=result(actual= actual_dt_Ecstasy,predicted = predictions_dt_Ecstasy)
result_tree_Heroin=result(actual= actual_dt_Heroin,predicted = predictions_dt_Heroin)
result_tree_Ketamine=result(actual= actual_dt_Ketamine,predicted = predictions_dt_Ketamine)
result_tree_Legalh=result(actual= actual_dt_Legalh,predicted = predictions_dt_Legalh)
result_tree_LSD=result(actual= actual_dt_LSD,predicted = predictions_dt_LSD)
result_tree_Meth=result(actual= actual_dt_Meth,predicted = predictions_dt_Meth)
result_tree_Mushrooms=result(actual= actual_dt_Mushrooms,predicted = predictions_dt_Mushrooms)
result_tree_VSA=result(actual= actual_dt_VSA,predicted = predictions_dt_VSA)

result_dt<-rbind(result_tree_Amphet,result_tree_Amyl,result_tree_Benzos,result_tree_Cannabis,result_tree_Coke,result_tree_Crack,result_tree_Ecstasy,result_tree_Heroin,result_tree_Ketamine,result_tree_Legalh,result_tree_LSD,result_tree_Meth,result_tree_Mushrooms,result_tree_VSA)
colnames(result_dt) <- c("CL0", "CL1","CL2", "CL3","CL4", "CL5","CL6")
Type <- rep('Decision Tree',nrow(result_dt))
result_dt <- cbind(result_dt,Type)
result_dt<-as.data.frame(result_dt)


#Better Adjusted R Squared


##LDA 
lda_1<-function(response_variable)
{print(response_variable)
model_lda= lda(response_variable~ Choc+Alcohol+Nicotine+Caff+Age+Ethnicity+Country+Education+Gender+Nscore+Escore+Oscore+AScore+Cscore+Impulsive+SS,data=consumption_train)
predictions=predict(model_lda, consumption_test, type = 'response')
#predictions_lda = data.frame(predict(model_lda, consumption_test, type = 'response'))

#predictions_lda_consumption= data.frame(predictions_lda$class, consumption_test$response_variable)
#conf_mat <- confusion_matrix(targets = predictions_lda_consumption$consumption_test.Amphet,predictions = predictions_lda_consumption$predictions_lda.class)
#plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]], add_sums = TRUE)
return(predictions)
}
#attach(consumption)


actual_lda_Amphet<-consumption_test$Amphet
actual_lda_Amyl<-consumption_test$Amyl
actual_lda_Benzos<-consumption_test$Benzos
actual_lda_Cannabis<-consumption_test$Cannabis
actual_lda_Coke<-consumption_test$Coke
actual_lda_Crack<-consumption_test$Crack
actual_lda_Ecstasy<-consumption_test$Ecstasy
actual_lda_Heroin<-consumption_test$Heroin
actual_lda_Ketamine<-consumption_test$Ketamine
actual_lda_Legalh<-consumption_test$Legalh
actual_lda_LSD<-consumption_test$LSD
actual_lda_Meth<-consumption_test$Meth
actual_lda_Mushrooms<-consumption_test$Mushrooms
actual_lda_VSA<-consumption_test$VSA


predictions_lda_Amphet<-lda_1(consumption_train$Amphet)
predictions_lda_Amyl<-lda_1(consumption_train$Amyl)
predictions_lda_Benzos<-lda_1(consumption_train$Benzos)
predictions_lda_Cannabis<-lda_1(consumption_train$Cannabis)
predictions_lda_Coke<-lda_1(consumption_train$Coke)
predictions_lda_Crack<-lda_1(consumption_train$Crack)
predictions_lda_Ecstasy<-lda_1(consumption_train$Ecstasy)
predictions_lda_Heroin<-lda_1(consumption_train$Heroin)
predictions_lda_Ketamine<-lda_1(consumption_train$Ketamine)
predictions_lda_Legalh<-lda_1(consumption_train$Legalh)
predictions_lda_LSD<-lda_1(consumption_train$LSD)
predictions_lda_Meth<-lda_1(consumption_train$Meth)
predictions_lda_Mushrooms<-lda_1(consumption_train$Mushrooms)
predictions_lda_VSA<-lda_1(consumption_train$VSA)

result_lda_Amphet=result(actual= actual_lda_Amphet,predicted = predictions_lda_Amphet$class)
result_lda_Amyl=result(actual= actual_lda_Amyl,predicted = predictions_lda_Amyl$class)
result_lda_Benzos=result(actual= actual_lda_Benzos,predicted = predictions_lda_Benzos$class)
result_lda_Cannabis=result(actual= actual_lda_Cannabis,predicted = predictions_lda_Cannabis$class)
result_lda_Coke=result(actual= actual_lda_Coke,predicted = predictions_lda_Coke$class)
result_lda_Crack=result(actual= actual_lda_Crack,predicted = predictions_lda_Crack$class)
result_lda_Ecstasy=result(actual= actual_lda_Ecstasy,predicted = predictions_lda_Ecstasy$class)
result_lda_Heroin=result(actual= actual_lda_Heroin,predicted = predictions_lda_Heroin$class)
result_lda_Ketamine=result(actual= actual_lda_Ketamine,predicted = predictions_lda_Ketamine$class)
result_lda_Legalh=result(actual= actual_lda_Legalh,predicted = predictions_lda_Legalh$class)
result_lda_LSD=result(actual= actual_lda_LSD,predicted = predictions_lda_LSD$class)
result_lda_Meth=result(actual= actual_lda_Meth,predicted = predictions_lda_Meth$class)
result_lda_Mushrooms=result(actual= actual_lda_Mushrooms,predicted = predictions_lda_Mushrooms$class)
result_lda_VSA=result(actual= actual_lda_VSA,predicted = predictions_lda_VSA$class)

result_lda<-rbind(result_lda_Amphet,result_lda_Amyl,result_lda_Benzos,result_lda_Cannabis,result_lda_Coke,result_lda_Crack,result_lda_Ecstasy,result_lda_Heroin,result_lda_Ketamine,result_lda_Legalh,result_lda_LSD,result_lda_Meth,result_lda_Mushrooms,result_lda_VSA)
colnames(result_lda) <- c("CL0", "CL1","CL2", "CL3","CL4", "CL5","CL6")


Type <- rep('LDA',nrow(result_lda))
result_lda <- cbind(result_lda,Type)
result_lda=as.data.frame(result_lda)

#Random Forest
rf<-function(response_variable)
{
myforest=randomForest(response_variable~Age+Gender+Education+Country+Ethnicity+Nscore+
                        Escore+Oscore+AScore+Cscore+Impulsive+SS+Caff+Nicotine+Choc, ntree=1000, 
                      data=consumption_train, importance=TRUE, na.action = na.omit)
print(myforest)
print(importance(myforest))
print(varImpPlot(myforest))
predicted=predict(myforest,consumption_test)
return(predicted)
}

#conf_mat <- confusion_matrix(targets = actual,predictions = predicted)
#plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]], add_sums = TRUE)


actual_rf_Amphet<-consumption_test$Amphet
actual_rf_Amyl<-consumption_test$Amyl
actual_rf_Benzos<-consumption_test$Benzos
actual_rf_Cannabis<-consumption_test$Cannabis
actual_rf_Coke<-consumption_test$Coke
actual_rf_Crack<-consumption_test$Crack
actual_rf_Ecstasy<-consumption_test$Ecstasy
actual_rf_Heroin<-consumption_test$Heroin
actual_rf_Ketamine<-consumption_test$Ketamine
actual_rf_Legalh<-consumption_test$Legalh
actual_rf_LSD<-consumption_test$LSD
actual_rf_Meth<-consumption_test$Meth
actual_rf_Mushrooms<-consumption_test$Mushrooms
actual_rf_VSA<-consumption_test$VSA


predictions_rf_Amphet<-rf(consumption_train$Amphet)
predictions_rf_Amyl<-rf(consumption_train$Amyl)
predictions_rf_Benzos<-rf(consumption_train$Benzos)
predictions_rf_Cannabis<-rf(consumption_train$Cannabis)
predictions_rf_Coke<-rf(consumption_train$Coke)
predictions_rf_Crack<-rf(consumption_train$Crack)
predictions_rf_Ecstasy<-rf(consumption_train$Ecstasy)
predictions_rf_Heroin<-rf(consumption_train$Heroin)
predictions_rf_Ketamine<-rf(consumption_train$Ketamine)
predictions_rf_Legalh<-rf(consumption_train$Legalh)
predictions_rf_LSD<-rf(consumption_train$LSD)
predictions_rf_Meth<-rf(consumption_train$Meth)
predictions_rf_Mushrooms<-rf(consumption_train$Mushrooms)
predictions_rf_VSA<-rf(consumption_train$VSA)


result_rf_Amphet=result(actual= actual_rf_Amphet,predicted = predictions_rf_Amphet)
result_rf_Amyl=result(actual= actual_rf_Amyl,predicted = predictions_rf_Amyl)
result_rf_Benzos=result(actual= actual_rf_Benzos,predicted = predictions_rf_Benzos)
result_rf_Cannabis=result(actual= actual_rf_Cannabis,predicted = predictions_rf_Cannabis)
result_rf_Coke=result(actual= actual_rf_Coke,predicted = predictions_rf_Coke)
result_rf_Crack=result(actual= actual_rf_Crack,predicted = predictions_rf_Crack)
result_rf_Ecstasy=result(actual= actual_rf_Ecstasy,predicted = predictions_rf_Ecstasy)
result_rf_Heroin=result(actual= actual_rf_Heroin,predicted = predictions_rf_Heroin)
result_rf_Ketamine=result(actual= actual_rf_Ketamine,predicted = predictions_rf_Ketamine)
result_rf_Legalh=result(actual= actual_rf_Legalh,predicted = predictions_rf_Legalh)
result_rf_LSD=result(actual= actual_rf_LSD,predicted = predictions_rf_LSD)
result_rf_Meth=result(actual= actual_rf_Meth,predicted = predictions_rf_Meth)
result_rf_Mushrooms=result(actual= actual_rf_Mushrooms,predicted = predictions_rf_Mushrooms)
result_rf_VSA=result(actual= actual_rf_VSA,predicted = predictions_rf_VSA)

result_rf<-rbind(result_rf_Amphet,result_rf_Amyl,result_rf_Benzos,result_rf_Cannabis,result_rf_Coke,result_rf_Crack,result_rf_Ecstasy,result_rf_Heroin,result_rf_Ketamine,result_rf_Legalh,result_rf_LSD,result_rf_Meth,result_rf_Mushrooms,result_rf_VSA)
colnames(result_rf) <- c("CL0", "CL1","CL2", "CL3","CL4", "CL5","CL6")


Type <- rep('RF',nrow(result_rf))
result_rf <- cbind(result_rf,Type)
result_rf=as.data.frame(result_rf)

##GBM
consumption_test$Alcohol=as.numeric(factor(consumption_test$Alcohol))
consumption_test$Alcohol=as.factor(consumption_test$Alcohol)

consumption_test$Caff=as.numeric(factor(consumption_test$Caff))
consumption_test$Caff=as.factor(consumption_test$Caff)

consumption_test$Nicotine=as.numeric(factor(consumption_test$Nicotine))
consumption_test$Nicotine=as.factor(consumption_test$Nicotine)

consumption_test$Age=as.numeric(factor(consumption_test$Age))
consumption_test$Age=as.factor(consumption_test$Age)

consumption_test$Ethnicity=as.numeric(factor(consumption_test$Ethnicity))
consumption_test$Ethnicity=as.factor(consumption_test$Ethnicity)

consumption_test$Country=as.numeric(factor(consumption_test$Country))
consumption_test$Country=as.factor(consumption_test$Country)

consumption_test$Education=as.numeric(factor(consumption_test$Education))
consumption_test$Education=as.factor(consumption_test$Education)

consumption_test$Gender=as.numeric(factor(consumption_test$Gender))
consumption_test$Gender=as.factor(consumption_test$Gender)

consumption_test$Choc=as.numeric(factor(consumption_test$Choc))
consumption_test$Choc= as.factor(consumption_test$Choc)

consumption_train$Alcohol=as.numeric(factor(consumption_train$Alcohol))
consumption_train$Alcohol=as.factor(consumption_train$Alcohol)

consumption_train$Caff=as.numeric(factor(consumption_train$Caff))
consumption_train$Caff=as.factor(consumption_train$Caff)

consumption_train$Nicotine=as.numeric(factor(consumption_train$Nicotine))
consumption_train$Nicotine=as.factor(consumption_train$Nicotine)

consumption_train$Age=as.numeric(factor(consumption_train$Age))
consumption_train$Age=as.factor(consumption_train$Age)

consumption_train$Ethnicity=as.numeric(factor(consumption_train$Ethnicity))
consumption_train$Ethnicity=as.factor(consumption_train$Ethnicity)

consumption_train$Country=as.numeric(factor(consumption_train$Country))
consumption_train$Country=as.factor(consumption_train$Country)

consumption_train$Education=as.numeric(factor(consumption_train$Education))
consumption_train$Education=as.factor(consumption_train$Education)

consumption_train$Gender=as.numeric(factor(consumption_train$Gender))
consumption_train$Gender=as.factor(consumption_train$Gender)

consumption_train$Choc=as.numeric(factor(consumption_train$Choc))
consumption_train$Choc= as.factor(consumption_train$Choc)

####### GBM Model
gbm_1<-function(response_variable)
{  
set.seed (1)
boosted=gbm(response_variable~ Choc+Alcohol+Nicotine+Caff+Age+Ethnicity+Country+Education+Gender+Nscore+Escore+Oscore+AScore+Cscore+Impulsive+SS,
            data=consumption_train,distribution="multinomial",n.trees=1000, interaction.depth=4)
print(boosted)

pred = predict.gbm(object = boosted,
                   newdata = consumption_test,
                   n.trees = 200,
                   type = "response")

labels = colnames(pred)[apply(pred, 1, which.max)]
return(labels)
}
actual_gbm_Amphet<-consumption_test$Amphet
actual_gbm_Amyl<-consumption_test$Amyl
actual_gbm_Benzos<-consumption_test$Benzos
actual_gbm_Cannabis<-consumption_test$Cannabis
actual_gbm_Coke<-consumption_test$Coke
actual_gbm_Crack<-consumption_test$Crack
actual_gbm_Ecstasy<-consumption_test$Ecstasy
actual_gbm_Heroin<-consumption_test$Heroin
actual_gbm_Ketamine<-consumption_test$Ketamine
actual_gbm_Legalh<-consumption_test$Legalh
actual_gbm_LSD<-consumption_test$LSD
actual_gbm_Meth<-consumption_test$Meth
actual_gbm_Mushrooms<-consumption_test$Mushrooms
actual_gbm_VSA<-consumption_test$VSA


predictions_gbm_Amphet<-gbm_1(consumption_train$Amphet)
predictions_gbm_Amyl<-gbm_1(consumption_train$Amyl)
predictions_gbm_Benzos<-gbm_1(consumption_train$Benzos)
predictions_gbm_Cannabis<-gbm_1(consumption_train$Cannabis)
predictions_gbm_Coke<-gbm_1(consumption_train$Coke)
predictions_gbm_Crack<-gbm_1(consumption_train$Crack)
predictions_gbm_Ecstasy<-gbm_1(consumption_train$Ecstasy)
predictions_gbm_Heroin<-gbm_1(consumption_train$Heroin)
predictions_gbm_Ketamine<-gbm_1(consumption_train$Ketamine)
predictions_gbm_Legalh<-gbm_1(consumption_train$Legalh)
predictions_gbm_LSD<-gbm_1(consumption_train$LSD)
predictions_gbm_Meth<-gbm_1(consumption_train$Meth)
predictions_gbm_Mushrooms<-gbm_1(consumption_train$Mushrooms)
predictions_gbm_VSA<-gbm_1(consumption_train$VSA)

result_gbm_Amphet=result(actual= actual_gbm_Amphet,predicted = predictions_gbm_Amphet)
result_gbm_Amyl=result(actual= actual_gbm_Amyl,predicted = predictions_gbm_Amyl)
result_gbm_Benzos=result(actual= actual_gbm_Benzos,predicted = predictions_gbm_Benzos)
result_gbm_Cannabis=result(actual= actual_gbm_Cannabis,predicted = predictions_gbm_Cannabis)
result_gbm_Coke=result(actual= actual_gbm_Coke,predicted = predictions_gbm_Coke)
result_gbm_Crack=result(actual= actual_gbm_Crack,predicted = predictions_gbm_Crack)
result_gbm_Ecstasy=result(actual= actual_gbm_Ecstasy,predicted = predictions_gbm_Ecstasy)
result_gbm_Heroin=result(actual= actual_gbm_Heroin,predicted = predictions_gbm_Heroin)
result_gbm_Ketamine=result(actual= actual_gbm_Ketamine,predicted = predictions_gbm_Ketamine)
result_gbm_Legalh=result(actual= actual_gbm_Legalh,predicted = predictions_gbm_Legalh)
result_gbm_LSD=result(actual= actual_gbm_LSD,predicted = predictions_gbm_LSD)
result_gbm_Meth=result(actual= actual_gbm_Meth,predicted = predictions_gbm_Meth)
result_gbm_Mushrooms=result(actual= actual_gbm_Mushrooms,predicted = predictions_gbm_Mushrooms)
result_gbm_VSA=result(actual= actual_gbm_VSA,predicted = predictions_gbm_VSA)

result_gbm<-rbind(result_gbm_Amphet,result_gbm_Amyl,result_gbm_Benzos,result_gbm_Cannabis,result_gbm_Coke,result_gbm_Crack,result_gbm_Ecstasy,result_gbm_Heroin,result_gbm_Ketamine,result_gbm_Legalh,result_gbm_LSD,result_gbm_Meth,result_gbm_Mushrooms,result_gbm_VSA)
colnames(result_gbm) <- c("CL0", "CL1","CL2", "CL3","CL4", "CL5","CL6")


Type <- rep('GBM',nrow(result_gbm))
result_gbm <- cbind(result_gbm,Type)
result_gbm=as.data.frame(result_gbm)
#library(caret)
#cm = confusionMatrix(consumption_test$Amphet, as.factor(labels))
#print(cm)
#summary(boosted)

#conf_mat <- confusion_matrix(targets = result$consumption_test.Amphet,
#                             predictions = result$labels)
#plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]], add_sums = TRUE)

df_results<-as.data.frame(rbind(result_dt,result_lda,result_rf,result_gbm))
df_results$CL0<-as.numeric(df_results$CL0)
df_results$CL1<-as.numeric(df_results$CL1)
df_results$CL2<-as.numeric(df_results$CL2)
df_results$CL3<-as.numeric(df_results$CL3)
df_results$CL4<-as.numeric(df_results$CL4)
df_results$CL5<-as.numeric(df_results$CL5)
df_results$CL6<-as.numeric(df_results$CL6)
df_results[is.na(df_results)]<-0

x<-rep(response_variables,times=4)
df_results$drug_type=x
library("writexl")
index <- df_results$CL4> 1
df_results$CL4[index] <- 0
index <- df_results$CL5> 1
df_results$CL5[index] <- 0
index <- df_results$CL6> 1
df_results$CL6[index] <- 0



#Results of all the models in this file
write_xlsx(df_results,"Results.xlsx")

ggplot(df_results,aes(fill=Type,y=CL0,x=drug_type))+geom_bar(position="dodge", stat="identity")+ylab("CL0 F1 Scores")+xlab("Drug Type")
ggplot(df_results,aes(fill=Type,y=CL1,x=drug_type))+geom_bar(position="dodge", stat="identity")+ylab("CL1 F1 Scores")+xlab("Drug Type")
ggplot(df_results,aes(fill=Type,y=CL2,x=drug_type))+geom_bar(position="dodge", stat="identity")+ylab("CL2 F1 Scores")+xlab("Drug Type")
ggplot(df_results,aes(fill=Type,y=CL3,x=drug_type))+geom_bar(position="dodge", stat="identity")+ylab("CL3 F1 Scores")+xlab("Drug Type")
ggplot(df_results,aes(fill=Type,y=CL4,x=drug_type))+geom_bar(position="dodge", stat="identity")+ylab("CL4 F1 Scores")+xlab("Drug Type")
ggplot(df_results,aes(fill=Type,y=CL5,x=drug_type))+geom_bar(position="dodge", stat="identity")+ylab("CL5 F1 Scores")+xlab("Drug Type")
ggplot(df_results,aes(fill=Type,y=CL6,x=drug_type))+geom_bar(position="dodge", stat="identity")+ylab("CL6 F1 Scores")+xlab("Drug Type")



## Comparisions in different models

df_result_1<-data.frame(t(unlist(result_lda)))
colnames(df_result_1) <- c("CL0", "CL1","CL2", "CL3","CL4", "CL5","CL6")
rownames(df_result_1) <- "LDA"

df_result_2<-data.frame(t(unlist(result_random_forest)))
colnames(df_result_2) <- c("CL0", "CL1","CL2", "CL3","CL4", "CL5","CL6")
rownames(df_result_2) <- "Random Forest"

df_result_3<-data.frame(t(unlist(result_gbm)))
colnames(df_result_3) <- c("CL0", "CL1","CL2", "CL3","CL4", "CL5","CL6")
rownames(df_result_3) <- "Gradient Boosted Trees"

df_result_4<-data.frame(t(unlist(result_tree)))
colnames(df_result_4) <- c("CL0", "CL1","CL2", "CL3","CL4", "CL5","CL6")
rownames(df_result_4) <- "Decision Trees"


df_result <- rbind(df_result_1,df_result_4, df_result_2,df_result_3)
df_result[is.na(df_result)] = 0


library(RColorBrewer)
coul <- brewer.pal(4, "Set3")
par(mfrow=c(3,3)) 
barplot(height=df_result$CL0,col=coul,xlab='CL0 F1 Scores',names.arg = c('LDA','Tree','RF','GBM'),plot = TRUE,horiz=T)
barplot(height=df_result$CL1,col=coul,xlab='CL1 F1 Scores',names.arg = c('LDA','Tree','RF','GBM'),plot = TRUE,horiz=T)
barplot(height=df_result$CL2,col=coul,xlab='CL2 F1 Scores',names.arg = c('LDA','Tree','RF','GBM'),plot = TRUE,horiz=T)
barplot(height=df_result$CL3,col=coul,xlab='CL3 F1 Scores',names.arg = c('LDA','Tree','RF','GBM'),plot = TRUE,horiz=T)
barplot(height=df_result$CL4,col=coul,xlab='CL4 F1 Scores',names.arg = c('LDA','Tree','RF','GBM'),plot = TRUE,horiz=T)
barplot(height=df_result$CL5,col=coul,xlab='CL5 F1 Scores',names.arg = c('LDA','Tree','RF','GBM'),plot = TRUE,horiz=T)
barplot(height=df_result$CL6,col=coul,xlab='CL6 F1 Scores',names.arg = c('LDA','Tree','RF','GBM'),plot = TRUE,horiz=T)
#barplot(height=df_result$CL0,col=coul,names.arg = c('LDA','Tree','RF','GBM'),plot = TRUE,horiz=T)
#axis(1, at = 0:1) 



#PCA
ggpairs(consumption_numeric)
pca=prcomp(consumption_numeric, scale=TRUE)
library(ggfortify)
autoplot(pca, data = consumption_numeric, loadings = TRUE, loadings.label = TRUE,col='Gray')

pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))
pca<-data.frame(pca[2])

#Kmeans
par(mfrow=c(1,1)) 

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(consumption_numeric, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


library("factoextra")
library("purrr")

km.4=kmeans(consumption_numeric, 4)
fviz_cluster(km.4, data = consumption_numeric)
attach(consumption_numeric)
plot=ggplot(consumption_numeric,aes(y=Nscore, x=Escore))
consumption_numeric$cluster=as.factor(km.4$cluster)
#plot+geom_point()
plot+geom_point(aes(colour=consumption_numeric$cluster))

###Interpretation
library(CGPfunctions)
boosted=gbm(Amphet~ Choc+Alcohol+Nicotine+Caff+Age+Ethnicity+Country+Education+Gender+Nscore+Escore+Oscore+AScore+Cscore+Impulsive+SS,
            data=consumption_train,distribution="multinomial",n.trees=1000, interaction.depth=4)
print(boosted)
pred = predict.gbm(object = boosted,
                   newdata = consumption_test,
                   n.trees = 200,
                   type = "response")

labels = colnames(pred)[apply(pred, 1, which.max)]
consumption_test$predictions<-labels
par(mfrow=c(3,2)) 
PlotXTabs(consumption_test,Gender,Predictions) 
PlotXTabs(consumption_test,Age,predictions) 
PlotXTabs(consumption_test,Education,predictions) 
PlotXTabs(consumption_test,Country,predictions) 
PlotXTabs(consumption_test,Ethnicity,predictions) 


library(CGPfunctions)
boosted=gbm(Coke~ Choc+Alcohol+Nicotine+Caff+Age+Ethnicity+Country+Education+Gender+Nscore+Escore+Oscore+AScore+Cscore+Impulsive+SS,
            data=consumption_train,distribution="multinomial",n.trees=1000, interaction.depth=4)
print(boosted)
pred = predict.gbm(object = boosted,
                   newdata = consumption_test,
                   n.trees = 200,
                   type = "response")

labels = colnames(pred)[apply(pred, 1, which.max)]
consumption_test$predictions<-labels
#par(mfrow=c(3,2)) 
PlotXTabs(consumption_test,Gender,Predictions) 
PlotXTabs(consumption_test,Age,predictions) 
PlotXTabs(consumption_test,Education,predictions) 
PlotXTabs(consumption_test,Country,predictions) 
PlotXTabs(consumption_test,Ethnicity,predictions) 


boosted=gbm(Cannabis~ Choc+Alcohol+Nicotine+Caff+Age+Ethnicity+Country+Education+Gender+Nscore+Escore+Oscore+AScore+Cscore+Impulsive+SS,
            data=consumption_train,distribution="multinomial",n.trees=1000, interaction.depth=4)
print(boosted)
pred = predict.gbm(object = boosted,
                   newdata = consumption_test,
                   n.trees = 200,
                   type = "response")

labels = colnames(pred)[apply(pred, 1, which.max)]
consumption_test$predictions<-labels
vtree(consumption_test, c("Age", "predictions"), 
      fillcolor = c( Age = "#e7d4e8", Gender = "#99d8c9",Education="#03fcdf"),
      horiz = FALSE)


boosted=gbm(Benzos~ Choc+Alcohol+Nicotine+Caff+Age+Ethnicity+Country+Education+Gender+Nscore+Escore+Oscore+AScore+Cscore+Impulsive+SS,
            data=consumption_train,distribution="multinomial",n.trees=1000, interaction.depth=4)
print(boosted)
pred = predict.gbm(object = boosted,
                   newdata = consumption_test,
                   n.trees = 200,
                   type = "response")

labels = colnames(pred)[apply(pred, 1, which.max)]
consumption_test$predictions<-labels
vtree(consumption_test, c("Education", "predictions"), 
      fillcolor = c( Age = "#e7d4e8", Gender = "#99d8c9",Education="#03fcdf"),
      horiz = FALSE)



boosted=gbm(Mushrooms~ Choc+Alcohol+Nicotine+Caff+Age+Ethnicity+Country+Education+Gender+Nscore+Escore+Oscore+AScore+Cscore+Impulsive+SS,
            data=consumption_train,distribution="multinomial",n.trees=1000, interaction.depth=4)
print(boosted)
pred = predict.gbm(object = boosted,
                   newdata = consumption_test,
                   n.trees = 200,
                   type = "response")

labels = colnames(pred)[apply(pred, 1, which.max)]
consumption_test$predictions<-labels
vtree(consumption_test, c("Gender", "predictions"), 
      fillcolor = c( Age = "#e7d4e8", Gender = "#99d8c9",Education="#03fcdf"),
      horiz = FALSE)
