install.packages("corrplot")
install.packages("pROC")
install.packages("MASS")
install.packages("glm2")
install.packages("class")
install.packages("e1071")
install.packages("randomForest")


# read Input 
MyData <- read.csv(file="C:\\Users\\nayan\\Downloads\\fifa-18-more-complete-player-dataset\\complete.csv", header=TRUE, sep=",")

#pirnt the top 5 observations
head(MyData, 5)

#print all column names
colnames(MyData)

colsToKeep <- c("special","age","height_cm","weight_kg","overall","potential","pac","sho","pas","dri","def","phy",
                "international_reputation","skill_moves","weak_foot","crossing","finishing","heading_accuracy","short_passing",
                "volleys","dribbling","curve","free_kick_accuracy","long_passing","ball_control","acceleration","sprint_speed",
                "agility","reactions","balance","shot_power","jumping","stamina","strength","long_shots","aggression",
                "interceptions","positioning","vision","penalties","composure","marking","standing_tackle","sliding_tackle",
                "gk_diving","gk_handling","gk_kicking","gk_positioning","gk_reflexes","rs","rw","rf","ram","rcm","rm","rdm","rcb",
                "rb","rwb","st","lw","cf","cam","cm","lm","cdm","cb","lb","lwb","ls","lf","lam","lcm","ldm","lcb","gk",
                "prefers_rs","prefers_rw","prefers_rf","prefers_ram","prefers_rcm","prefers_rm","prefers_rdm","prefers_rcb",
                "prefers_rb","prefers_rwb","prefers_st","prefers_lw","prefers_cf","prefers_cam","prefers_cm","prefers_lm",
                "prefers_cdm","prefers_cb","prefers_lb","prefers_lwb","prefers_ls","prefers_lf","prefers_lam","prefers_lcm",
                "prefers_ldm","prefers_lcb","prefers_gk")

#columns to keep in data
MyData<-MyData[colsToKeep]

#create new column for preferred position
MyData$Pref_Position <- 'NA'
MyData$Final_Position <- 'NA'

colnames(MyData)

#New data set with cleaned data
cleanedData = data.frame();

#set of all possible positions
position = c("rs","rw","rf","ram","rcm","rm","rdm","rcb","rb","rwb","st","lw","cf","cam","cm","lm","cdm","cb","lb","lwb","ls",
             "lf","lam","lcm","ldm","lcb","gk")

booleanPosition = c(1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0)


#Creates a new dataset with specifying preferred position. In case of multiple preferred positions, 
#duplicate rows of data is created with unique preferred positions for each row for each player  

for( i in 1:length(MyData[,1])){
  for(j in 77:103){
    if(MyData[i,j] == 'True'){
      MyData[i,]$Pref_Position = position[j-76]
      MyData[i,]$Final_Position = booleanPosition[j-76]
      cleanedData = rbind(cleanedData, MyData[i,])
    }
  }
}


removePrefers <- c("prefers_rs","prefers_rw","prefers_rf","prefers_ram","prefers_rcm","prefers_rm","prefers_rdm","prefers_rcb",
                   "prefers_rb","prefers_rwb","prefers_st","prefers_lw","prefers_cf","prefers_cam","prefers_cm","prefers_lm",
                   "prefers_cdm","prefers_cb","prefers_lb","prefers_lwb","prefers_ls","prefers_lf","prefers_lam","prefers_lcm",
                   "prefers_ldm","prefers_lcb","prefers_gk")


withoutPrefers <- cleanedData[,!colnames(cleanedData) %in% removePrefers]
withoutPrefers[is.na(withoutPrefers)] <- 0
withoutPrefers$Final_Position <- as.numeric(withoutPrefers$Final_Position)

# Split training and test data
train<-sample(nrow(withoutPrefers),nrow(withoutPrefers)/2)
wp.train<- withoutPrefers[train,]
wp.test<-withoutPrefers[-train,]

#plots to check collinearity
removeCharCols<- c("Pref_Position","special", "age", "height_cm","weight_kg",
                   "gk_diving","gk_handling","gk_kicking","gk_positioning","gk_reflexes","rs","rw","rf","ram","rcm","rm","rdm","rcb",
                   "rb","rwb","st","lw","cf","cam","cm","lm","cdm","cb","lb","lwb","ls","lf","lam","lcm","ldm","lcb","gk")
str(withoutPrefers)
correlation<-cor(withoutPrefers[,!colnames(withoutPrefers) %in% removeCharCols], method="pearson")
library(corrplot)
corrplot.mixed(correlation,tl.pos = 'lt')

collinear<-c("gk_diving","gk_handling","gk_kicking","gk_positioning","gk_reflexes","rs","rw","rf","ram","rcm","rm","rdm","rcb",
             "rb","rwb","st","lw","cf","cam","cm","lm","cdm","cb","lb","lwb","ls","lf","lam","lcm","ldm","lcb","gk")
corrplot.mixed(cor(withoutPrefers[,collinear]),tl.pos = 'lt')



#apply PCA
set.seed(1)
removePCAcols <- c("special","age","height_cm","weight_kg","Pref_Position","Final_Position")
PCAdata <- withoutPrefers[,!colnames(withoutPrefers) %in% removePCAcols]
apply(PCAdata, 2, mean)
apply(PCAdata, 2, var)
pr.out=prcomp(PCAdata, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(PCAdata)
dim(pr.out$x)
biplot(pr.out, scale=0)
pr.out$rotation=- pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)


#apply LDA
library(MASS)
set.seed(1)
lda.fit<-lda(Final_Position ~ . - special - age - height_cm - weight_kg - Pref_Position - Final_Position -overall 
             -gk_diving -gk_handling -gk_kicking -gk_positioning -gk_reflexes -rs -rw -rf -ram -rm -rdm -rcb
             - rb -rwb -st -lw -cf -cam -cm -lm- cdm -cb -lb -lwb -ls -lf -lam -lcm -ldm -lcb -gk, 
             data=wp.train)
summary(lda.fit)
lda.pred = predict(lda.fit, wp.test)
table(lda.pred$class, wp.test$Final_Position)
mean(lda.pred$class==wp.test$Final_Position)
plot(lda.fit, cex = 0.7, pch = 20,type="both")

#ROC Curve
library(pROC)
rocnn<- roc(lda.pred$class,wp.test$Final_Position)
plot(rocnn,col="red")
auc(rocnn)

#apply Logistic regression
set.seed(1)
library(glm2)
set.seed(1)
wp.logit<-glm(Final_Position ~ . - special - age - height_cm - weight_kg - Pref_Position - Final_Position -overall
              -gk_diving -gk_handling -gk_kicking -gk_positioning -gk_reflexes -rs -rw -rf -ram -rm -rdm -rcb
              - rb -rwb -st -lw -cf -cam -cm -lm- cdm -cb -lb -lwb -ls -lf -lam -lcm -ldm -lcb -gk, 
               data= wp.train, family="binomial")
summary(wp.logit)
wp.probs<-predict(wp.logit, wp.test, type="response")  
wp.pred<-rep(0,nrow(wp.test))
wp.pred[wp.probs>0.5]=1
table(wp.pred, wp.test$Final_Position)
mean(wp.pred==wp.test$Final_Position)
plot(wp.probs)

#ROC Curve
rocnn<- roc(wp.pred,wp.test$Final_Position)
plot(rocnn,col="red")
auc(rocnn)

#apply KNN
unusedCols<- c("special","age","height_cm","weight_kg","Pref_Position","Final_Position", "overall",
               "gk_diving","gk_handling","gk_kicking","gk_positioning","gk_reflexes","rs","rw","rf","ram","rcm","rm","rdm","rcb",
               "rb","rwb","st","lw","cf","cam","cm","lm","cdm","cb","lb","lwb","ls","lf","lam","lcm","ldm","lcb","gk")
wp.train.predictors <- wp.train[,!colnames(wp.train) %in% unusedCols]
wp.test.predictors<-  wp.test[,!colnames(wp.test) %in% unusedCols]
wp.train.response<- wp.train[,"Final_Position"]
set.seed(1)
library(class)
knn.pred=knn(wp.train.predictors, wp.test.predictors, wp.train.response, k=10)
summary(knn.pred)
table(knn.pred, wp.test$Final_Position)
mean(knn.pred==wp.test$Final_Position)

rocnn<- roc(knn.pred,wp.test$Final_Position)
plot(rocnn,col="red")
auc(rocnn)



#apply QDA
#QDA
set.seed(1)
library(MASS)
qda.fit<-qda(Final_Position ~ . - special - age - height_cm - weight_kg - Pref_Position - Final_Position -overall 
             -gk_diving -gk_handling -gk_kicking -gk_positioning -gk_reflexes -rs -rw -rf -ram -rm -rdm -rcb
             - rb -rwb -st -lw -cf -cam -cm -lm- cdm -cb -lb -lwb -ls -lf -lam -lcm -ldm -lcb -gk ,data=wp.train, na.rm=TRUE)
summary(qda.fit)
qda.class = predict(qda.fit, wp.test, na.rm = TRUE)$class
table(qda.class, wp.test$Final_Position)
mean(qda.class ==wp.test$Final_Position)

rocnn<- roc(qda.class,wp.test$Final_Position)
plot(rocnn,col="red")
auc(rocnn)


#apply SVC
set.seed(1)
wp.train$factors<- as.factor(wp.train$Final_Position)
wp.test$factors<- as.factor(wp.test$Final_Position)
library(ISLR)
library(e1071)

wp.svmfit <- svm(factors ~ . - special - age - height_cm - weight_kg - Pref_Position - Final_Position -overall 
                 -gk_diving -gk_handling -gk_kicking -gk_positioning -gk_reflexes -rs -rw -rf -ram -rm -rdm -rcb
                 - rb -rwb -st -lw -cf -cam -cm -lm- cdm -cb -lb -lwb -ls -lf -lam -lcm -ldm -lcb -gk , 
                 data= wp.train, kernel="linear", cost=1 ) 
summary(wp.svmfit)
train.predict<- predict(wp.svmfit, wp.train,  type="class")
table(predict=train.predict, truth=wp.train$factors)

test.predict<- predict(wp.svmfit, wp.test,  type="class")
table(predict=test.predict, truth=wp.test$factors)
mean(test.predict ==wp.test$factors)

library(pROC)
rocnn<- roc(as.numeric(test.predict),as.numeric(wp.test$factors))
plot(rocnn,col="red")
auc(rocnn)



#apply SVM radial
set.seed(1)
wp.train$factors<- as.factor(wp.train$Final_Position)
wp.test$factors<- as.factor(wp.test$Final_Position)
library(ISLR)
library(e1071)
wp.svmfit <- svm(factors ~ . - special - age - height_cm - weight_kg - Pref_Position - Final_Position -overall 
                 -gk_diving -gk_handling -gk_kicking -gk_positioning -gk_reflexes -rs -rw -rf -ram -rm -rdm -rcb
                 - rb -rwb -st -lw -cf -cam -cm -lm- cdm -cb -lb -lwb -ls -lf -lam -lcm -ldm -lcb -gk, 
                 data= wp.train, kernel="radial", cost=1 ) 
summary(wp.svmfit)
train.predict<- predict(wp.svmfit, wp.train,  type="class")
table(predict=train.predict, truth=wp.train$factors)

test.predict<- predict(wp.svmfit, wp.test,  type="class")
table(predict=test.predict, truth=wp.test$factors)
mean(test.predict ==wp.test$factors)

library(pROC)
rocnn<- roc(as.numeric(test.predict),as.numeric(wp.test$factors))
plot(rocnn,col="red")
auc(rocnn)



#apply SVM polynomial
set.seed(1)
wp.train$factors<- as.factor(wp.train$Final_Position)
wp.test$factors<- as.factor(wp.test$Final_Position)
library(ISLR)
library(e1071)
wp.svmfit <- svm(factors ~ . - special - age - height_cm - weight_kg - Pref_Position - Final_Position -overall 
                 -gk_diving -gk_handling -gk_kicking -gk_positioning -gk_reflexes -rs -rw -rf -ram -rm -rdm -rcb
                 - rb -rwb -st -lw -cf -cam -cm -lm- cdm -cb -lb -lwb -ls -lf -lam -lcm -ldm -lcb -gk, 
                 data= wp.train, kernel="polynomial", degree=2 ,cost=1 ) 
summary(wp.svmfit)
train.predict<- predict(wp.svmfit, wp.train,  type="class")
table(predict=train.predict, truth=wp.train$factors)

test.predict<- predict(wp.svmfit, wp.test,  type="class")
table(predict=test.predict, truth=wp.test$factors)
mean(test.predict ==wp.test$factors)

rocnn<- roc(as.numeric(test.predict),as.numeric(wp.test$factors))
plot(rocnn,col="red")
auc(rocnn)



#apply Random Forests
set.seed(1)
wp.train$factors<- as.factor(wp.train$Final_Position)
wp.test$factors<- as.factor(wp.test$Final_Position)
library(randomForest)
wp.ranforrest= randomForest(factors ~. - special - age - height_cm - weight_kg - Pref_Position - Final_Position -overall 
                            -gk_diving -gk_handling -gk_kicking -gk_positioning -gk_reflexes -rs -rw -rf -ram -rm -rdm -rcb
                            - rb -rwb -st -lw -cf -cam -cm -lm- cdm -cb -lb -lwb -ls -lf -lam -lcm -ldm -lcb -gk, 
                            data= wp.train, mtry= 7, ntree= 250, importance=TRUE)
summary(wp.ranforrest)
wp.rfpredict<- predict(wp.ranforrest, wp.test, type="class")
importance(wp.ranforrest)
table(predict=wp.rfpredict, truth=wp.test$factors)
mean(wp.rfpredict ==wp.test$factors)

rocnn<- roc(as.numeric(test.predict),as.numeric(wp.test$factors))
plot(rocnn,col="red")
auc(rocnn)
