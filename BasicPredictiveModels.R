setwd("/Users/asra/Documents/IFMR/PanelStudy/data/rural/individual")
library('haven')
library('Boruta')
library('caret')
library("e1071")

# Read individual common survey data
ir1 <- read_dta("isec01.dta")
# Read migration module data
ir7 <- read_dta("isec07.dta")
ir <- merge(ir1, ir7, by=c("hhid", "memid", "state", "district", "tehsil","listing_id","region", "block", "psu"))

#Clean data -- Remove columns which have >25% missing value
pMiss <- function(x){sum(is.na(x))/length(x)*100 > 25}
missCol <- apply(ir, 2, pMiss)
missColDF <- data.frame(names(missCol),missCol)
fil <- filter(missColDF, missColDF$missCol==FALSE)
ir_filtered <- subset(ir, select = names(ir) %in% fil$names.missCol.)

char_columns <- c("state","tehsil","block","q1026")
ir_filtered[,char_columns] <- lapply(ir_filtered[,char_columns], factor)
ir_filtered <- ir_filtered[complete.cases(ir_filtered),]

# Remove records for NA value in dependent variable
ir_filtered <- subset(ir_filtered, ir_filtered$q3001==1 | ir_filtered$q3001==2)
ir_filtered$q3001 <- as.factor(ir_filtered$q3001)

#Sample Indexes
indexes = sample(1:nrow(ir_filtered), size=0.3*nrow(ir_filtered))

# Split data
test = ir_filtered[indexes,]
dim(test)
train = ir_filtered[-indexes,]
dim(train)

set.seed(111)
boruta.train <- Boruta(q3001~.-memid, data = train, doTrace = 2)
boruta.train$finalDecision
final.boruta <- TentativeRoughFix(boruta.train)
attributes <- getSelectedAttributes(final.boruta, withTentative = F)
## attributes selected:
#[1] "hhid"       "district"   "tehsil"     "listing_id" "block"      "psu"        "age"        "gender"    
#[9] "educ"       "resid"      "q1002"      "q1003"      "q1004"      "q1006"      "q1026"      "s3mid"     
#[17] "s3rid"     
boruta.df <- attStats(final.boruta)
attributes_collapsed <- paste( attributes[attributes!="q1026"], collapse = " + ")

###  LOGISTIC REGRESSION #####
logistic_model <- glm(as.formula(paste("q3001 ~", attributes_collapsed)), data = train[!names(train) %in% c("q1026")], family = binomial)
pred_glm <- predict(logistic_model, test, type="response")
table(test$q3001, pred_glm > 0.5)
#    FALSE TRUE
#1   656  563
#2   374 2201

migrated_glm <- ifelse(pred_glm > .5, "yes", "no")
output <- cbind(test, migrated_glm)

##### DECISION TREE ######
library(rpart)
dt_model <- rpart(as.formula(paste("q3001 ~", attributes_collapsed)), data = train[!names(train) %in% c("q1026")], method="class")
summary(dt_model)
#Predict Output 
pred_dt <- predict(dt_model,test, type= "class")
output <- cbind(output, pred_dt)
table(output$q3001 == output$pred_dt)
#FALSE  TRUE 
#937  2857 

print(postResample(pred=pred_dt, obs=test[,"q3001"]))
#Accuracy     Kappa 
#0.7530311 0.3960247 

#### Support Vector Machine ####
model_svm <-svm(as.formula(paste("q3001 ~", attributes_collapsed)), data = train[!names(train) %in% c("q1026")])
summary(model_svm)
#Predict Output 
pred_svm <- predict(model_svm,test)
output <- cbind(output, pred_svm)
table(output$q3001 == output$pred_svm)

#FALSE  TRUE 
#913  2881 

print(postResample(pred=pred_svm, obs=test[,"q3001"]))
#Accuracy     Kappa 
#0.7593569 0.4761778 

##### Naive Bayes #####
model_bayes <-naiveBayes(as.formula(paste("q3001 ~", attributes_collapsed)), data = train[!names(train) %in% c("q1026")])
summary(model_bayes)
#Predict Output 
pred_bayes <- predict(model_bayes,test)
output <- cbind(output, pred_bayes)
table(output$q3001 == output$pred_bayes)
#FALSE  TRUE 
#1020  2774 
print(postResample(pred=pred_bayes, obs=test[,"q3001"]))
#Accuracy     Kappa 
#0.7311545 0.3948079 

#### Random Forest ####

library(randomForest)
model_rf <- randomForest(as.formula(paste("q3001 ~", attributes_collapsed)), data = train[!names(train) %in% c("q1026")],ntree=500)
summary(model_rf)
#Error in randomForest.default(m, y, ...) : 
#Can not handle categorical predictors with more than 53 categories.

# Excluding predictors with >53 categories
model_rf <- randomForest(as.formula(paste("q3001 ~", paste( attributes[!attributes %in% c("q1026","block","tehsil")], collapse = " + "))), data = train[!names(train) %in% c("q1026","block","tehsil")],ntree=500)
pred_rf <- predict(model_rf,test)
print(postResample(pred=pred_rf, obs=as.factor(test[,"q3001"])))
#Accuracy     Kappa 
#0.7688455 0.4538254 

#### Gradient Boosting Algorithm ####
fitControl <- trainControl( method = "repeatedcv", number = 4, repeats = 4)
model_gbm <- train(as.formula(paste("q3001 ~", attributes_collapsed)), data = train[!names(train) %in% c("q1026")] , method = "gbm", trControl = fitControl,verbose = FALSE)
pred_gbm <- predict(model_gbm, test, type= "raw")
output <- cbind(output, pred_gbm)
table(output$q3001 == output$pred_gbm)
print(postResample(pred=pred_gbm, obs=as.factor(test[,"q3001"])))
#FALSE  TRUE 
#849  2945 
print(postResample(pred=pred_gbm, obs=test[,"q3001"]))
#Accuracy     Kappa 
#0.7762256 0.4845714 

#### XGBoost #####
TrainControl <- trainControl( method = "repeatedcv", number = 4, repeats = 4)
model_xg <- train(as.formula(paste("q3001 ~", attributes_collapsed)), data = train[!names(train) %in% c("q1026")] , method = "xgbLinear", trControl = TrainControl,verbose = FALSE)
pred_xg <- predict(model_xg, test)
# No results returned by the model
