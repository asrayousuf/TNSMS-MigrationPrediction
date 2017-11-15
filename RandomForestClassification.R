setwd("/Users/asra/Documents/IFMR/PanelStudy/data/rural/individual")
library('haven')
library('randomForest')
library('pROC')

# Read individual common survey data
ir1 <- read_dta("isec01.dta")
# Read migration module data
ir7 <- read_dta("isec07.dta")
ir17 <- merge(ir1, ir7, by=c("hhid", "memid", "state", "district", "tehsil","listing_id","region", "block", "psu"))

# Remove records for NA value in dependent variable
ir17 <- subset(ir17, ir17$q3001==1 | ir17$q3001==2)
ir17$q3001 <- as.factor(ir17$q3001)

#Handling missing values through 'mice'
library('mice')
ir17_wo_3001 <- subset(ir17, select = -c(q3001))

imputed_Data <- mice(ir17_wo_3001, m=5, maxit = 10, method = 'pmm', seed = 500)
completeData <- complete(imputed_Data, 2)

#Adding q3001- the dependent variable back to the dataset
ir17_sub <- subset(ir17, select = c('hhid','memid', 'q3001'))
mergeDataset <- merge(completeData, ir17_sub, by=c('hhid', 'memid'))
#Remove 'occup' since all values are NA
mergeDataset <- subset(mergeDataset, select= -occup)

#Segregating the data into training and test
train <- mergeDataset[1:10000,]
test <- mergeDataset[10001:13656,]

#Modelling on char columns throws errors, hence removing them
exclude_cols <- c('state', 'tehsil', 'block', 'q1026')

# Modelling and predicting with all variables
model_rf <- randomForest(q3001 ~., data=train[!names(train) %in% exclude_cols])
preds <- predict(model_rf, test[,-59])
preds <- as.numeric(preds)
test$q3001 <- as.numeric(test$q3001)
auc(preds, test$q3001)
#Area under the curve: 0.7128

#Predicting with variables with high MeanDecreaseGini
model_rf1 <-randomForest(q3001 ~ gender+ age+ q1028+ listing_id+hhid+ psu+ q3002c+ memid+s3mid+ q1018+ q3004c+ q1025+q1027d+ q1022+ q1013+ q1030+s3rid+ q1010+ district+q1031+ resid+ q1017+ q1012+educ+q1005+q3005+ q1009+q1023+ proxy+s3prid+ q1011+q1021+ q1007+q1029+ q1006+q3004d+ q1008+q1027e+ q3003+q1015, data = train[ !names(train) %in% exclude_cols ])
preds1 <- predict(model_rf1,test[,-59])
preds <- as.numeric(preds)
test$q3001 <- as.numeric(test$q3001)
auc(preds1, test$q3001)
#Area under the curve: 0.7155

#Random forest with features from Boruta
model_rf1 <-randomForest(q3001 ~ memid + district + tehsil+listing_id+block + psu + age + gender +  educ + resid + q1002 + q1003 + q1004 + q1006 + q1010 + q1011 + q1012 + q1014 + q1020 + q1021 + q1022 + q1023 + q1024 + q1025 + q1026 + q1027d + q1027e + q1028 + q1029 + s3mid + s3rid, data = train[ !names(train) %in% exclude_cols ])
preds1 <- predict(model_rf1,test)
preds <- as.numeric(preds)
test$q3001 <- as.numeric(test$q3001)
auc(preds1, test$q3001)
#Area under the curve: 0.6924



