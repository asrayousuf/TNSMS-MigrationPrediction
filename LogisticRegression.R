setwd("/Users/asra/Documents/IFMR/PanelStudy/data/rural/individual")
library('haven')
library('pROC')
install.packages('caTools')
library(caTools)

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
imputed_Data <- mice(ir17_wo_3001, m=5, maxit = 50, method = 'polyreg', seed = 500)
completeData <- complete(imputed_Data, 2)

#Adding q3001- the dependent variable back to the dataset
ir17_sub <- subset(ir17, select = c('hhid','memid', 'q3001')) 
mergeDataset <- merge(completeData, ir17_sub, by=c('hhid', 'memid'))
#Remove 'occup' since all values are NA
mergeDataset <- subset(mergeDataset, select= -occup)

#Segregating the data into training and test
train <- mergeDataset[1:10000,]
test <- mergeDataset[10001:13656,]
pa
#Modelling on char columns throws errors, hence removing them
exclude_cols <- c('state', 'tehsil', 'block', 'q1026')

#logistic regression model
model <- glm (q3001 ~  gender+ age+ q1028+ listing_id+hhid+ psu+ q3002c+ memid+s3mid+ q1018+ q3004c+ q1025+q1027d+ q1022+ q1013+ q1030+s3rid+ q1010+ district+q1031+ resid+ q1017+ q1012+educ+q1005+q3005+ q1009+q1023+ proxy+s3prid+ q1011+q1021+ q1007+q1029+ q1006+q3004d+ q1008+q1027e+ q3003+q1015, data = train[!names(train) %in% exclude_cols], family = binomial)
summary(model)
predict <- predict(model, type = 'response', newdata=test)
table(train$q3001, predict > 0.5)

#FALSE TRUE
#1   619  408
#2   531 2098

library(ggplot2)
ggplot(train, aes(x=Rating, y=Recommended)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)
#using RandomForest for feature selection
model <- glm (q3001 ~  gender+ age+ q1028+ listing_id+hhid+ psu+ q3002c+ memid+s3mid+ q1018+ q3004c+ q1025+q1027d+ q1022+ q1013+ q1030+s3rid+ q1010+ district+q1031+ resid+ q1017+ q1012, data = train[!names(train) %in% exclude_cols], family = binomial)
predict <- predict(model, type = 'response', newdata=test)
table(test$q3001, predict > 0.5)

#FALSE TRUE
#1   630  397
#2   521 2108

#using Boruta for feature selection
model <- glm(q3001 ~ memid + district + tehsil + listing_id + block + psu + age + gender + educ + resid + proxy + q1002 + q1003 + q1004 + q1006 + q1010 + q1011 + q1012 + q1013 + q1014 + q1017 + q1018 + q1021 + q1023 + q1025 + q1026 + q1027d + q1028 + q1029 + q1030 + s3mid + s3rid + q3002c + q3002d + q3003 + q3004c + q3005, data = train[!names(train) %in% exclude_cols], family = binomial)
predict <- predict(model, type = 'response', newdata=test)
table(test$q3001, predict > 0.5)

model <- glm(q3001 ~ memid + district + tehsil+listing_id+block + psu + age + gender +  educ + resid + q1002 + q1003 + q1004 + q1006 + q1010 + q1011 + q1012 + q1014 + q1020 + q1021 + q1022 + q1023 + q1024 + q1025 + q1026 + q1027d + q1027e + q1028 + q1029 + s3mid + s3rid,  data = train[!names(train) %in% exclude_cols], family = binomial)
predict <- predict(model, type = 'response', newdata=test)
table(test$q3001, predict > 0.5)

