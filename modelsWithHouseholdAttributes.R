setwd("/Users/asra/Documents/IFMR/PanelStudy/data/rural/individual")
library('haven')
library('Boruta')
library('dplyr')
library('randomForest')
library('caret')
library('e1071')
library('rpart')

# Read individual common survey data
ir1 <- read_dta("isec01.dta")
ir3 <- read_dta("isec03.dta")
ir7 <- read_dta("isec07.dta")
ir10 <- read_dta("isec10.dta")
ir11 <- read_dta("isec11.dta")
ir12 <- read_dta("isec12.dta")

factorization <- function(convert_to_factor, df){
     df[,convert_to_factor] <- data.frame(apply(df[convert_to_factor], 2, as.factor))
     return(df)
 }

 removeDuplicates <- function(content, attr){
  return(content[!duplicated(content[,attr]),])
}

pMiss <- function(df){
  sum(is.na(df))/length(x)*100 > 80
}

filterMissingValues <- function(df){
missCol <- apply(df, 2, pMiss)
missColDF <- data.frame(names(missCol),missCol)
fil <- filter(missColDF, missColDF$missCol==FALSE)
df <- subset(df, select = names(df) %in% fil$names.missCol.)
return(df)
  }


## Identification & Primary Education
ir1[ir1 == ""] <- NA
i1 <- filterMissingValues(ir1)
convert_to_factor <- c(1:7, 9:18, 20:22, 24, 25:28, 30:34, 36:38)
i1[,convert_to_factor] <- data.frame(apply(i1[convert_to_factor], 2, as.factor))
convert_to_numeric <- c(19)
i1[,convert_to_numeric] <- data.frame(apply(i1[convert_to_numeric], 2, as.numeric))
sapply(i1, class)

## Education details ####
ir3[ir3 == ""] <- NA
i3 <- filterMissingValues(ir3)
i3[,] <- data.frame(apply(i3[,], 2, as.factor))

## Migration Details ##
i7 <- filterMissingValues(ir7)
i7[,] <- data.frame(apply(i7[,], 2, as.factor))
i7$q3005 <- as.numeric(i7$q3005)

### Labor allocation- Salary ###
i10 <- filterMissingValues(ir10)
i10[,] <- data.frame(apply(i10[,], 2, as.factor))

### Labor allocation: Non-agricultural casual ##
i11 <- filterMissingValues(ir11)
i11[,] <- data.frame(apply(i11[,], 2, as.factor))


i12 <- filterMissingValues(ir12)
convert_to_numeric <- c(19,20,25,26, 28, 29, 30, 31, 32, 33, 35, 45, 46, 47, 48, 49, 63)
i12[,convert_to_numeric] <- data.frame(apply(i12[convert_to_numeric], 2, as.numeric))
i12[,-convert_to_numeric] <- data.frame(apply(-i12[convert_to_numeric], 2, as.factor))
sapply(i12, class)

## Health
temp <- read_stata("isec04.dta")
health <- subset(temp, select = c(1:11,14,221,222))
health["haveHealthIssues"] <- 0
health[health<0] <- 0
for(i in 1:nrow(health)){
	row <- health[i,]
 if((!is.na(row[11]) & row[11]==3 ) |  (!is.na(row[12]) & row[12]==3) ){
 row["haveHealthIssues"] <- 1
  }
else{
	row["haveHealthIssues"] <- 2
}
 health[i,] <- row
}
x <- health[health$haveHealthIssues==1, "hhid"]
x <- x$hhid
 health["hhHasHeathIssues"] <- 2
health[health$hhid %in% x, "hhHasHeathIssues"] <- 1
health <- subset (health, select = c(1:7, 13,14,15,16))
health[,] <- data.frame(apply(health[,], 2, as.factor))



common_dim <- Reduce(intersect, list(names(i1), names(i3),names(i7),names(i10),names(i11),names(i12) ))

ir <- i1
for (f in c("i1", "i3","health","i7", "i10", "i11", "i12")) {
     if(f!="i1"){
         print(f)
         ir <- left_join(ir, removeDuplicates(get(f), common_dim), by=common_dim)
     }
 }
ir[ir == ""] <- NA
ir <- subset(ir, !is.na(ir$q3001))
ir <- subset(ir, ir$q3001 == (levels(ir$q3001)[2]) | ir$q3001 == (levels(ir$q3001)[3]))
ir$q3001 <- factor(ir$q3001)


###Household parameters###
temp <- read_dta("../household/sech01c.dta")
householdDetails <- subset(temp, select = c(1:7,48:67,68,88,108,128) )
householdDetails["MaleCount"] <- 0
householdDetails["FemaleCount"] <- 0
householdDetails["TotalMemberCount"] <- 0
householdDetails[householdDetails<0] <- 0
for(i in 1:nrow(householdDetails)){
	row <- householdDetails[i,]
 for(j in 8:27){
 if(!is.na(row[j]) & as.integer(row[j])==1){
 row["MaleCount"] <- row["MaleCount"] + 1 
}
 else if(!is.na(row[j]) & as.integer(row[j])==2){
 	row["FemaleCount"] <- row["FemaleCount"] + 1 
 }
 }
 row["TotalMemberCount"] <- row["MaleCount"] + row["FemaleCount"]
 householdDetails[i,] <- row
}
householdDetails <- subset(householdDetails, select = c(1:7, 28:34))
colnames(householdDetails)[3] <- "tehsil"
colnames(householdDetails)[8] <- "Jati"
colnames(householdDetails)[9] <- "Subcaste"
colnames(householdDetails)[10] <- "SocialGroup"
colnames(householdDetails)[11] <- "Religion"
householdDetails <- factorization(c(1:7), householdDetails)

## Assets (mainly farm) ####
temp <- read_dta("../household/sech07a.dta")
assetOwnership <- subset(temp, select = c(1:7, 49:171))
assetOwnership["CountOfAssets"] <- 0
assetOwnership["TotalValueOfAssets"] <- 0
assetOwnership[assetOwnership<0] <- 0
for(i in 1:nrow(assetOwnership)){
	row <- assetOwnership[i,]
 for(j in 49:89){
 if(!is.na(row[j]) & as.integer(row[j])==1){
 row["CountOfAssets"] <- row["CountOfAssets"] + 1 
  }
 }
 for(j in 90:130){
 if(!is.na(row[j])){
 row["TotalValueOfAssets"] <- row["TotalValueOfAssets"] + row[j]
  }
 }
 assetOwnership[i,] <- row
}
assetOwnership <- subset(assetOwnership, select= c(1:7, 131:132))
colnames(assetOwnership)[3] <- "tehsil"
assetOwnership <- factorization(c(1:7), assetOwnership)

#### livestock ownership #####

temp <- read_dta("../household/sech07f.dta")
livestockOwnership <- subset(temp, select = c(1:7, 74:139))
livestockOwnership[livestockOwnership<0] <- 0
livestockOwnership["CountOfLivestock"] <- 0
livestockOwnership["TotalValueOfLivestock"] <- 0
for(i in 1:nrow(livestockOwnership)){
	row <- livestockOwnership[i,]
 for(j in 8:40){
 if(!is.na(row[j])){
 row["CountOfLivestock"] <- row["CountOfLivestock"] + row[j]
  }
 }
 for(j in 41:73){
 if(!is.na(row[j])){
 row["TotalValueOfLivestock"] <- row["TotalValueOfLivestock"] + row[j]
  }
 }
 livestockOwnership[i,] <- row
}
colnames(livestockOwnership)[3] <- "tehsil"
livestockOwnership <- subset(livestockOwnership, select = c(1:7,74:75))
livestockOwnership <- factorization(c(1:7), livestockOwnership)

### Non-farm enterprise ####

temp <- read_dta("../household/sech08a.dta")
nonFarmEnterprise <- subset(temp, select = c(1:9))
colnames(nonFarmEnterprise)[9] <- "CountOfBusiness"
colnames(nonFarmEnterprise)[8] <- "IsInvolvedInBusiness"
colnames(nonFarmEnterprise)[3] <- "tehsil"
nonFarmEnterprise <- factorization(c(1:8), nonFarmEnterprise)

### Finances ####

temp <- read_dta("../household/sech09b.dta")
financialAssets <- subset(temp, select = c(1:7, 68:87))
financialAssets["financialValue"] <- 0
financialAssets[financialAssets<0] <- 0
for(i in 1:nrow(financialAssets)){
	row <- financialAssets[i,]
 for(j in 8:27){
 if(!is.na(row[j])){
 row["financialValue"] <- row["financialValue"] + row[j]
  }
   }
 financialAssets[i,] <- row
}
financialAssets <- subset(financialAssets, select = c(1:7,28))
colnames(financialAssets)[3] <- "tehsil"
convert_to_factor <- c(1:7)
financialAssets <- factorization(c(1:7), financialAssets)

### Loans ###

loans <- read_dta("../household/sech10a.dta")
loans <- subset(loans, select = c(1:9))
colnames(loans)[8] <- "HaveLoanCurrently"
colnames(loans)[9] <- "CountOfOutstandingLoans"
colnames(loans)[3] <- "tehsil"
loans <- factorization(c(1:8), loans)

### Loan Amount ###

temp <- read_dta("../household/sech10b.dta")
loanAmount <- subset(temp, select = c(1:7, 258:267))
loanAmount["ValueOfLoan"] <- 0
loanAmount[loanAmount<0] <- 0
for(i in 1:nrow(loanAmount)){
	row <- loanAmount[i,]
 for(j in 8:17){
 if(!is.na(row[j])){
 row["ValueOfLoan"] <- row["ValueOfLoan"] + row[j]
  }
   }
 loanAmount[i,] <- row
}
loanAmount <- subset(loanAmount, select = c(1:7,18))
colnames(loanAmount)[3] <- "tehsil"
loanAmount <- factorization(c(1:8), loanAmount)

### House Ownership ###
temp <- read_dta("../household/sech12.dta")
houseOwnership <- subset(temp, select = c(1:8))
colnames(houseOwnership)[8] <- "houseOwnerStatus"
houseOwnership["isTenant"] <- 0
houseOwnership[houseOwnership<0] <- 0
for(i in 1:nrow(houseOwnership)){
	row <- houseOwnership[i,]
 if(!is.na(row[8]) & as.integer(row[8])==6){
 row["isTenant"] <- 1
 }
 else{
 	 row["isTenant"] <- 2
 } 
 houseOwnership[i,] <- row
}
colnames(houseOwnership)[3] <- "tehsil"
houseOwnership <- factorization(c(1:9), houseOwnership)

### Join individual and household attributes ###
common_attr <- c("state", "district","tehsil","block","psu","hhid","listing_id")
hh_data <- householdDetails
for (f in c("assetOwnership", "livestockOwnership","nonFarmEnterprise", "loanAmount", "houseOwnership", "loans", "financialAssets")) {
             hh_data <- left_join(hh_data, removeDuplicates(get(f), common_attr), by=common_attr)
 }
     
convert_to_factor <- c(1:11,19, 22, 23)
hh_data[,convert_to_factor] <- data.frame(apply(hh_data[convert_to_factor], 2, as.factor))

ir[,common_dim] <- data.frame(apply(ir[common_dim], 2, as.factor))

ihh <- left_join(ir, removeDuplicates(hh_data, common_attr), by=common_attr)

### Data Cleaning ###

pMiss <- function(x){sum(is.na(x))/length(x)*100 > 15}
missCol <- apply(ihh, 2, pMiss)
missColDF <- data.frame(names(missCol),missCol)
fil <- filter(missColDF, missColDF$missCol==FALSE)
ihh_cleaned <- subset(ihh, select = names(ihh) %in% fil$names.missCol.)
ihh_filtered <- ihh_cleaned[complete.cases(ihh_cleaned),]

ihh_filtered[,common_dim] <- data.frame(apply(ihh_filtered[common_dim], 2, as.factor))

#Sample Indexes
indexes = sample(1:nrow(ihh_filtered), size=0.3*nrow(ihh_filtered))

# Split data
test = ihh_filtered[indexes,]
dim(test)
train = ihh_filtered[-indexes,]
dim(train)

set.seed(111)
boruta.train <- Boruta(q3001~.-memid, data = train, doTrace = 2)
boruta.train$finalDecision
final.boruta <- TentativeRoughFix(boruta.train)
attributes <- getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta)
attributes_collapsed <- paste( attributes, collapse = " + ")


#attributes_collapsed <- "district + tehsil + block + psu + hhid + listing_id + age + gender + educ + resid + q1002 + q1003 + q1004 + q1006 + s3mid + s3rid + s4mid + s4rid + q4001 + q4036a_1 + q4036a_2 + q4036a_3 + q4036b_1 + q4036b_2 + q4036b_3 + q4037_1 + q4037_2 + q4037_3"

### NaiveBayes ###
model_bayes <-naiveBayes(q3001 ~., data = train)
summary(model_bayes)
pred_bayes <- predict(model_bayes,test)
output <- cbind(test, pred_bayes)
table(output$q3001 == output$pred_bayes)
#FALSE  TRUE 
#  849  2337 
print(postResample(pred=pred_bayes, obs=output[,"q3001"]))
# Accuracy     Kappa 
#0.7335217 0.4144379 

 ### Gradient Boosting ####
 
fitControl <- trainControl( method = "repeatedcv", number = , repeats = 2)
model_gbm <- train(q3001 ~., data = train[!names(train) %in% c("state", "region")] , method = "gbm", trControl = fitControl,verbose = FALSE)
pred_gbm <- predict(model_gbm, test, type= "raw")
output <- cbind(output, pred_gbm)
table(output$q3001 == output$pred_gbm)
#FALSE  TRUE 
 # 731  2455 
 print(postResample(pred=pred_gbm, obs=output[,"q3001"]))
#Accuracy     Kappa 
#0.7705587 0.4854944 


#### Random Forest ###
model_rf <- randomForest(q3001 ~., data = train[!names(train) %in% c("block","tehsil","psu", "hhid", "listing_id", "Subcaste", "Jati")],ntree=500)
pred_rf <- predict(model_rf,test)
output <- cbind(test, pred_rf)
table(output$q3001 == output$pred_rf)
print(postResample(pred=pred_rf, obs=output[,"q3001"]))
#FALSE  TRUE 
#  762  2424 
print(postResample(pred=pred_rf, obs=output[,"q3001"]))
# Accuracy     Kappa 
# 0.7608286 0.4281082 

### SVM ####
model_svm <-svm(q3001 ~., data = train)
pred_svm <- predict(model_svm,test)
output <- cbind(test, pred_svm)
table(output$q3001 == output$pred_svm)
#FALSE  TRUE 
# 1041  2145 
print(postResample(pred=pred_svm, obs=output[,"q3001"]))
#Accuracy    Kappa 
#0.673258 0.000000 

### Decision Tree ###
dt_model <- rpart(q3001 ~., data = train, method="class")
pred_dt <- predict(dt_model,test, type= "class")
output <- cbind(output, pred_dt)
table(output$q3001 == output$pred_dt)
print(postResample(pred=pred_dt, obs=output[,"q3001"]))
# FALSE  TRUE 
# 1064  2122 
print(postResample(pred=pred_dt, obs=output[,"q3001"]))
# Accuracy     Kappa 
# 0.6660389 0.1520564 








