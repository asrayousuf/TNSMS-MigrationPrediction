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

pMiss <- function(x){
  sum(is.na(x))/length(x)*100 > 25
}

filterMissingValues <- function(df){
missCol <- apply(df, 2, pMiss)
missColDF <- data.frame(names(missCol),missCol)
fil <- filter(missColDF, missColDF$missCol==FALSE)
result <- subset(df, select = names(df) %in% fil$names.missCol.)
return(result)
  }


## Identification & Primary Education
ir1[ir1 == ""] <- NA
i1 <- filterMissingValues(ir1)
convert_to_factor <- c(1:7, 9:18)
i1[,convert_to_factor] <- data.frame(apply(i1[convert_to_factor], 2, as.factor))
convert_to_numeric <- c(8)
i1[,convert_to_numeric] <- data.frame(apply(i1[convert_to_numeric], 2, as.numeric))
sapply(i1, class)

## Education details ####
ir3[ir3 == ""] <- NA
i3 <- filterMissingValues(ir3)
i3[,] <- data.frame(apply(i3[,], 2, as.factor))

## Migration Details ##
i7 <- filterMissingValues(ir7)
i7[,] <- data.frame(apply(i7[,], 2, as.factor))
#i7$q3005 <- as.numeric(i7$q3005)

### Labor allocation- Salary ###
i10 <- filterMissingValues(ir10)
i10[,] <- data.frame(apply(i10[,], 2, as.factor))


common_dim <- Reduce(intersect, list(names(i1),names(i7),names(i10)))

ir <- i1
for (f in c("i1","i7", "i10")) {
     if(f!="i1"){
         ir <- left_join(ir, removeDuplicates(get(f), common_dim), by=common_dim)
     }
 }
ir[ir == ""] <- NA
ir <- subset(ir, !is.na(ir$q3001))
ir <- subset(ir, ir$q3001 == (levels(ir$q3001)[2]) | ir$q3001 == (levels(ir$q3001)[3]))
ir$q3001 <- trimws(ir$q3001, "l")
ir$q3001 <- factor(ir$q3001)


###Household parameters###
temp <- read_dta("../household/sech01c.dta")
householdDetails <- subset(temp, select = c(1:7,48:67,68,88,108,128) )

householdDetails <- subset(householdDetails, select = c(1:7, 28:31))
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

### Non-farm enterprise ####

temp <- read_dta("../household/sech08a.dta")
nonFarmEnterprise <- subset(temp, select = c(1:9))
colnames(nonFarmEnterprise)[9] <- "CountOfBusiness"
colnames(nonFarmEnterprise)[8] <- "isInvolvedInBusiness"
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
colnames(loans)[8] <- "hasLoan"
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
houseOwnership["hasHouse"] <- NA
houseOwnership[houseOwnership<0] <- NA
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
for(i in 1:nrow(houseOwnership)){
  row <- houseOwnership[i,]
 if(!is.na(row[8]) & as.integer(row[8])>=1 & as.integer(row[8])<=5 ){
 row["hasHouse"] <- 1
 }
 else if(!is.na(row[8]) & as.integer(row[8])==6) {
   row["hasHouse"] <- 2
 }
 else{
   row["hasHouse"] <- NA
 } 
 houseOwnership[i,] <- row
}
colnames(houseOwnership)[3] <- "tehsil"
houseOwnership <- factorization(c(1:9), houseOwnership)

### Join individual and household attributes ###
common_attr <- c("state", "district","tehsil","block","psu","hhid","listing_id")
hh_data <- householdDetails
for (f in c("assetOwnership","nonFarmEnterprise", "houseOwnership", "loans", "financialAssets")) {
             hh_data <- left_join(hh_data, removeDuplicates(get(f), common_attr), by=common_attr)
 }
     
convert_to_factor <- c(1:11,14,16:18)
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


truth <- sapply(df,is.factor)
ihh_filtered <- data.frame(cbind(sapply(ihh_filtered[,truth],trimws,which="both"),ihh_filtered[,!truth]))
library(data.table)
setDT(ihh_filtered)[financialValue >=0 & financialValue <=200, financialAsset := "0-200"]
ihh_filtered[financialValue >200 & financialValue <=1000, financialAsset := "200-1000"]
ihh_filtered[financialValue >1000 & financialValue <=5000, financialAsset := "1000-5000"]
ihh_filtered[financialValue >5000, financialAsset := ">5000"]

ihh_filtered[as.numeric(as.character(educ)) == 0, education := "illiterate"]
ihh_filtered[as.numeric(as.character(educ)) > 0  & as.numeric(as.character(educ)) <=8, education := "primary"]
ihh_filtered[as.numeric(as.character(educ)) > 8 & as.numeric(as.character(educ)) <=12, education := "secondary"]
ihh_filtered[as.numeric(as.character(educ)) >12 & as.numeric(as.character(educ)) <=16, education := "undergrad"]
ihh_filtered[as.numeric(as.character(educ)) >16 & as.numeric(as.character(educ)) <=19, education := "postgrad"]
ihh_filtered[as.numeric(as.character(educ))<0, education := "others"]

ihh_filtered[as.numeric(as.character(Religion)) == 1, religion := "hindu"]
ihh_filtered[as.numeric(as.character(Religion)) == 2, religion := "islam"]
ihh_filtered[as.numeric(as.character(Religion)) == 3, religion := "christian"]
ihh_filtered[as.numeric(as.character(Religion)) == 4, religion := "sikh"]
ihh_filtered[as.numeric(as.character(Religion)) <= 0 | as.numeric(as.character(Religion)) > 4, religion := "others"]

ihh_filtered[as.numeric(as.character(SocialGroup)) == 1, caste := "SC"]
ihh_filtered[as.numeric(as.character(SocialGroup)) == 2, caste := "ST"]
ihh_filtered[as.numeric(as.character(SocialGroup)) == 3 | as.numeric(as.character(SocialGroup)) == 4, caste := "OBC"]
ihh_filtered[as.numeric(as.character(SocialGroup)) < 1 | as.numeric(as.character(SocialGroup)) > 4, caste := "others"]


ihh_filtered[age >= 0 & age <= 14, ageCategory := "child"]
ihh_filtered[age >=15 & age <= 24, ageCategory := "youth"]
ihh_filtered[age >= 25 & age <=64, ageCategory := "adult"]
ihh_filtered[age >=65, ageCategory := "seniors"]

ihh_filtered[as.numeric(as.character(SocialGroup)) == 2, caste := "ST"]
ihh_filtered[as.numeric(as.character(SocialGroup)) == 3 | as.numeric(as.character(SocialGroup)) == 4, caste := "OBC"]
ihh_filtered[as.numeric(as.character(SocialGroup)) < 1 | as.numeric(as.character(SocialGroup)) > 4, caste := "others"]


ihh_filtered <- as.data.frame(ihh_filtered)
convert_to_factor <- c("education", "religion", "caste", "financialAsset", "ageCategory")
ihh_filtered[,convert_to_factor] <- data.frame(apply(ihh_filtered[convert_to_factor], 2, as.factor))

convert_to_numeric <- c("age")
ihh_filtered[,convert_to_numeric] <- data.frame(apply(ihh_filtered[convert_to_numeric], 2, as.numeric))

ihh_filtered <- subset(ihh_filtered, select = c("district", "gender", "q1002", "q1003", "q1004", "q1006", "q3001", "q4001", "isInvolvedInBusiness", "hasHouse", "isTenant", "hasLoan", "financialAsset", "education", "religion", "caste", "ageCategory"))

colnames(ihh_filtered)[2] <- "sex"
colnames(ihh_filtered)[3] <- "canRead"
colnames(ihh_filtered)[4] <- "canWrite"
colnames(ihh_filtered)[5] <- "attendedPreSchool"
colnames(ihh_filtered)[6] <- "attendedSchoolOrCollege"
colnames(ihh_filtered)[7] <- "hasMigrated"
colnames(ihh_filtered)[8] <- "employedAsSalaryWorkerLast12mon"

ihh_filtered$attendedSchoolOrCollege[as.numeric(as.character(ihh_filtered$attendedSchoolOrCollege)) <=0] <- NA
ihh_filtered$attendedPreSchool[as.numeric(as.character(ihh_filtered$attendedPreSchool)) <=0] <- NA
ihh_filtered$canRead[as.numeric(as.character(ihh_filtered$canRead)) <=0] <- NA
ihh_filtered$canRead[as.numeric(as.character(ihh_filtered$canWrite)) <=0] <- NA

ihh_filtered$employedAsSalaryWorkerLast12mon <- trimws(ihh_filtered$employedAsSalaryWorkerLast12mon, "l")
ihh_filtered$employedAsSalaryWorkerLast12mon <- factor(ihh_filtered$employedAsSalaryWorkerLast12mon)

ihh_filtered$sex <- trimws(ihh_filtered$sex, "l")
ihh_filtered$sex <- factor(ihh_filtered$sex)

ihh_filtered$hasHouse <- trimws(ihh_filtered$hasHouse, "l")
ihh_filtered$hasHouse <- factor(ihh_filtered$hasHouse)

missCol <- apply(ihh_filtered, 2, pMiss)
missColDF <- data.frame(names(missCol),missCol)
fil <- filter(missColDF, missColDF$missCol==FALSE)
ihh_filtered <- subset(ihh_filtered, select = names(ihh_filtered) %in% fil$names.missCol.)
ihh_filtered <- ihh_filtered[complete.cases(ihh_filtered),]

tnsmsData <- subset(ihh_filtered, select = c(sex, ageCategory, education, hasHouse, religion, caste, employedAsSalaryWorkerLast12mon, hasMigrated))

### NSSO Data ###

setwd("/Users/asra/Documents/IFMR/64thRound/Nss64_10.2/ExtractedData/")


pMiss <- function(x){
     sum(is.na(x))/length(x)*100 > 25
 }
 
filterMissingValues <- function(df){
     missCol <- apply(df, 2, pMiss)
     missColDF <- data.frame(names(missCol),missCol)
     fil <- filter(missColDF, missColDF$missCol==FALSE)
     result <- subset(df, select = names(df) %in% fil$names.missCol.)
     return(result)
 }


data1 <- read_dta("DataLevel1.dta")
data1$CommonItems <- paste(data1$centrecode, data1$fsuserialno, data1$rnd, data1$schedulenumber, data1$smpl, data1$sector, data1$stateregion, data1$district, data1$stratum, data1$substratum, data1$subround, data1$subsample, data1$fodsubregion, data1$hgsbno, data1$stage2stratumno, data1$samplehhldno, sep = "")
data1 <- subset(data1, select=-c(lvl,filler, blnk, specialchars))
data2 <- read_dta("DataLevel2.dta")
data2 <- subset(data2, select=-c(lvl,filler, blnk, specialchars))
data3 <- read_dta("DataLevel3.dta")
data3 <- subset(data3, select=-c(lvl,filler, blnk, specialchars))
colnames(data3)[4] <- "Age"

data4 <- read_dta("DataLevel4.dta")
data4[,c("PersonSerialno", "Age")] <- data.frame(apply(data4[c("PersonSerialno", "Age")], 2, as.numeric))
data4 <- subset(data4, select=-c(lvl,filler, blnk, specialchars))

data5 <- read_dta("DataLevel5.dta")
data5[,c("PersonSerialno","Age")] <- data.frame(apply(data5[c("PersonSerialno","Age")], 2, as.numeric))
data5 <- subset(data5, select=-c(lvl,filler, blnk, specialchars))

data6 <- read_dta("DataLevel6.dta")
data6[,c("PersonSerialno","Age")] <- data.frame(apply(data6[c("PersonSerialno","Age")], 2, as.numeric))
data6 <- subset(data6, select=-c(lvl,filler, blnk, specialchars))

  dt <- left_join(data1, data2, by="CommonItems")

  dt2 <- left_join(data4, data6, by=c("CommonItems", "PersonSerialno", "Age"))
  dt2 <- left_join(dt2, dt, by="CommonItems")

nssoData <- dt2[which(dt2$sector == '1'),]
nssoData$Landpossessedcode[nssoData$Landpossessedcode==""] <- 0 
nssoData <- subset(nssoData, select = c(sex, Age, GeneralEducation, Maritalstatus, NatureofMovement, Patternofmigration, WhetherStydAwyfromVllgeOrTown, PlcOfEnmrtnDiffersfromlastupr, Landpossessedcode, Religion, SocialGroup, ParticularsoflastUprStateCntry, Relationtohead, TechnicalEducation, UsualPrncplActvtyStatus, PlcOfEnmrtnWasUprAnytimeinPast, MonthlyHHconsumerexpenditure, HHSize, AmntofremittancesLast365days ))
convert_to_factor <- c("sex","GeneralEducation","Maritalstatus", "Patternofmigration", "PlcOfEnmrtnDiffersfromlastupr", "NatureofMovement", "WhetherStydAwyfromVllgeOrTown", "Landpossessedcode", "Religion", "SocialGroup", "ParticularsoflastUprStateCntry", "Relationtohead", "TechnicalEducation","UsualPrncplActvtyStatus", "PlcOfEnmrtnWasUprAnytimeinPast")
nssoData[,convert_to_factor] <- data.frame(apply(nssoData[convert_to_factor], 2, as.factor))
sapply(nssoData, class)
convert_to_numeric <- c("MonthlyHHconsumerexpenditure", "HHSize", "AmntofremittancesLast365days")
nssoData[,convert_to_numeric] <- data.frame(apply(nssoData[convert_to_numeric], 2, as.numeric))

nssoData$hasMigrated <- NA
nssoData$hasMigrated[as.numeric(as.character(nssoData$PlcOfEnmrtnDiffersfromlastupr))==1 & as.numeric(as.character(nssoData$NatureofMovement))==3] <- 1
nssoData$hasMigrated[as.numeric(as.character(nssoData$PlcOfEnmrtnDiffersfromlastupr))==2 | as.numeric(as.character(nssoData$NatureofMovement)) %in% c(1,2)] <- 2

nssoData[nssoData==""]<- NA
nssoData <- filterMissingValues(nssoData)
nssoData <- nssoData[complete.cases(nssoData),]

library(data.table)
setDT(nssoData)[as.numeric(as.character(GeneralEducation)) == 01, education := "illiterate"]
nssoData[as.numeric(as.character(GeneralEducation)) >= 6  & as.numeric(as.character(GeneralEducation)) <=8, education := "primary"]
nssoData[as.numeric(as.character(GeneralEducation)) >=10 & as.numeric(as.character(GeneralEducation)) <=11, education := "secondary"]
nssoData[as.numeric(as.character(GeneralEducation)) >=12 & as.numeric(as.character(GeneralEducation)) <=13, education := "undergrad"]
nssoData[as.numeric(as.character(GeneralEducation)) ==14, education := "postgrad"]
nssoData[as.numeric(as.character(GeneralEducation)) %in% c(2,3,4,5) | as.character(GeneralEducation) == "", education := "others"]

nssoData[as.numeric(as.character(Landpossessedcode)) != 0, hasHouse := "1"]
nssoData[as.numeric(as.character(Landpossessedcode)) == 0, hasHouse := "2"]


nssoData[as.numeric(as.character(Religion)) == 1, religion := "hindu"]
nssoData[as.numeric(as.character(Religion)) == 2, religion := "islam"]
nssoData[as.numeric(as.character(Religion)) == 3, religion := "christian"]
nssoData[as.numeric(as.character(Religion)) == 4, religion := "sikh"]
nssoData[as.numeric(as.character(Religion)) == 0 | as.numeric(as.character(Religion)) > 4, religion := "others"]

nssoData[as.numeric(as.character(SocialGroup)) == 1, caste := "ST"]
nssoData[as.numeric(as.character(SocialGroup)) == 2, caste := "SC"]
nssoData[as.numeric(as.character(SocialGroup)) == 3, caste := "OBC"]
nssoData[as.numeric(as.character(SocialGroup)) == 9, caste := "others"]

nssoData[Age >= 0 & Age <= 14, ageCategory := "child"]
nssoData[Age >=15 & Age <= 24, ageCategory := "youth"]
nssoData[Age >= 25 & Age <=64, ageCategory := "adult"]
nssoData[Age >=65, ageCategory := "seniors"]


nssoData[as.numeric(as.character(UsualPrncplActvtyStatus)) %in% c(11, 12, 21, 31, 41, 51), employedAsSalaryWorkerLast12mon := "1"]
nssoData[!as.numeric(as.character(UsualPrncplActvtyStatus)) %in% c(11, 12, 21, 31, 41, 51), employedAsSalaryWorkerLast12mon := "2"]

colnames(nssoData)[4] <- "sex"

nssoData <- as.data.frame(nssoData)
nssoData <- subset(nssoData, select = c(sex, ageCategory, education, hasHouse, religion, caste, employedAsSalaryWorkerLast12mon, hasMigrated))

convert_to_factor <- c("education", "religion", "caste", "hasMigrated", "hasHouse", "employedAsSalaryWorkerLast12mon", "ageCategory", "sex")
nssoData[,convert_to_factor] <- data.frame(apply(nssoData[convert_to_factor], 2, as.factor))

finalData <- rbind(tnsmsData, nssoData)

indexes = sample(1:nrow(nssoData), size=0.3*nrow(nssoData))
train <- rbind(tnsmsData, nssoData[indexes,])
test <- nssoData[-indexes,]

### Machine-learning Models ###

### NaiveBayes ###
set.seed(123)
model_bayes <-naiveBayes(hasMigrated ~., data = train)
pred_bayes <- predict(model_bayes,test)
output <- cbind(test, pred_bayes)
table(output$hasMigrated == output$pred_bayes)
print(postResample(pred=pred_bayes, obs=output[,"hasMigrated"]))


### Random Forest ###
set.seed(123)
model_rf <- randomForest(hasMigrated ~., data = train,ntree=500)
pred_rf <- predict(model_rf,test)
output <- cbind(output, pred_rf)
table(output$hasMigrated == output$pred_rf)
print(postResample(pred=pred_rf, obs=output[,"hasMigrated"]))


### GBM###
set.seed(123)
fitControl <- trainControl( method = "repeatedcv", number = 2, repeats = 2)
model_gbm <- train(hasMigrated ~., data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
pred_gbm <- predict(model_gbm, test, type= "raw")
output <- cbind(output, pred_gbm)
table(output$hasMigrated == output$pred_gbm)
print(postResample(pred=pred_gbm, obs=output[,"hasMigrated"]))


### Decision Tree ###
set.seed(123)
dt_model <- rpart(hasMigrated ~., data = train, method="class")
pred_dt <- predict(dt_model,test, type= "class")
output <- cbind(output, pred_dt)
table(output$hasMigrated == output$pred_dt)
print(postResample(pred=pred_dt, obs=output[,"hasMigrated"]))


#### Logistic Regression ####
set.seed(123)
model_glm <- glm (hasMigrated ~ ., data = train, family = binomial(link="logit"))
pred_glm <- predict(model_glm, type = 'response', newdata=test)
table(test$hasMigrated, pred_glm > 0.5)
pred_glm <- ifelse(pred_glm > 0.5,2,1)
output <- cbind(output, pred_glm)
table(output$hasMigrated == output$pred_glm)
print(postResample(pred=pred_glm, obs=output[,"hasMigrated"]))

### Plotting ROC Curve for the models ###

 prediction_rf <- prediction(as.numeric(pred_rf), as.numeric(test$hasMigrated))
 prediction_bayes <- prediction(as.numeric(pred_bayes), as.numeric(test$hasMigrated))
 prediction_gbm <- prediction(as.numeric(pred_gbm), as.numeric(test$hasMigrated))
 prediction_glm <- prediction(as.numeric(pred_glm), as.numeric(test$hasMigrated))
 prediction_dt <- prediction(as.numeric(pred_dt), as.numeric(test$hasMigrated))
 perf_rf <- performance(prediction_rf,"tpr","fpr")
 perf_bayes <- performance(prediction_bayes,"tpr","fpr")
 perf_gbm <- performance(prediction_gbm,"tpr","fpr")
 perf_glm <- performance(prediction_glm,"tpr","fpr")
 perf_dt <- performance(prediction_dt,"tpr","fpr")


 pr_rf <- performance(prediction_rf,"prec","rec")
 pr_bayes <- performance(prediction_bayes,"prec","rec")
 pr_gbm <- performance(prediction_gbm,"prec","rec")
 pr_glm <- performance(prediction_glm,"prec","rec")
 pr_dt <- performance(prediction_dt,"prec","rec")


 plot(pr_rf)
 par(new=TRUE)
 plot(pr_bayes, col="green", main="ROC plot")
 par(new=TRUE)
 plot(pr_gbm, col="red")
 par(new=TRUE)
 plot(pr_glm, col="blue")
 par(new=TRUE)
 plot(pr_dt, col="yellow")
 legend(0.9,0.4, legend=c("Random Forest", "GBM", "Decision Tree", "Naive Bayes", "GLM"), col=c("black","red", "yellow", "green", "blue"), lty=1:1, cex=0.6) 

