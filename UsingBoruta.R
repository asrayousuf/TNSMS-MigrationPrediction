setwd("/Users/asra/Documents/IFMR/PanelStudy/data/rural/individual")
library('haven')
library(Boruta)

# Read individual common survey data
ir1 <- read_dta("isec01.dta")
# Read migration module data
ir7 <- read_dta("isec07.dta")
ir17 <- merge(ir1, ir7, by=c("hhid", "memid", "state", "district", "tehsil","listing_id","region", "block", "psu"))

# Remove records for NA value in dependent variable
ir17 <- subset(ir17, ir17$q3001==1 | ir17$q3001==2)
ir17$q3001 <- as.factor(ir17$q3001)

#Clean data -- Remove columns which have >80% missing value
pMiss <- function(x){sum(is.na(x))/length(x)*100 > 80}
x <- apply(ir17, 2, pMiss)
y <- data.frame(names(x),x)
fil <- filter(y, y$x==FALSE)
ir17_filtered <- subset(ir17, select = names(ir17) %in% fil$names.x.)

#Handling missing values through 'mice'
library('mice')
ir17_wo_3001 <- subset(ir17_filtered, select = -c(q3001))
imputed_Data <- mice(ir17_wo_3001, m=5, maxit = 50, method = 'pmm', seed = 500)
completeData <- complete(imputed_Data, 2)

#Adding q3001- the dependent variable back to the dataset
ir17_sub <- subset(ir17, select = c('hhid','memid', 'q3001'))
mergeDataset <- merge(completeData, ir17_sub, by=c('hhid', 'memid'))
#Remove 'occup' since all values are NA
mergeDataset <- subset(mergeDataset, select= -occup)

#Segregating the data into training and test
train <- mergeDataset[1:10000,]
test <- mergeDataset[10001:13656,]

convert <- c(4:39,41:58)
train[,convert] <- data.frame(apply(train[convert], 2, as.factor))
set.seed(123)
boruta.train <- Boruta(q3001~.-hhid, data = train, doTrace = 2)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

boruta.train$finalDecision
final.boruta <- TentativeRoughFix(boruta.train)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)


#Results
#getSelectedAttributes(final.boruta, withTentative = F)
#[1] "memid"      "district"   "tehsil"     "listing_id" "block"      "psu"        "age"        "gender"    
#[9] "educ"       "resid"      "proxy"      "q1002"      "q1003"      "q1004"      "q1006"      "q1010"     
#[17] "q1011"      "q1012"      "q1013"      "q1014"      "q1017"      "q1018"      "q1021"      "q1023"     
#[25] "q1025"      "q1026"      "q1027d"     "q1028"      "q1029"      "q1030"      "s3mid"      "s3rid"     
#[33] "q3002c"     "q3002d"     "q3003"      "q3004c"     "q3005"     
