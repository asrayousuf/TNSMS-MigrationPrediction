setwd("/Users/asra/Documents/IFMR/PanelStudy/data/rural/individual")
library('haven')
library(Boruta)

# Read individual common survey data
ir1 <- read_dta("isec01.dta")
ir2 <- read_dta("isec02.dta")
ir3 <- read_dta("isec03.dta")
ir4 <- read_dta("isec04.dta")
ir5 <- read_dta("isec05.dta")
ir6 <- read_dta("isec06.dta")
ir7 <- read_dta("isec07.dta")
ir8 <- read_dta("isec08.dta")
ir9 <- read_dta("isec09.dta")
ir10 <- read_dta("isec10.dta")
ir11 <- read_dta("isec11.dta")
ir12 <- read_dta("isec12.dta")
ir13 <- read_dta("isec13.dta")
ir14 <- read_dta("isec14.dta")
ir15 <- read_dta("isec15.dta")
ir16 <- read_dta("isec16.dta")
ir17 <- read_dta("isec17.dta")
# Common features
common_dim <- Reduce(intersect, list(names(ir1), names(ir2), names(ir3), names(ir4), names(ir5),names(ir6),names(ir7), names(ir8),names(ir9),names(ir10),names(ir11),names(ir12),names(ir13),names(ir14),names(ir15),names(ir16),names(ir17) ))

#Join all datasets
ir_a <- join_all(list( unique(ir1),unique(ir2), unique(ir3), unique(ir4), unique(ir5), unique(ir6), unique(ir7), unique(ir8)), by=common_dim, type = 'full')
ir_b <- join_all(list( unique(ir9), unique(ir10), unique(ir11), unique(ir12), unique(ir13), unique(ir14), unique(ir15), unique(ir16), unique(ir17)), by=common_dim, type = 'full')
ir <- left_join(ir_a, ir_b, by=common_dim)

#Clean data -- Remove columns which have >80% missing value
pMiss <- function(x){sum(is.na(x))/length(x)*100 > 80}
missCol <- apply(ir, 2, pMiss)
missColDF <- data.frame(names(missCol),missCol)
fil <- filter(missColDF, missColDF$missCol==FALSE)
ir_filtered <- subset(ir, select = names(ir) %in% fil$names.x.)

# Remove records for NA value in dependent variable
ir_filtered <- subset(ir_filtered, ir_filtered$q3001==1 | ir_filtered$q3001==2)
ir_filtered$q3001 <- as.factor(ir_filtered$q3001)

ir_filtered <- ir_filtered[complete.cases(ir_filtered),]

set.seed(123)
boruta.train <- Boruta(q3001~.-memid, data = ir_filtered, doTrace = 2)
boruta.train$finalDecision
final.boruta <- TentativeRoughFix(boruta.train)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)

#"district + tehsil + block + psu + hhid + listing_id + age + gender + educ + resid + q1002 + q1003 + q1004 + q1006 + q1026 + s2mid + s2rid + q2001 + q2006 + q2008 + q2010 + q2012 + q2014 + q2015 + q2016d + q2016f + q2016h + q2016i + q2016j + q2016n + q2025a + q2025b + q2025c + q2049a + q2049b + q2049c + q2049d + s4mid + s4rid + q4037_1 + q4065_1 + q4065_2 + q4093_1 + q4093_2 + q4093_3 + q4098_1 + q4098_2 + q4098_3 + q4101_1 + q4101_2 + q4101_3 + q4125_2 + q4130_2 + s5mid + q6004a + q6005a1 + q6005a2 + q6006a + q6007a1 + q6007a2 + q7001h + q7002hdd1 + q7002hmm1 + q7002hyy1 + q7001w + q7002wdd1 + q7002wmm1 + q7002wyy1 + q7001u + q7002udd1 + q7002umm1 + q7002uyy1 + te8 + te9 + te20 + te21 + tq1 + tq2 + tq3 + tq8"



