library(plyr)
library(ggplot2)
library(reshape)
library(psych)
library(epicalc)
library(dplyr)
library(car)
library(stargazer)
library(stringr)
library(xtable)
library(repmis)


#Load the data 
#Selection method data
SM <- read.csv("~/Dropbox/MSDE Reports/AorE.csv")
#ATP test data 
Test.Data <- read.csv("~/Dropbox/MSDE Reports/SATP14.csv")
#Enrollment Data
enrollment <- read.csv("~/Dropbox/MSDE Reports/Enrollment14.csv")
#Poverty data 
poverty <- read.csv("~/Dropbox/MSDE Reports/poverty12.csv")
#Graduation rates data
gradrates <- read.csv("~/Dropbox/MSDE Reports/gradrates.csv")
#Teacher-student-ratio data
teacherstudentratio <- read.csv("~/Dropbox/MSDE Reports/TeacherPupilRatio12.csv")


#Look at only Districtwide data 
Test.Data <- subset(Test.Data, School.Name == "Districtwide Data")
enrollment <-subset(enrollment, School.Name == "District Level Data")
enroll2 <-subset(enrollment, Grade == "Total Enrollment")#Remove spaces from 'U.S. History' 
Test.Data$Subject <- sub(" ","",Test.Data$Subject)

#Convert from long to wide
Test.Data <- reshape(Test.Data, 
                     timevar = "Subject", 
                     idvar = c("District.Code", "District.Name", "School.Name", "School.Code"), 
                     direction = "wide")

#Create new variable called 'dname' that is the first 8 lower-case characters of the district name
Test.Data$dname <-c(str_sub(Test.Data$District.Name, 1, 8))
Test.Data$dname <- tolower(Test.Data$dname)
Test.Data$dname <- sub(" ","",Test.Data$dname)

#Create new variable called 'dname' that is the first 8 lower-case characters of the district name
SM$dname <-c(str_sub(SM$SchoolDistrict, 1, 8))
SM$dname <- tolower(SM$dname)
SM$dname <- sub("[.] ","",SM$dname)
SM$dname <- sub(" ","",SM$dname)

#Create consistent District names for merging purposes
teacherstudentratio$DDistrict <- c(str_sub(teacherstudentratio$DistrictName, 1, 10))
teacherstudentratio$DDistrict <- tolower(teacherstudentratio$DDistrict)
teacherstudentratio$DDistrict <- sub(" ","",teacherstudentratio$DDistrict)

#Convert Teacher Student Ratio into a number for analysis purposes
teacherstudentratio$StudentTeacherRatio <- as.vector(teacherstudentratio$StudentTeacherRatio)
teacherstudentratio$StudentTeacherRatio <- as.numeric(teacherstudentratio$StudentTeacherRatio)

#Standardize district variable in grad rates dataset 
gradrates$DDistrict <-c(str_sub(gradrates$District, 1, 10))
gradrates$DDistrict <- tolower(gradrates$DDistrict)
gradrates$DDistrict <- sub(" ","",gradrates$DDistrict)

#Merge selection method data with SATP test data 
mergedata <- merge(SM, Test.Data, by='dname', all.y = TRUE)

#Get rid of duplicate data 
newmerge <- mergedata[-c(55, 56, 59, 60, 87, 88, 89, 90, 114, 117, 122, 121), ]

#relabel missing data
newmerge[7, "AorE"] <- "Appointed"
newmerge[126, "AorE"] <- "Elected"

#Make new dataset only with columns we care about
sub <- newmerge[, c(1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)]


#Change values from w/e they were to integers for computation purposes 
sub$X..Passing.ALGEBRA <- as.vector(sub$X..Passing.ALGEBRA)
sub$X..Passing.U.S.HISTORY <- as.vector(sub$X..Passing.U.S.HISTORY)
sub$X..Passing.ENGLISH <- as.vector(sub$X..Passing.ENGLISH)
sub$X..Passing.BIOLOGY <- as.vector(sub$X..Passing.BIOLOGY)

sub$X..Passing.ALGEBRA <- as.integer(sub$X..Passing.ALGEBRA)
sub$X..Passing.U.S.HISTORY <- as.integer(sub$X..Passing.U.S.HISTORY)
sub$X..Passing.ENGLISH <- as.integer(sub$X..Passing.ENGLISH)
sub$X..Passing.BIOLOGY <- as.integer(sub$X..Passing.BIOLOGY)

#Get some descriptive statistics on all test scores 
aggregate(sub$X..Passing.ALGEBRA, by=list(sub$AorE), FUN=mean)
aggregate(sub$X..Passing.U.S.HISTORY , by=list(sub$AorE), FUN=mean)
aggregate(sub$X..Passing.ENGLISH, by=list(sub$AorE), FUN=mean)
aggregate(sub$X..Passing.BIOLOGY , by=list(sub$AorE), FUN=mean)

Selection.Method <- c("Appointed", "Elected")
Passing.Algebra <- c("81.83%", "82.46%")
Passing.Biology <- c("75.53%", "76.6%")
Passing.English <- c("69.28%", "70.27%")
Passing.History <- c("76.96%", "77.82%")
Average.Enrollment <- c("2312", "3201")
Average.Pct.In.Poverty <- c("37%", "32%")

DescriptiveStats <- data.frame(Selection.Method, Passing.Algebra, Passing.Biology,
                               Passing.English, Passing.History, Average.Enrollment,
                               Average.Pct.In.Poverty)



library(knitr)

enrollandsub <- merge(sub, enroll2, by = "District.Code")

enrollandsub$Number.Enrolled <- as.vector(enrollandsub$Number.Enrolled)
enrollandsub$Number.Enrolled <- as.integer(enrollandsub$Number.Enrolled)

#Change varaible names to make it easier to read
enrollandsub$MeanAlgebra <- enrollandsub$Mean.Scale.Score.ALGEBRA
enrollandsub$MeanHistory <- enrollandsub$Mean.Scale.Score.U.S.HISTORY
enrollandsub$MeanEnglish <- enrollandsub$Mean.Scale.Score.ENGLISH
enrollandsub$MeanBiology <- enrollandsub$Mean.Scale.Score.BIOLOGY

#Convert variables into intergers for analysis purposes 
enrollandsub$MeanAlgebra <- as.vector(enrollandsub$MeanAlgebra)
enrollandsub$MeanAlgebra <- as.integer(enrollandsub$MeanAlgebra)

enrollandsub$MeanEnglish <- as.vector(enrollandsub$MeanEnglish)
enrollandsub$MeanEnglish <- as.integer(enrollandsub$MeanEnglish)

enrollandsub$MeanBiology <- as.vector(enrollandsub$MeanBiology)
enrollandsub$MeanBiology <- as.integer(enrollandsub$MeanBiology)

enrollandsub$MeanHistory <- as.vector(enrollandsub$MeanHistory)
enrollandsub$MeanHistory <- as.integer(enrollandsub$MeanHistory)

#Create new variable called "New Mean" which is the combined average test scores of 
#Algebra, English, History, and Biology 
enrollandsub$NewMean <- ave(enrollandsub$MeanAlgebra, enrollandsub$MeanEnglish, 
                            enrollandsub$MeanHistory, enrollandsub$MeanBiology)

#Create a dummy varaible for the appointed and elected varaible 
enrollandsub$EA10 <- ifelse(enrollandsub$AorE == "Elected", 1, 0)

#Get rid of collumns that don't matter
enrollandsub$MeanScore <- NULL
enrollandsub$Mean.Scale.Score.ALGEBRA <- NULL
enrollandsub$Mean.Scale.Score.U.S.HISTORY <- NULL
enrollandsub$Mean.Scale.Score.ENGLISH <- NULL
enrollandsub$Mean.Scale.Score.BIOLOGY <- NULL
enrollandsub$Grade <- NULL
enrollandsub$School.Code <- NULL

#Relabel two mis-lableed observations
enrollandsub[38, "SchoolDistrict"] <- "Bay St. Louis School District"  
enrollandsub[124, "SchoolDistrict"] <- "Tate County School District"  

#Merge poverty level data set
all <- merge(enrollandsub, poverty, by.x = "SchoolDistrict", by.y = "Name", all.x = TRUE)

#Get rid of useless columns 
all$State.FIPS.Code <- NULL 
all$State.Postal.Code <- NULL 
all$School.Name <- NULL

library(ggplot2)

alg <- ggplot(enrollandsub, aes(x = MeanAlgebra))
alg + geom_density(aes(fill=factor(AorE)), alpha =.75)

bio <- ggplot(enrollandsub, aes(x = MeanBiology))
bio + geom_density(aes(fill=factor(AorE)), alpha= .75)

eng <- ggplot(enrollandsub, aes(x = MeanEnglish))
eng + geom_density(aes(fill=factor(AorE)), alpha= .75)

hist <- ggplot(enrollandsub, aes(x = MeanHistory))
hist + geom_density(aes(fill=factor(AorE)), alpha= .75)


fitsimple <- lm(MeanAlgebra ~ EA10 + Number.Enrolled + EA10*Number.Enrolled + PovertyPct, data = all)

all$DDistrict <-c(str_sub(all$District.Name, 1, 10))
all$DDistrict <- tolower(all$DDistrict)
all$DDistrict <- sub(" ","",all$DDistrict)

#Merge current data set with grad rates data set 
gg <- merge(gradrates, all, by.x ="DDistrict", by.y = "DDistrict", all.X = TRUE)

#Remove duplicated observations 
gg <- gg[-c(21, 23, 36, 37, 61, 62, 113, 114), ]

#Rename gradrate variable
gg$gradrate09 <- gg$Grad.Rate...09.Cohort..

#Convert gradrate varaible to an integer for analysis purposes 
gg$gradrate09 <- as.vector(gg$gradrate09)
gg$gradrate09 <- as.integer(gg$gradrate09)

#For some reason gradrate09 was coded as the graduation rate of the 2009 class * 2. 
gg$gradrate09 <- gg$gradrate09 / 2

#Convert Enrollment variable to an integer for analysis purposes 
gg$Number.Enrolled <- as.vector(gg$Number.Enrolled)
gg$Number.Enrolled <- as.integer(gg$Number.Enrolled)


gradrate <- ggplot(gg, aes(x = gradrate09))
gradrate + geom_density(aes(fill=factor(AorE)), alpha =.75) +theme_bw()


fitgrad <- lm(gradrate09 ~ EA10 + Number.Enrolled + EA10*Number.Enrolled +
                PovertyPct, data = gg)

fitmean <- lm(NewMean ~ EA10 + Number.Enrolled + EA10*Number.Enrolled +
                PovertyPct, data = gg)


#Merge teacher-student-ratio data
gg1 <- merge(gg, teacherstudentratio, by.x = "DDistrict", by.y = "DDistrict")

#Remove duplicated observations 
gg1 <- gg1[-c(33, 35, 59, 61, 111, 113), ]

gg1$PovertyPct <- gg1$PovertyPct*100

gg1$Enrolled100s <- gg1$Number.Enrolled / 100

newgradfit <- lm(gradrate09 ~ EA10  + Number.Enrolled + EA10*Number.Enrolled + 
                   PovertyPct + StudentTeacherRatio, data = gg1)

newmeanfit <-lm(NewMean ~ EA10  + Number.Enrolled + EA10*Number.Enrolled + 
                  PovertyPct + StudentTeacherRatio, data = gg1)

newalg <- lm(MeanAlgebra  ~ EA10  + Number.Enrolled + EA10*Number.Enrolled + 
               PovertyPct + StudentTeacherRatio, data = gg1)

newbio <- lm(MeanBiology ~ EA10  + Number.Enrolled + EA10*Number.Enrolled + 
               PovertyPct + StudentTeacherRatio, data = gg1)

newenglish <- lm(MeanEnglish ~ EA10  + Number.Enrolled + EA10*Number.Enrolled + 
                   PovertyPct + StudentTeacherRatio, data = gg1)

newhistory <- lm(MeanHistory ~ EA10  + Number.Enrolled + EA10*Number.Enrolled + 
                   PovertyPct + StudentTeacherRatio, data = gg1)


electedfit <- with(gg1,
                   data.frame(EA10 = 1, 
                              PovertyPct = 35.1, 
                              Number.Enrolled = 2700, 
                              "EA10*Number.Enrolled" = 2700, 
                              StudentTeacherRatio = 14.9))

electedfit$predicted <- predict(newgradfit, newdata = electedfit, type='response')

appointedfit <- with(gg1, 
                     data.frame(EA10 = 0, 
                                PovertyPct = 35.1, 
                                Number.Enrolled = 2700, 
                                "EA10*Number.Enrolled" = 0, 
                                StudentTeacherRatio = 14.9))

appointedfit$predicted <- predict(newgradfit, newdata = appointedfit, type='response')

together <- rbind(electedfit, appointedfit)

together <- data.frame(together)


cleandata <- gg1[, c("Number.Enrolled",
                     "NewMean", "EA10", "PovertyPct",
                     "gradrate09", "StudentTeacherRatio",
                     "AorE", "MeanAlgebra", "MeanBiology",
                     "MeanHistory", "MeanEnglish")]

appointed <- subset(cleandata, AorE == "Appointed")
elected <- subset(cleandata, AorE == "Elected")

simpleappointed <- appointed[,c(1, 2, 4, 5, 6, 8, 9, 10, 11)]
simpleelected <- elected[,c(1, 2, 4, 5, 6, 8, 9, 10, 11)]
simpleclean <- cleandata[,c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11)]


gradvpoverty <- qplot(gradrate09, PovertyPct, data=cleandata)

qplot(gradrate09, log(StudentTeacherRatio), data=cleandata) + stat_smooth(method= 'lm')
qplot(gradrate09, StudentTeacherRatio, data=cleandata) + stat_smooth(method= 'lm')


gradvpoverty + stat_smooth(method = 'lm') + geom_point(aes(color = AorE))
