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
library(lmtest)
library(RCurl)
library(Zelig)
library(lattice)



#Load the data 
#Selection method data
SM <- getURL("https://raw.githubusercontent.com/Henryjean/Assignment3/master/Data%20Files/AorE.csv")
SM <- read.csv(text = SM)

#SATP test data 
Test.Data <- getURL("https://raw.githubusercontent.com/Henryjean/Assignment3/master/Data%20Files/SATP14.csv")
Test.Data <- read.csv(text = Test.Data)

#Enrollment Data
enrollment <- getURL("https://raw.githubusercontent.com/Henryjean/Assignment3/master/Data%20Files/Enrollment14.csv")
enrollment <- read.csv(text = enrollment)

#Poverty data 
poverty <- getURL("https://raw.githubusercontent.com/Henryjean/Assignment3/master/Data%20Files/poverty12.csv")
poverty <- read.csv(text = poverty)

#Graduation rates data
gradrates <- getURL("https://raw.githubusercontent.com/Henryjean/Assignment3/master/Data%20Files/gradrates.csv")
gradrates <- read.csv(text = gradrates)

#Teacher-student-ratio data
teacherstudentratio <- getURL("https://raw.githubusercontent.com/Henryjean/Assignment3/master/Data%20Files/TeacherPupilRatio12.csv")
teacherstudentratio <- read.csv(text = teacherstudentratio)

#Subset Test.Data and Enrollment data to only look at District Wide Results
Test.Data <- subset(Test.Data, School.Name == "Districtwide Data")
enrollment <-subset(enrollment, School.Name == "District Level Data")
enroll2 <-subset(enrollment, Grade == "Total Enrollment")

#Remove spaces from 'U.S. History' 
Test.Data$Subject <- sub(" ","",Test.Data$Subject)

#Convert from "long" to "wide". Or rather, transpose the data. 
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
#Removing duplicated Jackson Public School District
#Removing duplicated Jackson County School District
#Removing duplicated Jefferson Davis County School District
#Removing duplicated Jefferson County School District
#Removing MS School for Math and Science (not enough data)
#Removing MS School for Perfomring Arts (not enoughd data)
#Removing duplicated Pontotoc City Schools
#Removing duplicated Pontotoc County Schools  
#Remvoing duplicated Quitman County School District
#Removing duplicated Quiitman School District

newmerge <- mergedata[-c(55, 56, 59, 60, 87, 88, 89, 90, 114, 117, 122, 121), ]

#relabel "Bay. St Louis" and "Tate County" 
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

#Create data frame that stores the descriptive stats 
DescriptiveStats <- data.frame(Selection.Method, Passing.Algebra, Passing.Biology,
                               Passing.English, Passing.History, Average.Enrollment,
                               Average.Pct.In.Poverty)


#Merge enrollment data by "District Code"
enrollandsub <- merge(sub, enroll2, by = "District.Code")

#Change "Number.Enrolled" into an integer
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

#Create new variable called "CompositeScore" which is the combined average test scores of 
#Algebra, English, History, and Biology 
enrollandsub$CompositeScore <- ave(enrollandsub$MeanAlgebra, enrollandsub$MeanEnglish, 
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

#Density graphs of descriptive stats, grouped by superintendent selection method
alg <- ggplot(enrollandsub, aes(x = MeanAlgebra))
alg + geom_density(aes(fill=factor(AorE)), alpha =.75)

bio <- ggplot(enrollandsub, aes(x = MeanBiology))
bio + geom_density(aes(fill=factor(AorE)), alpha= .75)

eng <- ggplot(enrollandsub, aes(x = MeanEnglish))
eng + geom_density(aes(fill=factor(AorE)), alpha= .75)

hist <- ggplot(enrollandsub, aes(x = MeanHistory))
hist + geom_density(aes(fill=factor(AorE)), alpha= .75)

#Clean up District.Name for merging purposes 
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

#Density plot of graduation rates
gradrate <- ggplot(gg, aes(x = gradrate09))
gradrate + geom_density(aes(fill=factor(AorE)), alpha =.75) +theme_bw()

#Merge teacher-student-ratio data
gg1 <- merge(gg, teacherstudentratio, by.x = "DDistrict", by.y = "DDistrict")

#Remove duplicated observations 
gg1 <- gg1[-c(33, 35, 59, 61, 111, 113), ]

#Change Poverty Percentage to a scale that ranges from 1 - 100
gg1$PovertyPct <- gg1$PovertyPct*100

#Create an enrollment variable that shows enrollment in 100's
gg1$Enrolled100s <- gg1$Number.Enrolled / 100

#Run first regressions 
newgradfit <- lm(gradrate09 ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
                   PovertyPct + StudentTeacherRatio, data = gg1)

newmeanfit <-lm(CompositeScore ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
                  PovertyPct + StudentTeacherRatio, data = gg1)

newalg <- lm(MeanAlgebra  ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
               PovertyPct + StudentTeacherRatio, data = gg1)

newbio <- lm(MeanBiology ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
               PovertyPct + StudentTeacherRatio, data = gg1)

newenglish <- lm(MeanEnglish ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
                   PovertyPct + StudentTeacherRatio, data = gg1)

newhistory <- lm(MeanHistory ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
                   PovertyPct + StudentTeacherRatio, data = gg1)

summary(newgradfit)
confint(newgradfit)

#Whats the difference in grad rates for a district with average characteristics, but 
#one has an appointed superintendent and one has en elected superintendent
electedfit <- with(gg1,
                   data.frame(EA10 = 1, 
                              Enrolled100s = 27, 
                              "EA10*Enrolled100s" = 27, 
                              PovertyPct = 35.1, 
                              StudentTeacherRatio = 14.9))

electedfit$predicted <- predict(newgradfit, newdata = electedfit, type='response')

appointedfit <- with(gg1, 
                     data.frame(EA10 = 0,  
                                Enrolled100s = 27, 
                                "EA10*Enrolled100s" = 0, 
                                PovertyPct = 35.1,
                                StudentTeacherRatio = 14.9))

appointedfit$predicted <- predict(newgradfit, newdata = appointedfit, type='response')

#Store predicted values into a data.frame for Rmarkdown purposes
together <- rbind(electedfit, appointedfit)
together <- data.frame(together)

electedfitsmall <- with(gg1,
                   data.frame(EA10 = 1, 
                              Enrolled100s = 10, 
                              "EA10*Enrolled100s" = 27, 
                              PovertyPct = 35.1, 
                              StudentTeacherRatio = 14.9))

electedfitsmall$predicted <- predict(newgradfit, newdata = electedfitsmall, type='response')

appointedfitsmall <- with(gg1, 
                     data.frame(EA10 = 0,  
                                Enrolled100s = 10, 
                                "EA10*Enrolled100s" = 0, 
                                PovertyPct = 35.1,
                                StudentTeacherRatio = 14.9))

appointedfitsmall$predicted <- predict(newgradfit, newdata = appointedfitsmall, type='response')

together2 <- rbind(electedfitsmall, appointedfitsmall)
together2 <- data.frame(together2)

#Create a subset of data with just variables we used in the regressions
cleandata <- gg1[, c("Enrolled100s",
                     "CompositeScore", "EA10", "PovertyPct",
                     "gradrate09", "StudentTeacherRatio",
                     "AorE", "MeanAlgebra", "MeanBiology",
                     "MeanHistory", "MeanEnglish")]

#Omit any missing observations that don't have compelte data
completeclean <- na.omit(cleandata)

#Stepup model. Start with just selection method and move up until we have the full model
one <- lm(gradrate09 ~ EA10, data = completeclean)
summary(one)

#Add Poverty Percentage
two <- lm(gradrate09 ~ EA10 + PovertyPct, data = completeclean)
summary(two)

#Add student teacher ratio
three <- lm(gradrate09 ~ EA10 + PovertyPct + StudentTeacherRatio, data = completeclean)
summary(three)

#Add Enrollment
four <- lm(gradrate09 ~ EA10 + PovertyPct + StudentTeacherRatio + Enrolled100s, data = completeclean)
summary(four)

#Full model. Includes interaction variable 
newgradfit2 <- lm(gradrate09 ~ EA10 + Enrolled100s + EA10*Enrolled100s
                  + PovertyPct + StudentTeacherRatio, data = completeclean)

fitted(newgradfit2)
summary(newgradfit2)
confint(newgradfit2, level=0.90)
plot(newgradfit2, which =2)

anova(newgradfit2)

xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)
xyplot(completeclean$gradrate09 ~ completeclean$PovertyPct, data = completeclean, type = c("p","r"), col.line = "red")


Z1 <- zelig(gradrate09 ~ EA10 + EA10:Enrolled100s
            + PovertyPct + StudentTeacherRatio, cite = FALSE,
            data = completeclean, model = 'ls')

setZ1 <- setx(Z1, Enrolled100s = 0:200)
simZ1 <- sim(Z1, x = setZ1)
plot(simZ1)

Z2 <- zelig(gradrate09 ~ Enrolled100s
            + PovertyPct + StudentTeacherRatio, cite = FALSE,
            data = simpleelected, model = 'ls')

setZ2 <- setx(Z2, Enrolled100s = 0:200)
simZ2 <- sim(Z2, x = setZ1)
plot(simZ2)

?zelig
help.zelig("models")

#Subset the data just to look at appointed or just elected districts
appointed <- subset(completeclean, AorE == "Appointed")
elected <- subset(completeclean, AorE == "Elected")

simpleappointed <- appointed[,c(1, 2, 4, 5, 6, 8, 9, 10, 11)]
simpleelected <- elected[,c(1, 2, 4, 5, 6, 8, 9, 10, 11)]
simpleclean <- completeclean[,c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11)]

#Here's something interesting. The realtionship b/w enrollment and graduation rates 
#is 0.28 for districts with elected superintendents. Meaning, as districts get bigger, 
#graduation rates go up in elected districts. 
enrollmentgradelected <- qplot(Enrolled100s, gradrate09, data = simpleelected)
enrollmentgradelected
cor(simpleelected$Enrolled100s, simpleelected$gradrate09)

#However, that relationship does not hold for appointed districts. In these districts 
#there is no relationship b/w enrollment and graduatation rates. 
enrollmentgradappointed <- qplot(Enrolled100s, gradrate09, data = simpleappointed)
enrollmentgradappointed
cor(simpleappointed$Enrolled100s, simpleappointed$gradrate09)

#Those really big distircts are doing really well in elected districts. That's 
#probably why they're bigger 
qplot(gradrate09, Enrolled100s, data=simpleelected) + stat_smooth(method= 'lm')
qplot(gradrate09, Enrolled100s, data=simpleappointed) + stat_smooth(method= 'lm')

#Poverty effects grad rates the same way 
qplot(gradrate09, PovertyPct, data=simpleelected) + stat_smooth(method= 'lm')
qplot(gradrate09, PovertyPct, data=simpleappointed) + stat_smooth(method= 'lm')

#More or less, they're effected the same way. Both positive. 
qplot(gradrate09, StudentTeacherRatio, data=simpleelected) + stat_smooth(method= 'lm')
qplot(gradrate09, StudentTeacherRatio, data=simpleappointed) + stat_smooth(method= 'lm')

#Seperate realtionship does not hold for test scores. Explains why I see an effect with 
#grad rates, but not test scores. Interesting. 
qplot(CompositeScore, Enrolled100s, data=simpleelected) + stat_smooth(method= 'lm')
qplot(CompositeScore, Enrolled100s, data=simpleappointed) + stat_smooth(method= 'lm')


 


