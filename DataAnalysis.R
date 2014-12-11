# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
    }

packages <- c("psych", "car", "RCurl", "ggplot2", "reshape", "epicalc", "dplyr", "plyr", "stargazer",
              "stringr", "knitr", "xtable", "repmis", "Zelig", "maptools", "ggmap", "RColorBrewer")
ipak(packages)
rm(packages)
rm(ipak)

setwd("~/Desktop/DataCollab/Assignment3")

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

#Merge enrollment data by "District Code"
enrollandsub <- merge(sub, enroll2, by = "District.Code")

#Change "Number.Enrolled" into an integer
enrollandsub$Number.Enrolled <- as.vector(enrollandsub$Number.Enrolled)
enrollandsub$Number.Enrolled <- as.integer(enrollandsub$Number.Enrolled)

#Change varaible names to make it easier to read
enrollandsub$Algebra <- enrollandsub$Mean.Scale.Score.ALGEBRA
enrollandsub$History <- enrollandsub$Mean.Scale.Score.U.S.HISTORY
enrollandsub$English <- enrollandsub$Mean.Scale.Score.ENGLISH
enrollandsub$Biology <- enrollandsub$Mean.Scale.Score.BIOLOGY

#Convert variables into intergers for analysis purposes 
enrollandsub$Algebra <- as.vector(enrollandsub$Algebra)
enrollandsub$Algebra <- as.integer(enrollandsub$Algebra)

enrollandsub$English <- as.vector(enrollandsub$English)
enrollandsub$English <- as.integer(enrollandsub$English)

enrollandsub$Biology <- as.vector(enrollandsub$Biology)
enrollandsub$Biology <- as.integer(enrollandsub$Biology)

enrollandsub$History <- as.vector(enrollandsub$History)
enrollandsub$History <- as.integer(enrollandsub$History)

#Create new variable called "CompositeScore" which is the combined average test scores of 
#Algebra, English, History, and Biology 
enrollandsub$CompositeScore <- ave(enrollandsub$Algebra, enrollandsub$English, 
                            enrollandsub$History, enrollandsub$Biology)

#Create a dummy varaible for the appointed and elected variable. Elected = 1 and Appointed = 0. 
enrollandsub$EA10 <- ifelse(enrollandsub$AorE == "Elected", 1, 0)

#Relabel two mis-lableed observations
enrollandsub[38, "SchoolDistrict"] <- "Bay St. Louis School District"  
enrollandsub[124, "SchoolDistrict"] <- "Tate County School District"  

#Merge poverty level data set
all <- merge(enrollandsub, poverty, by.x = "SchoolDistrict", by.y = "Name", all.x = TRUE)

#Clean up District.Name for merging purposes 
all$DDistrict <-c(str_sub(all$District.Name, 1, 10))
all$DDistrict <- tolower(all$DDistrict)
all$DDistrict <- sub(" ","",all$DDistrict)

#Merge current data set with grad rates data set 
gg <- merge(gradrates, all, by.x ="DDistrict", by.y = "DDistrict", all.X = TRUE)

#Remove duplicated observations 
gg <- gg[-c(21, 23, 36, 37, 61, 62, 113, 114), ]

#Rename gradrate variable
gg$GradRate <- gg$Grad.Rate...09.Cohort..

#Convert gradrate varaible to an integer for analysis purposes 
gg$GradRate <- as.vector(gg$GradRate)
gg$GradRate <- as.integer(gg$GradRate)

#For some reason gradrate09 was coded as the graduation rate of the 2009 class * 2. 
gg$GradRate <- gg$GradRate / 2

#Convert Enrollment variable to an integer for analysis purposes 
gg$Number.Enrolled <- as.vector(gg$Number.Enrolled)
gg$Number.Enrolled <- as.integer(gg$Number.Enrolled)

#Merge teacher-student-ratio data
gg1 <- merge(gg, teacherstudentratio, by.x = "DDistrict", by.y = "DDistrict")

#Remove duplicated observations 
gg1 <- gg1[-c(33, 35, 59, 61, 111, 113), ]

#Change Poverty Percentage to a scale that ranges from 1 - 100
gg1$PovertyPct <- gg1$PovertyPct*100

#Create an enrollment variable that shows enrollment in 100's
gg1$Enrolled100s <- gg1$Number.Enrolled / 100

#Create a subset of data with just variables we used in the regressions
cleandata <- gg1[, c("Enrolled100s",
                     "CompositeScore", "EA10", "PovertyPct",
                     "GradRate", "StudentTeacherRatio",
                     "AorE", "Algebra", "Biology",
                     "History", "English")]

#Omit any missing observations that don't have compelte data
completeclean <- na.omit(cleandata)


#Density graphs of descriptive stats, grouped by superintendent selection method
alg <- ggplot(completeclean, aes(x = Algebra))
alg + geom_density(aes(fill=factor(AorE)), alpha =.75)

bio <- ggplot(completeclean, aes(x = Biology))
bio + geom_density(aes(fill=factor(AorE)), alpha= .75)

eng <- ggplot(completeclean, aes(x = English))
eng + geom_density(aes(fill=factor(AorE)), alpha= .75)

hist <- ggplot(completeclean, aes(x = History))
hist + geom_density(aes(fill=factor(AorE)), alpha= .75)

gradrate <- ggplot(completeclean, aes(x = GradRate))
gradrate + geom_density(aes(fill=factor(AorE)), alpha =.75) +theme_bw()

enrollment <- ggplot(completeclean, aes(x = Enrolled100s))
enrollment + geom_bar(aes(fill=factor(AorE)), alpha = .75)

melted <- melt(completeclean)
submelted <- subset(melted, variable == "Algebra" | variable == "Biology" | variable == "History" | variable == "English")
way <- ggplot(submelted, aes(value, color = factor(AorE)))
way + geom_density(aes(fill = factor(AorE)), alpha = .75) + facet_wrap(~ variable)


#Run first regressions 
newgradfit <- lm(GradRate ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
                   PovertyPct + StudentTeacherRatio, data = completeclean)

newmeanfit <-lm(CompositeScore ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
                  PovertyPct + StudentTeacherRatio, data = completeclean)

newalg <- lm(Algebra  ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
               PovertyPct + StudentTeacherRatio, data = completeclean)

newbio <- lm(Biology ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
               PovertyPct + StudentTeacherRatio, data = completeclean)

newenglish <- lm(English ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
                   PovertyPct + StudentTeacherRatio, data = completeclean)

newhistory <- lm(History ~ EA10  + Enrolled100s + EA10*Enrolled100s + 
                   PovertyPct + StudentTeacherRatio, data = completeclean)
summary(newgradfit)
summary(newmeanfit)

#Stepup model. Start with just selection method and move up until we have the full model
one <- lm(GradRate ~ EA10, data = completeclean)
summary(one)

#Add Poverty Percentage
two <- lm(GradRate ~ EA10 + PovertyPct, data = completeclean)
summary(two)

#Add student teacher ratio
three <- lm(GradRate ~ EA10 + PovertyPct + StudentTeacherRatio, data = completeclean)
summary(three)

#Add Enrollment
four <- lm(GradRate ~ EA10 + PovertyPct + StudentTeacherRatio + Enrolled100s, data = completeclean)
summary(four)

five <- lm(GradRate ~ EA10 + PovertyPct + StudentTeacherRatio + Enrolled100s + Enrolled100s:EA10, data = completeclean)
summary(five)

#Full model. Includes interaction variable 
newgradfit2 <- lm(GradRate ~ EA10 + PovertyPct + StudentTeacherRatio + Enrolled100s + EA10:Enrolled100s,
                  data = completeclean)
summary(newgradfit2)
#Bootstrap it 

Z1 <- zelig(GradRate ~ EA10 + Enrolled100s + EA10:Enrolled100s + PovertyPct + StudentTeacherRatio, 
            cite = FALSE, data = completeclean, model = 'ls')

setZ1 <- setx(Z1, Enrolled100s = 1:62, EA10 = 0) 
simZ1 <- sim(Z1, x = setZ1)

setZ2 <- setx(Z1, Enrolled100s = 2:140, EA10 = 1) 
simZ2 <- sim(Z1, x = setZ2)


smalldistricts <- subset(completeclean, Enrolled100s < 20)

smallfit <- lm(GradRate ~ EA10 + Enrolled100s + EA10*Enrolled100s
               + PovertyPct + StudentTeacherRatio, data = smalldistricts)

summary(smallfit)

#Subset the data just to look at appointed or just elected districts
appointed <- subset(completeclean, AorE == "Appointed")
elected <- subset(completeclean, AorE == "Elected")

simpleappointed <- appointed[,c(1, 2, 4, 5, 6, 8, 9, 10, 11)]
simpleelected <- elected[,c(1, 2, 4, 5, 6, 8, 9, 10, 11)]
simpleclean <- completeclean[,c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11)]

#Here's something interesting. The realtionship b/w enrollment and graduation rates 
#is 0.28 for districts with elected superintendents. Meaning, as districts get bigger, 
#graduation rates go up in elected districts. 
enrollmentgradelected <- qplot(Enrolled100s, GradRate, data = simpleelected)
enrollmentgradelected
cor(simpleelected$Enrolled100s, simpleelected$GradRate)

#However, that relationship does not hold for appointed districts. In these districts 
#there is no relationship b/w enrollment and graduatation rates. 
enrollmentgradappointed <- qplot(Enrolled100s, GradRate, data = simpleappointed)
enrollmentgradappointed
cor(simpleappointed$Enrolled100s, simpleappointed$GradRate)

#Those really big distircts are doing really well in elected districts. That's 
#probably why they're bigger 
qplot(GradRate, Enrolled100s, data=simpleelected) + stat_smooth(method= 'lm')
qplot(GradRate, Enrolled100s, data=simpleappointed) + stat_smooth(method= 'lm')

#Poverty effects grad rates the same way 
qplot(GradRate, PovertyPct, data=simpleelected) + stat_smooth(method= 'lm')
qplot(GradRate, PovertyPct, data=simpleappointed) + stat_smooth(method= 'lm')

#More or less, they're effected the same way. Both positive. 
qplot(GradRate, StudentTeacherRatio, data=simpleelected) + stat_smooth(method= 'lm')
qplot(GradRate, StudentTeacherRatio, data=simpleappointed) + stat_smooth(method= 'lm')

#Seperate realtionship does not hold for test scores. Explains why I see an effect with 
#grad rates, but not test scores. Interesting. 
qplot(CompositeScore, Enrolled100s, data=simpleelected) + stat_smooth(method= 'lm')
qplot(CompositeScore, Enrolled100s, data=simpleappointed) + stat_smooth(method= 'lm')

qplot(StudentTeacherRatio, GradRate, data=completeclean) + stat_smooth(method = 'lm')
qplot(StudentTeacherRatio, GradRate, data=simpleappointed) + stat_smooth(method = 'lm')

small <- qplot(Enrolled100s, GradRate, data=smalldistricts, color = factor(AorE))
small

small2 <- ddply(smalldistricts, "AorE", summarise, mean=mean(GradRate))

reallysmall <- subset(smalldistricts, Enrolled100s <11)
qplot(Enrolled100s, GradRate, data=reallysmall, color = factor(AorE))
reallysmall2 <- ddply(reallysmall, "AorE", summarise, mean=mean(GradRate))

fullsample <- ddply(completeclean, "AorE", summarise, sum=sum(AorE == "Elected"))
small2 <- ddply(smalldistricts, "AorE", summarise, sum=sum(AorE == "Elected"))
reallysmall2 <- ddply(reallysmall, "AorE", summarise, sum=sum(AorE == "Elected"))


write.csv(completeclean, file = "cleanddata.csv")
write.csv(simpleappointed, file = "appointed.csv")
write.csv(simpleelected, file = "elected.csv")



