library(stargazer)
library(xtable)
library(epiDisplay)
library("readxl")
library(ggplot2)
library(gridExtra)
library(plyr)
library(Hmisc)
library(MASS)
library(vcd)
library(dplyr)
library(psych)
library(leaps)
library(faraway)
library(car)
library(summarytools)
library(jtools)
library(vcdExtra)
library(VGAM)
library(pROC)
library(erer)
library(tidyverse)
library(caret)
library(bestglm)
library(mfx)

# read in the four datasets
clicks <- read.csv("studentVle.csv")
studentInfo <- read.csv("studentInfo.csv")
tma <- read.csv("studentAssessment.csv")
register <- read.csv("studentRegistration.csv")

# subset the clicks for the chosen module and term, then add the clicks for each student
clicks_bbb2013J <- subset(clicks, 
                          code_module == "BBB" & code_presentation == "2013J", 
                          select = c(id_student, sum_click))
sumclicks_bbb2013J <- summarise_at(group_by(clicks_bbb2013J, id_student),
                                   vars(sum_click),
                                   funs(sum(.,na.rm=TRUE)))
names(sumclicks_bbb2013J) <- c("id_student", "sum_click")

# subset chosen demographic information
studentDem_bbb2013J <- subset(studentInfo, 
                              code_module == "BBB" & code_presentation == "2013J", 
                              select = c(id_student, gender, highest_education, age_band, num_of_prev_attempts, 
                                         studied_credits, disability, date_registration, final_result))

# subset the first tma assessment for each student
tma_bbb2013J <- subset(tma, 
                       id_assessment == 14996, 
                       select = c(id_student,score))
names(tma_bbb2013J) <- c("id_student", "tma")

# subset the students who were enrolled for at least 30 days
register_bbb2013J <- subset(register, 
                            code_module == "BBB" & code_presentation == "2013J" & 
                              (date_unregistration > 30 | is.na(date_unregistration)), 
                            select = c("id_student"))

# join all subsets by student id
bbb2013J <- merge(register_bbb2013J, studentDem_bbb2013J, by = "id_student")
bbb2013J <- merge(bbb2013J, tma_bbb2013J, by = "id_student")
bbb2013J <- merge(bbb2013J, sumclicks_bbb2013J, by = "id_student")
bbb2013J <- bbb2013J[,2:11] #remove student id

# recategorize final results to Not Complete and Complete
bbb2013J$final_result <- sub("Withdrawn", "Not Complete", bbb2013J$final_result)
bbb2013J$final_result <- sub("Fail", "Not Complete", bbb2013J$final_result)
bbb2013J$final_result <- sub("Distinction", "Complete", bbb2013J$final_result)
bbb2013J$final_result <- sub("Pass", "Complete", bbb2013J$final_result)

# recategorize highest education to Lower Than A Level, A Level, Higher Education
bbb2013J$highest_education <- sub("No Formal quals", "Lower Than A Level", bbb2013J$highest_education)
bbb2013J$highest_education <- sub("Lower Than A Level", "Lower Than A Level", bbb2013J$highest_education)
bbb2013J$highest_education <- sub("A Level or Equivalent", "A Level", bbb2013J$highest_education)
bbb2013J$highest_education <- sub("HE Qualification", "Higher Education", bbb2013J$highest_education)
bbb2013J$highest_education <- sub("Post Graduate Qualification", "Higher Education", bbb2013J$highest_education)

# recategorize age to 35 and over and under 35
bbb2013J$age_band <- sub("55<=", "35+", bbb2013J$age_band)
bbb2013J$age_band <- sub("35-55", "35+", bbb2013J$age_band)
bbb2013J$age_band <- sub("0-35", "0-35", bbb2013J$age_band)

# recategorize previous attempts to yes(Y) or no(N)
bbb2013J$num_of_prev_attempts <- sub("1", "Y", bbb2013J$num_of_prev_attempts)
bbb2013J$num_of_prev_attempts <- sub("2", "Y", bbb2013J$num_of_prev_attempts)
bbb2013J$num_of_prev_attempts <- sub("3", "Y", bbb2013J$num_of_prev_attempts)
bbb2013J$num_of_prev_attempts <- sub("4", "Y", bbb2013J$num_of_prev_attempts)
bbb2013J$num_of_prev_attempts <- sub("5", "Y", bbb2013J$num_of_prev_attempts)
bbb2013J$num_of_prev_attempts <- sub("0", "N", bbb2013J$num_of_prev_attempts)

# ensure categorical variables are factors
bbb2013J$highest_education <- factor(bbb2013J$highest_education)
bbb2013J$final_result <- ordered(factor(bbb2013J$final_result),
                                 levels = c("Not Complete", "Complete"))
bbb2013J$num_of_prev_attempts <- factor(bbb2013J$num_of_prev_attempts)
bbb2013J$age_band <- factor(bbb2013J$age_band)
bbb2013J$disability <- factor(bbb2013J$disability)
bbb2013J$gender <- factor(bbb2013J$gender)

# plot categorical variables
plot1 <- ggplot(bbb2013J) + geom_bar(aes(x = gender))
plot2 <- ggplot(bbb2013J) + geom_bar(aes(x = age_band)) + labs(x="age band")
plot3 <- ggplot(bbb2013J) + geom_bar(aes(x = disability))
plot4 <- ggplot(bbb2013J) + geom_bar(aes(x = num_of_prev_attempts)) + labs(x="previous attempt")
plot5 <- ggplot(bbb2013J) + geom_bar(aes(x = highest_education)) + labs(x="highest education") + theme(axis.text.x = element_text(angle = 45,hjust = 1))
plot6<-ggplot(bbb2013J) + geom_bar(aes(x = final_result)) + labs(x="final result")

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=3)

#plot continuous variables
par(mfrow = c(2, 2))
stripchart(bbb2013J$studied_credits, method = "jitter", xlab="Number of credits")
stripchart(bbb2013J$sum_click, method = "jitter", xlab = "Number of clicks")
stripchart(bbb2013J$date_registration, method = "jitter", xlab = "Day of registration")
stripchart(bbb2013J$tma, method = "jitter", xlab = "TMA score")

xtable(prop.table(xtabs(~gender+final_result, data = bbb2013J),1), caption="Gender Proportions", digits=c(0,3,3))
xtable(prop.table(xtabs(~age_band+final_result, data = bbb2013J),1), caption="Age Band Proportion", digits=c(0,3,3))
xtable(prop.table(xtabs(~disability+final_result, data = bbb2013J),1), caption="Disability Proportion", digits=c(0,3,3))
xtable(prop.table(xtabs(~num_of_prev_attempts+final_result, data = bbb2013J),1), caption="Previous Attempts Proportion", digits=c(0,3,3))
xtable(prop.table(xtabs(~highest_education+final_result, data = bbb2013J),1), caption="Highest Education Proportion", digits=c(0,3,3))

GenderResult <- as.matrix(xtabs(~gender+final_result, data = bbb2013J))
CMHtest(GenderResult)
AgeResult <- as.matrix(xtabs(~age_band+final_result, data = bbb2013J))
CMHtest(AgeResult)
DisabilityResult <- as.matrix(xtabs(~disability+final_result, data = bbb2013J))
CMHtest(DisabilityResult)
AttemptsResult <- as.matrix(xtabs(~num_of_prev_attempts+final_result, data = bbb2013J))
CMHtest(AttemptsResult)
HEResult <- as.matrix(xtabs(~highest_education+final_result, data = bbb2013J))
CMHtest(HEResult)

aggregate(x = bbb2013J$studied_credits, by = list(bbb2013J$final_result), FUN = mean) 
aggregate(x = bbb2013J$sum_click, by = list(bbb2013J$final_result), FUN = mean) 
aggregate(x = bbb2013J$date_registration, by = list(bbb2013J$final_result), FUN = mean) 
aggregate(x = bbb2013J$tma, by = list(bbb2013J$final_result), FUN = mean) 

clM <- glm(final_result ~ ., family = binomial, data = bbb2013J)

clMcf <- summary(clM)$coefficients
rownames(clMcf)<-c("Intercept","Gender: M","Highest Education: Higher Education","Highest Education: Lower Than A Level","Age band: 35+", "Previous Attempts: Y","Studied Credits","Disability: Y", "Registration Day", "TMA Score", "Total Clicks")

xtable(clMcf, caption="Estimated Coefficients - Full Model", digits=c(0,4,3,3,3))

stepAIC(clM)

cl2 <- glm(formula = final_result ~ highest_education+num_of_prev_attempts+ 
             studied_credits+tma+sum_click, family = binomial, data = bbb2013J)

cl2cf <- summary(cl2)$coefficients
rownames(cl2cf)<-c("Intercept","Highest Education: Higher Education","Highest Education: Lower Than A Level", "Previous Attempts: Y","Studied Credits", "TMA Score", "Total Clicks")

xtable(cl2cf, caption="Estimated Coefficients - Reduced Model", digits=c(0,4,3,3,3))

cl2an <- Anova(cl2)
rownames(cl2an)<-c("Highest Education", "Previous Attempts: Y","Studied Credits", "TMA Score", "Total Clicks")

xtable(cl2an, caption="Analysis of Deviance Table")

cl2ci<-confint(cl2)
rownames(cl2ci)<-c("Intercept","Highest Education: Higher Education","Highest Education: Lower Than A Level", "Previous Attempts: Y","Studied Credits", "TMA Score", "Total Clicks")

xtable(cl2ci, caption="Confidence Interval", digits=c(0,4,4))

lo<-logitmfx(cl2, atmean=FALSE, data=bbb2013J)
cl2log<-lo$mfxest
rownames(cl2log)<-c("Highest Education: Higher Education","Highest Education: Lower Than A Level", "Previous Attempts: Y","Studied Credits", "TMA Score", "Total Clicks")

xtable(cl2log, caption="Marginal Effects", digits=c(0,5,3,3,3))

prop <- sum(bbb2013J$final_result=="Complete")/nrow(bbb2013J)

fit <- glm(formula = final_result ~ highest_education + num_of_prev_attempts + 
             studied_credits + tma + sum_click, family = binomial, data = bbb2013J)

predicted <- as.numeric(fitted(fit)>prop)

cl2class1<-xtabs(~bbb2013J$final_result+predicted)
colnames(cl2class1)<-c("Not Complete","Complete")

xtable(cl2class1, caption="Classification Table - February 2013")

rocplot <- roc(final_result~fitted(cl2), data = bbb2013J)

plot.roc(rocplot,legacy.axes=TRUE)

auc(rocplot)

clicks_bbb2013B <- subset(clicks, code_module == "BBB" & code_presentation == "2013B", select = c(id_student,sum_click))
sumclicks_bbb2013B <- summarise_at(group_by(clicks_bbb2013B,id_student),vars(sum_click),funs(sum(.,na.rm=TRUE)))
names(sumclicks_bbb2013B) <- c("id_student","sum_click")

studentDem_bbb2013B <- subset(studentInfo, code_module == "BBB" & code_presentation == "2013B", select = c(id_student,gender,highest_education,age_band,num_of_prev_attempts,studied_credits,disability,date_registration,final_result))

tma_bbb2013B <- subset(tma, id_assessment == 14984, select = c(id_student,score))
names(tma_bbb2013B) <- c("id_student","TMA")

register_bbb2013B <- subset(register, code_module == "BBB" & code_presentation == "2013B" & (date_unregistration > 30 | is.na(date_unregistration)), select = c("id_student"))

bbb2013B <- merge(register_bbb2013B,studentDem_bbb2013B,by="id_student")
bbb2013B <- merge(bbb2013B,tma_bbb2013B,by="id_student")
bbb2013B <- merge(bbb2013B,sumclicks_bbb2013B,by="id_student")
bbb2013B <- bbb2013B[,2:11]

bbb2013B$final_result <- sub("Withdrawn", "Not Complete", bbb2013B$final_result)
bbb2013B$final_result <- sub("Fail", "Not Complete", bbb2013B$final_result)
bbb2013B$final_result <- sub("Distinction", "Complete", bbb2013B$final_result)
bbb2013B$final_result <- sub("Pass", "Complete", bbb2013B$final_result)

bbb2013B$highest_education <- sub("No Formal quals", "Lower Than A Level", bbb2013B$highest_education)
bbb2013B$highest_education <- sub("Lower Than A Level", "Lower Than A Level", bbb2013B$highest_education)
bbb2013B$highest_education <- sub("A Level or Equivalent", "A Level", bbb2013B$highest_education)
bbb2013B$highest_education <- sub("HE Qualification", "Higher Education", bbb2013B$highest_education)
bbb2013B$highest_education <- sub("Post Graduate Qualification", "Higher Education", bbb2013B$highest_education)

bbb2013B$age_band <- sub("55<=", "35+", bbb2013B$age_band)
bbb2013B$age_band <- sub("35-55", "35+", bbb2013B$age_band)
bbb2013B$age_band <- sub("0-35", "0-35", bbb2013B$age_band)

bbb2013B$num_of_prev_attempts <- sub("1", "Y", bbb2013B$num_of_prev_attempts)
bbb2013B$num_of_prev_attempts <- sub("2", "Y", bbb2013B$num_of_prev_attempts)
bbb2013B$num_of_prev_attempts <- sub("3", "Y", bbb2013B$num_of_prev_attempts)
bbb2013B$num_of_prev_attempts <- sub("4", "Y", bbb2013B$num_of_prev_attempts)
bbb2013B$num_of_prev_attempts <- sub("5", "Y", bbb2013B$num_of_prev_attempts)
bbb2013B$num_of_prev_attempts <- sub("0", "N", bbb2013B$num_of_prev_attempts)

#reorder level
bbb2013B$highest_education <- factor(bbb2013B$highest_education,levels=c("Lower Than A Level","A Level","Higher Education"))
bbb2013B$final_result <- ordered(factor(bbb2013B$final_result),levels=c("Not Complete","Complete"))
bbb2013B$num_of_prev_attempts <- factor(bbb2013B$num_of_prev_attempts)
bbb2013B$age_band <- factor(bbb2013B$age_band)
bbb2013B$disability <- factor(bbb2013B$disability)
bbb2013B$gender <- factor(bbb2013B$gender)

bbb2013B <- na.omit(bbb2013B)

prop <- sum(bbb2013B$final_result=="Complete")/nrow(bbb2013B)

fit <- glm(formula = final_result ~ highest_education + num_of_prev_attempts + 
             studied_credits + TMA + sum_click, family = binomial, data = bbb2013B)

predicted <- as.numeric(fitted(fit)>prop)

cl2class2<-xtabs(~bbb2013B$final_result+predicted)
colnames(cl2class2)<-c("Not Complete","Complete")

xtable(cl2class2,caption="Classification Table - October 2013")