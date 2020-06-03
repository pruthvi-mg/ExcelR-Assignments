#2 Sample T test

getwd()
cutlets <- read.csv("C:/Users/pruth/Downloads/assignments/Hypothesis testing/Cutlets.csv")
attach(cutlets)
colnames(cutlets)<-c("A","B")
View(cutlets)
colnames(cutlets)
shapiro.test(A)
attach(cutlets)

shapiro.test(A) #as p value > 0.05 the data are normal
shapiro.test(B) #as p value > 0.05 the data are normal

var.test(A,B)   #as p value > 0.05 the data have equal variance

t.test(A,B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)
t.test(A,B,alternative = "greater",var.equal = T)


#ANOVA Test

labtat <-read.csv("C:/Users/pruth/Downloads/assignments/Hypothesis testing/LabTAT.csv")
attach(labtat)
colnames(labtat)<-c("lab1","lab2","lab3","lab4")
stacklab <- stack(labtat)
attach(labtat)
shapiro.test(Laboratory.1)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)

install.packages("car")
library(car)
leveneTest(stacklab$values~stacklab$ind, data = stacklab)
anova <- aov(values~ind,data = stacklab)
summary(anova)

#3rd question
BR <- read.csv("C:/Users/pruth/Downloads/assignments/Hypothesis testing/BuyerRatio.csv")
View(BR)
attach(BR)
c1<- BR$East
c2 <- BR$West
c3<- BR$North
c4<- BR$South
b1 <- data.frame(c1,c2,c3,c4)
View(b1)
chisq.test(b1)


#4th Question
getwd()
customer <- read.csv("C:/Users/pruth/Downloads/assignments/Hypothesis testing/Costomer+OrderForm.csv")
View(customer)
attach(customer)
philip <- as.numeric(Phillippines)
indo <- as.numeric(Indonesia)
malta<- as.numeric(Malta)
india <- as.numeric(India)
COF <- data.frame(philip,indo,malta,india)
View(COF)
attach(COF)
stackcof <- stack(COF)
table(stackcof)
chisq.test(table(stackcof))

#############################################
fant <- read.csv("C:/Users/pruth/Downloads/assignments/Hypothesis testing/Faltoons.csv")
View(fant)
attach(fant)
c1<- as.numeric(fant$Weekdays)
c2<- as.numeric(fant$Weekend)
fant<- data.frame(c1,c2)
View(fant)
stackfant <-stack(fant)
View(stackfant)
table(stackfant)
prop.test(x=c(113,167),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
#### as p value is less than 0.05
prop.test(x=c(113,167),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "greater")



