getwd()
cc <- calories_consumed
summary(cc)
library(moments)
skewness(cc$`Weight gained (grams)`)
attach(cc)
skewness(`Calories Consumed`)
kurtosis(`Weight gained (grams)`)
kurtosis(`Calories Consumed`)
plot(`Calories Consumed`,`Weight gained (grams)`)
plot(`Weight gained (grams)`,`Calories Consumed`)
cor(`Calories Consumed`,`Weight gained (grams)`)
reg <- lm(`Weight gained (grams)`~`Calories Consumed`)
summary(reg)

pred <- predict(reg)
View(pred)
reg$residuals
sum(reg$residuals)

confint(reg,level=0.95)
predict (reg,interval="predict")


########################################
View(dt)
summary(dt)
attach(dt)
skewness(`Delivery Time`)
skewness(`Sorting Time`)
kurtosis(`Delivery Time`)
kurtosis(`Sorting Time`)

plot(`Delivery Time`,`Sorting Time`)
cor(`Delivery Time`,`Sorting Time`)
plot(`Sorting Time`,`Delivery Time`)

rmodel <- lm(`Delivery Time`~`Sorting Time`)
summary(rmodel)
model2 <- lm(`Delivery Time`~log(`Sorting Time`))
summary(model2)
model3 <- lm(log(`Delivery Time`)~`Sorting Time`)
summary(model3)
model4 <- lm(log(`Delivery Time`)~log(`Sorting Time`))
summary(model4)


confint(model4,level = 0.95)
predict (model4,interval="predict")
model4$residuals

############################################
emp_data <- read_csv("C:/Users/pruth/Downloads/assignments/Linear Regression/emp_data.csv")
attach(emp_data)
colnames(emp_data) <- c("salary","churn")
summary(emp_data)
kurtosis(Salary_hike)
kurtosis(Churn_out_rate)
skewness(Salary_hike)
skewness(Churn_out_rate)

plot(Salary_hike,Churn_out_rate)
cor(Salary_hike,Churn_out_rate)

cs1 <- lm(Churn_out_rate~Salary_hike)
summary(cs1)

confint(cs1,level = 0.95)
predict(cs1, interval = "predict")
cs1$residuals
cs1$coefficients

#################################
Salary_Data <- read_csv("C:/Users/pruth/Downloads/assignments/Linear Regression/Salary_Data.csv")
summary(Salary_Data)
attach(Salary_Data) 
colnames(Salary_Data) <-c("YOE","salary")
View(Salary_Data)
kurtosis(YearsExperience)
kurtosis(Salary)
skewness(YearsExperience)
skewness(Salary)

plot(YearsExperience,Salary)
cor(YearsExperience,Salary)

sd1 <- lm(Salary~YearsExperience)
summary(sd1)
confint(sd1, level = 0.95)
predict(sd1,interval = "predict")
sd1$coefficients
sd1$residuals