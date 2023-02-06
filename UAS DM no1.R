#Load library
library(readxl)
library(lindia)

#Load dataset
Insurance <- read_excel("C:/Users/Reynaldi/Downloads/UAS DM/Insurance.xlsx")
View(Insurance)

head(Insurance)
summary(Insurance)

#Normalizing data type
#Numerical (age, BMI, children)
Insurance$age <- as.numeric(Insurance$age)
Insurance$bmi <- as.numeric(Insurance$bmi)
Insurance$children <- as.numeric(Insurance$children)

#Nominal (sex, smoker, region)
Insurance$sex <- as.factor(Insurance$sex)
Insurance$smoker <- as.factor(Insurance$smoker)
Insurance$region <- as.factor(Insurance$region)

summary(Insurance)

colSums(is.na(Insurance))#no missing data

#Generate Model
model <- lm(expenses ~., data = Insurance)
summary(model)

#Plotting
lindia::gg_diagnose(lm(expenses~age, data = Insurance))
lindia::gg_diagnose(lm(expenses~bmi, data = Insurance))
lindia::gg_diagnose(lm(expenses~smoker, data = Insurance))