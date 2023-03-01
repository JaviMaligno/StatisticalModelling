install.packages("readxl")
library(readxl)
library(dplyr)
data_brazil <- read_excel("data_Brazil.xlsx", na = "NA")
#attach(data_brazil)

data_state <- filter(data_brazil, federal_unit == 25, age > 18, economical_activity == 1, income >0)
summary(data_state$income)

data_state <- mutate(data_state, real_income = ifelse(income > 1e11, NA, income)) # they added a very large number where there was a missing value

summary(data_state$real_income)
summary(data_state$age)

plot(data_state$age, data_state$real_income)

ggplot(data_state) + geom_point(aes(x = age, y = real_income, color = factor(sex)))
  
model <- lm(formula =  real_income ~ age + factor(sex), data= data_state)
table(data_state$sex)
summary(model)

predict(model, newdata = data.frame(age = 30, sex = 4))
 
install.packages("quantreg")
library(quantreg)
data_state$real_income
data_state <- mutate(data_state, race_factor = ifelse(race == 2, "White", "Non-White"), sex_factor = ifelse(sex == 2, "Male", "Female"))
model_rq <- rq(real_income ~ age + sex_factor + years_education + race_factor, tau = 1:39/40, data = data_state)
plot(model_rq)
plot(summary(model_rq))

ggplot(data_state) + geom_histogram(aes(x = real_income), fill = "darkviolet", color = "grey")+facet_wrap(~sex_factor)+theme_minimal()
ggplot(data_state) + geom_histogram(aes(x = real_income), fill = "darkviolet", color = "grey")+facet_wrap(~race_factor)+theme_minimal()

ggplot(data_state) + aes(x = age, y = real_income)+geom_point()+facet_grid(race_factor~sex_factor)

