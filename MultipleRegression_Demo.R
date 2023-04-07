set.seed(123) # set seed for reproducibility
age <- runif(100, 18, 65) # generate 100 random ages between 18 and 65
income <- rnorm(100, 50000, 10000) # generate 100 random incomes with mean 50000 and SD 10000
savings <- 10000 + 500 * age + 0.2 * income + rnorm(100, 0, 10000) # generate savings based on age, income, and some noise
data <- data.frame(age, income, savings) # create a data frame with the three variables
model <- lm(savings ~ age + income, data = data)
summary(model) # print summary of the model
coef(model) # print coefficients
new_data <- data.frame(age = c(30, 40, 50), income = c(60000, 70000, 80000))
predict(model, newdata = new_data) # predict savings for new data
