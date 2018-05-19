install.packages("car")
library(car)
str(cars)

scatter.smooth(x = cars$speed, y = cars$dist, main = "Dist ~ Speed")

#box plot is used for identifying the outliers
boxplot(cars$speed)
boxplot(cars$dist)

head(sort(cars$dist, TRUE))
head(sort(cars$dist, FALSE))

# density plot is used to 
install.packages("e1071")
library(e1071)

#divide graph area in 2 columns
par(mfrow = c(1, 2))
#density plot for speed
plot(density(cars$speed), main = "Density Plot: Speed", ylab = "Frequency", sub = paste("Skewness:", round(e1071::skewness(cars$speed), 2)))
polygon(density(cars$speed), col = "red")

plot(density(cars$dist), main = "Density Plot: Distance", ylab = "Frequency", sub = paste("Skewness:", round(e1071::skewness(cars$dist), 2)))
polygon(density(cars$dist), col = "red")


cor(cars$speed, cars$dist)

linearmod <- lm(dist ~ speed, data = cars)
print(linearmod)
summary(linearmod)

AIC(linearmod)
BIC(linearmod)


set.seed(1234)
no_of_records <- sample(1:nrow(cars), 0.8 * nrow(cars))

training_data <- cars[no_of_records,]
testing_data <- cars[-no_of_records,]
testing_data

lr_model <- lm(dist ~ speed, data = training_data)
dist_predicted <- predict(lr_model, testing_data)
dist_predicted

summary(lr_model)
tail(cars)

actuals_preds <- data.frame(cbind(actuals = testing_data$dist, predicted = dist_predicted))
actuals_preds

# correlation accurary
cor(actuals_preds)

# When min max accuracy is 0 then prediction is accurate. when its 1 the prediction is worst.
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# Mean Absolute Percentage Error
mape <- mean(abs(actuals_preds$predicted - actuals_preds$actuals) / actuals_preds$actuals)
mape

install.packages("DAAG")
library(DAAG)

cvResults <- suppressWarnings(CVlm(data = cars, form.lm = dist ~ speed, m = 5, dots = FALSE, seed = 29, legend.pos = "topleft", printit = FALSE, main = "Small symbols are predicted values while bigger ones are actuals."))
summary(cvResults)
