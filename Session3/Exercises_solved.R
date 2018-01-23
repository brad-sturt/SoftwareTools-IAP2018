# Solutions to the Exercises

#### FIRST SET OF EXERCISES: Data Wrangling
# 1. Working with dates
# The `mutate_at` command is very similar to `mutate_if` and
# `mutate_all` commands, but you can specify the variables that
# you want to change directly, for example, we can say:
# listingsOrig %>%
#   mutate_at(vars(city, market), as.factor)
# Using the `mutate_at` and `as.Date()` commands,
# convert the variables first_review and last_review to
# dates, and fill in the missing values with the median dates.
head(listingsOrig$first_review)
head(listingsOrig$last_review)
?as.Date
# Both are in the same format, so we can use mutate_at command
# with the same conversion: function(x) as.Date(x, "%Y-%m-%d")
listingsNew <- listingsOrig %>%
  mutate_at(vars(first_review, last_review),
            function(x) as.Date(x, "%Y-%m-%d")) %>%
  mutate_at(vars(first_review, last_review),
            function(x) coalesce(x, median(x, na.rm = TRUE)))
summary(listingsNew$first_review)
summary(listingsNew$last_review)
?mutate_at

# 2. Converting to NA's
# Sometimes the data set can be tricky, and we need to specify
# which values are NA's for ourselves.  For instance, look at
# the `host_response_rate` column.  Convert the appropriate 
# values to NA's using the `na_if()` command and impute the
# missing values with the median host_response_rate.
table(listingsOrig$host_response_rate)
?na_if
listingsNew <- listingsOrig %>%
  mutate(host_response_rate = na_if(host_response_rate, "N/A")) %>%
  mutate(host_response_rate = as.numeric(gsub("%", "", host_response_rate))) %>%
  mutate(host_response_rate =
           coalesce(host_response_rate, median(host_response_rate, na.rm = TRUE)))
table(listingsNew$host_response_rate, useNA = "always")

#### SECOND SET OF EXERCISES: Linear Regression
# 1. Building a simple model
# Regress price on review_scores_rating. Plot the regression
# line and the actual training points, and find the in-sample
# R^2. (Read below for more details if you need them.)

# DETAILS:
# -Use `lm()` to learn the linear relationship
# -In-sample R^2 is one of the outputs of `summary()`
# -Use `add_predictions()` and ggplot tools for the plotting
mod <- lm(price ~ review_scores_rating, data = listingsTrain)
summary(mod)$r.squared

listingsTrain %>%
  add_predictions(mod) %>%
  ggplot(aes(x = review_scores_rating)) + 
  geom_line(aes(y = pred)) +
  geom_point(aes(y = price), color = "dark blue")

# 2. Adding more variables
# Try to beat the out-of-sample performance of the
# price ~ accommodates model by adding other variables. You can use
# `names(listings)` to explore potential predictors.
# If you start getting  errors or unexpected behavior, make sure
# the predictors are in the format you think they are.
# You can check this using the `summary()` and `str()` functions
# on listings$<variable of interest>.
better_lm <- lm(price ~ accommodates + neighbourhood_cleansed,
                data = listingsTrain)
summary(better_lm)
pred_test <- predict(better_lm, newdata = listingsTest)
OSR2_new <- 1 - sum((pred_test - listingsTest$price) ^ 2) /
  sum((mean(listingsTrain$price) - listingsTest$price) ^ 2)
OSR2_new
results # Original model R^2 and OSR^2

# 3. Median Regression
# # Since we're dealing with data on price,
# we expect that there will be high outliers. While least-squares
# regression is reliable in many settings, it has the property 
# that the estimates it generates depend quite a bit on the outliers.
# One alternative, median regression, minimizes *absolute* error
# rather than squared error. This has the effect of regressing
# on the median rather than the mean, and is more robust to outliers.
# In R, it can be implemented using the `quantreg` package.

# For this exercise, install the quantreg package, and compare
# the behavior of the median regression fit (using the `rq()`)
# function) to the least squares fit from `lm()` on the original
# listings data set given below which includes all the price outliers.
data <- listingsOrig %>%
  mutate(price = as.numeric(gsub("\\$|,", "", price)))
# Hint: Enter ?rq for info on the rq function.

# DETAILS:
# -Split into training/testing set
# -Fit the median and linear regression models
# -Plot the two lines together using `gather_predictions`,
# which is very similar to the `add_predictions` function
# that we saw in class. 
# -Add "color = model" as a geom_line aesthetic
# to differentiate the two models in the plot.
set.seed(123)
split <- sample.split(data$price, SplitRatio = 0.7)
dataTrain <- subset(data, split == TRUE)
dataTest <- subset(data, split == FALSE)

# install.packages("quantreg")
library(quantreg)
mr_model <- rq(price ~ accommodates, data = dataTrain)
lm_model <- lm(price ~ accommodates, data = dataTrain)
summary(mr_model)
summary(lm_model)

dataTest %>%
  gather_predictions(mr_model, lm_model) %>%
  ggplot(aes(x = accommodates)) +	
  geom_point(aes(y = price)) +	
  geom_line(aes(y = pred, color = model))

#### THIRD SET OF EXERCISES: glmnet
# 1. The glmnet package is actually more versatile than just
# LASSO regression. It also does ridge regression (with the l2 norm),
# and any mixture of LASSO and ridge. The mixture is controlled
# by the parameter alpha: alpha=1 is the default and corresponds
# to LASSO, alpha=0 is ridge, and values in between are mixtures
# between the two (check out the formula using ?glmnet).
# One could use cross validation to choose this
# parameter as well. For now, try just a few different values of
# alpha on the model we built for LASSO using `cv.glmnet()`
# (which does not cross-validate for alpha automatically).
# How do the new models do on out-of-sample R^2?
mod_alpha_0 <- cv.glmnet(xTrain, yTrain, alpha = 0)
mod_alpha_0.5 <- cv.glmnet(xTrain, yTrain, alpha = 0.5)

pred_test_0 <- predict.cv.glmnet(mod_alpha_0, newx = xTest, s = "lambda.min")
pred_test_0.5 <- predict.cv.glmnet(mod_alpha_0.5, newx = xTest, s = "lambda.min")
OSR2_alpha_0 <- 1 - sum((pred_test_0 - yTest) ^ 2) /
  sum((mean(yTrain) - yTest) ^ 2)
OSR2_alpha_0.5 <- 1 - sum((pred_test_0.5 - yTest) ^ 2) /
  sum((mean(yTrain) - yTest) ^ 2)
OSR2_alpha_0        
OSR2_alpha_0.5
results # Original model R^2 and OSR^2

#### Fourth Set of Exercises: Classification
# 1. Add more variables to Logistic Regression
# Try to beat the out-of-sample performance for logistic
# regression of elevators on price by adding new variables.
# Compute the out-of-sample AUC of the final model,
# and plot the ROC curve.  
logReg2 <- glm(amenity_Elevator_in_Building ~
                 price + neighbourhood_cleansed,
               family = "binomial", data = listingsGLMTrain)
summary(logReg2)

pred_test <- predict(logReg2, newdata = listingsGLMTest, type = "response")
pred_obj <- prediction(pred_test, listingsGLMTest$amenity_Elevator_in_Building)
perf <- performance(pred_obj, 'tpr', 'fpr')
performance(pred_obj, 'auc')@y.values
plot(perf, colorize = TRUE)

# 2. Tuning a CART model
# Let's try building a more complicated CART model and
# tuning the parameters. Using the below formula, build a
# model to predict `neighbourhood_cleansed` based on price
# and all of the amenities, tuning the cp parameter.
tree_formula <- as.formula(paste("neighbourhood_cleansed ~ price", amenities_string, sep = " +  "))
# Plot the final tree with the option "varlen = 0",
# and save the result as a pdf.  Upload your result to stellar.  
treeBig2 <- rpart(tree_formula,
                 data = listingsGLMTrain,
                 cp = 0.001)
plotcp(treeBig2)
treeFinal2 <- prune(treeBig2, 0.0024)
prp(treeFinal2, varlen =  0)
