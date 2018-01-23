## Statistical Modelling and Machine Learning in R ##

#### Introduction ####
# Within the world of supervised learning, we can divide tasks
# into two parts. In settings where the response variable is
# continuous we call the modelling *regression*, and when the
# response is categorical we call it *classification*. We will
# begin with regression to understand what factors influence
# price in the AirBnB data set.	

# Let's start by loading the data (after first setting the
# correct working directory). We'll use the 'listings.csv' file
# for now. Since we'll be wrangling and visualizing, we'll
# also load the `tidyverse` package. (Instead of `tidyverse`,
# it also works to load `tidyr`, `dplyr`, and `ggplot2` as
# we saw last session.)	
library(tidyverse)
listingsOrig <- read.csv("data/listings.csv", stringsAsFactors = FALSE)
# Note that when we do a lot of data wrangling, sometimes it's nice
# to keep a copy of the original data set so we don't have to read it in again.

#### Data Wrangling ####
# We're going to prepare the data set a bit so that we can build
# models to predict the price of an Airbnb in Boston. 
# As a review, we need to change the price column
# to a numeric form.
listings <- listingsOrig %>%
  mutate(price = as.numeric(gsub("\\$|,", "", price)))
summary(listings$price) # Check to make sure things worked

# Now, which variables may be predictive of price? We can
# use `names(listings)` to get a look at all the variable names.
names(listings)

# Let's begin by looking at the relationship between
# `listings$accommodates` and `listings$price`. As a first look
# (using our ggplot tools from last time):
ggplot(data = listings) +
  geom_point(aes(x = accommodates, y = price))

# Looks like there are some outliers on both axes. There are fancier 
# ways to deal with this statistically, but for today let's just get
# rid of the outliers and fit a model on the cleaner data:	
listings <- listings %>%	
  filter(accommodates <= 10, price <= 1000)

# Where are these Airbnbs, and what type are they? This information
# is included in the `property_type` and `neighbourhood_cleansed`
# variables.
sort(table(listings$property_type))
sort(table(listings$neighbourhood_cleansed))
listings <- listings %>%
  filter(property_type %in% c("Apartment", "House", "Bed & Breakfast",
                              "Condominium", "Loft", "Townhouse"),	
         !(neighbourhood_cleansed %in%
             c("Leather District","Longwood Medical Area")))

# Next, let's check if any of variables in our data set have
# missing values.  We can use the following handy command:
sort(colSums(is.na(listings)))

# Note that some of the variables are missing almost all of their values,
# so we can filter these out.  Let's filter out all variables with
# >90% missing values using the `select_if` function:
?select_if
listings <- listings %>%
  select_if(function(x) sum(is.na(x)) < 0.9 * nrow(listings))

# It seems like we still have a fair amount of missing values
# for the review_scores variables, so the best solution would be
# to do missing data imputation here.  There are many good R packages
# for this, for example `mice` for multiple imputation or `missForest`
# for random forest imputation.  However, two quick and fast solutions
# are:
# 1) Complete-case analysis: Drop all rows with missing values, using
# the `na.omit()` command
# 2) Mean/median impute: Impute the mean (or median) for each missing value
# using the `coalesce(., median(., na.rm = TRUE))` command
# Let's try (2):
listings <- listings %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_all(function(x) coalesce(x, median(x, na.rm = TRUE)))
sum(is.na(listings)) # Check that there are no more missing values

#### FIRST SET OF EXERCISES: Data Wrangling

# In dplyr, another way of writing functions instead of
# "function(x) <code here with x as a variable>" is
# "~ <code here with . as a variable".  In summary, this is
# our data pipeline from the beginning using this new notation:
listings <- listingsOrig %>%
  mutate(price = as.numeric(gsub("\\$|,", "", price))) %>%
  filter(accommodates <= 10, price <= 1000) %>%
  filter(property_type %in% c("Apartment", "House", "Bed & Breakfast",
                              "Condominium", "Loft", "Townhouse"),	
         !(neighbourhood_cleansed %in%
             c("Leather District","Longwood Medical Area"))) %>%
  select_if(~sum(is.na(.)) < 0.9 * nrow(listings)) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_all(~coalesce(., median(., na.rm = TRUE))) %>%
  mutate_if(is.character, as.factor)
# The great thing is, we can just run the previous command and be caught
# up to speed with all of the data wrangling stuff.  Woohoo dplyr!

## Regression
#### Linear Regression ####
# Before we build a linear model to predict price, we'll reserve
# a portion of our data to be a test set. There are lots of ways
# to do this. We'll use the `caTools` package.
# install.packages("caTools")
library(caTools)
set.seed(123)
spl <- sample.split(listings$price, SplitRatio = 0.7)
listingsTrain <- subset(listings, spl == TRUE)
listingsTest <- subset(listings, spl == FALSE)

# We now have two data frames which contain the training and
# testing data, respectively.  The command `set.seed(123)`
# ensures that our random splits will be the same.  (There is a random
# number generator in R initialized with the value 123.) 

# In R, we specify a model structure and then use the corresponding
# function to tell R to optimize for the best-fitting model.
# For linear regression, the function is `lm()`:	
lm1 <- lm(price ~ accommodates, data = listingsTrain)
# We'll talk more about the '~' notation soon	

# Let's check out the lm1 object. It is a list of a bunch of relevant
# information generated by the `lm()` function call, and we can use
# the `$` to view different elements. 
names(lm1)
lm1$coefficients

# The function `summary()` is overloaded for many different objects
# and often gives a useful snapshot of the model as well.
summary(lm1)

# There we go, this is more useful! First, let's look at the section
# under "Coefficients". Notice that R automatically adds an intercept
# term unless you tell it not to (we'll see how to do this later).
# In the "estimate" column, we see that the point estimates for the
# linear model here say that the price is \$55.20 plus \$37.79 for
# every person accommodated. Notice the '***' symbols at the end of
# the "(Intercept)" and "accommodates" rows. These indicate that
# according to a statistical t-test, both coefficients are extremely
# significantly different than zero, so things are okay from an
# inference perspective.	

# As another check on inference quality, let's plot the fitted line.
# There are some nifty functions in the `modelr` package that make
# interacting with models easy in the `tidyr` and `dplyr` setting.
# We'll use `modelr::add_predictions()` here.	
# install.packages("modelr")
library(modelr)
listingsTrain %>%	
  add_predictions(lm1) %>%	
  ggplot(aes(x = accommodates)) +	
  geom_point(aes(y = price)) +	
  geom_line(aes(y = pred), color = 'red')

# Nice. We can also remove the linear trend and check the residual
# uncertainty, which we'll do here using `modelr::add_residuals()`.
# This is helpful to make sure that the residual uncertainty looks
# like random noise rather than an unidentified trend.	
listingsTrain %>%	
  add_residuals(lm1, var = "resid") %>%	
  ggplot(aes(x = accommodates, y = resid)) + 
  geom_point()

# Since we have finitely many values, maybe box plots tell a better story:	
listingsTrain %>%	
  add_residuals(lm1, var = "resid") %>%	
  group_by(as.factor(accommodates)) %>%	
  ggplot(aes(x = as.factor(accommodates), y = resid)) + 
  geom_boxplot()

# Things are pretty centered around zero, with the exception of
# 9- & 10-person accommodations. Maybe the model doesn't apply
# so well here, why might that be?	

# Now, what if we wanted to *quantify* how well the model predicts
# these out-of-sample values? We'll look at the out-of-sample R^2 (OSR^2),
# also known as the "coefficient of determination":

# OSR^2 = 1 - SSE / SST,
# where:
# SSE = $\sum_{i=1}^n (\hat{y}_i - y_i)^2$  ("Sum of Squares Error")
# SST = $\sum_{i=1}^n (\bar{y} - y_i)^2$  ("Sum of Squares Total")

# In these equations, $\hat{y}_i$ is the predicted value for test
# observation $i$, $y_i$ is the actual value, $n$ is the size of
# the test set, and $\bar{y}$ is the mean of $y_i$ in the training set.
# Let's code this up.
pred_test <- predict(lm1, newdata = listingsTest)
OSR2 <- 1 - sum((pred_test - listingsTest$price) ^ 2) /
  sum((mean(listingsTrain$price) - listingsTest$price) ^ 2)

# The in-sample R^2 value should agree with the "Multiple R-squared"
# value returned by summary().  Are we overfitting?
summary(lm1)
OSR2

# Let's save this into a data.frame to be accessed later
results <- data.frame(model = "original", R2 = summary(lm1)$r.squared,
                      OSR2 = OSR2, stringsAsFactors = F)

#### SECOND SET OF EXERCISES: Linear Regression

## Summary and More about Formulas	
# First, let's review the pattern, because it can be generalized to
# a whole bunch of more complex models. We asked the questions:
# How does listing price depend on the number of people it
# accommodates? How well does accommodation size predict price?
# Since we were interested in prediction, we reserved part of our
# data as a test set. We then chose to use a linear model to
# answer these questions, and found the corresponding function `lm()`.
# This function, and modelling functions in general, takes as arguments:

# * Data on the response and predictor variables,
# usually through a `formula` object	
# * Model parameters (in the case of `lm()`, we used all
# the default values)	

# R then automatically found the "best" linear model by computing
# the least squares estimate, and returned a `lm` object, which
# was a list including information about:

# * Fitted coefficients	
# * Residuals	
# * Statistical significance	
# * And more...	

# We interacted with the model to evaluate goodness-of-fit and
# out-of-sample performance. In our case, we used the `caTools`
# and `dplyr` framework to do this cleanly.	

# We didn't say too much about the `price ~ accommodates` syntax.
# Many modelling functions in R take `formula`s as arguments,
# together with a `data` argument. The `data` argument specifies
# the data frame, and the `formula` argument tells the model
# which are the responses and which are the predictors. We'll
# play around with formulas in the exercises, but here are a
# few helpful pointers:	

# * The `~` separates the response (on the left) from the
# predictors (on the right)	
# * Predictors are separated with a `+`	
# * Use `.` on the right-hand side to include all predictors
# in a given data frame	
# * You can also use `.-x` to include all predictors except `x`	
# * To include interactions between variables, use the `*` symbol.
# For example: `y ~ x + z + x*z` expresses the form:
# "regress `y` on `x`, `z`, and on `x` interacted with `z`	
# * To exclude the intercept term, include `-1` or `+0` on
# the right-hand side	
# For more detailed info, see
# <https://stat.ethz.ch/R-manual/R-devel/library/stats/html/formula.html>.	

#### Model Selection and Tuning	####
# Let's work a bit harder on predicting price, this time using more
# than one predictor. In fact, we'll add a bunch of predictors to
# the model and see what happens.	

# As one set of predictors, the column listings$amenities looks interesting:
listings$amenities[1:2]

# This could be good predictive information if we can separate out which
# listing has which amenity. Our goal here is to turn the amenities
# column into many columns, one for each amenity, and with logical
# values indicating whether each listing has each amenity. This is
# just a bit tricky, so I've written a function called `expand_amenities`
# that will do this for us. We need to `source()` the file that has this
# function in it, and then we'll call it on the `listings` data frame.	
source("expand_amenities.R")
listingsBig <- expand_amenities(listings)

# In total, we'll use all of these predictors:
# * accommodates	
# * property_type	
# * review_scores_rating	
# * neighbourhood_cleansed	
# * accommodates*room_type	
# * property_type*neighbourhood_cleansed 	
# * review_scores_rating*neighbourhood_cleansed 	
# * accommodates*review_scores_rating	
# * All columns created from the amenities column	

# Note that whenever we include a non-numeric (or categorical)
# variable, R is going to create one indicator variable for all
# but one unique value of the variable. We'll see this in the
# output of `lm()`.	

# First, let's separate our new data set `listingsBig`
# into training and test sets:
set.seed(123)
spl <- sample.split(listingsBig$price, SplitRatio = 0.7)
listingsBigTrain <- subset(listingsBig, spl == TRUE)
listingsBigTest <- subset(listingsBig, spl == FALSE)

# To get R to learn the model, we need to pass it a formula.
# We don't want to write down all those amenity variables by hand.
# Luckily, we can use the `paste()` function to string all the
# variable names together, and then the `as.formula()` function
# to translate a string into a formula.	
amenities_string <- listingsBigTrain %>%
  select(starts_with("amenity")) %>%
  names() %>%
  paste(collapse = " + ")
amenities_string # Taking a look to make sure things worked	

# Next, let's paste in the amenities string with the
# interaction terms to get the full formula.
big_formula <- as.formula(paste("price ~ accommodates + accommodates*room_type + property_type + neighbourhood_cleansed + property_type*neighbourhood_cleansed + review_scores_rating*neighbourhood_cleansed + accommodates*review_scores_rating", amenities_string, sep = " + "))
big_formula

# Now we can use the `lm()` function:
lm2 <- lm(big_formula, data = listingsBigTrain)	
summary(lm2)
# What happens when we compare in-sample and
# out-of-sample prediction performance?
R2_2 <- summary(lm2)$r.squared
pred_test <- predict(lm2, newdata = listingsBigTest)
OSR2_2 <- 1 - sum((pred_test - listingsBigTest$price) ^ 2) /
  sum((mean(listingsBigTrain$price) - listingsBigTest$price) ^ 2)

# Let's add these values as a new row to our results data.frame
# that we created earlier
results <- results %>%
  rbind(list(model = "big", R2 = R2_2, OSR2 = OSR2_2))
results

# This is way better than our initial model, but we've got an overfitting
# problem here, meaning that the training error is smaller than the
# test error. The model is too powerful for the amount of data we have.
# Note that R recognizes this by giving warnings about a "rank-deficient fit."	


#### Regularized/Penalized Regression	####
# But is there still a way to use the info from all these
# variables without overfitting? Yes! One way to do this is by
# regularized, or penalized, regression.	

# Mathematically, we add a term to the optimization problem that
# we're solving when fitting a model, a term which penalizes models
# that get too fancy without enough data. If we call $\beta$ the
# coefficient vector that we'd like to learn about for linear
# regression, then the regular regression we've worked with
# looks like	
# $$	
# \min_\beta \sum_{i=1}^n (y_i-x_i^T\beta)^2,	
# $$	
# but penalized regression looks like	
# $$	
# \min_\beta \sum_{i=1}^n (y_i-x_i^T\beta)^2 + \lambda ||\beta||.	
# $$	

# There are two types of flexibility within this framework:
# * Choice of norm, a structural decision, and	
# * Choice of $\lambda$, a parametric decision.	

# Two natural choices of norm are the Euclidean 1- and 2-norms.
# When we use the 2-norm, it's often called "ridge regression."
# We'll focus today on the 1-norm, or "LASSO regression". On a
# very simple level, both types of regression shrink all the
# elements of the unconstrained $\beta$ vector towards zero,
# some more than others in a special way. LASSO shrinks the coefficients
# so that some are equal to zero. This feature is nice because it helps
# us interpret the model by getting rid of the effects of many
# of the variables.	

# To do LASSO, we'll use the `glmnet` package. Of note, this package
# doesn't work very elegantly with the `tidyverse` since it uses
# matrix representations of the data rather than data frame
# representations. However, it does what it does quite well, and
# will give us a chance to see some base R code. Let's load the
# package and check out the function `glmnet()`. We can see the
# documentation from the command line using `?glmnet`.	
library(glmnet)
?glmnet

# Notice that `glmnet()` doesn't communicate with the data via
# formulas. Instead, it wants a matrix of predictor variables and
# a vector of values for the variable we're trying to predict,
# including all the categorical variables that R automatically
# expanded into indicator variables. Fortunately, R has a
# `model.matrix()` function which takes a data frame and gets it
# into the right form for `glmnet()` and other functions with this
# type of input.	

# First, we need to convert the data to a matrix for X and a vector for Y
# We'll use the big_formula that we made previously for our linear
# regression, but with the dependent variable removed.
model_formula <- as.formula(gsub("price", "", paste(big_formula)))
x <- model.matrix(model_formula, data = listingsBig)
y <- as.vector(listingsBig$price)

# Next, split into training/testing sets
set.seed(123)
spl <- sample.split(y, SplitRatio = 0.7)
xTrain <- x[spl, ]
xTest <- x[!spl, ]
yTrain <- y[spl]
yTest <- y[!spl]

# Finally, let's fit a our first LASSO model. There's a way to
# specify lambda manually, but let's just accept the default
# for now and see what we get.
lasso1 <- glmnet(xTrain, yTrain)

# This time the `summary()` function isn't quite as useful:
summary(lasso1)

# It does give us some info, though. Notice that "lambda" is a
# vector of length 86. The `glmnet()` function has defined 86
# different values of lambda and found the corresponding optimal
# beta vector for each one! We have 86 different models here.
# Let's look at some of the coefficients for the different models.
# We'll start with one where lambda is really high:	
lasso1$lambda[1]
lasso1$beta[, 1] # How many coefficients are nonzero?	

# Here the penalty on the size of the coefficients is so high
# that R sets them all to zero. Moving to some smaller lambdas:	
lasso1$lambda[10]	
beta <- lasso1$beta[, 10] 
beta[which(beta != 0)]
	
lasso1$lambda[20]
beta <- lasso1$beta[, 20]
beta[which(beta != 0)]

## Cross-Validation
# How do we choose which of the 86 models to use? Or in other words,
# how do we "tune" the $\lambda$ parameter? We'll use a similar idea
# to the training-test set split called cross-validation.	

# The idea behind cross-validation is this: what if we trained our
# family of models (in this case 86) on only some of the training data
# and left out some other data points? Then we could use those other
# data points to figure out which of the lambdas works best
# out-of-sample. So we'd have a training set for training all the
# models, a validation set for choosing the best one, and a test set
# to evaluate performance once we've settled on a model.	

# There's just one other trick: since taking more samples
# reduces noise, could we somehow take more validation set
# samples? Here's where *cross*-validation comes in.
# We divide the training data into groups called *folds*,
# and for each fold repeat the train-validate procedure on
# the remaining training data and use the current fold as a
# validation set. We then average the performance of each model
# on each fold and pick the best one.	

# This is a very common *resampling* method that applies in lots and
# lots of settings. Lucky for us the glmnet package has a very handy
# function called `cv.glmnet()` which does the entire process
# automatically. Let's look at the function arguments using `?cv.glmnet`.	

# The relevant arguments for us right now are	
# * x, the matrix of predictors	
# * y, the response variable	
# * nfolds, the number of ways to split the training set (defaults to 10)
# * type.measure, the metric of prediction quality. It defaults to
# mean squared error, the square of RMSE, for linear regression	

# Let's do the cross-validation:
lasso2 <- cv.glmnet(xTrain, yTrain)
summary(lasso2) # What does the model object look like?	

# Notice the "lambda.min". This is the best lambda as determined by
# the cross validation. "lambda.1se" is the largest lambda such that
# the "error is within 1 standard error of the minimum."	

# There's another automatic plotting function for `cv.glmnet()`
# which shows the error for each model:	
plot.cv.glmnet(lasso2)

# The first vertical dotted line shows `lambda.min`, and the
# second is `lambda.1se`. The figure illustrates that we cross-validate
# to find the "sweet spot" where there's not too much bias (high lambda)
# and not too much noise (low lambda). The left-hand side of this graph
# is flatter than we'd sometimes see, meaning that the unpenalized model
# may not be too bad. However, increasing lambda increases
# interpretability at close to no loss in prediction accuracy!	

# Let's again compare training and test error. Because we are using
# glmnet we need to use the specialized `predict.cv.glmnet()` function
# to get the in-sample and out-of-sample predictions:
?predict.cv.glmnet
pred_train <- predict.cv.glmnet(lasso2, newx = xTrain, s = "lambda.min")
pred_test <- predict.cv.glmnet(lasso2, newx = xTest, s = "lambda.min")
# Then use the formula to compute R^2 and OSR^2:
R2_lasso <- 1 - sum((pred_train - yTrain) ^ 2) /
  sum((mean(yTrain) - yTrain) ^ 2)
OSR2_lasso <- 1 - sum((pred_test - yTest) ^ 2) /
  sum((mean(yTrain) - yTest) ^ 2)

# Let's add these as a row to our results data.frame
results <- results %>%
  rbind(list(model = "lasso", R2 = R2_lasso, OSR2 = OSR2_lasso))
results

# The overfitting problem has gotten better, but hasn't yet gone
# away completely. I added a bunch variables for dramatic effect
# that we could probably screen out before running the LASSO if we
# really wanted a good model.	

# One more note on cross-validation: the `glmnet` package has built-in
# functionality for cross-validation. In situations where that's not
# the case, `modelr::crossv_kfold()` will prepare data for
# cross-validation in a nice way.	

#### THIRD SET OF EXERCISES: glmnet
#### 10-min break

## Classification	
# So far we've looked at models which predict a continuous response
# variable. There are many related models which predict categorical
# outcomes, such as whether an email is spam or not, or which digit
# a handwritten number is. We'll take a brief look at two of these:
# logistic regression and classification trees.	

#### Logistic Regression ####
# Logistic regression is part of the class of generalized linear
# models (GLMs), which build directly on top of linear regression.
# These models take the linear fit and map it through a non-linear
# function. For logistic regression this function is the logistic
# function, $f(x) = \exp(x)/(1+\exp(x))$, which looks like this:	
xs <- seq(-10, 10, 0.25)
ys <- exp(xs) / (1 + exp(xs))
plot(xs, ys)

# Since the function stays between zero and one, it can be interpreted
# as a mapping from predictor values to a probability of being in one
# of two classes.	

# Let's apply this model to the `listings` data. Let's try to predict
# which listings have elevators in the building by price. 
set.seed(123)
spl <- sample.split(listingsBig$amenity_Elevator_in_Building, SplitRatio = 0.7)
listingsGLMTrain <- subset(listingsBig, spl == TRUE)
listingsGLMTest <- subset(listingsBig, spl == FALSE)

# One nice thing about using `sample.split` for classification is that
# it preserves the ratio of class labels in the training and testing sets.

# Instead of the `lm()` function, we'll now use `glm()`, but the
# syntax is almost exactly the same:	
logReg1 <- glm(amenity_Elevator_in_Building ~ price,
            family = "binomial", data = listingsGLMTrain)
summary(logReg1)

# Again, we can add predictions to the data frame and plot
# these along with the actuals, although the result doesn't
# look nearly as clean:	
listingsGLMTest %>%
  mutate(pred = predict(logReg1, newdata = listingsGLMTest, type = "response")) %>%
  ggplot(aes(x = price)) + 
  geom_line(aes(y = pred)) + 
  geom_point(aes(y = amenity_Elevator_in_Building + 0))

# One way to get a more informative plot is by using the
# `logi.hist.plot()` function in the `popbio` package.	

# In the meantime, we can explore out-of-sample performance. 
# Ultimately, we want to predict whether or not a listing has an
# elevator. However, logistic regression gives us something a bit
# different: a probability that each listing has an elevator. This
# gives us flexibility in the way we predict. The most natural
# thing would be to predict that any listing with predicted
# probability above 0.5 *has* an elevator, and any listing with
# predicted probability below 0.5 *does not have* an elevator. But
# what if I use a wheelchair and I want to be really confident that
# there's going to be an elevator? I may want to use a cutoff value
# of 0.9 rather than 0.5. In fact, we could choose any cutoff value
# and have a corresponding prediction model.	

# There's a really nice metric that measures the quality of all
# cutoffs simultaneously: *AUC*, for "Area Under the receiver
# operating characteristic Curve." That's a mouthful, but the idea
# is simpler: For every cutoff, we'll plot the *false positive rate*
# against the *true positive rate* and then take the area under this
# curve. (A *positive* in our case is a listing that has an elevator.
# So a *true positive* is a listing that we predict has an elevator
# and really does have an elevator, while a *false positive* is a
# listing that we predict has an elevator and does *not* actually
# have an elevator.)	

# As the cutoff shrinks down from 1 to 0, the rate of total positives
# will increase. If the rate of true positives increases faster than
# the rate of false positives, this is one indication that the model
# is good. This is what AUC measures.	

# The `ROCR` package is one implementation that allows us to plot
# ROC curves and calculate AUC. Here's an example:	
# install.packages("ROCR")
library(ROCR)
pred_test <- predict(logReg1, newdata = listingsGLMTest, type = "response")	
pred_obj <- prediction(pred_test, listingsGLMTest$amenity_Elevator_in_Building)
# Creating a prediction object for ROCR	
perf <- performance(pred_obj, 'tpr', 'fpr')	
plot(perf, colorize = T) # ROC curve
performance(pred_obj, 'auc')@y.values # AUC - a scalar measure of performance	

# As you can see, the `performance()` function in the `ROCR` package
# is versatile and allows you to calculate and plot a bunch of
# different performance metrics.	

# In our case, this model gives an AUC of 0.7. The worst possible
# is 0.5 - random guessing. We're definitely better than random here,
# and could likely improve by adding more predictors.	

# We've covered basic logistic regression, but just as with linear
# regression there are many, many extensions. For example, we could
# do regularized logistic regression if we wanted to use many predictors,
# using the `glmnet` package.	

#### Classification Trees ####
# Finally, let's explore classification trees (often referred to as
# CART, for Classification And Regression Trees). 
 
# A (binary) classification tree makes predictions by grouping similar
# observations and then assigning a probability to each group using the
# proportion of observations within that group that belong to the
# positive class. Groups can be thought of as nodes on a tree, and
# tree branches correspond to logical criteria on the predictor
# variables. There's a lot of neat math that goes into building the
# trees, but we won't get into that today. For now let's get
# familiarized by looking at a simple example. We need the
# `rpart` library.	
library(rpart)

# The model construction step follows the same established pattern.
# We use the modelling function `rpart()`, which takes a formula
# and a data frame (and optional parameters) as arguments.
tree1 <- rpart(amenity_Elevator_in_Building ~ price +
                 neighbourhood_cleansed,
               data = listingsGLMTrain)
summary(tree1)

# This is another case when the `summary()` function is less
# helpful. We can plot the resulting tree:	
library(rpart.plot)
prp(tree1)

# To evaluate the prediction accuracy of our classification tree,
# we count up the number of times each of the following occurs:
# Y = 1, prediction = 1 (True Positive)
# Y = 0, prediction = 1 (False Positive)
# Y = 1, prediction = 0 (False Negative)
# Y = 0, prediction = 0 (True Negative)
# A table that holds these values is called a "confusion matrix".
# Then, accuracy = (# True Positives + # True Negatives) / (Total # of observations)
# Let's construct the confusion matrix and calculate
# the testing accuracy for our model
pred_test <- predict(tree1, newdata = listingsGLMTest)
confusionMatrix <- table(listingsGLMTest$amenity_Elevator_in_Building,
                         ifelse(pred_test > 0.5, "pred = 1", "pred = 0"))
accTest <- sum(diag(confusionMatrix)) / nrow(listingsGLMTest)

# What is the baseline out-of-sample accuracy?
# This is just the frequency of the most common class in the training set.
table(listingsGLMTest$amenity_Elevator_in_Building)[1] /
  nrow(listingsGLMTest)

# Note that we could have also computed the confusion matrices, training/testing
# accuracy, and baseline accuracy for our logistic regression model.

#### Tuning the CART model ####
# If we want to construct high accuracy decision tree models,
# then we need to tune the parameters.  CART has many parameters that 
# specify how the decision tree is constructed, but one of the
# most important is cp, the complexity parameter. 
# cp is a non-negative parameter which typically takes values
# like 0.1, 0.1, 0.01, 0.001, etc. (default = 0.01).
# It is the minimum complexity threshold that the CART algorithm uses
# to decide whether or not to make a split.  So:

# - If cp is low => low splitting threshold => big tree
# - If cp is high => high splitting threshold => small tree

# Similar to lambda in the LASSO model, cp controls the
# "complexity" of the model, so it important to tune it to avoid
# over-fitting or under-fitting. You can think of it like this:

# - If the tree is too big => too many splits => we have an over-fitting problem.
# - If the tree is too small => too few splits => we have an under-fitting problem.

# We want to find a tree in the middle, that is "just right".  
# The rpart package makes it easy to perform tune cp via cross-validation.
# Basically, we start out with a big tree, then "prune" it
# down to get the right sized tree.

# Let's begin by constructing a tree with a small cp parameter,
# which will vary depending upon the problem.  Here, let's do 0.001.  
treeBig <- rpart(amenity_Elevator_in_Building ~ price + neighbourhood_cleansed,
                 data = listingsGLMTrain,
                 cp = 0.001)
prp(treeBig)

# We can use the `printcp()` command to see the
# cross-validated error for different values of cp,
# where:
# "nsplit"    = number of splits in tree
# "rel error" = scaled training error
# "xerror"    = scaled cross-validation error
# "xstd"      = standard deviation of xerror
printcp(treeBig)

# Cool, so rpart automatically computes all of the cross-validated
# errors for trees with cp = 0.001 and up! We can also see these results
# visually using the `plotcp` command.  In this plot:
#  - size of tree = (number of splits in tree) + 1
#  - the dotted line occurs at 1 std. dev. above the minimum xerror
plotcp(treeBig)

# A rule of thumb is to select the cp value which first
# goes below the dotted line, and then prune the tree using
# this value.
treeFinal <- prune(treeBig, cp = 0.011)
prp(treeFinal)

# In this case, because the best cp value = 0.011 is very close to the 
# default cp value of 0.01, this tree is identical to the initial tree
# that we constructed.  This occurs because the best tree in this case
# is relatively simple.

#### FOURTH SET OF EXERCISES: Classification
