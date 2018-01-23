# install.packages("tidyverse")
library(tidyverse)
# install.packages("modelr")
library(modelr)
# install.packages("ROCR")
library(ROCR)
# install.packages("caTools")
library(caTools)
# install.packages("rpart")
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
# install.packages("glmnet")
library(glmnet)

e_net <- glmnet(matrix(c(1,2,3,4,3,4,5,6), nrow = 4), c(2,4,6,8))
e_net$beta[1,]
# The output should match up with "output-1.png"
