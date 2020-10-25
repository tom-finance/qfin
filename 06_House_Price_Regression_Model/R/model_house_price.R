################################################################################
# House Price Model - Application Task
#
# Thomas Ludwig
################################################################################

# packages for analysis
library(dplyr)

################################################################################

# read data into R

# 1) training sample, which also contains test sample. We need to filter the
# test sample out of this data by using the IDs provided in the second file!
data_training <- read.csv("../data/housing_data.csv",
                          sep = " ", dec = ",") # be careful with seperators

# IDs to identify the out of sample set which is used to evaluate the fit of the model
data_test <- read.csv("../data/test_scoutId.csv",
                      sep = "", dec = ",")

################################################################################

# filter data of interest and separate training and test sample
data_training_clean <- data_training %>%
  filter(obj_regio1 == "Berlin") %>% # model should be fitted for Berlin only
  distinct(obj_scoutId, .keep_all = TRUE) %>%
  filter(!obj_scoutId %in% 
           data_test$obj_scoutId) %>% # throw out IDs from the test sample
  select(obj_purchasePrice,
    obj_lotArea, obj_cellar, obj_courtage, obj_barrierFree,
         obj_livingSpace, obj_noRooms, ga_cd_via, obj_buildingType,
         geo_bg) %>%
  filter(complete.cases(.)) %>% # I only want complete cases of the selected variables
  mutate(obj_cellar = as.factor(obj_cellar)) %>% # transform to factor variable
  mutate(obj_courtage = as.factor(obj_courtage)) %>%
  mutate(obj_barrierFree = as.factor(obj_barrierFree)) %>%
  mutate(ga_cd_via = as.factor(ga_cd_via)) %>%
  mutate(obj_buildingType = as.factor(obj_buildingType)) %>%
  mutate(geo_bg = as.factor(geo_bg)) %>%
  filter(obj_purchasePrice != 21000000) %>% # exclude this large value from sample
  filter(obj_noRooms != 196) %>% # exclude this large value from sample
  filter(obj_lotArea > 0) %>% # lotArea has to be greater than zero!
  mutate(obj_purchasePrice = log(obj_purchasePrice)) %>% # apply simple log transformation of variable
  mutate(obj_lotArea = log(obj_lotArea)) %>%
  mutate(obj_noRooms = log(obj_noRooms)) %>%
  mutate(obj_livingSpace = log(obj_livingSpace))
  
################################################################################
# now find a linear model for the data

# fit full model
model.full <- lm(obj_purchasePrice~.,
                 data=data_training_clean)

# check output
summary(model.full)


# fit empty model
model.empty <- lm(obj_purchasePrice~1,
                  data=data_training_clean)

# run step-wise variable selection procedure from full to empty model using forward direction
model.step.for <- step(model.empty,scope=formula(model.full),
                       direction=c("forward"))

# run step-wise variable selection from full model
model_from_full <- step(model.full)

# check selected models - in both cases the same models are selected!
# I could use some more sophisticated variable selection models, but for this sake I'm ok with the result.
summary(model.step.for)
summary(model_from_full)

################################################################################
# OUT OF SAMPLE PREDICTION

# prepare test sample
data_test_clean <- data_training %>%
  filter(obj_regio1 == "Berlin") %>%
  distinct(obj_scoutId, .keep_all = TRUE) %>%
  filter(obj_scoutId %in% 
           data_test$obj_scoutId) %>% # keep only IDs from test sample
  select(obj_purchasePrice,
         obj_lotArea, obj_cellar, obj_courtage, obj_barrierFree,
         obj_livingSpace, obj_noRooms, ga_cd_via, obj_buildingType,
         geo_bg) %>%
  mutate(obj_cellar = as.factor(obj_cellar)) %>%
  mutate(obj_courtage = as.factor(obj_courtage)) %>%
  mutate(obj_barrierFree = as.factor(obj_barrierFree)) %>%
  mutate(ga_cd_via = as.factor(ga_cd_via)) %>%
  mutate(obj_buildingType = as.factor(obj_buildingType)) %>%
  mutate(geo_bg = as.factor(geo_bg)) %>%
  mutate(obj_purchasePrice = log(obj_purchasePrice)) %>%
  mutate(obj_lotArea = log(obj_lotArea)) %>%
  mutate(obj_noRooms = log(obj_noRooms)) %>%
  mutate(obj_livingSpace = log(obj_livingSpace))

# make prediction for test sample. Output is a vector with predictions. Be careful,
# we predict log(purchaseprice), hence use exp(prediction) to go back to actual prices!

pred.full <- predict(model_from_full, 
                     data_test_clean)

# log predictions
pred.full

# actual predictions
exp(pred.full)

################################################################################
# calculate prediction error and compare with true value

# write function to calculate the prediction error
prediction_error <- function(y_pred, y_true) {
  pe <- sum((log(y_pred) - log(y_true))^2)/length(y_pred)
  return(pe)
}


# calculate error for the selected model, use both time exponential function to go back to actual prices!
prediction_error(y_pred = exp(pred.full),
                 y_true = exp(data_test_clean$obj_purchasePrice))

# calculate error for a naive estimate using the mean from the training sample
prediction_error(y_pred = exp(mean(data_training_clean$obj_purchasePrice)),
                 y_true = exp(data_test_clean$obj_purchasePrice))


# create data frame with the results
output_final <- tibble(predicted = exp(pred.full),
                      true = exp(data_test_clean$obj_purchasePrice))
# show output
output_final

################################################################################
################################################################################