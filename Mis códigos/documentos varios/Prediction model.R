library(caret)
library(mlbench)

data(Sonar)

set.seed(107)
inTrain <- createDataPartition(
  y = Sonar$Class,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)
## The format of the results

## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)

#By default, createDataPartition does a stratified random split of the data. To partition the data:
  
  training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]

nrow(training)
#> [1] 157
nrow(testing)
#> [1] 51


# To tune a model using the algorithm above, the train function can be used. More details on this function can be found at https://topepo.github.io/caret/model-training-and-tuning.html. 
# Here, a partial least squares discriminant analysis (PLSDA) model will be tuned over the number of PLS components that should be retained. The most basic syntax to do this is:
  
  plsFit <- train(
    Class ~ .,
    data = training,
    method = "pls",
    ## Center and scale the predictors for the training
    ## set and all future samples.
    preProc = c("center", "scale")
  )

  #However, we would probably like to customize it in a few ways:
  
  # expand the set of PLS models that the function evaluates. By default, the function will tune over three values of each tuning parameter.
# the type of resampling used. The simple bootstrap is used by default. We will have the function use three repeats of 10-fold cross-validation.
# the methods for measuring performance. If unspecified, overall accuracy and the Kappa statistic are computed. 
# For regression models, root mean squared error and R2 are computed. Here, the function will be altered to estimate the area under the ROC curve, the sensitivity and specificity
# To change the candidate values of the tuning parameter, either of the tuneLength or tuneGrid arguments can be used. 
# The train function can generate a candidate set of parameter values and the tuneLength argument controls how many are evaluated. 
# In the case of PLS, the function uses a sequence of integers from 1 to tuneLength. If we want to evaluate all integers between 1 and 15, setting tuneLength = 15 would achieve this. 
# The tuneGrid argument is used when specific values are desired. A data frame is used where each row is a tuning parameter setting and each column is a tuning parameter. An example is used below to illustrate this.

plsFit <- train(
  Class ~ .,
  data = training,
  method = "pls",
  preProc = c("center", "scale"),
  ## added:
  tuneLength = 15
)

# To modify the resampling method, a trainControl function is used. 
# The option method controls the type of resampling and defaults to "boot". 
# Another method, "repeatedcv", is used to specify repeated K-fold cross-validation (and the argument repeats controls the number of repetitions). 
# K is controlled by the number argument and defaults to 10. The new syntax is then:
  
  ctrl <- trainControl(method = "repeatedcv", repeats = 3)

plsFit <- train(
  Class ~ .,
  data = training,
  method = "pls",
  preProc = c("center", "scale"),
  tuneLength = 15,
  ## added:
  trControl = ctrl
)


# Finally, to choose different measures of performance, additional arguments are given to trainControl. 
# The summaryFunction argument is used to pass in a function that takes the observed and predicted values and estimate some measure of performance. 
# Two such functions are already included in the package: defaultSummary and twoClassSummary. 
# The latter will compute measures specific to two-class problems, such as the area under the ROC curve, the sensitivity and specificity. 
# Since the ROC curve is based on the predicted class probabilities (which are not computed automatically), another option is required. 
# The classProbs = TRUE option is used to include these calculations.

# Lastly, the function will pick the tuning parameters associated with the best results. 
# Since we are using custom performance measures, the criterion that should be optimized must also be specified. 
# In the call to train, we can use metric = "ROC" to do this.

ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

set.seed(123)
plsFit <- train(
  Class ~ .,
  data = training,
  method = "pls",
  preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl,
  metric = "ROC"
)

plsFit



# In this output the grid of results are the average resampled estimates of performance. 
# The note at the bottom tells the user that 3 PLS components were found to be optimal. 
# Based on this value, a final PLS model is fit to the whole data set using this specification and this is the model that is used to predict future samples.

# The package has several functions for visualizing the results. One method for doing this is the ggplot function for train objects. 
# The command ggplot(plsFit) produced the results seen in Figure ???
# and shows the relationship between the resampled performance values and the number of PLS components.

ggplot(plsFit)


# To predict new samples, predict.train can be used. For classification models, the default behavior is to calculate the predicted class. 
# The option type = "prob" can be used to compute class probabilities from the model. For example:
  
  plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)

plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

# caret contains a function to compute the confusion matrix and associated statistics for the model fit:
  
  confusionMatrix(data = plsClasses, testing$Class)

# To fit an another model to the data, train can be invoked with minimal changes. 
# Lists of models available can be found at https://topepo.github.io/caret/available-models.html or https://topepo.github.io/caret/train-models-by-tag.html. 
# For example, to fit a regularized discriminant model to these data, the following syntax can be used:
  
  ## To illustrate, a custom grid is used
  rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(
  Class ~ .,
  data = training,
  method = "rda",
  tuneGrid = rdaGrid,
  trControl = ctrl,
  metric = "ROC"
)

rdaFit

rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

# How do these models compare in terms of their resampling results? The resamples function can be used to collect, summarize and contrast the resampling results. Since the random number seeds were initialized to the same value prior to calling `train}, the same folds were used for each model. To assemble them:
  
  resamps <- resamples(list(pls = plsFit, rda = rdaFit))
  summary(resamps)

# There are several functions to visualize these results. For example, a Bland-Altman type plot can be created using
  
  xyplot(resamps, what = "BlandAltman")
  
  
# The results look similar. Since, for each resample, there are paired results a paired t-test can be used to assess whether there is a difference in the average resampled area under the ROC curve. The diff.resamples function can be used to compute this:
    
    diffs <- diff(resamps)
  summary(diffs)
