
##########
# random forest prediction
##########

# dat <- dat_mod[1:1000,]
# k_folds <- 10
# class = T
# write function for random forest prediction 
pred_random_forest <- function(dat, k_folds, class){
  
  # get complete cases
  dat <- dat[complete.cases(dat),]
  
  # turn any character to factor
  for(col in 1:ncol(dat)) {
    
    if(typeof(dat[, col]) == 'character') {
      
      dat[, col] <- as.factor(dat[, col])
      
    } else  {
      print('not character')
    }
  }
  
  # get response variable 
  if(class) {
    
    # get class outcome 
    y_var <- as.factor(make.names(dat$opiate_cat))
    
    # remove reg variable 
    dat$sum_bene.opiate <- NULL
    
    # set training parameters
    NFOLDS = 10
    summaryFunc <- twoClassSummary
    
    # determines how you train the model.
    fitControl <- trainControl( 
      method = "repeatedcv",  # could train on boostrap resample, here use repeated cross validation.
      number = NFOLDS, 
      classProbs = TRUE,     
      repeats = 3,
      allowParallel = TRUE,
      summaryFunction = summaryFunc)
    
    # set metric to opitmize
    metric_family <- 'ROC'
    
    
  } else {
    
    # get reg outcome
    y_var <- dat$sum_bene.opiate
    
    # remove class variable
    dat$opiate_cat <- NULL
    
    # set training parameters
    fitControl <- trainControl( 
      method = "repeatedcv",  # could train on boostrap resample, here use repeated cross validation.
      number = NFOLDS,      
      repeats = 3,
      allowParallel = TRUE)
    
    # set metric to optimize
    metric_family <- 'RMSE'
    
  }
  
  # creat fold vecct
  fold_vec <- sample(1:k_folds, nrow(dat), replace = T)
  
  # get lists to hold results
  model <- list()
  importance <- list()
  test.stats <- list()
  reg_results <- list()
  # loop through k folds and assign training and test 
  for(i in 1:k_folds) {
    
    # get x 
    train_index <- !grepl(i, fold_vec)
    test_index <- !train_index
    
    # get train x and y
    train_y <- y_var[train_index]
    train_x <- dat[train_index,]
    
    # get test x and test y
    test_y <- y_var[test_index]
    test_x <- dat[test_index,]
    
    # mtry: Number of variables randomly sampled as candidates at each split.
    # ntree: Number of trees to grow.
    mtry <- sqrt(ncol(train_x))
    tunegrid <- expand.grid(.mtry=mtry)
    
    model[[i]]  <- train(x = train_x
                    , y =train_y
                    , method = "rf"
                    , trControl = fitControl
                    , tuneGrid = tunegrid
                    , importance = T
                    , metric = metric_family
                    , verbose = FALSE)
    
    importance[[i]] <- varImp(model[[i]])[[1]]

    ##########
    # Predictions on test data
    ##########
    
    if (class) {
      # This returns 100 prediction with 1-100 lambdas
      test.predictions <- predict(model[[i]], 
                                  data.matrix(test_x),
                                  type = 'prob')
      
      # set pred cutoff
      pred_cutoff <- 0.5
      
      # original should be fine, something wrong with caret package
      test.predictions <- test.predictions$TRUE.
      test.predictions <- as.factor(ifelse(test.predictions >= pred_cutoff, 'yes', 'no'))
      test.predictions <- factor(test.predictions, levels = c('yes', 'no'))
      
      test_y <- as.factor(ifelse(test_y >= pred_cutoff, 'yes', 'no'))
      test_y <- factor(test_y, levels = c('yes', 'no'))
      
      test_stats[[i]] <- caret::confusionMatrix(test.predictions, test_y)
      
      return(test_stats, importance)
      
      
    } else {
      
      # predict on test data
      test.predictions <- predict(model[[i]],
                                  newdata = test_x)
      
      reg_results[[i]] <- cbind(test.predictions, test_y) 
      return(test.predictions, test_y)
    }
   
  }
  
}



##########
# function for logistic regression
##########
dat <- dat_mod[1:1000,]
k_folds <- 10
class <- T

logit_fit <- function(dat, kfolds, class) {
  
  # get complete cases
  dat <- dat[complete.cases(dat),]
  
  # turn any character to factor
  for(col in 1:ncol(dat)) {
    
    if(typeof(dat[, col]) == 'character') {
      
      dat[, col] <- as.factor(dat[, col])
      
    } else  {
      print('not character')
    }
  }
  
  glm()
  
}

##########
# function for knn imputation
##########

# get imputation column names
knn_imputation <- function(original_data, column_name) {
  
  # get imputation data
  original_data$npi <- as.factor(original_data$npi)
  incomplete_data <- as.data.frame(original_data[, c('npi', column_name)])
  
  # get column vector(s) to impute on
  npi_vector <- incomplete_data$npi
  missing_vector <- incomplete_data[, column_name]
  
  # Transpose the data for KNN
  # KNN requires samples in the columns
  missing_vector <- as.matrix((t(missing_vector)))
  # Impute the missing data
  imputed_data <- impute.knn(missing_vector, 
                             k = 10, 
                             rowmax = 1, 
                             colmax = 1)$data
  
  
  # transpose back to samples in rows
  imputed_data <- t(imputed_data)
  
  # get missing index
  missing_index <- is.na(imputed_data)
  
  # remove NAs that were not imputed on
  imputed_data <- imputed_data[!is.na(imputed_data)]
  
  # subset npi by missing index 
  npi_vector <- npi_vector[!missing_index]
  
  # combine imputed data and npi and transpose 
  complete_data <- cbind(npi_vector, imputed_data)
  
  # subset full data by missing index and fill old varibales
  original_data <- original_data[!missing_index, ]
  original_data$beneficiary_average_risk_score <- complete_data[,2]
  
  return(original_data)
}




