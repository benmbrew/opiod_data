# function to grab drug class names from drug_class dataset and convert to grepl format
get_generics <- function(class) {
  drug_names <- toupper(paste(drug_class[, class][!is.na(drug_class[, class])] , collapse = '|'))
  return(drug_names)
}


# function for classifying drugs 
classify_drugs <- function(temp_dat){
  
  # get the generic drugs associated with this class in one string
  opiates <- get_generics('opiate')
  benzo <- get_generics('benzo')
  barb <- get_generics('barb')
  cns <- get_generics('cns')
  sleep <- get_generics('sleep')
  anti <- get_generics('anti_dep')
  psych <- get_generics('psychotic')
  pain <- get_generics('pain')
  muscle <- get_generics('muscle')
  
  
  # if else statement to classify each drug
  temp_dat$class <- ifelse(grepl(opiates, temp_dat$drug_name), 'opiate',
                               ifelse(grepl(benzo, temp_dat$drug_name), 'benzo',
                                      ifelse(grepl(barb, temp_dat$drug_name), 'barb',
                                             ifelse(grepl(cns, temp_dat$drug_name), 'cns',
                                                    ifelse(grepl(sleep, temp_dat$drug_name), 'sleep',
                                                           ifelse(grepl(anti, temp_dat$drug_name), 'anti',
                                                                  ifelse(grepl(psych, temp_dat$drug_name), 'psych', 
                                                                         ifelse(grepl(muscle, temp_dat$drug_name), 'musc_relax',
                                                                                ifelse(grepl(pain, temp_dat$drug_name), 'pain','other')))))))))
  
  return(temp_dat)
  
}





# function to clean drug_class data
clean_drug_class <- function(temp_dat) {
  # names lower case
  colnames(temp_dat) <- tolower(colnames(temp_dat))
  
  # remove all trailing and leading white spaces
  temp_dat <- as.data.frame(apply(temp_dat, 2, function(x) trimws(x)), stringsAsFactors = F)
  
  # make all text lower case
  temp_dat <- as.data.frame(apply(temp_dat, 2, function(x) tolower(x)), stringsAsFactors = F)
  
  # add and remove to homogenize columns
  temp_dat$benzo <- gsub(' ', '_', temp_dat$benzo)
  temp_dat$barb <- gsub(' ', '_', temp_dat$barb)
  temp_dat$cns <- unlist(lapply(strsplit(temp_dat$cns, ' '), function(x) x[1]))
  temp_dat$opiate <- unlist(lapply(strsplit(temp_dat$opiate, ' '), function(x) x[1]))
  temp_dat$psychotic <- unlist(lapply(strsplit(temp_dat$psychotic, ' '), function(x) x[1]))
  temp_dat$muscle <- unlist(lapply(strsplit(temp_dat$muscle, ' '), function(x) x[1]))
  temp_dat$pain <- unlist(lapply(strsplit(temp_dat$pain, '\\s+'), function(x) x[1]))
  
  return(temp_dat)
  
}






keep_columns <- function(temp_dat){
  # keep only relevant columns 
  keep_cols <- c('npi', 'nppes_credentials', 'nppes_provider_gender', "nppes_provider_street1", 
                 "nppes_provider_street2", "nppes_provider_city","nppes_provider_zip5", 
                 "nppes_provider_zip4","nppes_provider_state", "specialty_description",
                 "medicare_prvdr_enroll_status","total_claim_count","total_day_supply",
                 "bene_count", "total_claim_count_ge65",  "total_drug_cost_ge65",
                 "total_day_supply_ge65","bene_count_ge65", "brand_claim_count", "brand_drug_cost", "generic_suppress_flag",
                 "generic_claim_count", "generic_drug_cost", "average_age_of_beneficiaries",
                 "beneficiary_female_count","beneficiary_male_count", "beneficiary_race_white_count", 
                 "beneficiary_race_black_count","beneficiary_race_asian_pi_count", "beneficiary_race_hispanic_count",  
                 "beneficiary_race_nat_ind_count", "beneficiary_race_other_count", "beneficiary_nondual_count", 
                 "beneficiary_dual_count", "beneficiary_average_risk_score")
  
  temp_dat <- temp_dat[, keep_cols]
  
  # clean column names
  colnames(temp_dat) <- gsub('nppes_provider_|nppes_', '', colnames(temp_dat))
  
  
  return(temp_dat)
  
}

# function for cleaning credientials
clean_creds <- function(temp_dat) {
  # creat temp object to clean up credentials
  temp_cred <- temp_dat$credentials
  temp_cred_clean <- as.data.frame(gsub('[,]| |[-]|[.]|[&]|[/]', replacement = '', temp_cred))
  colnames(temp_cred_clean) <- 'original'
  
  temp_cred_clean$original <- as.character(temp_cred_clean$original)
  md <- '^MD$|MD$|^MD|MEDICALDOCTOR'
  dentist <- '^DDS$|^DMD$|FAGD$|^DDS|DDS$|^DMD'
  np_pa <- '^NP$|^PA*|ASSISTANT|RACTITIONER|^PA|ARNP|APRN|CRNP|NP*|RPAC|PA$|PAC$'
  rn <- '^RN'
  od <- '^OD$|OD$|^OD'
  do <- '^DO$|DO$|^DO'
  dpm <- '^DPM$|DPM$|^DPM'
  
  # match professions
  temp_cred_clean$abbr <- ifelse(grepl(rn, temp_cred_clean$original), 'RN', 
                                 ifelse(grepl(np_pa, temp_cred_clean$original), 'NP_PA',
                                        ifelse(grepl(dentist, temp_cred_clean$original), 'Dentist',
                                               ifelse(grepl(md, temp_cred_clean$original), 'MD', 
                                                      ifelse(grepl(od, temp_cred_clean$original), 'OD', 
                                                             ifelse(grepl(dpm, temp_cred_clean$original), 'Podiatrist', 
                                                                    ifelse(grepl(do, temp_cred_clean$original), 'DO', 'Other')))))))
  
  # create new variables in temp_dat data
  temp_dat$credentials <- temp_cred_clean$abbr
  
  return(temp_dat)
  
}



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

# temp_dat = dat_full
# x = 'sum_bene.opiate' 
# y = 'sum_bene.anti' 
# xlab = 'Total opiates'
# ylab = 'Total anti depressants' 
# pch = 1
# size = 0.5
# stroke = 1
# alpha = 0.1
# remove_zero = FALSE 
# num_rows = nrow(dat_full)

get_scatter_plot <- function(temp_dat,
                             x, 
                             y, 
                             xlab, 
                             ylab, 
                             pch,
                             size,
                             stroke,
                             alpha,
                             remove_zero, 
                             num_rows) {
  
  # subset data 
  temp_plot <- temp_dat[ ,c(x, y)]
  names(temp_plot) <- c('V1', 'V2')
  
  # # get max value 
  # max_x <- max(temp_plot$V1)
  # max_y <- max(temp_plot$V2)
  # 
  # max_value <- max(max_x, max_y)
  
  if(remove_zero){
    temp_plot %>% filter(V1 > 0) %>% filter(V2 > 0)
  }
  corr_coef <- cor(temp_plot$V1, temp_plot$V2)
  ggplot(temp_plot[1:num_rows,],
         aes(V1, V2)) + 
    geom_point(size = size,
               pch = pch,
               stroke = stroke,
               alpha = alpha) +
    xlab(xlab) + 
    ylab(ylab) + 
    labs(subtitle = paste0('Correlation = ', round(corr_coef, 2))) +
    custome_theme()
  
  
}





# ##########
# # function for logistic regression
# ##########
# 
# 
# logit_fit <- function(dat, kfolds, class) {
#   
#   # get complete cases
#   dat <- dat[complete.cases(dat),]
#   
#   # turn any character to factor
#   for(col in 1:ncol(dat)) {
#     
#     if(typeof(dat[, col]) == 'character') {
#       
#       dat[, col] <- as.factor(dat[, col])
#       
#     } else  {
#       print('not character')
#     }
#   }
#   
#   glm()
#   
# }
# 
# ##########
# # function for knn imputation
# ##########
# 
# # get imputation column names
# knn_imputation <- function(original_data, column_name) {
#   
#   # get imputation data
#   original_data$npi <- as.factor(original_data$npi)
#   incomplete_data <- as.data.frame(original_data[, c('npi', column_name)])
#   
#   # get column vector(s) to impute on
#   npi_vector <- incomplete_data$npi
#   missing_vector <- incomplete_data[, column_name]
#   
#   # Transpose the data for KNN
#   # KNN requires samples in the columns
#   missing_vector <- as.matrix((t(missing_vector)))
#   # Impute the missing data
#   imputed_data <- impute.knn(missing_vector, 
#                              k = 10, 
#                              rowmax = 1, 
#                              colmax = 1)$data
#   
#   
#   # transpose back to samples in rows
#   imputed_data <- t(imputed_data)
#   
#   # get missing index
#   missing_index <- is.na(imputed_data)
#   
#   # remove NAs that were not imputed on
#   imputed_data <- imputed_data[!is.na(imputed_data)]
#   
#   # subset npi by missing index 
#   npi_vector <- npi_vector[!missing_index]
#   
#   # combine imputed data and npi and transpose 
#   complete_data <- cbind(npi_vector, imputed_data)
#   
#   # subset full data by missing index and fill old varibales
#   original_data <- original_data[!missing_index, ]
#   original_data$beneficiary_average_risk_score <- complete_data[,2]
#   
#   return(original_data)
# }
# 
# 
# 
# 
