# bene_count – The total number of unique Medicare Part D beneficiarieswith at least one claim for the drug. 
# Counts fewer than 11 are suppressed and are indicated by a blank\
# pdp: stand-alone prescription drug plans (PDPs) 
# mapd: integrated managed care (or Medicare Advantage) plans that also provide prescription drug coverage (MA-PDs).
# lis : low income subsideies
# hrm : high risk medications
# maybe impute random 1-10 in NAs
# interesting to look at opiate prescription load per zip code
# Risk scores: Those scores estimate how beneficiaries’ FFS spending will compare to the overall average for the entire Medicare 
# population. The average risk score is set at 1.08; beneficiaries with scores greater than that are expected to have 
# above-average spending, and vice versa. Risk scores are based on a beneficiary’s age and sex; whether the beneficiary 
# is eligible for Medicaid, first qualified for Medicare on the basis of disability, or lives in an institution 
# (usually a nursing home); and the beneficiary’s diagnoses from the previous year

##########
# libraries
##########
library(tidyverse)
library(reshape2)
library(plotly)
library(databrew)
library(rworldmap)
library(ggmap)
library(databrew)
library(caret)
library(glmnet)
library(randomForest)


##########
# initiate folders
##########
home_folder <- '/home/benbrew/Documents/'
data_brew <- paste0(home_folder,'data_brew_blogs/')
scripts_folder <- paste0(data_brew,'Scripts/')
data_folder <- paste0(data_brew, 'Data/')
opiod_data_folder <- paste0(data_folder, 'opiod/')


# read in model data
dat_mod <- readRDS(paste0(data_folder, 'dat_drug_model.rda'))

# read in imputed data
dat_mod_imp <- readRDS(paste0(data_folder, 'dat_drug_model_imputation.rda'))

##########
# source functions 
##########
source(paste0(scripts_folder, 'functions.R'))

##########
# classification - random forest 
##########

# remove uunneeded variables and variables associated with outcome 
dat_mod$npi <- dat_mod$specialty_description <- NULL
dat_mod_imp$npi <- dat_mod$specialty_description <- NULL

# do random forest - returns confusion matrix for each test set and importance variables
mod_class_rf <- pred_random_forest(dat_mod, 
                                   k_folds = 10, 
                                   class = T)

##########
# classification - logistic regression 
##########

