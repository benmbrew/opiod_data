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
library(impute)




# get data directories 
prescriber_dir <- '../Data/prescirbers/'
opiod_dir <- '../Data/opioid/'

row_nums <- 1000


if('prescribers.RData' %in% dir(prescriber_dir)){
  load('prescribers.RData')
} else {
  # read in unique prescriber/drug data
  system.time(dat_drug <- read.delim(paste0(data_dir, 'raw_prescriber.txt'), # 24 milliong (24525869)
                                     stringsAsFactors = F,
                                     nrows = row_nums))
}

##########
# drug classes
##########

# load drug class data
drug_class <- read.csv(paste0(opiod_data_folder, 'drug_classes.csv'), 
                       stringsAsFactors = F, na.strings = c("", NA))

# names lower case
colnames(drug_class) <- tolower(colnames(drug_class))

# remove all trailing and leading white spaces
drug_class <- as.data.frame(apply(drug_class, 2, function(x) trimws(x)), stringsAsFactors = F)


# make all text lower case
drug_class <- as.data.frame(apply(drug_class, 2, function(x) tolower(x)), stringsAsFactors = F)

# add and remove to homogenize columns
drug_class$benzo <- gsub(' ', '_', drug_class$benzo)
drug_class$barb <- gsub(' ', '_', drug_class$barb)
drug_class$cns <- unlist(lapply(strsplit(drug_class$cns, ' '), function(x) x[1]))
drug_class$opiate <- unlist(lapply(strsplit(drug_class$opiate, ' '), function(x) x[1]))
drug_class$psychotic <- unlist(lapply(strsplit(drug_class$psychotic, ' '), function(x) x[1]))
drug_class$muscle <- unlist(lapply(strsplit(drug_class$muscle, ' '), function(x) x[1]))
drug_class$pain <- unlist(lapply(strsplit(drug_class$pain, '\\s+'), function(x) x[1]))

# function to grab drug class names from drug_class dataset and convert to grepl format
get_generics <- function(class) {
  drug_names <- toupper(paste(drug_class[, class][!is.na(drug_class[, class])] , collapse = '|'))
  return(drug_names)
}

# apply function
opiates <- get_generics('opiate')
benzo <- get_generics('benzo')
barb <- get_generics('barb')
cns <- get_generics('cns')
sleep <- get_generics('sleep')
anti <- get_generics('anti_dep')
psych <- get_generics('psychotic')
pain <- get_generics('pain')
muscle <- get_generics('muscle')

# read in unique prescriber/drug data
system.time(dat_drug <- read.delim(paste0(data_dir, 'raw_prescription_instance.txt'), # 24 milliong (24525869)
                                   stringsAsFactors = F,
                                   nrows = 100))

# if else statement to classify each drug
dat_drug$class <- ifelse(grepl(opiates, dat_drug$drug_name), 'opiate',
                         ifelse(grepl(benzo, dat_drug$drug_name), 'benzo',
                                ifelse(grepl(barb, dat_drug$drug_name), 'barb',
                                       ifelse(grepl(cns, dat_drug$drug_name), 'cns',
                                              ifelse(grepl(sleep, dat_drug$drug_name), 'sleep',
                                                     ifelse(grepl(anti, dat_drug$drug_name), 'anti',
                                                            ifelse(grepl(psych, dat_drug$drug_name), 'psych', 
                                                                   ifelse(grepl(muscle, dat_drug$drug_name), 'musc_relax',
                                                                          ifelse(grepl(pain, dat_drug$drug_name), 'pain','other')))))))))


##########
# impute na bene count with random 1-10
##########

# get random 1-10 for 6050 samples 
imputed_bene_count <- sample(1:10, length(which(is.na(dat_drug$bene_count))), replace = T)

# now fill na bene count with this 
dat_drug$bene_count[is.na(dat_drug$bene_count)] <- imputed_bene_count

##########
# group by npi, class and summarise
##########
dat_drug_summary <- dat_drug %>%
  group_by(npi, class) %>%
  summarise(counts =n(),
            sum_bene = sum(bene_count, na.rm = T))

# remove counts 
dat_drug_summary$counts <- NULL

# dat_drug_summary$key <- paste0(dat_drug_summary$npi, '_', dat_drug_summary$class)
dat_drug_summary <- as.data.frame(dat_drug_summary)

# get data in wide format
dat_wide <- reshape(dat_drug_summary, idvar = 'npi', timevar = 'class', direction = "wide")

# fill NAs with zeros
dat_wide[is.na(dat_wide)] <- 0

# create a categorical variable for if they have prescribed the drug once 
dat_wide$anti_cat <- ifelse(dat_wide$sum_bene.anti > 0, TRUE, FALSE)
dat_wide$other_cat <- ifelse(dat_wide$sum_bene.other > 0, TRUE, FALSE)
dat_wide$psych_cat <- ifelse(dat_wide$sum_bene.psych > 0, TRUE, FALSE)
dat_wide$anti_cat <- ifelse(dat_wide$sum_bene.anti > 0, TRUE, FALSE)
dat_wide$barb_cat <- ifelse(dat_wide$sum_bene.barb > 0, TRUE, FALSE)
dat_wide$benzo_cat <- ifelse(dat_wide$sum_bene.benzo > 0, TRUE, FALSE)
dat_wide$musc_relax_cat <- ifelse(dat_wide$sum_bene.musc_relax > 0, TRUE, FALSE)
dat_wide$opiate_cat <- ifelse(dat_wide$sum_bene.opiate > 0, TRUE, FALSE)
dat_wide$pain_cat <- ifelse(dat_wide$sum_bene.pain > 0, TRUE, FALSE)
dat_wide$cns_cat <- ifelse(dat_wide$sum_bene.cns > 0, TRUE, FALSE)

# read in unique prescriber data
system.time(dat_prescriber <- read.delim(paste0(data_dir, 'raw_prescriber.txt'), nrows = 1000))

# remove columns with 
remove_strings <- 'mi|entity_code|street|description_flag|30_day|suppress_flag|beneficiary_age'
dat_prescriber <- dat_prescriber[, !grepl(remove_strings, colnames(dat_prescriber))]

# clean column names
colnames(dat_prescriber) <- gsub('nppes_provider_|nppes_', '', colnames(dat_prescriber))

##########
# clean up variables
##########

# creat temp object to clean up credentials
temp_cred <- dat_prescriber$credentials
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

# create new variables in dat_prescriber data
dat_prescriber$credentials <- temp_cred_clean$abbr


##########
# get model data and featurize
##########

# first remove string characters with no predictive sig
remove_strings <- 'last_org_name|suppress_flg|first_name|city|zip|total_claim_count_ge65|total_drug_cost_ge65|total_day_supply_ge65|bene_count_ge65|generic|brand|other_claim'


dat_model <- dat_prescriber[!grepl(remove_strings, colnames(dat_prescriber))]

##########
# find overlapping npi between dat_model and dat_drug_summary
##########

# first remove other unneeded objects
rm(dat_drug, dat_prescriber, drug_class, temp_cred_clean)

# get overlapping npi 
length(which(dat_drug_summary$npi %in% dat_model$npi)) # all of them are represented in dat_model 

# unique npi 
length(unique(dat_wide$npi)) # 332 unique
length(unique(dat_model$npi)) # unique 

# joine the two 
dat_drug <- inner_join(dat_wide, dat_model, by = 'npi')

##########
# remove columns that are more than 50% NA 
##########

# get missing index 
temp_missing <- apply(dat_drug, 2, function(x) length(which(is.na(x))))

# get percent 
temp_missing_per <- temp_missing/nrow(dat_drug)

# get index for removing columns with missing pecent over .20
temp_index <- temp_missing_per > .20

# subset dat_drug by temp_index
dat_drug <- dat_drug[, !temp_index]

# get nrow with complete cases
nrow(dat_drug[complete.cases(dat_drug),])

# # impute risk scores
summary(dat_drug$beneficiary_average_risk_score)


# TEMP
dat_drug <- readRDS(paste0(data_folder, 'dat_drug_model.rda'))


# apply knn to beneficiary_avg_risk_score
imputed_data <- knn_imputation(dat_drug, 'beneficiary_average_risk_score')

# # remove problem columns
# dat_drug <- readRDS(paste0(data_folder, 'dat_drug_model.rda'))
# imputed_data <- readRDS(paste0(data_folder, 'dat_drug_model_imputation.rda'))

# recode some state variable to other 
summary(as.factor(dat_drug$state))
dat_drug$state <- as.character(dat_drug$state)
imputed_data$state <- as.character(imputed_data$state)
state_small <- 'AA|AE|AP|ZZ|AS|AR|GU|MP|XX|VI|HI|AL'
dat_drug$state <- ifelse(grepl(state_small, as.character(dat_drug$state)), 'other', dat_drug$state)
imputed_data$state <- ifelse(grepl(state_small, as.character(imputed_data$state)), 'other', imputed_data$state)

# save data for modelling 
saveRDS(dat_drug, paste0(data_folder, 'dat_drug_model.rda'))

# save imputed data
saveRDS(imputed_data, paste0(data_folder, 'dat_drug_model_imputation.rda'))
