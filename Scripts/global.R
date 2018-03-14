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
prescriber_dir <- '../Data/prescribers/'
opiod_dir <- '../Data/opioids/'

source('functions.R')

row_nums <- 1000


if('prescribers.RData' %in% dir(prescriber_dir)){
  load('prescribers.RData')
} else {
  
  # load drug class data
  drug_class <- read.csv(paste0(opiod_dir, 'drug_classes.csv'), 
                         stringsAsFactors = F, na.strings = c("", NA))
  
  # clean data
  drug_class <- clean_drug_class(drug_class)
  
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
  
  # read in unique prescriber/drug data
  system.time(dat_instance <- read.delim(paste0(prescriber_dir, 'raw_prescription_instance.txt'), # 24 milliong (24525869)
                                     stringsAsFactors = F))
  
  # if else statement to classify each drug
  dat_instance$class <- ifelse(grepl(opiates, dat_instance$drug_name), 'opiate',
                           ifelse(grepl(benzo, dat_instance$drug_name), 'benzo',
                                  ifelse(grepl(barb, dat_instance$drug_name), 'barb',
                                         ifelse(grepl(cns, dat_instance$drug_name), 'cns',
                                                ifelse(grepl(sleep, dat_instance$drug_name), 'sleep',
                                                       ifelse(grepl(anti, dat_instance$drug_name), 'anti',
                                                              ifelse(grepl(psych, dat_instance$drug_name), 'psych', 
                                                                     ifelse(grepl(muscle, dat_instance$drug_name), 'musc_relax',
                                                                            ifelse(grepl(pain, dat_instance$drug_name), 'pain','other')))))))))
  
  
  # impute na bene count with random 1-10
  # get random 1-10 for 6050 samples 
  imputed_bene_count <- sample(1:10, length(which(is.na(dat_instance$bene_count))), replace = T)
  
  # now fill na bene count with this 
  dat_instance$bene_count[is.na(dat_instance$bene_count)] <- imputed_bene_count
  
  # group by npi, class and get total amount of beneficiaries and percent prescribed for each 
  # class
  dat_instance_summary <- 
    dat_instance %>% group_by(npi) %>%  
    mutate(total_sum_bene = sum(bene_count, na.rm = TRUE)) %>%
    group_by(npi, class) %>%  
    summarise(counts =n(),
              sum_bene = sum(bene_count, na.rm = TRUE),
              per_bene = sum(bene_count/total_sum_bene, na.rm = TRUE))
  
  # remove counts 
  dat_instance_summary$counts <- NULL
  
  # dat_instance_summary$key <- paste0(dat_instance_summary$npi, '_', dat_instance_summary$class)
  dat_instance_summary <- as.data.frame(dat_instance_summary)
  
  # get data in wide format
  dat_wide <- reshape(dat_instance_summary, idvar = 'npi', timevar = 'class', direction = "wide")
  # fill NAs with zeros
  dat_wide[is.na(dat_wide)] <- 0
  
  # read in unique prescriber data
  system.time(dat_prescriber <- read.delim(paste0(prescriber_dir, 'raw_prescriber.txt'), nrows = 100000))
  
  # keep only certain columns and clean
  dat_prescriber <- keep_columns(dat_prescriber)
  
  # clean credentials
  dat_prescriber <- clean_creds(dat_prescriber)
  
  
  # get overlapping npi 
  length(which(dat_instance_summary$npi %in% dat_prescriber$npi)) # all of them are represented in dat_model 
  
  # unique npi 
  length(unique(dat_wide$npi)) # 332 unique
  length(unique(dat_prescriber$npi)) # unique 
  
  # joine the two 
  dat_full <- inner_join(dat_prescriber, dat_wide,by = 'npi')
  
}

