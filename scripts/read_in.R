##########
# libraries
##########
library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(rworldmap)
library(ggmap)
library(impute)
library(zipcode)
library(ggthemes)


# get data directories 
saved_dir <- '../data/saved_data/'
prescriber_dir <- '../data/prescribers/'
opiod_dir <- '../data/opioids/'

source('functions.R')


if('dat_full.rda' %in% dir(saved_dir)){
  dat_full <- readRDS('../data/saved_data/dat_full.rda')
} else {
  
  # load drug class data
  drug_class <- read.csv(paste0(opiod_dir, 'drug_classes.csv'), 
                         stringsAsFactors = F, na.strings = c("", NA))
  
  # clean data
  drug_class <- clean_drug_class(drug_class)
  
  
  # read in unique prescriber/drug data
  system.time(dat_instance <- read.delim(paste0(prescriber_dir, 'raw_prescription_instance.txt'), # 24 milliong (24525869)
                                     stringsAsFactors = F))
  
  # subset to florida only
  # dat_instance <- dat_instance[dat_instance$nppes_provider_state == 'FL',]
  
  # get drug classification
  dat_instance <- classify_drugs(dat_instance)
 
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
  system.time(dat_prescriber <- read.delim(paste0(prescriber_dir, 'raw_prescriber.txt')))
  # load('~/Desktop/temp_pres.RData')
  # HERE
  
  # keep only certain columns and clean
  dat_prescriber <- keep_columns(dat_prescriber)
  
  # clean credentials
  dat_prescriber <- clean_creds(dat_prescriber)

  # get overlapping npi 
  length(which(dat_instance_summary$npi %in% dat_prescriber$npi)) # all of them are represented in dat_model 
  
  # unique npi 
  length(unique(dat_wide$npi)) # 332 unique
  length(unique(dat_prescriber$npi)) # all unique 
  
  # joine the two 
  dat_full <- inner_join(dat_prescriber, dat_wide,by = 'npi')
  
  # get zipcode data
  data(zipcode)
  
  # conver to character for join
  dat_full$zip5 <- as.character(dat_full$zip5)
  dat_full$zip5 <- clean.zipcodes(dat_full$zip5)
  
  # convert dat_Full factors to characters 
  dat_full$city <- as.character(dat_full$city)
  dat_full$state <- as.character(dat_full$state)
  
  # join to zipcode data
  dat_full <- inner_join(dat_full, zipcode, by = c('zip5' = 'zip'))
  
  # remove unneccary columns
  dat_full$city.x <- tolower(dat_full$city.x)
  dat_full$city.y <- tolower(dat_full$city.y)
  dat_full$city.x <- dat_full$state.x <- NULL
  names(dat_full) <- gsub('.y', '',names(dat_full), fixed = TRUE)
  
  # recode speciality so that any level with under 200 observations is classified as other
  # first get counts
  dat_full <- dat_full %>% group_by(specialty_description) %>% mutate(counts = n())
  
  # if any counts under 200, rename to toher
  dat_full$specialty_description <- ifelse(dat_full$counts < 3500, 'Other', as.character(dat_full$specialty_description))
  
  # save data 
  saveRDS(dat_full, '../data/saved_data/dat_full.rda')
  
}
