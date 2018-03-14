
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

##########
# initiate folders
##########
home_folder <- '/home/benbrew/Documents/'
data_brew <- paste0(home_folder,'data_brew_blogs/')
data_folder <- paste0(data_brew, 'Data/')
opiod_data_folder <- paste0(data_folder, 'opiod/')

##########
# drug classes
##########

# load other drug classes
drug_class <- read.csv(paste0(opiod_data_folder, 'drug_classes.csv'), 
                   stringsAsFactors = F, na.strings = c("", NA))

# make all text lower case
drug_class <- as.data.frame(apply(drug_class, 2, function(x) tolower(x)), stringsAsFactors = F)

# add and remove to homogenize columns
drug_class$benzo <- gsub(' ', '_', drug_class$benzo)
drug_class$barb <- gsub(' ', '_', drug_class$barb)
drug_class$cns <- unlist(lapply(strsplit(drug_class$cns, ' '), function(x) x[1]))
drug_class$opiate <- unlist(lapply(strsplit(drug_class$opiate, ' '), function(x) x[1]))
drug_class$psychotic <- unlist(lapply(strsplit(drug_class$psychotic, ' '), function(x) x[1]))

# remove all trailing and leading white spaces
drug_class <- as.data.frame(apply(drug_class, 2, function(x) trimws(x)), stringsAsFactors = F)

###########
# prescribers 
###########

#########
# A long list of drugs with numeric values indicating the total number of prescriptions written for the year
# by that individual
# 
# Opioid.Prescriber - a boolean label indicating whether or not that individual prescribed opiate 
# drugs more than 10 times in the year
##########

# read in
docs <- read.csv(paste0(data_dir, 'prescriber.csv'), 
                 stringsAsFactors = F, na.strings=c(""," ","NA"))

# make column names lower case
colnames(docs) <- tolower(colnames(docs))


# recode opiod.prescriber
docs$opioid.prescriber <- ifelse(docs$opioid.prescriber == 1, 'yes', 'no')

##########
# clean up variables
##########

# creat temp object to clean up credentials
temp_cred <- docs$credentials
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

# create new variables in docs data
docs$cred_clean <- temp_cred_clean$abbr

#############################
# create features
# get drug list (column names)
drug_cols <- colnames(docs)[6:(ncol(docs) -2)]

# make lower case and substitue '.' for '_'
drug_names <- tolower(drug_cols)
drug_names <- gsub('.', '_', drug_names,fixed =  T)

# subset docs
docs <- docs[, c('gender', 'state', 'cred_clean', 'specialty', 'opioid.prescriber', drug_cols)]

# ##########
# # see if we can classify each drug
# ##########
# 
# # first concatenate all generic drug names
# all_drugs <- c(drug_class$benzo, drug_class$barb, drug_class$cns, drug_class$opiate, drug_class$sleep, drug_class$anti_dep, drug_class$psychotic)
# all_drugs <- all_drugs[!is.na(all_drugs)]
# all_drugs <- paste(all_drugs, collapse = '|')
# 
# # check to see if each of the drugs in docs has a corresponding generic name in class data set
# length(which(grepl(all_drugs, drug_names)))

##########
# get total prescriptions and prescriptions by class
##########

# first make column for total prescribed 
docs$tot <- apply(docs[, 6:ncol(docs)], 
                  1,
                  function(x) sum(x))

# function to match drug generic names to that in prescriber data set
sum_class <- function(column) {
  
  drug_names <- paste(drug_class[, column][!is.na(drug_class[, column])] , collapse = '|')
  
  summed_class <- apply(docs[, 5:ncol(docs)][which(grepl(drug_names, colnames(docs)[5:ncol(docs)]))], 
                        1,
                        function(x) sum(x))
  
  return(summed_class)
  
}

docs$opiate_tot <- sum_class('opiate')
docs$benzo_tot <- sum_class('benzo')
docs$barb_tot <- sum_class('barb')
docs$cns_tot <- sum_class('cns')
docs$sleep_tot <- sum_class('sleep')
docs$anti_tot <- sum_class('anti_dep')
docs$psych_tot <- sum_class('psychotic')


###########
# get percent for each drug class
###########
docs$opiate_per <- round((docs$opiate_tot/docs$tot)*100, 2)

docs$benzo_per <- round((docs$benzo_tot/docs$tot)*100,2)

docs$barb_per <- round((docs$barb_tot/docs$tot)*100,2)

docs$cns_per <- round((docs$cns_tot/docs$tot)*100,2)

docs$sleep_per <- round((docs$sleep_tot/docs$tot)*100,2)

docs$anti_per <- round((docs$anti_tot/docs$tot)*100,2)

docs$psych_per <- round((docs$psych_tot/docs$tot)*100,2)



# remove outliers
docs <- docs[docs$opiate_tot < 5000,]
docs <- docs[docs$benzo_tot < 5000,]
docs <- docs[docs$cns_tot < 1000,]

###########
# subset to look at totals only
###########
docs_model <- docs[, c('gender', 'state', 'cred_clean', 'specialty', 'opioid.prescriber', 
                       'opiate_tot', 'benzo_tot', 'barb_tot', 'cns_tot', 'sleep_tot', 'anti_tot', 'psych_tot',
                       'opiate_per', 'benzo_per', 'barb_per', 'cns_per', 'sleep_per', 'anti_per', 'psych_per')]


#############################
# Visualization


#########
# correlation plots 
#########

# plot opiate against each other class
point_plot <- function(dat,
                       x, 
                       y, 
                       xlab, 
                       ylab,
                       alpha_num,
                       title,
                       just_opiates) {
  
  if(just_opiates)

  ggplot(dat, aes(x, y)) + 
    geom_point(size = 3, alpha = alpha_num, colour = 'darkorange') +
    xlab(xlab) +
    ylab(ylab) + 
    geom_smooth(method = "lm", se = F) + 
    ggtitle(title) + 
    theme_databrew()
  
}

# opiate and benzo
point_plot(dat = docs_model,
           x = docs_model$opiate_tot, 
           y = docs_model$benzo_tot,
           xlab = 'opiate prescriptions',
           ylab = 'benzodiazepine prescriptions', 
           alpha_num = 0.2,
           title = 'opiates and benzos')

# opiate and barb
point_plot(dat = docs_model,
           docs_model$opiate_tot, 
           docs_model$barb_tot,
           xlab = 'opiate prescriptions',
           ylab = 'barbiturates prescriptions',
           alpha_num = 0.2,
           title = 'opiates and barbiturates')

# opiate and cns
point_plot(dat = docs_model,
           docs_model$opiate_tot, 
           docs_model$cns_tot,
           xlab = 'opiate prescriptions',
           ylab = 'cns (stimulants) prescriptions',
           alpha_num = 0.2,
           title = 'opiates and stimulants')

# opiate and sleep
point_plot(dat = docs_model,
           docs_model$opiate_tot, 
           docs_model$sleep_tot,
           xlab = 'opiate prescriptions',
           ylab = 'sleep prescriptions',
           alpha_num = 0.2,
           title = 'opiates and sleep medication')

# opiate and anti
point_plot(dat = docs_model,
           docs_model$opiate_tot, 
           docs_model$anti_tot,
           xlab = 'opiate prescriptions',
           ylab = 'anti-depressants prescriptions',
           alpha_num = 0.2,
           title = 'opiates and anti-depressants')


# opiate and sleep
point_plot(dat = docs_model,
           docs_model$opiate_tot, 
           docs_model$psych_tot,
           xlab = 'opiate prescriptions',
           ylab = 'psychotic prescriptions',
           alpha_num = 0.2,
           title = 'opiates and psychotic meds')

# row_index <- docs_model$opioid.prescriber == 'no'
# any(apply(as.data.frame(docs_model[row_index ,6]), 1, function(x) sum(x)))
# anybody that prescribes opiates is simply an 'yes'opiate.prescriber column

##########
# Gender 
##########

# groupby gender, counts, number of opiate prescribers, mean % opiate, benzo, barbs, cns, sleep
gen_dat <- docs_model %>%
  group_by(gender) %>%
  summarise(counts = n(),
            num_prescribers = sum(opioid.prescriber == 'yes'),
            mean_opiod_per = mean(opiate_per, na.rm = T),
            mean_benzo_per = mean(benzo_per, na.rm = T))

# make long form
gen_dat_long <- melt(gen_dat, id.vars = 'gender')

# drop counts and num_prescribers
gen_dat_long <- gen_dat_long[!grepl('counts|num_prescribers', gen_dat_long$variable),]

cbPalette <- c("#E69F00", "#56B4E9")

# plot percent prescribed by gender
p <- ggplot(gen_dat_long, aes(gender, value, group = variable, fill = variable)) + 
  geom_bar(stat = 'identity', alpha = 0.7, position = 'dodge') + 
  xlab('Gender') +
  ylab('% of yearly prescriptions') +
  scale_fill_manual(name = '', 
                    breaks = c('mean_opiod_per', 'mean_benzo_per'),
                    labels = c('Opiates', 'Benzos'),
                    values = cbPalette)+ 
  ggtitle("Prescriptions by gender") +
  theme_databrew() +
  theme(axis.text.x = element_text(angle = 0, hjust=1), legend.position = 'right') 

p
##########
# State 
##########

# read in opiod data - overdoses and prescribers
overdose <- read.csv(paste0(opiod_data_folder, 'overdoses.csv'), stringsAsFactors = F)

# make columns names lower case
colnames(overdose) <- tolower(colnames(overdose))

# make numeric variables numeric
## first remove ','
overdose$population <- gsub(',', '', overdose$population)
overdose$population <- as.numeric(overdose$population)
overdose$deaths <- gsub(',', '', overdose$deaths)
overdose$deaths <- as.numeric(overdose$deaths)
overdose$per <- round((overdose$deaths/overdose$population)*100000, 3)


# groupby stateder, counts, number of opiate prescribers, mean % opiate, benzo, barbs, cns, sleep
state_dat <- docs_model %>%
  group_by(state) %>%
  summarise(counts = n(),
            num_prescribers = sum(opioid.prescriber == 'yes'),
            per_prescriber = round((num_prescribers/counts*100),2),
            mean_opiod_per = mean(opiate_per, na.rm = T),
            mean_benzo_per = mean(benzo_per, na.rm = T),
            mean_anti_per = mean(anti_per, na.rm = T),
            mean_psych_per = mean(psych_per, na.rm = T))
  

# correlation between opiod deaths in each state and percent opiod prescriptions
state_dat <- inner_join(overdose, state_dat, by = c('abbrev'='state'))


# deaths per 100k against % of doctors in state prescribing 
point_plot(state_dat, 
           state_dat$per_prescriber, 
           state_dat$per, 
           xlab = '% of doctors prescribing opiates', 
           ylab = 'Opiate deaths per 100k',
           alpha_num = 0.7,
           title = 'More prescritpions, more deaths')

# deaths per 100k against % avg opiate prescription rate 
point_plot(state_dat, 
           state_dat$mean_opiod_per, 
           state_dat$per, 
           xlab = '% of doctors prescribing opiates', 
           ylab = 'Opiate deaths per 100k',
           alpha_num = 0.7,
           title = 'Prescrition Rate vs Death Rate')






# drop counts and num_prescribers
state_dat_long <- state_dat_long[!grepl('counts|num_prescribers', state_dat_long$variable),]

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#999999")

# plot percent prescribed by stateder
p <- ggplot(state_dat_long, aes(stateder, value, group = variable, fill = variable)) + 
  geom_bar(stat = 'identity', alpha = 0.7, position = 'dodge') + 
  xlab('stateder') +
  ylab('% of yearly prescriptions') +
  scale_fill_manual(name = '', 
                    breaks = c('mean_opiod_per', 'mean_benzo_per', 'mean_barb_per', 'mean_cns_per', 'mean_sleep_per'),
                    labels = c('Opiates', 'Benzos', 'Barbs', 'CNS', 'Sleep Meds'),
                    values = cbPalette)+ 
  ggtitle("Prescriptions by stateder") +
  theme_databrew() +
  theme(axis.text.x = element_text(angle = 0, hjust=1), lestated.position = 'right') 


