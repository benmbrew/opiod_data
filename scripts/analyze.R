# this script will read in full data or juts florida data and analyze

# source read_in.R to get data, functions, and libraries
source('read_in.R')
source('custom_theme.R')
# impute zero into NA for  "bene_count", "total_claim_count_ge65", "total_drug_cost_ge65", "total_day_supply_ge65", 
# "bene_count_ge65", "brand_claim_count, brand_drug_cost","generic_claim_count"            
# "generic_drug_cost"               "average_age_of_beneficiaries"    "beneficiary_female_count"       
# "beneficiary_male_count"          "beneficiary_race_white_count"    "beneficiary_race_black_count"   
# "beneficiary_race_asian_pi_count" "beneficiary_race_hispanic_count" "beneficiary_race_nat_ind_count" 
# "beneficiary_race_other_count"    "beneficiary_nondual_count"       "beneficiary_dual_count"         
# "beneficiary_average_risk_score" 

dat_full$bene_count[is.na(dat_full$bene_count)] <- 0
dat_full$total_claim_count_ge65[is.na(dat_full$total_claim_count_ge65)] <- 0
dat_full$bene_count_ge65[is.na(dat_full$bene_count_ge65)] <- 0
dat_full$beneficiary_female_count[is.na(dat_full$beneficiary_female_count)] <- 0
dat_full$beneficiary_male_count[is.na(dat_full$beneficiary_male_count)] <- 0
dat_full$beneficiary_race_white_count[is.na(dat_full$beneficiary_race_white_count)] <- 0
dat_full$beneficiary_race_black_count[is.na(dat_full$beneficiary_race_black_count)] <- 0
dat_full$beneficiary_race_asian_pi_count[is.na(dat_full$beneficiary_race_asian_pi_count)] <- 0
dat_full$beneficiary_race_hispanic_count[is.na(dat_full$beneficiary_race_hispanic_count)] <- 0
dat_full$beneficiary_race_nat_ind_count[is.na(dat_full$beneficiary_race_nat_ind_count)] <- 0
dat_full$beneficiary_race_other_count[is.na(dat_full$beneficiary_race_other_count)] <- 0

# create variable indicating a prescriber flag for each drug class
dat_full$opiate_flag <- ifelse(dat_full$sum_bene.opiate == 0, 'no', 'yes')
dat_full$anti_flag <- ifelse(dat_full$sum_bene.anti == 0, 'no', 'yes')
dat_full$pain_flag <- ifelse(dat_full$sum_bene.pain == 0, 'no', 'yes')
dat_full$musc_relax_flag <- ifelse(dat_full$sum_bene.musc_relax == 0, 'no', 'yes')
dat_full$benzo_flag <- ifelse(dat_full$sum_bene.benzo == 0, 'no', 'yes')
dat_full$psych_flag <- ifelse(dat_full$sum_bene.psych == 0, 'no', 'yes')
dat_full$sleep_flag <- ifelse(dat_full$sum_bene.sleep == 0, 'no', 'yes')
dat_full$cns_flag <- ifelse(dat_full$sum_bene.cns == 0, 'no', 'yes')
dat_full$barb_flag <- ifelse(dat_full$sum_bene.barb == 0, 'no', 'yes')



# compare percent and absolute number of beneficary prescrptions for opiate, anti, pain, psych, barb, cns, sleep, musc_relax, other

## TOTALS

# opiate, anti
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$anti_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'sum_bene.opiate', 
                 y = 'sum_bene.anti', 
                 xlab = 'Total opiates',
                 ylab = 'Total anti depressants', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, musc_relax
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$musc_relax_flag== 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'sum_bene.opiate', 
                 y = 'sum_bene.musc_relax', 
                 xlab = 'Total opiates',
                 ylab = 'Total muscle relaxers', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, benzo
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$benzo_flag== 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'sum_bene.opiate', 
                 y = 'sum_bene.benzo', 
                 xlab = 'Total opiates',
                 ylab = 'Total benzodiazepines', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))


# opiate, pain
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$pain_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'sum_bene.opiate', 
                 y = 'sum_bene.pain', 
                 xlab = 'Total opiates',
                 ylab = 'Total other pain meds', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))


# opiate, psych
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$psych_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'sum_bene.opiate', 
                 y = 'sum_bene.psych', 
                 xlab = 'Total opiates',
                 ylab = 'Total psych medicine', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, cns
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$cns_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'sum_bene.opiate', 
                 y = 'sum_bene.cns', 
                 xlab = 'Total opiates',
                 ylab = 'Total CNS (anxiety)', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, sleep
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$sleep_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'sum_bene.opiate', 
                 y = 'sum_bene.sleep', 
                 xlab = 'Total opiates',
                 ylab = 'Total sleeping aids', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, barb
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$barb_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'sum_bene.opiate', 
                 y = 'sum_bene.barb', 
                 xlab = 'Total opiates',
                 ylab = 'Total barbiturates', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))


## PERCENTS
# opiate, anti
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$anti_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'per_bene.opiate', 
                 y = 'per_bene.anti', 
                 xlab = 'Percent opiates',
                 ylab = 'Percent anti depressants', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, musc_relax
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$musc_relax_flag== 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'per_bene.opiate', 
                 y = 'per_bene.musc_relax', 
                 xlab = 'Percent opiates',
                 ylab = 'Percent muscle relaxers', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, benzo
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$benzo_flag== 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'per_bene.opiate', 
                 y = 'per_bene.benzo', 
                 xlab = 'Percent opiates',
                 ylab = 'Percent benzodiazepines', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))


# opiate, pain
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$pain_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'per_bene.opiate', 
                 y = 'per_bene.pain', 
                 xlab = 'Percent opiates',
                 ylab = 'Percent other pain meds', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))


# opiate, psych
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$psych_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'per_bene.opiate', 
                 y = 'per_bene.psych', 
                 xlab = 'Percent opiates',
                 ylab = 'Percent psych medicine', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, cns
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$cns_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'per_bene.opiate', 
                 y = 'per_bene.cns', 
                 xlab = 'Percent opiates',
                 ylab = 'Percent CNS (anxiety)', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, sleep
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$sleep_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'per_bene.opiate', 
                 y = 'per_bene.sleep', 
                 xlab = 'Percent opiates',
                 ylab = 'Percent sleeping aids', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

# opiate, barb
temp <- dat_full[dat_full$opiate_flag == 'yes' & dat_full$barb_flag == 'yes',]
get_scatter_plot(temp_dat = temp,
                 x = 'per_bene.opiate', 
                 y = 'per_bene.barb', 
                 xlab = 'Percent opiates',
                 ylab = 'Percent barbiturates', 
                 pch = 1,
                 size = 0.5,
                 stroke = 1,
                 alpha = 0.1,
                 remove_zero = FALSE, 
                 num_rows = nrow(dat_full))

#### BARPLOT

## GENDER


## RACE
temp_race <- dat_full[, c('opiate_flag','per_bene.opiate', 'beneficiary_race_white_count', 
                          'beneficiary_race_black_count', 'beneficiary_race_asian_pi_count',
                          'beneficiary_race_hispanic_count', 'beneficiary_race_nat_ind_count',
                          'beneficiary_race_other_count')]
temp_race <- temp_race[temp_race$opiate_flag == 'yes',]
names(temp_race) <- c('opiate_flag', 'per_bene.opiate', 'White', 'Black', 'Asian (PI)', 
                      'Hispanic', 'Native american','Other')

temp_race <- melt(temp_race, id.vars = c('opiate_flag', 'per_bene.opiate'))
temp_race <- temp_race %>% 
  group_by(variable) %>%
  summarise(mean_counts = round(mean(value), 2))


ggplot(temp_race,
       aes(reorder(variable, -mean_counts),
           mean_counts)) +
  geom_bar(stat= 'identity',
           fill = 'black',
           color = 'black',
           alpha = 0.5) +
  ylim(c(0, 170)) +
  geom_text(aes(label = mean_counts), vjust = -0.5) +
  xlab('Race') +
  ylab('Avg opioid') +
  custome_theme()
  

## medicaid

## MED TYPOE
