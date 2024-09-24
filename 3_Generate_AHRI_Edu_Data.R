###
### This code collates Education data for individuals in the Southern PIPSA region of the AHRI site
### This version of the code uses Round-Years to aggregate the data
### take the year of the mean of the visit dates for each census round and then aggregate by this year

####
# Clear any existing data from the data set
rm(list = ls())
#####

# Define vector of package names

package_names <- c('haven','dplyr','tidyverse','lubridate')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023'
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'

#### Loading Education data 
### Loading Individual level data for Education variables 

stata_data_file <- '/RD07-99 ACDIS HSE-I All.dta'
ACDIS_ind <- as.data.frame(haven::read_dta(paste0(data_dir,stata_data_file))) 

### Extract Visit Year , Id, Highest School level 
### and remove duplicate data
ACDIS_edu <- ACDIS_ind[c('IIntId', 'VisitDate','DSRound', 'HighestSchoolLevel','HighestTertiaryLevel')]
ACDIS_edu <- unique(ACDIS_edu)



#### Loading Surveillance Episode Data to link individuals to their households at a particular visit 
stata_data_file <- "/SurveillanceEpisodesHIV.dta"
ACDIS_hh_epi <- haven::read_dta(paste0(data_dir,stata_data_file))
ACDIS_ind_hh <- ACDIS_hh_epi[c('IIntId', 'HouseholdId', 'StartDate','EndDate')]
#### removing duplicates 
ACDIS_ind_hh <- unique(ACDIS_ind_hh)

####
#### Use dplyr to merge the two dfs
#### where the visit date for the education data is between the episode date range
#### 


ACDIS_edu_hh  <- ACDIS_edu %>% 
  left_join(ACDIS_ind_hh, by = join_by(IIntId, between(VisitDate, 
                                                       StartDate,
                                                       EndDate)))

#####
#### Use household asset file to link Households with BSId's
#####
### Loading Household Asset data 

stata_data_file <- '/RD06-99 ACDIS HSE-H All.dta'
ACDIS_hh <- haven::read_dta(paste0(data_dir,stata_data_file))
ACDIS_BS_hh <- ACDIS_hh[c('HHIntId', 'VisitDate','BSIntId')]

#### Households can have multiple BS's so create Household BS episodes

ACDIS_BS_hh_epi = ACDIS_BS_hh %>% 
  group_by(HHIntId,BSIntId) %>% 
  dplyr::summarise(bs_start_date =min(VisitDate), 
                   bs_end_date= max(VisitDate))

### rename HHIntId to HouseholdId

ACDIS_BS_hh_epi <- ACDIS_BS_hh_epi %>% dplyr::rename( 'HouseholdId' = 'HHIntId')

### Merge Education HH data to BS Id episode data
ACDIS_edu_hh_BS  <- ACDIS_edu_hh %>% 
  left_join(ACDIS_BS_hh_epi, by = join_by(HouseholdId, between(VisitDate, 
                                                               bs_start_date,
                                                               bs_end_date)))
#### approx 300 extra rows added

### Drop duplicates
ACDIS_edu_hh_BS <- ACDIS_edu_hh_BS[c('IIntId','HouseholdId', 'DSRound','VisitDate','BSIntId','HighestSchoolLevel','HighestTertiaryLevel')]
ACDIS_edu_hh_BS <- unique(ACDIS_edu_hh_BS)


### Loading Bounded Structure Data to filter for only Southern PIPSA PIPSA==1

stata_data_file <- '/RD01-03 ACDIS BoundedStructures.dta'
ACDIS_BS <- haven::read_dta(paste0(data_dir,stata_data_file))

BS_PIP <- ACDIS_BS[c('BSIntId','PIPSA','IsUrbanOrRural','KmToNearestClinic')]
ACDIS_edu_hh_BS_PIP <- merge(ACDIS_edu_hh_BS,BS_PIP,by='BSIntId',all.x=TRUE)

ACDIS_edu_hh_BS_SPIP = ACDIS_edu_hh_BS_PIP[(ACDIS_edu_hh_BS_PIP$PIPSA %in% c(1)), ]

### Filter to drop rows in round 14 with visit date 2005-02-01 - misassigned dates 

ACDIS_edu_hh_BS_SPIP_sub <- ACDIS_edu_hh_BS_SPIP[!(ACDIS_edu_hh_BS_SPIP$DSRound==14 & 
                                                     ACDIS_edu_hh_BS_SPIP$VisitDate=="2005-02-01"), ]
### Create a Round_Year variable

ACDIS_edu_hh_BS_SPIP_sub$Visit_Year <- lubridate::year(ACDIS_edu_hh_BS_SPIP_sub$VisitDate)

years_rd_edu = ACDIS_edu_hh_BS_SPIP_sub %>% 
  group_by(DSRound) %>% 
  dplyr::summarise(mdate=mean(VisitDate))

years_rd_edu$Round_Year <- lubridate::year(years_rd_edu$mdate)

years_rd_edu <- years_rd_edu[c('DSRound','Round_Year')]

### Merge Round Year to Education file by DSRound

ACDIS_edu_full <- merge(ACDIS_edu_hh_BS_SPIP_sub,years_rd_edu,by='DSRound')


#### If multiple visits in a Round_Year keep first 

## Ranking by HH Id , Year, year_diff
ACDIS_edu_full <- ACDIS_edu_full %>%
  group_by(IIntId,Round_Year) %>%
  dplyr::mutate(rank = order(order(VisitDate, decreasing=FALSE)))

ACDIS_edu_full <- ACDIS_edu_full %>% filter(rank==1)


### https://www.researchgate.net/publication/267391685_RACIAL_DIFFERENCES_IN_EDUCATIONAL_ATTAINMENT_IN_SOUTH_AFRICA

### Recode School (Highest School Level Variable)
### None = 1,2
### Lower primary (Grades 1,2,3,4) = 3,4,5,6,7 
### Higher Primary (Grades 5,6,7) = 8,9,10
### Lower Secondary (Grades 8,9,10) = 11,12,13
### Higher Secondary (Grades 11,12) = 14,15

### Recode Tertiary (Highest Tertiary Level )
### Tertiary = 16,17,18,19,20

# ACDIS_edu_full$highest_edu <- NA
# 
# ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestSchoolLevel %in% c(1,2)] <- "None"
# ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestSchoolLevel %in% c(3,4,5,6,7)] <- "Lower Primary"
# ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestSchoolLevel %in% c(8,9,10)] <- "Higher Primary"
# ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestSchoolLevel %in% c(11,12,13)] <- "Lower Secondary"
# ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestSchoolLevel %in% c(14,15)] <- "Higher Secondary"
# ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestTertiaryLevel %in% c(16,17,18,19,20)] <- "Tertiary"


### Recode to three levels

ACDIS_edu_full$highest_edu <- NA

ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestSchoolLevel %in% c(1,2)] <- "None"
ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestSchoolLevel %in% c(3,4,5,6,7,8,9,10)] <- "Primary"
ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestSchoolLevel %in% c(11,12,13,14,15)] <- "Secondary"
ACDIS_edu_full$highest_edu[ACDIS_edu_full$HighestTertiaryLevel %in% c(16,17,18,19,20)] <- "Tertiary"

#### Impute missing data - if no data in a particular round year impute from last recorded value using LOCF

#### Unique list of Round_Years
all_round_years <- as.data.frame(unique(ACDIS_edu_full$Round_Year))
names(all_round_years)[1] <- "Round_Year"
#### Unique list of IDs
all_ids <- as.data.frame(unique(ACDIS_edu_full$IIntId))
names(all_ids)[1] <- "IIntId"

all_rd_year_id <- cross_join(all_round_years,all_ids)

ACDIS_edu_full_imp <- merge(all_rd_year_id,ACDIS_edu_full,by=c('Round_Year','IIntId'),all.x = TRUE)
ACDIS_edu_full_imp$highest_edu_imp <- ACDIS_edu_full_imp$highest_edu

#### Drop ID's where all edu values are missing

ACDIS_edu_full_imp <- ACDIS_edu_full_imp  %>%
                      group_by(IIntId) %>%
                      filter(!all(is.na(highest_edu_imp)))

ACDIS_edu_full_imp <- ungroup(ACDIS_edu_full_imp)

#### Convert highest_edu_imp to integer 
ACDIS_edu_full_imp$highest_edu_imp_int <- as.integer(as.factor(ACDIS_edu_full_imp$highest_edu_imp))


ACDIS_edu_full_imp$urban_rural  <- NA

ACDIS_edu_full_imp$urban_rural[ACDIS_edu_full_imp$IsUrbanOrRural %in% c(2)] <- "Peri-Urban"
ACDIS_edu_full_imp$urban_rural[ACDIS_edu_full_imp$IsUrbanOrRural %in% c(3)] <- "Rural"
ACDIS_edu_full_imp$urban_rural[ACDIS_edu_full_imp$IsUrbanOrRural %in% c(4)] <- "Urban"

ACDIS_edu_full_imp$urban_rural_fact <- factor(ACDIS_edu_full_imp$urban_rural,
                                         levels = c("Rural","Urban","Peri-Urban"))


ACDIS_edu_full_imp$km_clinic_cat <- NA
ACDIS_edu_full_imp$km_clinic_cat[(ACDIS_edu_full_imp$KmToNearestClinic >= 0 & ACDIS_edu_full_imp$KmToNearestClinic <= 2 )] <- "0-2"
ACDIS_edu_full_imp$km_clinic_cat[(ACDIS_edu_full_imp$KmToNearestClinic > 2 & ACDIS_edu_full_imp$KmToNearestClinic <= 4 )] <- ">2-4"
ACDIS_edu_full_imp$km_clinic_cat[(ACDIS_edu_full_imp$KmToNearestClinic > 4 & ACDIS_edu_full_imp$KmToNearestClinic <= 6 )] <- ">4-6"
ACDIS_edu_full_imp$km_clinic_cat[(ACDIS_edu_full_imp$KmToNearestClinic > 6 )] <- ">6"

ACDIS_edu_full_imp$km_clinic_fact <- factor(ACDIS_edu_full_imp$km_clinic_cat,
                                       levels = c("0-2",">2-4",">4-6",">6"))



### Keep required variables

ACDIS_edu_all <- ACDIS_edu_full_imp[c('IIntId','HouseholdId','Round_Year','highest_edu','highest_edu_imp_int','urban_rural_fact','km_clinic_fact')]

#### Drop duplicates 

ACDIS_edu_all <- unique(ACDIS_edu_all)


###Impute missing data values after first education data recorded
### Imputing highest education level and household Id - assuming that they stay 
### living in the same household
ACDIS_edu_all <- ACDIS_edu_all %>%
  group_by(IIntId) %>%
  arrange(Round_Year) %>%
  mutate(highest_edu.imp1 = imputeTS::na_locf(x=highest_edu_imp_int, option = "locf", na_remaining = "keep")) %>%
  mutate(household.imp1 = imputeTS::na_locf(x=HouseholdId, option = "locf", na_remaining = "keep"))

ACDIS_edu_all  <- ungroup(ACDIS_edu_all)

### Recode variables

ACDIS_edu_all$ACDIS_edu_all_char <- ""

ACDIS_edu_all$ACDIS_edu_all_char[ACDIS_edu_all$highest_edu.imp1 == 1] <- "None"
ACDIS_edu_all$ACDIS_edu_all_char[ACDIS_edu_all$highest_edu.imp1 == 2] <- "Primary"
ACDIS_edu_all$ACDIS_edu_all_char[ACDIS_edu_all$highest_edu.imp1 == 3] <- "Secondary"
ACDIS_edu_all$ACDIS_edu_all_char[ACDIS_edu_all$highest_edu.imp1 == 4] <- "Tertiary"


ACDIS_edu_all <- dplyr::rename(ACDIS_edu_all, 'highest_edu_imp' = 'ACDIS_edu_all_char')
ACDIS_edu_all <- dplyr::rename(ACDIS_edu_all, 'HouseholdId_imp' = 'household.imp1')

### Keep required variables

ACDIS_edu_all <- ACDIS_edu_all[c('IIntId','HouseholdId','HouseholdId_imp','Round_Year','highest_edu','highest_edu_imp','urban_rural_fact','km_clinic_fact')]



#### save Education data file 
save(ACDIS_edu_all, file = paste0(output_dir,'/ACDIS_edu_all.RData'))