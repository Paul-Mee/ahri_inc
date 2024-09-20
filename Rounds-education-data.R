#### Looking at Round definition in Education data - Individual level data

####
# Clear any existing data from the data set
rm(list = ls())
#####

# Define vector of package names

package_names <- c('haven','dplyr','sqldf','tidyverse','lubridate')


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

BS_PIP <- ACDIS_BS[c('BSIntId','PIPSA')]
ACDIS_edu_hh_BS_PIP <- merge(ACDIS_edu_hh_BS,BS_PIP,by='BSIntId',all.x=TRUE)

ACDIS_edu_hh_BS_SPIP = ACDIS_edu_hh_BS_PIP[(ACDIS_edu_hh_BS_PIP$PIPSA %in% c(1)), ]

### Filter to drop rows in round 14 with visit date 2005-02-01 - misassigned dates 

ACDIS_edu_hh_BS_SPIP_sub <- ACDIS_edu_hh_BS_SPIP[!(ACDIS_edu_hh_BS_SPIP$DSRound==14 & 
                                  ACDIS_edu_hh_BS_SPIP$VisitDate=="2005-02-01"), ]


### Count records per Visit_Year
ACDIS_edu_hh_BS_SPIP_sub$Visit_Year <- lubridate::year(ACDIS_edu_hh_BS_SPIP_sub$VisitDate)

#### Summarise ACDIS DS_Round data 


years_rd_edu = ACDIS_edu_hh_BS_SPIP_sub %>% 
  group_by(DSRound) %>% 
  dplyr::summarise(earliest_visit_date =min(VisitDate), 
                   latest_visit_date= max(VisitDate),
                   mdate=mean(VisitDate),
                   edu_visit_count = n(),)

years_rd_edu$Round_Year <- lubridate::year(years_rd_edu$mdate)

### Now summarise by Round_Year 
round_year_edu = years_rd_edu %>% 
  group_by(Round_Year) %>% 
  dplyr::summarise(earliest_visit_date =min(earliest_visit_date), 
                   latest_visit_date = max(latest_visit_date),
                   edu_visit_count =sum(edu_visit_count),)

# ### Drop data from before 2004
# 
rounds_education = subset(round_year_edu, Round_Year >= 2004)


### Save file for later plotting
save(rounds_education, file = paste0(output_dir,'/edu_rounds.RData'))


### Output as a csv file 
output_fname <- '/year_rd_edu_summ.csv'

write.csv(years_rd_edu,paste0(output_dir,output_fname),row.names=FALSE)




