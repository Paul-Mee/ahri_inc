###
### This code takes the AHRI Surveillance Episodes HIV dataset and creates a row for each year an individual is
### resdident in a household
### 

# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# Set file paths
## AHRI data
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'


# Define vector of package names

package_names <- c('haven','dplyr','survival','psych','lubridate','schoRsch','data.table',
                   'imputeTS','DescTools','PerformanceAnalytics','qgraph','corrplot','Compind',
                   'ggplot2')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)


#### Now use the surveillance episodes dataset to get a 
#### quantile value for each individual in each year they were
#### included in the surveillance by i) linking the household quantile data 
#### to the individual surveillance and ii) interpolating for missing data using 
#### locf as before 

stata_data_file <- "/SurveillanceEpisodesHIV.dta"
ACDIS_epi <- haven::read_dta(paste0(data_dir,stata_data_file))
# 
ACDIS_epi$Start_Year <- lubridate::year(ACDIS_epi$StartDate)
ACDIS_epi$End_Year <- lubridate::year(ACDIS_epi$EndDate)
#
### Drop rows if individual is not resident for the duration of the episode - may need to review this step !!
#
ACDIS_epi <- ACDIS_epi %>% filter(Resident==1)
#
#### Filter first 1000 Id's  to test 
#
#ACDIS_epi <- ACDIS_epi %>% filter(IIntId < 10000)
#
#### Create a Year row for each year that the individual is resident in the household
#### This will be a slow step for full data (> 1 hour to run ?)
#
ACDIS_epi_full <- ACDIS_epi %>% 
  rowwise() %>% 
  do(data.frame(IIntId= .$IIntId, 
                HouseholdId= .$HouseholdId, 
                StartDate = .$StartDate, 
                EndDate = .$EndDate, 
                Res_Year = .$Start_Year:.$End_Year)) %>% 
  arrange(IIntId,StartDate)

### If in two residences in same year keep one with earliest start date

## Ranking by Individual Id , Mid_year
ACDIS_epi_full <- ACDIS_epi_full %>%
  group_by(IIntId,Res_Year) %>%
  dplyr::mutate(rank = order(order(StartDate, decreasing=FALSE)))
# 
# 
# ### If multiple episodes in same residence year just use first one
ACDIS_epi_full <- ACDIS_epi_full %>% filter(rank==1)

### Keep only required variables 

ACDIS_epi_full <- ACDIS_epi_full[,c('IIntId','HouseholdId','Res_Year')]

### Save as RDS file 

R_fname_house_res <- paste0(data_dir,"/House_Res_Episodes.RDS")
### Saving as RDS file
saveRDS(ACDIS_epi_full , file = R_fname_house_res)
