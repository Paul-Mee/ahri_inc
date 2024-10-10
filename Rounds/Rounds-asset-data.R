#### Looking at Round definition in Household asset data 

####
# Clear any existing data from the data set
rm(list = ls())
#####

# Define vector of package names

package_names <- c('haven','dplyr','reshape2','ggplot2')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023'
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'

### Loading Household Asset data 

stata_data_file <- '/RD06-99 ACDIS HSE-H All.dta'
ACDIS_hh <- haven::read_dta(paste0(data_dir,stata_data_file))

### Loading Bounded Structure Data to filter for only Southern PIPSA

stata_data_file <- '/RD01-03 ACDIS BoundedStructures.dta'
ACDIS_BS <- haven::read_dta(paste0(data_dir,stata_data_file))

BS_PIP <- ACDIS_BS[c('BSIntId','PIPSA')]
ACDIS_hh_PIP <- merge(ACDIS_hh,BS_PIP,by='BSIntId',all.x=TRUE)

ACDIS_hh_SPIP = ACDIS_hh_PIP[(ACDIS_hh_PIP$PIPSA %in% c(1)), ]

### Filter to drop rows in round 14 with visit date 2005-02-01 - misassigned dates 

ACDIS_hh_SPIP_sub <- ACDIS_hh_SPIP[!(ACDIS_hh_SPIP$DSRound==14 & 
                                       ACDIS_hh_SPIP$VisitDate=="2005-02-01"), ]


### Count records per Round

# HH_round_counts <- table(ACDIS_hh$DSRound)
# print.table(HH_round_counts )

#### Summarise ACDIS DS_Round data 


rounds_hh = ACDIS_hh_SPIP_sub %>% 
  group_by(DSRound) %>% 
  dplyr::summarise(earliest_visit_date =min(VisitDate), 
                   latest_visit_date = max(VisitDate), 
                   visit_count = n(),
                   m_year =mean.Date(as.Date(VisitDate), 
                                     format=c("%Y-%m-%d")), 
                   Round_Year = substr(m_year, 1,4))

### Drop data from before 2004

rounds_hh = rounds_hh[!(rounds_hh$Round_Year %in% c('2000','2001','2002','2003')), ]

### Order by Round_Year and Visit_Date
rounds_hh <- rounds_hh %>% dplyr::arrange(Round_Year,earliest_visit_date)

### Create a count variable for each unique round 
rounds_hh_count <- rounds_hh %>%
  dplyr::group_by(Round_Year) %>%
  dplyr::mutate(icount=row_number())

rounds_hh_count$Round_Year_count <- paste0(rounds_hh_count$Round_Year,'-',as.character(rounds_hh_count$icount))


### Summarise revised round_year data 

round_hh_summ <- rounds_hh_count[c('DSRound','Round_Year_count','earliest_visit_date','latest_visit_date','visit_count')]



#### Summary data ranges for each Round Year 
round_year_date_range = rounds_hh %>% 
  group_by(Round_Year) %>% 
  dplyr::summarise(earliest_visit_date =min(earliest_visit_date), 
                   latest_visit_date = max(latest_visit_date), 
                   visit_count = sum(visit_count))

### Output as a csv file 
output_fname <- '/hh_round_summary.csv'
write.csv(round_year_date_range,paste0(output_dir,output_fname),row.names=FALSE)


rounds_asset <- round_year_date_range
### Save file for later plotting
save(rounds_asset, file = paste0(output_dir,'/hh_rounds.RData'))




#### Now looking at number of unique household visits in each Round-Year


### Link to Census Round
nrounds = rounds_hh[, c(1,6)]
ACDIS_Round_Year <- merge(ACDIS_hh_SPIP, nrounds, by = "DSRound")

round_year_hh <- unique(ACDIS_Round_Year[c('Round_Year','HHIntId')])

round_year_hh_count <- ACDIS_Round_Year %>% 
  group_by(Round_Year) %>% 
  dplyr::summarise(visit_count = n())

### Output as a csv file 
output_fname <- '/round_year_hh_count.csv'

write.csv(round_year_hh_count,paste0(output_dir,output_fname),row.names=FALSE)



