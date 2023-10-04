# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# Define vector of package names

package_names <- c('haven','dplyr','ggplot2','ggthemes','zoo')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'

stata_data_file <- "/RD05-99 ACDIS HIV All.dta"
ACDIS_hiv_all <- haven::read_dta(paste0(data_dir,stata_data_file))

hiv_dates <- ACDIS_hiv_all  %>% 
  group_by (DSRound) %>% 
  summarize( min_date=min(VisitDate),max_date=max(VisitDate),mid_date=min(VisitDate)+(max(VisitDate) - min(VisitDate))/2 )

write.csv2(hiv_dates,paste0(data_dir,"/round_dates.csv"))