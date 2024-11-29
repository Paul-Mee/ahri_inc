####
#### This code loads the HIV data - generates the Round_Year variable 
#### and prepares the data for later Time to event analysis
####


####
# Clear any existing data from the data set
rm(list = ls())
#####

# Define vector of package names

package_names <- c('haven','dplyr','survival')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023'
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'
code_dir <- 'C:/github/ahri_inc/avdm_ahri_code/R/'

### Set parameters for setting Args values 

sim_num = 1 # Multiple imputation simulations 
age_min = 15 # minimum age for incidence calculation
age_max = 100 # maximum age for incidence calculation
gender = "all" # Include Males (male) Females (female) or both (all)
start_year = 2004
end_year = 2023

## This is a download of the files from Alain Vandermael's AHRI R library 
## The files with suffix 'PM' have been updated to reflect changes in data file names 
## or R packages . 

### list of R code to be sourced

file_list <- c("ahri.R","data.R","getARTData.R","getBSData-PM.R","getEpisodes-PM.R",
               "getFiles.R","getHealthData.R","getHIV-PM.R","getIncidence-PM.R","imputeMethods.R",
               "intCens.R","setArgs.R","setData-PM.R","splitData-PM.R","test_ahri.R")

## Source the file in the list
for(i in 1:length(file_list)){
  source(paste0(code_dir,file_list[i]))
}

getFiles = setFiles("C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023")
setFiles(
  folder = "C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023",
  hivfile = "RD05-99 ACDIS HIV All.dta",
)


#Set Args 
if(gender=="all") {
  Args <- setArgs(Years=c(start_year:end_year), 
                  Age=list(All=c(age_min,age_max)),
                  imputeMethod=imputeRandomPoint, nSim=sim_num)
} else if(gender=="male") {
  Args <- setArgs(Years=c(start_year:end_year), 
                  Age=list(Mal=c(age_min,age_max)),
                  imputeMethod=imputeRandomPoint, nSim=sim_num)
} else if(gender=="female") {
  Args <- setArgs(Years=c(start_year:end_year), 
                  Age=list(Fem=c(age_min,age_max)),
                  imputeMethod=imputeRandomPoint, nSim=sim_num)
} else {
  print("Gender not specified - using All") 
  Args <- setArgs(Years=c(start_year:end_year), 
                  Age=list(All=c(age_min,age_max)),
                  imputeMethod=imputeRandomPoint, nSim=sim_num)
}      


hiv = readHIVData(dropTasP = TRUE, addVars = "DSRound")

epi <- readEpisodes()
hiv <- setHIV(Args)

### Generate Seroconversion date
### This step estimates the seroconversion date between the last negative HIV test 
### and the first positive HIV test.

# Make one imputed dataset
rtdat <- getRTData(hiv)
mdat <- MIdata(rtdat, Args)

## Using first imputed dataset
hiv_imput.df <- mdat[[1]]

### Earliest start date
hiv_imput.df <- hiv_imput.df %>%
  group_by(IIntID) %>%
  dplyr::mutate(first_start_date = (min(obs_start)))

### Latest observation date 
hiv_imput.df <- hiv_imput.df %>%
  group_by(IIntID) %>%
  dplyr::mutate(last_end_date = (max(obs_end)))

### Final status
hiv_imput.df <- hiv_imput.df %>%
  group_by(IIntID) %>%
  dplyr::mutate(final_sero_status = (max(sero_event)))

hiv_imput.df <- ungroup(hiv_imput.df)

### Generate mid point of each episode

hiv_imput.df <-  hiv_imput.df %>% 
  mutate(
    obs_start=as.Date(obs_start),
    obs_end=as.Date(obs_end)) %>% 
    rowwise %>%
    mutate(obs_mean_date=mean.Date(c(obs_start,obs_end))) %>% data.frame()


                                        

#### Need to assign a DSRound to each episode
#### Get date ranges for each DSRound
#### This code is based on Elphas' review of data

rounds = hiv %>%
  group_by(DSRound) %>%
  dplyr::summarise(earliest_visit_date =min(VisitDate),
                   latest_visit_date = max(VisitDate),
                   m_year =mean.Date(as.Date(VisitDate),
                                     format=c("%Y-%m-%d")),
                   Round_Year = substr(m_year, 1,4),
                   hiv_visit_count = n(),)




### These steps combine 2003 and 2004 data and use the mid year for the original 2004 data
rounds$mid_year = rounds$m_year
rounds$mid_year[rounds$Round_Year == 2004]<-mean.Date(rounds$m_year[rounds$Round_Year==2004])

rounds$Round_Year = substr(rounds$mid_year, 1,4)

rounds$final_mid_year = rounds$mid_year
rounds$final_mid_year[rounds$Round_Year == 2003] <-unique(rounds$mid_year[rounds$Round_Year == 2004])

##change 2003  to 2004 
rounds$Round_Year = substr(rounds$final_mid_year, 1,4)

#### Generate - earliest and latest visit dates based on Round_Years

round_years = rounds %>%
  group_by(Round_Year) %>%
  dplyr::summarise(earliest_visit_date =min(earliest_visit_date),
                   latest_visit_date = max(latest_visit_date)) %>%
                    rowwise %>%
                    mutate(rd_year_mean_date=mean.Date(c(earliest_visit_date,latest_visit_date))) %>% data.frame()
  

rounds_sub <- round_years[c('Round_Year','rd_year_mean_date')]

###
### Assign Round_Year to HIV episode data 
### Assign to Round year with mid point closest to mid point of episode
###

#### Cross join hiv_imput.df to rounds_sub and calculate the difference in days

hiv_imput_cross <- dplyr::cross_join(hiv_imput.df,rounds_sub)
hiv_imput_cross$date_diff <- abs(as.integer(hiv_imput_cross$obs_mean_date - hiv_imput_cross$rd_year_mean_date))

## Ranking by IIntID , obs_start, date_diff
hiv_imput_cross_rank <- hiv_imput_cross %>%
  group_by(IIntID , obs_start) %>%
  dplyr::mutate(rank = order(order(date_diff, decreasing=FALSE)))

hiv_imput_cross_rank  <- ungroup(hiv_imput_cross_rank)


### Select closest visit
hiv_imput_cross_rank <- hiv_imput_cross_rank %>% filter(rank==1)
hiv_imput_full <- hiv_imput_cross_rank 

### Drop columns not needed

hiv_imput_full <- subset(hiv_imput_full, select = -c(obs_mean_date,rd_year_mean_date,date_diff,rank))

#### save HIV data file 
save(hiv_imput_full, file = paste0(output_dir,'/ACDIS_hiv_full.RData'))
