# Clear any existing data from the data set
rm(list = ls())


# Define vector of package names

package_names <- c('haven','dplyr','ggplot2','ggthemes','zoo','stringr','survival',
                   'ggsurvfit','survivalAnalysis','NCmisc','devtools')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

# Set file paths
## AHRI data
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'
## Local copy of AHRI R code
code_dir <- 'C:/github/ahri_inc/avdm_ahri_code/R/'

## This is a download of the files from Alain Vandermael's AHRI R library 
## The files with suffix 'PM' have been updated to reflect changes in data file names 
## or R packages . 

### list of R code to be sourced

file_list <- c("ahri.R","data.R","getARTData.R","getBSData.R","getEpisodes-PM.R",
               "getFiles.R","getHealthData.R","getHIV-PM.R","getIncidence-PM.R","imputeMethods.R",
               "intCens.R","setArgs.R","setData-PM.R","splitData-PM.R","test_ahri.R")

## Source the file in the list
for(i in 1:length(file_list)){
  source(paste0(code_dir,file_list[i]))
}

### Set parameters for setting Args values 



sim_num = 1 # Multiple imputation simulations 
age_min = 15 # minimum age for incidence calculation
age_max = 100 # maximum age for incidence calculation
gender = "all" # Include Males (male) Females (female) or both (all)
start_year = 2004
end_year = 2023


### Current default AHRI filenames 
hiv_fname="RD05-99 ACDIS HIV All.dta"
wgh_fname="RD03-99 ACDIS WGH ALL.dta"
mgh_fname="RD04-99 ACDIS MGH ALL.dta" 
bsi_fname="RD01-03 ACDIS BoundedStructures.dta"
epi_fname="SurveillanceEpisodesHIV.dta"


getFiles <- setFiles(folder=data_dir,
                     hivfile=hiv_fname,
                     epifile=epi_fname,
                     wghfile=wgh_fname, 
                     mghfile=mgh_fname, 
                     bsifile=bsi_fname)
getFiles()[1:5]

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

### Load HIV surveillance data 
### All data is in Southern PIPSA area 

hiv <- setHIV(Args)

### Generate Seroconversion date
### This step estimates the seroconversion date between the last negative HIV test 
### and the first positive HIV test.
### Different methods are possible for this

### Method 1 
# make one imputed dataset
rtdat <- getRTData(hiv)
mdat <- MIdata(rtdat, Args)


sero_data_imput.df <- mdat[[1]]

### Earliest start date
sero_data_imput.df <- sero_data_imput.df %>%
  group_by(IIntID) %>%
  dplyr::mutate(first_start_date = (min(obs_start)))

### Latest observation date 
sero_data_imput.df <- sero_data_imput.df %>%
  group_by(IIntID) %>%
  dplyr::mutate(last_end_date = (max(obs_end)))

### Final status
sero_data_imput.df <- sero_data_imput.df %>%
  group_by(IIntID) %>%
  dplyr::mutate(final_sero_status = (max(sero_event)))

# right censored the data at the latest HIV-negative date (if uninfected) or at the imputed seroconversion  date (if infected)

sero_data_imput.df$censor_date <- NA

sero_data_imput.df$censor_date <- ifelse(sero_data_imput.df$final_sero_status == 0, sero_data_imput.df$late_neg,
                        ifelse(sero_data_imput.df$final_sero_status == 1, sero_data_imput.df$sero_date, "No"))

sero_data_imput.df$censor_date <- as.Date(as.numeric(sero_data_imput.df$censor_date), origin = "1970-01-01" )



### Read saved RDS file with SES quantiles
R_fname_SES <- paste0(data_dir,"/Surv_SES_Data.RDS")
Vis_SES <- readRDS(R_fname_SES)

### Merge SES data 
sero_data_imput_ses.df  <- merge(sero_data_imput.df ,Vis_SES,by.x=c('IIntID','Year'),by.y=c('IIntId','Mid_Year'),all.x=TRUE)

# ### Count number of NA values in imputed asset data by year 
summary_dat <- sero_data_imput_ses.df %>% group_by(Year) %>% summarise(NA_sum = sum(is.na(wealth_quant.imp1)),n_ind = n())
# ### Percentage NA values by year 
summary_dat$percent_NA <- summary_dat$NA_sum/summary_dat$n_ind*100
# 
print(n=25,summary_dat)

### Impute missing wealth quantile data for individuals with at least one recorded wealth quantile


### locf - where data missing carry over last observation but only in cases 
### where there is any data for the Individual
#

sero_data_imput_ses.df     <- sero_data_imput_ses.df %>% 
                              group_by(IIntID) %>%
                              arrange(Year) %>%
                              mutate(wealth_quant.imp2 = if(all(is.na(wealth_quant.imp1))) NA 
                                                       else imputeTS::na_locf(wealth_quant.imp1))
sero_data_imput_ses.df <- ungroup(sero_data_imput_ses.df)

### Count number of individuals with missing data for wealth_quant.imp2 

tmp_miss_ses  <-    sero_data_imput_ses.df %>%
                    filter(is.na(wealth_quant.imp2)) 

n_miss_ses <- NROW(unique(tmp_miss_ses[c('IIntID')]))

n_all_ind <- NROW(unique(sero_data_imput_ses.df[c('IIntID')]))

percent_miss <- n_miss_ses/n_all_ind*100

print(paste0("Individuals with no imputed SES - n/N(%) = ",
             as.character(n_miss_ses),
             "/",
             as.character(n_all_ind),
             "(",
             as.character(percent_miss),
             ")"))


## Total individuals 
tmp_miss_ses <- unique(sero_data_imput_ses.df[c('IIntID')])

### If no recorded Household ID in a particular year place individual in the last recorded household

sero_data_imput_ses.df     <- sero_data_imput_ses.df %>% 
  group_by(IIntID) %>%
  arrange(Year) %>%
  mutate(HouseholdId_imp = if(all(is.na(HouseholdId))) NA 
         else imputeTS::na_locf(HouseholdId))
sero_data_imput_ses.df <- ungroup(sero_data_imput_ses.df)

### Generate categorical age group variable
### age_cat 15-25,25-40,40-65, >65
        
sero_data_imput_ses.df$age_cat <-  ifelse((sero_data_imput_ses.df$Age >= 15 & sero_data_imput_ses.df$Age < 25), "15-24",
                                   ifelse((sero_data_imput_ses.df$Age >= 25 & sero_data_imput_ses.df$Age < 40), "25-39",
                                   ifelse((sero_data_imput_ses.df$Age >= 40 & sero_data_imput_ses.df$Age < 65), "40-64",
                                   ifelse((sero_data_imput_ses.df$Age >= 65 ), ">65",F))))


 

sero_data_imput_ses.df$age_cat  <- factor(sero_data_imput_ses.df$age_cat , levels = c("15-24", "25-39" , "40-64",   ">65"),
              labels = c("15-24", "25-39" , "40-64",   ">65"))

### Generate cohort of interest 
### Include all individuals HIV negative and under surveillance at the start date 
### Follow until sero-conversion , last known HIV negative test or loss to follow-up 

### SES as a factor with  labels 

sero_data_imput_ses.df$SES_1_char <- as.character(sero_data_imput_ses.df$wealth_quantile)
sero_data_imput_ses.df$SES_1 <- as.factor(sero_data_imput_ses.df$SES_1_char)

sero_data_imput_ses.df$SES_2_char <- as.character(sero_data_imput_ses.df$wealth_quant.imp1)
sero_data_imput_ses.df$SES_2 <- as.factor(sero_data_imput_ses.df$SES_2_char)

sero_data_imput_ses.df$SES_3_char <- as.character(sero_data_imput_ses.df$wealth_quant.imp2)
sero_data_imput_ses.df$SES_3 <- as.factor(sero_data_imput_ses.df$SES_3_char)

### Sex as a factor 
sero_data_imput_ses.df$sex <- as.factor(sero_data_imput_ses.df$Female)


R_fname_survdat <- paste0(data_dir,"/Survdata.RDS")
### Saving as RDS file
saveRDS(sero_data_imput_ses.df  , file = R_fname_survdat)

