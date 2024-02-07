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
## Using first imputed dataset
sero_data_imput.df <- mdat[[1]]

## Check using random date between late_neg and early positive
#### Get a unique list of seroconverters
sero_con.df <- sero_data_imput.df  %>% filter(!is.na(early_pos))
sero_con.df <- unique(sero_con.df[,c('IIntID','late_neg','early_pos' )])
#### Days between late neg and ealry pos
sero_con.df$sero_days <- difftime(as.POSIXct(sero_con.df$early_pos), as.POSIXct(sero_con.df$late_neg), units="days")
### Random number between 0 and 1 for each row
sero_con.df$randoms <- runif(nrow(sero_con.df), min = 0, max = 1)
sero_con.df$days_add <- round(sero_con.df$sero_days*sero_con.df$randoms)  
sero_con.df$rand_sero_date <- sero_con.df$late_neg + sero_con.df$days_add
sero_con.df <- unique(sero_con.df[,c('IIntID','rand_sero_date' )])
### Merge to original dataframe
sero_data_imput.df <- merge(sero_data_imput.df,sero_con.df,by='IIntID',all.X=TRUE)


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

sero_data_imput.df <- ungroup(sero_data_imput.df)

# right censor the data at the latest HIV-negative date (if uninfected) or at the imputed seroconversion  date (if infected)

sero_data_imput.df$censor_date <- NA

sero_data_imput.df$censor_date <- ifelse(sero_data_imput.df$final_sero_status == 0, sero_data_imput.df$late_neg,
                        ifelse(sero_data_imput.df$final_sero_status == 1, sero_data_imput.df$sero_date, "No"))

sero_data_imput.df$censor_date <- as.Date(as.numeric(sero_data_imput.df$censor_date), origin = "1970-01-01" )



### Read saved RDS file with SES quantiles
R_fname_SES_edu <- paste0(data_dir,"/Ind_Edu_SES_BS_year.RDS") ### SES quantiles calculated within each year 
#R_fname_SES_edu <- paste0(data_dir,"/Ind_Edu_SES_BS_combined.RDS") ### SES quantiles calculated across all years
Vis_SES <- readRDS(R_fname_SES_edu)

### Merge SES data 
sero_data_imput_ses.df  <- merge(sero_data_imput.df ,Vis_SES,by.x=c('IIntID','Year'),by.y=c('IIntId','Mid_Year'),all.x=TRUE)

# # ### Count number of NA values in imputed asset data by year 
# summary_dat <- sero_data_imput_ses.df %>% group_by(Year) %>% summarise(NA_sum = sum(is.na(wealth_quant.imp1)),n_ind = n())
# # ### Percentage NA values by year 
# summary_dat$percent_NA <- summary_dat$NA_sum/summary_dat$n_ind*100
# # 
# print(n=25,summary_dat)

### Impute covariate data for missing episodes

#### Remove individuals where all SES values are NA
sero_data_imput_ses.df <- sero_data_imput_ses.df %>%
  group_by(IIntID) %>%
  filter( sum(!is.na(wealth_quant_pca.imp1)) > 0) %>%
  ungroup

#### Remove individuals where all Education values are NA
sero_data_imput_ses.df <- sero_data_imput_ses.df %>%
  group_by(IIntID) %>%
  filter( sum(!is.na(highest_edu_fact)) > 0) %>%
  ungroup

### Numeric code for education levels 
sero_data_imput_ses.df$highest_edu_num <- as.numeric(sero_data_imput_ses.df$highest_edu_fact)
### Numeric code for PIPSA
sero_data_imput_ses.df$pipsa_num <- as.numeric(sero_data_imput_ses.df$pipsa_fact)
### Numeric code for Urban - Rural 
sero_data_imput_ses.df$urban_rural_num <- as.numeric(sero_data_imput_ses.df$urban_rural_fact)


sero_data_imput_ses.df  <- sero_data_imput_ses.df %>%
  group_by(IIntID) %>%
  arrange(Year) %>%
  mutate(wealth_quant_pca.imp2 = imputeTS::na_locf(wealth_quant_pca.imp1)) %>%
  mutate(wealth_quant_fa.imp2 = imputeTS::na_locf(wealth_quant_fa.imp1)) %>%
  mutate(highest_edu_num.imp = imputeTS::na_locf(highest_edu_num)) %>% 
  mutate(urban_rural_num.imp = imputeTS::na_locf(urban_rural_num)) 

sero_data_imput_ses.df <- ungroup(sero_data_imput_ses.df)  
  
### Convert education and urban - rural back to Factors

sero_data_imput_ses.df$highest_edu_imp_fact <- NA

sero_data_imput_ses.df$highest_edu_imp_fact[sero_data_imput_ses.df$highest_edu_num.imp ==1] <- "None"
sero_data_imput_ses.df$highest_edu_imp_fact[sero_data_imput_ses.df$highest_edu_num.imp ==2] <- "Lower Primary"
sero_data_imput_ses.df$highest_edu_imp_fact[sero_data_imput_ses.df$highest_edu_num.imp ==3] <- "Higher Primary"
sero_data_imput_ses.df$highest_edu_imp_fact[sero_data_imput_ses.df$highest_edu_num.imp ==4] <- "Lower Secondary"
sero_data_imput_ses.df$highest_edu_imp_fact[sero_data_imput_ses.df$highest_edu_num.imp ==5] <- "Higher Secondary"
sero_data_imput_ses.df$highest_edu_imp_fact[sero_data_imput_ses.df$highest_edu_num.imp ==6] <- "Tertiary"

sero_data_imput_ses.df$highest_edu_imp_fact <- factor(sero_data_imput_ses.df$highest_edu_imp_fact,
                                     levels = c("None","Lower Primary","Higher Primary",
                                                "Lower Secondary","Higher Secondary","Tertiary"))

sero_data_imput_ses.df$urban_rural_imp_fact <- NA

sero_data_imput_ses.df$urban_rural_imp_fact[sero_data_imput_ses.df$urban_rural_num.imp == 1 ] <- "Rural"
sero_data_imput_ses.df$urban_rural_imp_fact[sero_data_imput_ses.df$urban_rural_num.imp == 2 ] <- "Urban"
sero_data_imput_ses.df$urban_rural_imp_fact[sero_data_imput_ses.df$urban_rural_num.imp == 3 ]  <- "Peri-Urban"

sero_data_imput_ses.df$urban_rural_imp_fact <- factor(sero_data_imput_ses.df$urban_rural_imp_fact,
                                         levels = c("Rural","Urban","Peri-Urban"))

### Count number of individuals with missing data for wealth_quant.imp2 

# tmp_miss_ses  <-    sero_data_imput_ses.df %>%
#                     filter(is.na(wealth_quant.imp2)) 
# 
# n_miss_ses <- NROW(unique(tmp_miss_ses[c('IIntID')]))
# 
# n_all_ind <- NROW(unique(sero_data_imput_ses.df[c('IIntID')]))
# 
# percent_miss <- n_miss_ses/n_all_ind*100
# 
# print(paste0("Individuals with no imputed SES - n/N(%) = ",
#              as.character(n_miss_ses),
#              "/",
#              as.character(n_all_ind),
#              "(",
#              as.character(percent_miss),
#              ")"))
# 
# 
# ## Total individuals 
# tmp_miss_ses <- unique(sero_data_imput_ses.df[c('IIntID')])
# 
# ### If no recorded Household ID in a particular year place individual in the last recorded household
# 
# sero_data_imput_ses.df     <- sero_data_imput_ses.df %>% 
#   group_by(IIntID) %>%
#   arrange(Year) %>%
#   mutate(HouseholdId_imp = if(all(is.na(HouseholdId))) NA 
#          else imputeTS::na_locf(HouseholdId))
# sero_data_imput_ses.df <- ungroup(sero_data_imput_ses.df)

### Generate categorical age group variable
### age_cat 15-25,25-40,40-65, >65
        
sero_data_imput_ses.df$age_cat <-  ifelse((sero_data_imput_ses.df$Age >= 15 & sero_data_imput_ses.df$Age < 25), "15-24",
                                   ifelse((sero_data_imput_ses.df$Age >= 25 & sero_data_imput_ses.df$Age < 40), "25-39",
                                   ifelse((sero_data_imput_ses.df$Age >= 40 & sero_data_imput_ses.df$Age < 65), "40-64",
                                   ifelse((sero_data_imput_ses.df$Age >= 65 ), ">65",F))))


 

sero_data_imput_ses.df$age_cat  <- factor(sero_data_imput_ses.df$age_cat , levels = c("15-24", "25-39" , "40-64",   ">65"),
              labels = c("15-24", "25-39" , "40-64",   ">65"))

### Sex as a factor 
sero_data_imput_ses.df$sex <- factor(sero_data_imput_ses.df$Female,  levels = c(0, 1),
                                        labels = c("Male", "Female"))

### Select variables of interest

sero_data_imput_ses.df <- sero_data_imput_ses.df[c('IIntID','Year','sex','age_cat','late_neg','early_pos','sero_event','sero_date','rand_sero_date',
                                                   'obs_start','obs_end','first_start_date','last_end_date','final_sero_status',
                                                   'censor_date','highest_edu_imp_fact','urban_rural_imp_fact',
                                                   'wealth_quant_fa.imp2','wealth_quant_pca.imp2')]
sero_data_imput_ses.df <- dplyr::rename(sero_data_imput_ses.df, Highest_Education = highest_edu_imp_fact )
sero_data_imput_ses.df <- dplyr::rename(sero_data_imput_ses.df, Urban_Rural = urban_rural_imp_fact)
sero_data_imput_ses.df <- dplyr::rename(sero_data_imput_ses.df, Wealth_Quant_FA = wealth_quant_fa.imp2)
sero_data_imput_ses.df <- dplyr::rename(sero_data_imput_ses.df, Wealth_Quant_PCA = wealth_quant_pca.imp2)

R_fname_survdat <- paste0(data_dir,"/Survdata.RDS")
### Saving as RDS file
saveRDS(sero_data_imput_ses.df  , file = R_fname_survdat)

