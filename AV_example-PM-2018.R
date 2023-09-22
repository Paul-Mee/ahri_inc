# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# The first thing to do is set the file paths to the AHRI datasets. 


data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'
#code_dir <- 'C:/github/avdm_code_download/R/'


# Define vector of package names

package_names <- c('haven','dplyr','ggplot2','ggthemes','ahri')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

#### Need to first run AHRI_SES.R and create a set of stata file with SES data for each quantile in each year 
#### Loop through number of quantiles to calcuate incidence for each


### Load filenames 

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


### Set parameters for setting Args

start_year = 2005
end_year = 2018
#n_fact = 3 # Number of quantiles in SES
sim_num = 3 # Multiple imputation simulations 
age_min = 15 # minimum age for incidence calculation
age_max = 54 # maximum age for incidence calculation
gender = "male" # Include Males (male) Females (female) or both (all)



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

dat <- setHIV(Args)

# make multiple imputed datasets, run model on each
rtdat <- getRTData(dat)
mdat <- MIdata(rtdat, Args)

# Load data giving SES for each year of HIV data 
R_fname_SES <- paste0(data_dir,"/Yr_SES_Data.RDS")
Year_SES <- readRDS(R_fname_SES)

#Year_SES$wealth_quantile <- as.factor(Year_SES$wealth_quantile)

### Merge each dataframe in mdat with actual SES for that year 
for (i in 1:sim_num) {
print(i)
mdat[[i]]  <- merge(mdat[[i]] ,Year_SES,by=c('IIntID','Year'))
}

temp.df <- mdat[[1]]

sformula = "sero_event ~ -1 + as.factor(Year) + as.factor(wealth_quantile) + as.factor(Year):as.factor(wealth_quantile) + offset(log(tscale))"
  

mdat <- mitools::imputationList(mdat)
mods <- with(mdat, stats::glm(as.formula(sformula), family=poisson))

# used in predict step to estimate by year
newdata <- group_by(temp.df, Year) %>% summarize(wealth_quantile = mean(wealth_quantile)) %>%
  mutate(Year = factor(Year), tscale = 1)
pois_inc <- MIpredict(mods, newdata)
