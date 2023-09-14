# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# The first thing to do is set the file paths to the AHRI datasets. 


data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'
code_dir <- 'C:/github/avdm_code_download/R/'

## Running R files 

source(paste0(code_dir,"ahri.R"))
source(paste0(code_dir,"data.R"))
source(paste0(code_dir,"getARTData.R"))
source(paste0(code_dir,"getBSData.R"))
source(paste0(code_dir,"getEpisodes-PM.R"))
source(paste0(code_dir,"getFiles.R"))
source(paste0(code_dir,"getHealthData.R"))
source(paste0(code_dir,"getHIV.R"))
source(paste0(code_dir,"getIncidence.R"))
source(paste0(code_dir,"imputeMethods.R"))
source(paste0(code_dir,"intCens.R"))
source(paste0(code_dir,"setArgs.R"))
source(paste0(code_dir,"setData.R"))
source(paste0(code_dir,"splitData-PM.R"))
source(paste0(code_dir,"test_ahri.R"))


# Define vector of package names

package_names <- c('haven','dplyr')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)


### Load filenames 

getFiles <- setFiles(folder=data_dir,
                     hivfile="RD05-99 ACDIS HIV All.dta",
                     epifile="q3_SES_Data.dta",
                     #epifile="SurveillanceEpisodesHIV.dta",
                     wghfile="RD03-99 ACDIS WGH ALL.dta", 
                     mghfile="RD04-99 ACDIS MGH ALL.dta", 
                     bsifile="RD01-03 ACDIS BoundedStructures.dta")
getFiles()[1:5]


### Alter Get_Episodes to select each asset quintile in turn
### Calculate incidence for each year for each quintile
### Graph the data 

readEpisodes()
getBirthDate()

# Args <- setArgs(Years = c(2020:2021),
#                 Age = list(Mal = c(15, 54)), nSim = 3)

#Read the HIV data and set the Years of interest, and set the age groups
Args <- setArgs(Years=c(2004:2023), 
                Age=list(All=c(15, 49)),
                imputeMethod=imputeRandomPoint, nSim=10)

hiv <- getHIV()

#### Check Year is where Round info is set 

rtdat <- getRTData(hiv)
# Note could fix getRTData to suppress warning messages

idat <- getIncData(rtdat, bdat=getBirthDate(), Args)


levels(idat$AgeCat)
idat <- dplyr::mutate(idat, Year = as.factor(Year))

sformula = "sero_event ~ -1 + as.factor(Year) + Age + as.factor(Year):Age + offset(log(tscale))"
mod <- stats::glm(as.formula(sformula), data=idat, family=poisson)


age_dat <- getAgeYear(dat=setHIV(Args))

#Calculate the age-adjusted HIV incidence rates from 10 imputed datasets
SES_inc_3 <- MIpredict(mod, newdata=age_dat)

## Output data
R_fname <- paste0(data_dir,"/SEP_Inc3.RDS")
csv_fname <- paste0(data_dir,"/SEP_Inc3.csv")
### Saving as RDS file
saveRDS(SES_inc_3, file = R_fname) 
write.csv(SES_inc_3, csv_fname, row.names=TRUE)

csv_fname <- paste0(data_dir,"/SEP_Inc2.csv")
write.csv(SES_inc_2, csv_fname, row.names=TRUE)

csv_fname <- paste0(data_dir,"/SEP_Inc1.csv")
write.csv(SES_inc_1, csv_fname, row.names=TRUE)





