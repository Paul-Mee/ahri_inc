###
### This code collates Education data for individuals in the Southern PIPSA region of the AHRI site
### This version of the code uses Round-Years to aggregate the data
### take the year of the mean of the visit dates for each census round and then aggregate by this year

### 

# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
# Ensure only the SA CRAN repository is accessed 
options(repos = getOption("repos")["CRAN"])

# Set file paths
## AHRI data
#data_dir <- 'E:/PaulMee/HDSS'
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023'
#output_dir <- 'E:/PaulMee/Outputs'
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'

# Define vector of package names

package_names <- c('haven','dplyr')


# This code installs all the other required packages if they are not currently installed and loads 
# all the libraries

pacman::p_load(char=package_names)


#### Load data-set with a row for each year an individual was resident in a household
#### To create this file need to run "O_Household_Residence_Episodes.R" once

# epi_data_file <- "/House_Res_Episodes.RDS"
# ACDIS_epi_full <- readRDS(paste0(output_dir,epi_data_file))


# 
# ## NB this includes years in which no household asset data was collected - hence NA values below are not really missing data 
# 
# ### Merge with asset data 
# 
# ACDIS_epi_quant <- merge(ACDIS_epi_full,ass_ses_full_imp,by.x = (c("HouseholdId","Res_Year")),
#                          by.y= (c("HHIntId","Visit_Year")),all.x=TRUE)
# 
# # ### Count number of NA values by year 
# summary_dat <- ACDIS_epi_quant %>% group_by(Mid_Year) %>% summarise(NA_sum = sum(is.na(wealth_quantile)),n_ind = n())
# # ### Percentage NA values by year 
# summary_dat$percent_NA <- summary_dat$NA_sum/summary_dat$n_ind*100
# # 
# print(n=25,summary_dat)
# 
# 
# 

#### Keep Required variables 

# ACDIS_Ind_SES <- ACDIS_epi_quant[,c('Res_Year','IIntId','HouseholdId','pca_1d_score','wealth_quantile_pca',
#                                     'wealth_quant_pca.imp1','fa_score','wealth_quantile_fa','wealth_quant_fa.imp1',
#                                     'mca_score','wealth_quantile_mca','wealth_quant_mca.imp1')]


#### Loading Education data 
### Loading Individual level data for Education variables 

stata_data_file <- '/RD07-99 ACDIS HSE-I All.dta'
ACDIS_ind <- haven::read_dta(paste0(data_dir,stata_data_file))

### Extract Visit Year , Id, Highest School level 

ACDIS_edu <- ACDIS_ind[c('IIntId', 'VisitDate', 'HighestSchoolLevel','HighestTertiaryLevel')]
ACDIS_edu$Visit_Year <- lubridate::year(ACDIS_edu$VisitDate)

### If multiple Education records in a year select the one for the first visit and if two visits 
### on same day lowest education level
ACDIS_edu$HS_int <- as.numeric(ACDIS_edu$HighestSchoolLevel)

ACDIS_edu <- ACDIS_edu %>% dplyr::arrange(IIntId,Visit_Year,HS_int)

  ACDIS_edu <- ACDIS_edu %>% 
  dplyr::group_by(IIntId,Visit_Year) %>% 
  dplyr::mutate(icount=row_number())

ACDIS_edu <- ACDIS_edu %>% filter(icount==1)


### https://www.researchgate.net/publication/267391685_RACIAL_DIFFERENCES_IN_EDUCATIONAL_ATTAINMENT_IN_SOUTH_AFRICA

### Recode School (Highest School Level Variable)
### None = 1,2
### Lower primary (Grades 1,2,3,4) = 3,4,5,6,7 
### Higher Primary (Grades 5,6,7) = 8,9,10
### Lower Secondary (Grades 8,9,10) = 11,12,13
### Higher Secondary (Grades 11,12) = 14,15

### Recode Tertiary (Highest Tertiary Level )
### Tertiary = 16,17,18,19,20

ACDIS_edu$highest_edu <- NA

ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(1,2)] <- "None"
ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(3,4,5,6,7)] <- "Lower Primary"
ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(8,9,10)] <- "Higher Primary"
ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(11,12,13)] <- "Lower Secondary"
ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(14,15)] <- "Higher Secondary"
ACDIS_edu$highest_edu[ACDIS_edu$HighestTertiaryLevel %in% c(16,17,18,19,20)] <- "Tertiary"

#table(ACDIS_edu$highest_edu)

### What to do when highest education reported decreases over time ? 

ACDIS_edu$highest_edu_fact <- factor(ACDIS_edu$highest_edu,
                                     levels = c("None","Lower Primary","Higher Primary",
                                                "Lower Secondary","Higher Secondary","Tertiary"))


#Merge with SES data
ACDIS_edu <- ACDIS_edu[c('IIntId', 'Visit_Year', 'highest_edu_fact')]

ACDIS_Ind_SES_edu <- merge(ACDIS_Ind_SES,ACDIS_edu,by.x = c('IIntId','Res_Year'),by.y = c('IIntId','Visit_Year'),all.x = TRUE)

# Loading Bonded Structure Data

bsi_fname="/RD01-03 ACDIS BoundedStructures.dta"
ACDIS_bsi<- haven::read_dta(paste0(data_dir,bsi_fname))

# ggplot(ACDIS_bsi, aes(x=KmToNearestClinic)) +
#   geom_histogram()

### Keep required variables 


ACDIS_bsi_tmp <- ACDIS_bsi[c('BSIntId','Isigodi','IsUrbanOrRural','PIPSA','KmToNearestClinic')]

ACDIS_bsi_tmp$urban_rural  <- NA

ACDIS_bsi_tmp$urban_rural[ACDIS_bsi_tmp$IsUrbanOrRural %in% c(2)] <- "Peri-Urban"
ACDIS_bsi_tmp$urban_rural[ACDIS_bsi_tmp$IsUrbanOrRural %in% c(3)] <- "Rural"
ACDIS_bsi_tmp$urban_rural[ACDIS_bsi_tmp$IsUrbanOrRural %in% c(4)] <- "Urban"

ACDIS_bsi_tmp$urban_rural_fact <- factor(ACDIS_bsi_tmp$urban_rural,
                                         levels = c("Rural","Urban","Peri-Urban"))

ACDIS_bsi_tmp$pipsa  <- NA
ACDIS_bsi_tmp$pipsa[ACDIS_bsi_tmp$PIPSA %in% c(1)] <- "Southern"
ACDIS_bsi_tmp$pipsa[ACDIS_bsi_tmp$PIPSA %in% c(2)] <- "Northern"

ACDIS_bsi_tmp$pipsa_fact <- factor(ACDIS_bsi_tmp$pipsa,
                                   levels = c("Southern","Northern"))

ACDIS_bsi_tmp$km_clinic_cat <- NA
ACDIS_bsi_tmp$km_clinic_cat[(ACDIS_bsi_tmp$KmToNearestClinic >= 0 & ACDIS_bsi_tmp$KmToNearestClinic <= 2 )] <- "0-2"
ACDIS_bsi_tmp$km_clinic_cat[(ACDIS_bsi_tmp$KmToNearestClinic > 2 & ACDIS_bsi_tmp$KmToNearestClinic <= 4 )] <- ">2-4"
ACDIS_bsi_tmp$km_clinic_cat[(ACDIS_bsi_tmp$KmToNearestClinic > 4 & ACDIS_bsi_tmp$KmToNearestClinic <= 6 )] <- ">4-6"
ACDIS_bsi_tmp$km_clinic_cat[(ACDIS_bsi_tmp$KmToNearestClinic > 6 )] <- ">6"

ACDIS_bsi_tmp$km_clinic_fact <- factor(ACDIS_bsi_tmp$km_clinic_cat,
                                       levels = c("0-2",">2-4",">4-6",">6"))

# ggplot(ACDIS_bsi, aes(x=KmToNearestClinic)) +
#   geom_histogram(binwidth = 0.2)
# 
# ggplot(ACDIS_bsi_tmp, aes(x=km_clinic_fact)) +
#   geom_histogram(stat = "count")


ACDIS_bsi_tmp2 <- ACDIS_bsi_tmp[c('BSIntId','Isigodi','urban_rural_fact','pipsa_fact','km_clinic_fact')]

### Getting mapping between HHId and BSId and Visit Year as a household can move to a new BS Id over time 

HH_BS_Year <- unique(ACDIS_hh[c('HHIntId','BSIntId','Visit_Year')])

### Merge HH_BS with ACDIS_Ind_SES_edu
ACDIS_Ind_SES_edu_BS <- merge(ACDIS_Ind_SES_edu,HH_BS_Year,by.x=c('HouseholdId','Res_Year'),  by.y = c('HHIntId','Visit_Year') )

### Merge with ACDIS_bsi_tmp2

ACDIS_Ind_SES_edu_BS_full <- merge(ACDIS_Ind_SES_edu_BS,ACDIS_bsi_tmp2,by=c('BSIntId') )


### Save as RDS file 

R_fname_edu_bs <- paste0(output_dir,"/Ind_Edu_SES_BS_year.RDS")
### Saving as RDS file
saveRDS(ACDIS_Ind_SES_edu_BS_full  , file = R_fname_edu_bs)