###
### This code takes the AHRI household data and calculates SES rankings for households and 
### individuals in those households 
### In this version SES quantiles are calculated within each year 
### Variables are only included which have been consistently collected over time
### in order to allow comparison of changes over time
### Interpolation is used to substitute for missing data 
### Three methods are use to generate the summary Wealth Index , Factor Analysis, Multiple Correspondence Analysis
### and Principal Components  Analysis 
### This version of the code uses Round-Years to aggrgate the data
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

package_names <- c('haven','dplyr','survival','psych','lubridate','schoRsch','data.table',
                   'imputeTS','DescTools','PerformanceAnalytics','qgraph','corrplot',
                   'ggplot2','Compind','lmreg','factoextra','FactoMineR','knitr')


# This code installs all the other required packages if they are not currently installed and loads 
# all the libraries

pacman::p_load(char=package_names)

#### Utility to generate bibtek files to reference R packages
knitr::write_bib(c("FactoMineR", "factoextra"), file = paste0(output_dir,"/packages.bib"))



### Define Function - Cat_Bin - Converts a categorical variable to a series of binary variables each 
### representing a level of the categorical variable as a yes/no
### Input variables - Df name , index_vars (vector of the index column names) , 
### categorical value column name , max non-missing value 


# df = 'ACDIS_hh'
# index_vars = c('HHIntId','Visit_Year')
# col_name = 'DrinkWaterSource'
# max_val=16 

cat_bin <- function(df,index_vars, col_name,max_val){
  ## Header 
  head_cols <- append(index_vars,col_name)
  df <- get(df)
  head.df <- df[head_cols]
  ## Create numeric vector of column
  var_vec <- as.numeric(unlist(df[col_name]))
  ## substitute "9999" for all values over the maximum
  var_vec <- replace(var_vec, var_vec > max_val, 9999)
  ## Create a set of binary variables (1 or 0) for each level of categorical variable names Vn to Vm
  bins <- as.data.frame(binaries(var_vec))
  ## convert column names to numeric 
  names(bins) <- sub(pattern = "^v.","", colnames(bins))
  ## Sort in numerical order
  bins <- bins[, order(as.numeric(names(bins)))]
  ## Append original variable name 
  sub_name=paste0("mv_",col_name,".")
  names(bins) <- sub(pattern = "^",sub_name, colnames(bins))
  ## Drop columns representing unknown values 
  bins <- bins %>% dplyr::select(-contains("9999"))
  new_df <- cbind(head.df,bins)
  new_df
}


# Define Parameters 

n_quant = 3 # Number of quantiles for SES indices 

### Loading Household Asset data 

stata_data_file <- '/RD06-99 ACDIS HSE-H All.dta'
ACDIS_hh <- haven::read_dta(paste0(data_dir,stata_data_file))

### Filter to drop rows in round 14 with visit date 2005-02-01 - misassigned dates 
ACDIS_hh <- ACDIS_hh[!(ACDIS_hh$DSRound==14 & ACDIS_hh$VisitDate=="2005-02-01"), ]

ACDIS_hh$Visit_Year <- lubridate::year(ACDIS_hh$VisitDate)

### Move Visit data column
ACDIS_hh  <- ACDIS_hh  %>% relocate(Visit_Year, .after=VisitDate)

### AC_1.df used for MCA calculations
AC_1.df <- ACDIS_hh


### Recoding and normalising variables 
### For each variable recode such that 1 represents the most wealthy and 0 least wealthy 
### these definitions are somewhat subjective 

### Water Source 

ACDIS_hh$water <- NA

## Update codes based on wealth ranking for water sources 

ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 9 ] <- 1  # Borehole
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 1 ] <- 2  # Piped Internal
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 3 ] <- 3  # Piped Yard Tap
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 5 ] <- 4  # Piped Public tap (paid)
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 7 ] <- 4  # Piped Public tap (free)
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 15 ] <- 4  # Neighbour's tab
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 16 ] <- 4  # Borehole outside yard
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 8 ] <- 5  # Water carrier tanker
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 4 ] <- 6  # Well non-borehole
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 6 ] <- 6  # Protected Spring
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 11 ] <- 6  # Rainwater
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 13 ] <- 7  # River flowing stream
ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 2 ] <- 8  # Dam / Stagnant water

# check data 
table(ACDIS_hh$Visit_Year,ACDIS_hh$water,useNA = "ifany")

### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$water_norm <- (max(ACDIS_hh$water,na.rm = TRUE) - ACDIS_hh$water)/(max(ACDIS_hh$water,na.rm = TRUE) - min(ACDIS_hh$water,na.rm = TRUE))

### Create binary variables for MCA
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Visit_Year'), 
                  col_name = 'DrinkWaterSource',
                  max_val=16 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Visit_Year')))


### Toilet Types

ACDIS_hh$toilet <- NA

## Update codes based on wealth ranking for toilets 

ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 1 ] <- 1  # Flush Toilet
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 10 ] <- 1  # Flush Toilet
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 11] <- 1  # Flush Toilet
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 12] <- 2  # Pour Flush Toilet
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 3 ] <- 2  # VIP
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 5 ] <- 3  # Other Pit Latrine
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 9] <- 4  # Chemical Toilet
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 7] <- 5  # Bucket Toilet
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 2] <- 5  # Other Toilet
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 8] <- 6  # None
ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 14] <- 6  # Open Defecation 

# check data 
table(ACDIS_hh$Visit_Year,ACDIS_hh$toilet,useNA = "ifany")


### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$toilet_norm <- (max(ACDIS_hh$toilet,na.rm = TRUE) - ACDIS_hh$toilet)/(max(ACDIS_hh$toilet,na.rm = TRUE) - min(ACDIS_hh$toilet,na.rm = TRUE))

### Create binary variables for MCA
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Visit_Year'), 
                  col_name = 'ToiletType',
                  max_val=14 )
AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Visit_Year')))

# Household electricity Supply

ACDIS_hh$electric <-  NA
ACDIS_hh$electric[as.integer(ACDIS_hh$IsElectrified)== 1] <- 1  # Yes
ACDIS_hh$electric[as.integer(ACDIS_hh$IsElectrified)== 2] <- 0  # No

# check data 
table(ACDIS_hh$Visit_Year,ACDIS_hh$electric,useNA = "ifany")

# Energy - Cooking Fuels 

ACDIS_hh$energy <-  NA

ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 2] <- 1  # Electricity from Generator
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 4] <- 1  # Electricity from Solar
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 3] <- 2  # Electricity from Grid
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 5] <- 3  # Gas(LPG)
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 6] <- 4  # Paraffin
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 1] <- 5  # Coal
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 7] <- 6  # Wood

#check data 
table(ACDIS_hh$Visit_Year,ACDIS_hh$energy,useNA = "ifany")

### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$energy_norm <- (max(ACDIS_hh$energy,na.rm = TRUE) - ACDIS_hh$energy)/(max(ACDIS_hh$energy,na.rm = TRUE) - min(ACDIS_hh$energy,na.rm = TRUE))
table(ACDIS_hh$energy_norm)

### Create binary variables for MCA
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Visit_Year'),
                  col_name = 'MainCookingFuel',
                  max_val=7 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Visit_Year')))

### Wall materials ### Only used in last 2 or 3 years 

ACDIS_hh$wall_mat <-  NA

ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 2] <- 1  # Bricks
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 4] <- 2  # Cement Block
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 10] <- 2  # Pre-Fabricated
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 18] <- 2  # Other modern
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 14] <- 2  # Tile
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 1] <- 3  # Asbestos
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 11] <- 3  # Sail
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 5] <- 4  # Corrugated iron
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 6] <- 5  # Damp Course
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 7] <- 6  # Mud & Cement
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 12] <- 6  # Stone & Lath
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 13] <- 6  # Thatching
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 15] <- 6  # Wood
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 16] <- 6  # Wattle and daub
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 7] <- 6  # Mud & Cement
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 19] <- 7  # Stabilised Mud
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 8] <- 8  # Mud
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 20] <- 8  # Traditional Mud
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 9] <- 9  # Plastic
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 21] <- 9  # Other informal
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 22] <- 9  # Carpet
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 23] <- 9  # Dirt
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 24] <- 9  # Mat
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 25] <- 9  # Other traditional
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 26] <- 9  # Dung
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 27] <- 9  # Parquet
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 28] <- 9  # Vinyl
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 3] <- 9  # Cardboard

# check data 
table(ACDIS_hh$Visit_Year,ACDIS_hh$wall_mat,useNA = "ifany")

### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$wall_mat_norm <- (max(ACDIS_hh$wall_mat,na.rm = TRUE) - ACDIS_hh$wall_mat)/(max(ACDIS_hh$wall_mat,na.rm = TRUE) - min(ACDIS_hh$wall_mat,na.rm = TRUE))
table(ACDIS_hh$wall_mat_norm)

### Create binary variables for MCA
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Visit_Year'),
                  col_name = 'WallMaterial',
                  max_val=28 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Visit_Year')))

### Roof materials

ACDIS_hh$roof_mat <-  NA

ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 2] <- 1  # Bricks
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 4] <- 2  # Cement Block
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 10] <- 2  # Pre-Fabricated
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 18] <- 2  # Other modern
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 14] <- 2  # Tile
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 1] <- 3  # Asbestos
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 11] <- 3  # Sail
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 5] <- 4  # Corrugated iron
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 6] <- 5  # Damp Course
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 7] <- 6  # Mud & Cement
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 12] <- 6  # Stone & Lath
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 13] <- 6  # Thatching
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 15] <- 6  # Wood
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 16] <- 6  # Wattle and daub
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 7] <- 6  # Mud & Cement
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 19] <- 7  # Stabilised Mud
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 8] <- 8  # Mud
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 20] <- 8  # Traditional Mud
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 9] <- 9  # Plastic
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 21] <- 9  # Other informal
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 22] <- 9  # Carpet
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 23] <- 9  # Dirt
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 24] <- 9  # Mat
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 25] <- 9  # Other traditional
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 26] <- 9  # Dung
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 27] <- 9  # Parquet
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 28] <- 9  # Vinyl
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 3] <- 9  # Cardboard

# check data 
table(ACDIS_hh$Visit_Year,ACDIS_hh$roof_mat,useNA = "ifany")

### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$roof_mat_norm <- (max(ACDIS_hh$roof_mat,na.rm = TRUE) - ACDIS_hh$roof_mat)/(max(ACDIS_hh$roof_mat,na.rm = TRUE) - min(ACDIS_hh$roof_mat,na.rm = TRUE))
table(ACDIS_hh$roof_mat_norm)

### Create binary variables for MCA
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Visit_Year'),
                  col_name = 'RoofMaterial',
                  max_val=28 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Visit_Year')))

#### Variables already coded as binary 

# Household electricity Supply

AC_1.df$electric <-  NA
AC_1.df$electric[as.integer(AC_1.df$IsElectrified)== 1] <- 1  # Yes
AC_1.df$electric[as.integer(AC_1.df$IsElectrified)== 2] <- 0  # No

## Other Variables All coded 1 - Yes and 0 - No
## Convert 9's unknown to NA
# V78 BED Doe HH have ... Bed
AC_1.df$BED[as.integer(AC_1.df$BED)== 9] <- NA
# V79 BIC Doe HH have ... Bicycle
AC_1.df$BIC[as.integer(AC_1.df$BIC)== 9] <- NA
# V80 BLM Doe HH have ... Blockmaker
AC_1.df$BLM[as.integer(AC_1.df$BLM)== 9] <- NA
# V81 CAR Doe HH have ... Car
AC_1.df$CAR[as.integer(AC_1.df$CAR)== 9] <- NA
# V82 CBE Doe HH have ... Car battery
AC_1.df$CBE[as.integer(AC_1.df$CBE)== 9] <- NA
# V83 CTL Doe HH have ... Cattle
AC_1.df$CTL[as.integer(AC_1.df$CTL)== 9] <- NA
# V84 ECO Doe HH have ... Electric cooker with oven
AC_1.df$ECO[as.integer(AC_1.df$ECO)== 9] <- NA
# V85 EHP Doe HH have ... Electric hotplate
AC_1.df$EHP[as.integer(AC_1.df$EHP)== 9] <- NA
# V86 EKT Doe HH have ... Electric kettle
AC_1.df$EKT[as.integer(AC_1.df$EKT)== 9] <- NA
# V87 FRG Doe HH have ... Fridge
AC_1.df$FRG[as.integer(AC_1.df$FRG)== 9] <- NA
# V88 GCK Doe HH have ... Gas cooker
AC_1.df$GCK[as.integer(AC_1.df$GCK)== 9] <- NA
# V89 HSF Doe HH have ... Hoe/Spade/Fork
AC_1.df$HSF[as.integer(AC_1.df$HSF)== 9] <- NA
# V90 KLT Doe HH have ... Kombi/Lorry/Tractor
AC_1.df$KLT[as.integer(AC_1.df$KLT)== 9] <- NA
# V91 KTS Doe HH have ... Kitchen sink
AC_1.df$KTS[as.integer(AC_1.df$KTS)== 9] <- NA
# V92 MCS Doe HH have ... Motorcycle
AC_1.df$MCS[as.integer(AC_1.df$MCS)== 9] <- NA
# V93 OLS Doe HH have ... Other livestock (i.e. not cattle)
AC_1.df$OLS[as.integer(AC_1.df$OLS)== 9] <- NA
# V94 PMC Doe HH have ... Primus cooker
AC_1.df$PMC[as.integer(AC_1.df$PMC)== 9] <- NA
# V95 RAD Doe HH have ... Radio
AC_1.df$RAD[as.integer(AC_1.df$RAD)== 9] <- NA
# V96 SOF Doe HH have ... Sofa
AC_1.df$SOF[as.integer(AC_1.df$SOF)== 9] <- NA
# V97 SWM Doe HH have ... Sewing machine
AC_1.df$SWM[as.integer(AC_1.df$SWM)== 9] <- NA
# V98 TBC Doe HH have ... Table & Chairs
AC_1.df$TBC[as.integer(AC_1.df$TBC)== 9] <- NA
# V99 TLL Doe HH have ... Telephone
AC_1.df$TLL[as.integer(AC_1.df$TLL)== 9] <- NA
# V100 TMB Doe HH have ... Cellphone
AC_1.df$TMB[as.integer(AC_1.df$TMB)== 9] <- NA
# V101 TVS Doe HH have ... TV
AC_1.df$TVS[as.integer(AC_1.df$TVS)== 9] <- NA
# V102 VCR Doe HH have ... Video
AC_1.df$VCR[as.integer(AC_1.df$VCR)== 9] <- NA
# V103 WBR Doe HH have ... Wheelbarrow
AC_1.df$WBR[as.integer(AC_1.df$WBR)== 9] <- NA
# V104 HWG Doe HH have ... hot water geyser
AC_1.df$HWG[as.integer(AC_1.df$HWG)== 9] <- NA
# V105 WSM Doe HH have ... Washing machine
AC_1.df$WSM[as.integer(AC_1.df$WSM)== 9] <- NA
# V106 EHT Doe HH have ... Electric heater
AC_1.df$EHT[as.integer(AC_1.df$EHT)== 9] <- NA
# V107 PHT Doe HH have ... Paraffin heater
AC_1.df$PHT[as.integer(AC_1.df$PHT)== 9] <- NA
# V108 SHF Doe HH have ... Stereo or hi-fi
AC_1.df$SHF[as.integer(AC_1.df$SHF)== 9] <- NA
# V109 CPT Doe HH have ... Computer or laptop
AC_1.df$CPT[as.integer(AC_1.df$CPT)== 9] <- NA
# V110 VAN Doe HH have ... Combi van or laptop
AC_1.df$VAN[as.integer(AC_1.df$VAN)== 9] <- NA
# V111 LOR Doe HH have ... Lorry or transport
AC_1.df$LOR[as.integer(AC_1.df$LOR)== 9] <- NA
# V112 TFV Doe HH have ... Tractor or farm vehicle
AC_1.df$TFV[as.integer(AC_1.df$TFV)== 9] <- NA
# V113 FRN Doe HH have ... Furnishings
AC_1.df$FRN[as.integer(AC_1.df$FRN)== 9] <- NA
# V114 JWT Doe HH have ... Jewellery and watches
AC_1.df$JWT[as.integer(AC_1.df$JWT)== 9] <- NA
# V115 ACN Doe HH have ... Air conditioner
AC_1.df$ACN[as.integer(AC_1.df$ACN)== 9] <- NA
# V116 DIS Doe HH have ... Dish washing machine
AC_1.df$DIS[as.integer(AC_1.df$DIS)== 9] <- NA
# V117 MIC Doe HH have ... Microwave Oven
AC_1.df$MIC[as.integer(AC_1.df$MIC)== 9] <- NA
# V118 PTV Doe HH have ... Pay TV Subscription
AC_1.df$PTV[as.integer(AC_1.df$PTV)== 9] <- NA
# V119 SEC Doe HH have ... Home security service
AC_1.df$SEC[as.integer(AC_1.df$SEC)== 9] <- NA
# V120 SWP Doe HH have ... Swimming pool
AC_1.df$SWP[as.integer(AC_1.df$SWP)== 9] <- NA
# V121 TUM Doe HH have ... Tumble dryer
AC_1.df$TUM[as.integer(AC_1.df$TUM)== 9] <- NA
# V122 VAC Doe HH have ... Vacuum cleaner/ floor polisher
AC_1.df$VAC[as.integer(AC_1.df$VAC)== 9] <- NA

### Drop ".x" from column names
names(AC_1.df) <- sub(pattern = ".x$","", colnames(AC_1.df))

### Drop columns with ".y" in name 
AC_1.df <- AC_1.df %>% dplyr::select(-contains(".y"))


## Other Variables All coded 1 - Yes and 0 - No
## Convert 9's unknown to NA
# V78 BED Doe HH have ... Bed
ACDIS_hh$BED[as.integer(ACDIS_hh$BED)== 9] <- NA
# V79 BIC Doe HH have ... Bicycle
ACDIS_hh$BIC[as.integer(ACDIS_hh$BIC)== 9] <- NA
# V80 BLM Doe HH have ... Blockmaker
ACDIS_hh$BLM[as.integer(ACDIS_hh$BLM)== 9] <- NA
# V81 CAR Doe HH have ... Car
ACDIS_hh$CAR[as.integer(ACDIS_hh$CAR)== 9] <- NA
# V82 CBE Doe HH have ... Car battery
ACDIS_hh$CBE[as.integer(ACDIS_hh$CBE)== 9] <- NA
# V83 CTL Doe HH have ... Cattle
ACDIS_hh$CTL[as.integer(ACDIS_hh$CTL)== 9] <- NA
# V84 ECO Doe HH have ... Electric cooker with oven
ACDIS_hh$ECO[as.integer(ACDIS_hh$ECO)== 9] <- NA
# V85 EHP Doe HH have ... Electric hotplate
ACDIS_hh$EHP[as.integer(ACDIS_hh$EHP)== 9] <- NA
# V86 EKT Doe HH have ... Electric kettle
ACDIS_hh$EKT[as.integer(ACDIS_hh$EKT)== 9] <- NA
# V87 FRG Doe HH have ... Fridge
ACDIS_hh$FRG[as.integer(ACDIS_hh$FRG)== 9] <- NA
# V88 GCK Doe HH have ... Gas cooker
ACDIS_hh$GCK[as.integer(ACDIS_hh$GCK)== 9] <- NA
# V89 HSF Doe HH have ... Hoe/Spade/Fork
ACDIS_hh$HSF[as.integer(ACDIS_hh$HSF)== 9] <- NA
# V90 KLT Doe HH have ... Kombi/Lorry/Tractor
ACDIS_hh$KLT[as.integer(ACDIS_hh$KLT)== 9] <- NA
# V91 KTS Doe HH have ... Kitchen sink
ACDIS_hh$KTS[as.integer(ACDIS_hh$KTS)== 9] <- NA
# V92 MCS Doe HH have ... Motorcycle
ACDIS_hh$MCS[as.integer(ACDIS_hh$MCS)== 9] <- NA
# V93 OLS Doe HH have ... Other livestock (i.e. not cattle)
ACDIS_hh$OLS[as.integer(ACDIS_hh$OLS)== 9] <- NA
# V94 PMC Doe HH have ... Primus cooker
ACDIS_hh$PMC[as.integer(ACDIS_hh$PMC)== 9] <- NA
# V95 RAD Doe HH have ... Radio
ACDIS_hh$RAD[as.integer(ACDIS_hh$RAD)== 9] <- NA
# V96 SOF Doe HH have ... Sofa
ACDIS_hh$SOF[as.integer(ACDIS_hh$SOF)== 9] <- NA
# V97 SWM Doe HH have ... Sewing machine
ACDIS_hh$SWM[as.integer(ACDIS_hh$SWM)== 9] <- NA
# V98 TBC Doe HH have ... Table & Chairs
ACDIS_hh$TBC[as.integer(ACDIS_hh$TBC)== 9] <- NA
# V99 TLL Doe HH have ... Telephone
ACDIS_hh$TLL[as.integer(ACDIS_hh$TLL)== 9] <- NA
# V100 TMB Doe HH have ... Cellphone
ACDIS_hh$TMB[as.integer(ACDIS_hh$TMB)== 9] <- NA
# V101 TVS Doe HH have ... TV
ACDIS_hh$TVS[as.integer(ACDIS_hh$TVS)== 9] <- NA
# V102 VCR Doe HH have ... Video
ACDIS_hh$VCR[as.integer(ACDIS_hh$VCR)== 9] <- NA
# V103 WBR Doe HH have ... Wheelbarrow
ACDIS_hh$WBR[as.integer(ACDIS_hh$WBR)== 9] <- NA
# V104 HWG Doe HH have ... hot water geyser
ACDIS_hh$HWG[as.integer(ACDIS_hh$HWG)== 9] <- NA
# V105 WSM Doe HH have ... Washing machine
ACDIS_hh$WSM[as.integer(ACDIS_hh$WSM)== 9] <- NA
# V106 EHT Doe HH have ... Electric heater
ACDIS_hh$EHT[as.integer(ACDIS_hh$EHT)== 9] <- NA
# V107 PHT Doe HH have ... Paraffin heater
ACDIS_hh$PHT[as.integer(ACDIS_hh$PHT)== 9] <- NA
# V108 SHF Doe HH have ... Stereo or hi-fi
ACDIS_hh$SHF[as.integer(ACDIS_hh$SHF)== 9] <- NA
# V109 CPT Doe HH have ... Computer or laptop
ACDIS_hh$CPT[as.integer(ACDIS_hh$CPT)== 9] <- NA
# V110 VAN Doe HH have ... Combi van or laptop
ACDIS_hh$VAN[as.integer(ACDIS_hh$VAN)== 9] <- NA
# V111 LOR Doe HH have ... Lorry or transport
ACDIS_hh$LOR[as.integer(ACDIS_hh$LOR)== 9] <- NA
# V112 TFV Doe HH have ... Tractor or farm vehicle
ACDIS_hh$TFV[as.integer(ACDIS_hh$TFV)== 9] <- NA
# V113 FRN Doe HH have ... Furnishings
ACDIS_hh$FRN[as.integer(ACDIS_hh$FRN)== 9] <- NA
# V114 JWT Doe HH have ... Jewellery and watches
ACDIS_hh$JWT[as.integer(ACDIS_hh$JWT)== 9] <- NA
# V115 ACN Doe HH have ... Air conditioner
ACDIS_hh$ACN[as.integer(ACDIS_hh$ACN)== 9] <- NA
# V116 DIS Doe HH have ... Dish washing machine
ACDIS_hh$DIS[as.integer(ACDIS_hh$DIS)== 9] <- NA
# V117 MIC Doe HH have ... Microwave Oven
ACDIS_hh$MIC[as.integer(ACDIS_hh$MIC)== 9] <- NA
# V118 PTV Doe HH have ... Pay TV Subscription
ACDIS_hh$PTV[as.integer(ACDIS_hh$PTV)== 9] <- NA
# V119 SEC Doe HH have ... Home security service
ACDIS_hh$SEC[as.integer(ACDIS_hh$SEC)== 9] <- NA
# V120 SWP Doe HH have ... Swimming pool
ACDIS_hh$SWP[as.integer(ACDIS_hh$SWP)== 9] <- NA
# V121 TUM Doe HH have ... Tumble dryer
ACDIS_hh$TUM[as.integer(ACDIS_hh$TUM)== 9] <- NA
# V122 VAC Doe HH have ... Vacuum cleaner/ floor polisher
ACDIS_hh$VAC[as.integer(ACDIS_hh$VAC)== 9] <- NA


### tabulate 

item_list <- c( "BED",
                "BIC",
                "BLM",
                "CAR",
                "CBE",
                "CTL",
                "ECO",
                "EHP",
                "EKT",
                "FRG",
                "GCK",
                "HSF",
                "KLT",
                "KTS",
                "MCS",
                "OLS",
                "PMC",
                "RAD",
                "SOF",
                "SWM",
                "TBC",
                "TLL",
                "TMB",
                "TVS",
                "VCR",
                "WBR",
                "HWG",
                "WSM",
                "EHT",
                "PHT",
                "SHF",
                "CPT",
                "VAN",
                "LOR",
                "TFV",
                "FRN",
                "JWT",
                "ACN",
                "DIS",
                "MIC",
                "PTV",
                "SEC",
                "SWP",
                "TUM",
                "VAC")

## check data 

### Create tab.df which shows the number of times each variable was collected in each year 

tab.df <- as.data.frame(unique(ACDIS_hh$Visit_Year))
names(tab.df)[1] <- "Visit_Year"

tab.df <-  tab.df %>% arrange(Visit_Year)

for (var_nam in item_list) {
  dat.df <- ACDIS_hh %>% 
    group_by(Visit_Year) %>% 
    summarise(x = sum(.data[[var_nam]]))
  names(dat.df)[2] <- var_nam
  tab.df <- merge(tab.df,dat.df,by='Visit_Year')
}

write.csv2(tab.df,paste0(output_dir,'/asset_item.csv'))




### All variables scaled from 0 to 1 - calculate SES quintiles for each year

### Create a list of assets to be included in the index
### Comment out those not to be used in the calculation 

asset_list <- c("HHIntId",
                "BSIntId",
                "VisitDate",
                "DSRound",
                "Visit_Year",
                "water_norm",
                "toilet_norm",
                "electric",
                "energy_norm",
                "wall_mat_norm", 
                "roof_mat_norm",
                "BED",
                "BIC",
                "BLM",
                "CAR",
                "CBE",
                "CTL",
                "ECO",
                "EHP",
                "EKT",
                "FRG",
                "GCK",
                "HSF",
                "KLT",
                "KTS",
                "MCS",
                "OLS",
                "PMC",
                "RAD",
                "SOF",
                "SWM",
                "TBC",
                "TLL",
                "TMB",
                "TVS",
                "VCR",
                "WBR",
                "HWG",
                "WSM",
                "EHT",
                "PHT",
                "SHF",
                "CPT",
                "VAN",
                "LOR",
                "TFV",
                "FRN",
                "JWT",
                "ACN",
                "DIS",
                "MIC",
                "PTV",
                "SEC",
                "SWP",
                "TUM",
                "VAC"
)

mca_col_list <- c("HHIntId",
              "BSIntId",
              "VisitDate",
              "Visit_Year",
              "DSRound",
              "mv_DrinkWaterSource",
              "mv_ToiletType",
              "mv_MainCookingFuel",
              "mv_RoofMaterial",
              "mv_WallMaterial",
              "electric",
              "BED",
              "BIC",
              "BLM",
              "CAR",
              "CBE",
              "CTL",
              "ECO",
              "EHP",
              "EKT",
              "FRG",
              "GCK",
              "HSF",
              "KLT",
              "KTS",
              "MCS",
              "OLS",
              "PMC",
              "RAD",
              "SOF",
              "SWM",
              "TBC",
              "TLL",
              "TMB",
              "TVS",
              "VCR",
              "WBR",
              "HWG",
              "WSM"
              # "EHT",
              # "PHT",
              # "SHF",
              # "CPT",
              # "VAN",
              # "LOR",
              # "TFV",
              # "FRN",
              # "JWT",
              # "ACN",
              # "DIS",
              # "MIC",
              # "PTV",
              # "SEC",
              # "SWP",
              # "TUM",
              # "VAC"
)
### Create new dataframe with just required data 

ass_data <- ACDIS_hh[,asset_list]

ass_data_mca <- AC_1.df[,grep(paste(mca_col_list , collapse = "|"), x=names(AC_1.df))]


# Count NA values in each column for MCA file using dplyr
na_counts_mca <- ass_data_mca %>%
  summarise_all(~ sum(is.na(.)))
NROW(ass_data_mca)
## Only electric has missing data - 6439 rows out of 238930 



### Rename variables
ass_data <- dplyr::rename(ass_data, 'WAL' = 'wall_mat_norm')
ass_data <- dplyr::rename(ass_data, 'ROO' = 'roof_mat_norm')
ass_data <- dplyr::rename(ass_data, 'WAT' = 'water_norm')
ass_data <- dplyr::rename(ass_data, 'TOI' = 'toilet_norm')
ass_data <- dplyr::rename(ass_data, 'ENE' = 'energy_norm')
ass_data <- dplyr::rename(ass_data, 'ELC' = 'electric')

### Rename variables 
ass_data_mca <- dplyr::rename(ass_data_mca, 'ELC' = 'electric')


#### PCA/FA data filter for Southern PIPSA and drop records with mis-assigned dates 
### Loading Bounded Structure Data to filter for only Southern PIPSA

stata_data_file <- '/RD01-03 ACDIS BoundedStructures.dta'
ACDIS_BS <- haven::read_dta(paste0(data_dir,stata_data_file))
BS_PIP <- ACDIS_BS[c('BSIntId','PIPSA')]


ass_data_PIP <- merge(ass_data,BS_PIP,by='BSIntId',all.x=TRUE)

ass_data_SPIP = ass_data_PIP[(ass_data_PIP$PIPSA %in% c(1)), ]

### Filter to drop rows in round 14 with visit date 2005-02-01 - mis-assigned dates 

ass_data_SPIP_sub <- ass_data_SPIP[!(ass_data_SPIP$DSRound==14 & 
                                       ass_data_SPIP$VisitDate=="2005-02-01"), ]


#### MCA data filter for Southern PIPSA and drop records with misassigned dates 
### Loading Bounded Structure Data to filter for only Southern PIPSA

ass_data_mca_PIP <- merge(ass_data_mca,BS_PIP,by='BSIntId',all.x=TRUE)

ass_data_mca_SPIP = ass_data_mca_PIP[(ass_data_mca_PIP$PIPSA %in% c(1)), ]

### Filter to drop rows in round 14 with visit date 2005-02-01 - mis-assigned dates 

ass_data_mca_SPIP_sub <- ass_data_mca_SPIP[!(ass_data_mca_SPIP$DSRound==14 & 
                                               ass_data_mca_SPIP$VisitDate=="2005-02-01"), ]



### Generate a Round Year Variable which will be used to merge data with other datasets

rounds_hh = ass_data_SPIP_sub %>% 
  group_by(DSRound) %>% 
  dplyr::summarise(earliest_visit_date =min(VisitDate), 
                   latest_visit_date = max(VisitDate), 
                   visit_count = n(),
                   m_year =mean.Date(as.Date(VisitDate), 
                                     format=c("%Y-%m-%d")), 
                   Round_Year = substr(m_year, 1,4))

### Keep required variables
round_rd_year_hh <- rounds_hh[c('DSRound','Round_Year')] 

### Merge to asset data files

ass_data_SPIP_sub_rd_year <- merge(ass_data_SPIP_sub,round_rd_year_hh,by='DSRound')
ass_data_mca_SPIP_sub_rd_year <- merge(ass_data_mca_SPIP_sub,round_rd_year_hh,by='DSRound')


## PCA/FA unique visits
### Count number of visits per Round Year per household

# ranking by HHID and Visit Date 

## Ranking by HH Id , Year, year_diff
ass_data_SPIP_sub_rd_year <- ass_data_SPIP_sub_rd_year %>%
  group_by(HHIntId,Round_Year) %>%
  dplyr::mutate(rank = order(order(VisitDate, decreasing=FALSE)))


### If two visits in a year just use first 
ass_data_SPIP_sub_rd_year <- ass_data_SPIP_sub_rd_year %>% filter(rank==1)

## MCA unique visits 
### Count number of visits per year per household

# ranking by HHID and Visit Date 

## Ranking by Individual Id , Year, year_diff
ass_data_mca_SPIP_sub_rd_year <- ass_data_mca_SPIP_sub_rd_year %>%
  group_by(HHIntId,Round_Year) %>%
  dplyr::mutate(rank = order(order(VisitDate, decreasing=FALSE)))


### If two visits in a year just use first 
ass_data_mca_SPIP_sub_rd_year <- ass_data_mca_SPIP_sub_rd_year %>% filter(rank==1)

## Select range of Round Years 

first_round_year = 2005
last_round_year = 2023

ass_data_select <- ass_data_SPIP_sub_rd_year %>% filter(Round_Year >= first_round_year & Round_Year <= last_round_year)
ass_data_select_mca <- ass_data_mca_SPIP_sub_rd_year %>% filter(Round_Year >= first_round_year & Round_Year <= last_round_year)

## No data on ownership of individual assets in Rounds 45 and 46 - drop data for these rounds
ass_data_select <- ass_data_select %>% filter(DSRound!=45 & DSRound!=46)
ass_data_select_mca <- ass_data_select_mca %>% filter(DSRound!=45 & DSRound!=46)


### Vector of unique Round_Years

year_list <- sort(unique(ass_data_select$Round_Year))
year_list

#year = 2021

#### Loop through a list of years for PCA/FA calculation 
for (year in year_list){
  print("FA/PCA calculation")
  print(as.character(year))
  
  ass_data_year <- ass_data_select %>% filter(Round_Year == year)
  ##Count NA values for each asset variable
  
  print(paste0("Total number of rows = ",as.character(nrow(ass_data_year)) ))
  
  print(paste0("Total number of rows with missing data = ",as.character(sum(!complete.cases(ass_data_year))) ))
  

  for (var in colnames(ass_data_select)){
    print(paste0("Total number of rows with missing data for ",var," = ",as.character(sum(is.na(ass_data_year[var]))) ))
  }
  
  ### Drop any columns if all values are NA
  ass_data_year <- ass_data_year[colSums(is.na(ass_data_year)) == 0]
  
  ### Drop any rows with NA values as this will led to problems with PCA calculation 
  
  print(paste0("Rows before dropping NA values = ",as.character(nrow(ass_data_year))))
  ass_data_tmp <-  na.omit(ass_data_year)
  print(paste0("Rows after dropping NA values = ",as.character(nrow(ass_data_tmp))))
  
  #n_zero <- nrow(ass_data_tmp[rowSums(ass_data_tmp[ , c(5:(ncol(ass_data_tmp) - 2))], na.rm=TRUE) == 0,])
  #print(paste0("Total number of rows with all asset values equal to zero =  ",as.character(n_zero)))
  
  ### Drop rows with Sum = 0 for all asset values (i.e. all 0 ) 
  ass_data_tmp$ses_sum <- rowSums(ass_data_tmp[ , c(5:(ncol(ass_data_tmp) - 2))], na.rm=TRUE)
  ass_data_tmp <-  dplyr::filter(ass_data_tmp, ses_sum > 0 )
  print(paste0("Rows after dropping all zero values = ",as.character(nrow(ass_data_tmp))))
  
  ass_data_tmp<- as.data.frame(ass_data_tmp)
  ### Use HHIntID as row name for later merge
  rownames(ass_data_tmp) <- ass_data_tmp[,3]
  
  ### Select columns of vlaues used for PCA/FA calculation
  ass_data_tmp <- ass_data_tmp[6:(ncol(ass_data_tmp)- 4)]
  
  print(paste0("Number of columns of asset data before dropping columns with all values as 0 = ",as.character(ncol(ass_data_tmp))))
  ass_data_tmp <- ass_data_tmp[, colSums(ass_data_tmp != 0, na.rm = TRUE) > 0]
  print(paste0("Number of columns of asset data after dropping columns with all values as 0 = ",as.character(ncol(ass_data_tmp))))
  
  ### This section of code is used for investigating the correlation of variables and can be commented out 
  ### if not needed
  
  #ass_data_tmp <- cbind(ass_data_tmp_head,ass_data_tmp_tail)
  
  #head(ass_data_tmp)
  
  ##Correlation Plot
  # PerformanceAnalytics::chart.Correlation(ass_data_tmp , method = c("spearman")
  #                   ,histogram=FALSE, pch=20, cex=2.5,sym_cex = 0.001)
  ### Check correlation
  #corr.matrix1 <- cor(ass_data_tmp, method = "spearman",  use = "pairwise.complete.obs")
  
  ## Another approach to better visualize correlation between indicators is to 
  ## represent them through a network with the ggpraph package.
  
  #this.indicators.label <- rownames(corr.matrix1)
  
  ### for output
  # plot_name <- paste0('/correlation_network-',as.character(year))
  # qgraph::qgraph(cor(ass_data_tmp),
  #        # shape = "circle",
  #        # posCol = "darkgreen",
  #        # negCol = "darkred",
  #        # threshold = "bonferroni", #The threshold argument can be used to remove edges that are not significant.
  #        # sampleSize = nrow(scores.this.norm),
  #        # graph = "glasso",
  #        esize = 5, ## Size of node
  #        vsize = 5,
  #        vTrans = 600,
  #        posCol = "#003399", ## Color positive correlation Dark powder blue
  #        negCol = "#FF9933", ## Color negative correlation Deep Saffron
  #        alpha = 0.05,
  #        cut = 0.4, ## cut off value for correlation
  #        maximum = 1, ## cut off value for correlation
  #        palette = 'pastel', # adjusting colors
  #        borders = TRUE,
  #        details = FALSE,
  #        layout = "spring",
  #        nodeNames = this.indicators.label ,
  #        labels = rownames(corr.matrix1),
  #        legend.cex = 0.1,
  #        label.cex = 0.7,
  #        label.scale = TRUE,
  #        title = "Correlations Network",
  #        line = -2,
  #        cex.main = 1,
  #        filetype = "png",
  #        filename = paste0(output_dir,plot_name))
  
  ### References to methods/packages used 
  
  ### http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials
  
  ### Factor Analysis Composite Scales - user guide
  ### https://www-users.york.ac.uk/~mb55/msc/clinimet/week7/scales.pdf
  
  ### PCA analysis 
  
  res.pca <- FactoMineR::PCA(ass_data_tmp, scale.unit = TRUE, ncp = 5, graph = FALSE)
  pca_score <- as.data.frame(res.pca$ind$coord[,1])
  
  pca_score$HHIntId <- row.names(pca_score)
  #### Add 1st dimension of PCA analysis to ass_data_tmp dataframe
  names(pca_score)[1] <- "pca_1d_score"
  
  #### Factor Analysis - Composite Index
  
  #### Composite Index using Factor Analysis - Methods
  ### https://humanitarian-user-group.github.io/post/compositeindicator/ 
  ### Or this
  ### https://bluefoxr.github.io/COINrDoc/
  ## Fixes a grpahics error in ci_factor
  par(mar=c(1,1,1,1))
  CI_Factor_estimated <-  Compind::ci_factor(ass_data_tmp,
                                             indic_col = (1:ncol(ass_data_tmp)),
                                             method = "ONE",
                                             dim=3)
  ## Overall Score
  fa_score <- data.frame( CI_Factor_estimated$ci_factor_est)
  fa_score$HHIntId <- row.names(fa_score)
  names(fa_score)[1] <- "fa_score"
  
  ## Merge Results 
  ass_data_year <- merge(ass_data_year,pca_score,by='HHIntId')
  ass_data_year <- merge(ass_data_year,fa_score,by='HHIntId')
  
  # Calculate Wealth quantiles 1 = poorest to n = richest
  ass_data_year$wealth_quantile_pca <-  schoRsch::ntiles(ass_data_year, dv = "pca_1d_score", bins=n_quant)
  ass_data_year$wealth_quantile_fa <-  schoRsch::ntiles(ass_data_year, dv = "fa_score", bins=n_quant)
  
  ### this code is used to check the results of the PCA and FA scores
  ## basic Scatter Plot
  # basic scatterplot
  # ggplot(ass_data_year, aes(x=pca_1d_score, y=fa_score)) +
  #   geom_point()
  
  ### Analysing PCA output
  # 
  # 
  # ### Percentage of variance explained by each dimension     
  # eig.val <- factoextra::get_eigenvalue(res.pca)
  # eig.val
  # 
  # ### Scree Plot
  # factoextra::fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 25))
  # 
  # ### Graphs of Variables
  # var <- factoextra::get_pca_var(res.pca)
  # var
  # 
  # head(var$contrib)
  # 
  # 
  # corrplot(var$contrib, is.corr=FALSE, 
  #          cl.pos='r',cl.align.text='l',cl.ratio=0.5)    
  #     
  # head(var$cos2, 4)
  
  ### http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials
  
  ### Factor Analysis Composite Scales - user guide
  ### https://www-users.york.ac.uk/~mb55/msc/clinimet/week7/scales.pdf
  ## Keep HHId, Year , prn_score , wealth_quantile
  
  ass_data_ses_year <- ass_data_year[,c('HHIntId','Round_Year','pca_1d_score','wealth_quantile_pca', 'fa_score','wealth_quantile_fa' )]
  
  #ass_data_ses_year <- ass_data_year[,c('HHIntId','Visit_Year','pca_1d_score','wealth_quantile_pca' )]
  
  
  ### Append to overall dataframe      
  if(exists("ass_ses_all")==FALSE) {
    ass_ses_all <- ass_data_ses_year
  } else{ 
    ass_ses_all <-  rbind(ass_ses_all,ass_data_ses_year)
  }
}

#### Loop through a list of years
for (year in year_list){
  print("MCA calculation")
  print(as.character(year))
  
  ass_data_year_mca <- ass_data_select_mca %>% filter(Round_Year == year)
  
  ### Drop any rows with NA values as this will led to problems with MCA calculation 
  
  print(paste0("Rows before dropping NA values = ",as.character(nrow(ass_data_year_mca))))
  ass_data_tmp <-  na.omit(ass_data_year_mca)
  print(paste0("Rows after dropping NA values = ",as.character(nrow(ass_data_tmp))))
  
  n_zero <- nrow(ass_data_tmp[rowSums(ass_data_tmp[ , c(6:(ncol(ass_data_tmp) - 3))], na.rm=TRUE) == 0,])
  print(paste0("Total number of rows with all asset values equal to zero =  ",as.character(n_zero)))
  
  ### Drop rows with Sum = 0 for all asset values (i.e. all 0 ) 
  ass_data_tmp$ses_sum <- rowSums(ass_data_tmp[ , c(6:(ncol(ass_data_tmp) - 3))], na.rm=TRUE)
  ass_data_tmp <-  dplyr::filter(ass_data_tmp, ses_sum > 0 )
  print(paste0("Rows after dropping all zero values = ",as.character(nrow(ass_data_tmp))))
  
  ass_data_tmp_df <- as.data.frame(ass_data_tmp)
  rownames(ass_data_tmp_df) <- ass_data_tmp_df$HHIntId
  
  ### Keep first 4 columns then drop columns if all values for SES variables are 0 
  
  
  ass_data_tmp_head <- ass_data_tmp_df[c('HHIntId','BSIntId','DSRound','VisitDate','Round_Year')]
  ass_data_tmp_tail <- ass_data_tmp_df[6:(ncol(ass_data_tmp_df) - 4)]
  
  print(paste0("Number of columns of asset data before dropping columns with all values as 0 = ",as.character(ncol(ass_data_tmp_tail))))
  
  ass_data_tmp_tail2 <- ass_data_tmp_tail[, colSums(ass_data_tmp_tail) > 0]
  
  print(paste0("Number of columns of asset data after dropping columns with all values as 0 = ",as.character(ncol(ass_data_tmp_tail))))
  
  #ass_data_tmp_tail2$HHIntId <- row.names(ass_data_tmp_tail)
  
  #ass_data_tmp2 <- merge(ass_data_tmp_head,ass_data_tmp_tail2,by='HHIntId')
  
  ass_data_tmp2 <- cbind(ass_data_tmp_head,ass_data_tmp_tail2)
  
  ### Convert all columns to factors
  
  ass_data_tmp_tail_fact <-data.frame(lapply(ass_data_tmp_tail,factor))
  
 
  ### MCA alternative approaches 
  
  ### Run MCA analysis for each year 
  ## MCA based on this example 
  ## http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials
  
  ### Possible useful paper
  ### https://www.scirp.org/journal/paperinformation?paperid=103350
  
  ### MCA for asset scores- Traissac and Prevel
  ### https://academic.oup.com/ije/article/41/4/1207/690856?login=false 
  
  ### https://personal.utdallas.edu/~herve/Abdi-MCA2007-pretty.pdf 
  
  ### Tutorial and R Cookbook for MCA analysis 
  ### https://vgonzenbach.github.io/multivariate-cookbook/multiple-correspondence-analysis.html#row-factor-scores-1
  ### https://github.com/vgonzenbach/multivariate-cookbook/blob/master/03-MCA.Rmd 
  
  res.mca <- FactoMineR::MCA(ass_data_tmp_tail_fact, graph = FALSE)
 
  ## The function get_mca_ind() [in factoextra] is used to extract the results for individuals. 
  ## This function returns a list containing the coordinates, the cos2 and the contributions of individuals:
  ## Assumption that the original row order is maintained
  
  ind <- factoextra::get_mca_ind(res.mca)
  HH_coord <- as.data.frame(ind$coord)
  
  # ### Percentage of variance explained by each dimension     
  eig_val.mca <- factoextra::get_eigenvalue(res.mca)
  eig_val.mca
  
  ### Using coord values (factor scores for the 1st component)
  ass_data_tmp2$mca_score <- HH_coord[,1]
  ### Generate Wealth Quintiles
  ass_data_tmp2$wealth_quantile_mca <-  schoRsch::ntiles(ass_data_tmp2, dv = "mca_score", bins=n_quant)
  # 
  # ## Keep HHId, Year , mca_score , wealth_quantile_mca
  # 
  ass_data_ses_year <- ass_data_tmp2[,c('HHIntId','Round_Year','mca_score','wealth_quantile_mca')]
  # 
  ### Append to overall dataframe      
  if(exists("ass_ses_all_mca")==FALSE) {
    ass_ses_all_mca <- ass_data_ses_year
  } else{
    ass_ses_all_mca <-  rbind(ass_ses_all_mca,ass_data_ses_year)
  }
}

## Merge MCA data to PCA/FA data 

ass_ses_all_pcmc <- merge(ass_ses_all,ass_ses_all_mca,by=c('HHIntId','Round_Year'))

ass_tmp <- ass_ses_all_pcmc %>% filter(Round_Year==2005)

rsq_plot <- ggplot(data=ass_tmp, aes(x=pca_1d_score, y=mca_score)) + 
  geom_point()

rsq_plot

rsq <- function (x,y) cor(x, y) ^ 2
x=ass_ses_all_pcmc$pca_1d_score
y=ass_ses_all_pcmc$mca_score

rsq_val <- rsq(x,y)

print( paste0("R-squared value = ",as.character(rsq_val)))

### Interpolation for missing data
### Interpolate to get a value for each household for each Round_Year
### use locf - carry forward last observation
### If value missing in a particular year use most recent value
### This method will create interpolated values after the  first non-missing value

### Get a df with Each HHId and each Round_Year

all_HH.df <- as.data.frame(unique(ass_ses_all_pcmc$HHIntId))
## Merge with list of all years from first to last year
min_year <- min(ass_ses_all$Round_Year)
max_year <- max(ass_ses_all$Round_Year)

years.df  = as.data.frame(seq(min_year, max_year, 1))
names(years.df)[1] <- "Year"

HH_years.df <- cross_join(years.df,all_HH.df)
names(HH_years.df)[1]<- "Round_Year"
names(HH_years.df)[2] <- "HHIntId"

# ### Merge this with ass_data_ses
ass_ses_full <- merge(HH_years.df,ass_ses_all_pcmc,by=c('Round_Year','HHIntId'),all.x=TRUE)


#

# ### Count number of NA values by year
summary_dat <- ass_ses_full %>% group_by(Round_Year) %>% summarise(NA_sum = sum(is.na(wealth_quantile_pca)),
                                                                   n_tot = n())
# ### Percentage NA values by year
summary_dat$percent_NA <- summary_dat$NA_sum/summary_dat$n_tot*100
#
#print(n=21,summary_dat)
#

ass_ses_full_imp  <- ass_ses_full %>%
  group_by(HHIntId) %>%
  arrange(Round_Year) %>%
  mutate(wealth_quant_pca.imp1 = imputeTS::na_locf(wealth_quantile_pca)) %>%
  mutate(wealth_quant_fa.imp1 = imputeTS::na_locf(wealth_quantile_fa)) %>%
  mutate(wealth_quant_mca.imp1 = imputeTS::na_locf(wealth_quantile_mca))

ass_ses_full_imp  <- ungroup(ass_ses_full_imp )

#### save SES data file 
save(ass_ses_full_imp, file = paste0(output_dir,'/ass_ses_full_imp.RData'))


