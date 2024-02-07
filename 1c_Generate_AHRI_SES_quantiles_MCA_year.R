###
### This code takes the AHRI household data and calculates SES rankings for households and 
### individuals in those households 
### In this version SES quantiles are calculated within each year 
### Variables are only included which have been consistently collected over time
### in order to allow comparison of changes over time
### Interpolation is used to substitute for missing data 
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
                   'imputeTS','DescTools','lmreg','stringr','FactoMineR', 'factoextra','MASS')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

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

# Define Parameters for MCA calculation 

n_quant = 5 # Number of quantiles

### Loading Household Asset data 

stata_data_file <- '/RD06-99 ACDIS HSE-H All.dta'
ACDIS_hh <- haven::read_dta(paste0(data_dir,stata_data_file))

### Extract year from visit date 

ACDIS_hh$Visit_Year <- lubridate::year(ACDIS_hh$VisitDate)

### Move Visit data column
ACDIS_hh  <- ACDIS_hh  %>% relocate(Visit_Year, .after=VisitDate)

AC_1.df <- ACDIS_hh

### Water Source 
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 9 ] <- 1  # Borehole
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 1 ] <- 2  # Piped Internal
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 3 ] <- 3  # Piped Yard Tap
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 5 ] <- 4  # Piped Public tap (paid)
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 7 ] <- 4  # Piped Public tap (free)
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 15 ] <- 4  # Neighbour's tab
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 16 ] <- 4  # Borehole outside yard
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 8 ] <- 5  # Water carrier tanker
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 4 ] <- 6  # Well non-borehole
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 6 ] <- 6  # Protected Spring
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 11 ] <- 6  # Rainwater
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 13 ] <- 7  # River flowing stream
# ACDIS_hh$water[as.integer(ACDIS_hh$DrinkWaterSource)== 2 ] <- 8  # Dam / Stagnant water


### Create binary variables
tmp.df <- cat_bin(df = 'ACDIS_hh', 
               index_vars = c('HHIntId','Visit_Year'), 
               col_name = 'DrinkWaterSource',
               max_val=16 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Visit_Year')))


### Toilet Type
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 1 ] <- 1  # Flush Toilet
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 10 ] <- 1  # Flush Toilet
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 11] <- 1  # Flush Toilet
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 12] <- 2  # Pour Flush Toilet
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 3 ] <- 2  # VIP
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 5 ] <- 3  # Other Pit Latrine
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 9] <- 4  # Chemical Toilet
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 7] <- 5  # Bucket Toilet
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 2] <- 5  # Other Toilet
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 8] <- 6  # None
# ACDIS_hh$toilet[as.integer(ACDIS_hh$ToiletType)== 14] <- 6  # Open Defecation 


### Create binary variables
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Visit_Year'), 
                  col_name = 'ToiletType',
                  max_val=14 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Visit_Year')))

# Energy - Cooking Fuels 

# ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 2] <- 1  # Electricity from Generator
# ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 4] <- 1  # Electricity from Solar
# ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 3] <- 2  # Electricity from Grid
# ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 5] <- 3  # Gas(LPG)
# ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 6] <- 4  # Paraffin
# ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 1] <- 5  # Coal
# ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 7] <- 6  # Wood

### Create binary variables
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Visit_Year'),
                  col_name = 'MainCookingFuel',
                  max_val=7 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Visit_Year')))



### Wall materials ### Only used in last 2 or 3 years 

# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 2] <- 1  # Bricks
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 4] <- 2  # Cement Block
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 10] <- 2  # Pre-Fabricated
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 18] <- 2  # Other modern
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 14] <- 2  # Tile
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 1] <- 3  # Asbestos
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 11] <- 3  # Sail
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 5] <- 4  # Corrugated iron
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 6] <- 5  # Damp Course
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 7] <- 6  # Mud & Cement
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 12] <- 6  # Stone & Lath
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 13] <- 6  # Thatching
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 15] <- 6  # Wood
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 16] <- 6  # Wattle and daub
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 7] <- 6  # Mud & Cement
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 19] <- 7  # Stabilised Mud
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 8] <- 8  # Mud
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 20] <- 8  # Traditional Mud
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 9] <- 9  # Plastic
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 21] <- 9  # Other informal
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 22] <- 9  # Carpet
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 23] <- 9  # Dirt
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 24] <- 9  # Mat
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 25] <- 9  # Other traditional
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 26] <- 9  # Dung
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 27] <- 9  # Parquet
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 28] <- 9  # Vinyl
# ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 3] <- 9  # Cardboard

### Create binary variables
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Visit_Year'),
                  col_name = 'WallMaterial',
                  max_val=28 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Visit_Year')))

### Roof materials

# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 2] <- 1  # Bricks
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 4] <- 2  # Cement Block
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 10] <- 2  # Pre-Fabricated
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 18] <- 2  # Other modern
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 14] <- 2  # Tile
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 1] <- 3  # Asbestos
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 11] <- 3  # Sail
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 5] <- 4  # Corrugated iron
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 6] <- 5  # Damp Course
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 7] <- 6  # Mud & Cement
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 12] <- 6  # Stone & Lath
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 13] <- 6  # Thatching
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 15] <- 6  # Wood
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 16] <- 6  # Wattle and daub
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 7] <- 6  # Mud & Cement
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 19] <- 7  # Stabilised Mud
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 8] <- 8  # Mud
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 20] <- 8  # Traditional Mud
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 9] <- 9  # Plastic
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 21] <- 9  # Other informal
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 22] <- 9  # Carpet
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 23] <- 9  # Dirt
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 24] <- 9  # Mat
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 25] <- 9  # Other traditional
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 26] <- 9  # Dung
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 27] <- 9  # Parquet
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 28] <- 9  # Vinyl
# ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 3] <- 9  # Cardboard

### Create binary variables
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

### Create a new dataframe with just the required data 
### Selecting columns by pattern matching

### list of patterns to be identified in the column names to be included 

col_list <- c("HHIntId",
              "BSIntId",
              "VisitDate",
              "Visit_Year",
              "mv_DrinkWaterSource",
              "mv_ToiletType",
              "mv_MainCookingFuel",
              # "mv_RoofMaterial",
              # "mv_WallMaterial",
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
ass_data <- AC_1.df[,grep(paste(col_list , collapse = "|"), x=names(AC_1.df))]

### Create Vector of column names 

asset_list <- names(ass_data)

### Count number of visits per year per household

# ranking by HHID and Visit Date 

## Ranking by Individual Id , Year, year_diff
ass_data <- ass_data %>%
  group_by(HHIntId,Visit_Year) %>%
  dplyr::mutate(rank = order(order(VisitDate, decreasing=FALSE)))


### If two visits in a year just use first 
ass_data <- ass_data %>% filter(rank==1)


## Select data for 2003 to 2021 and 2023

ass_data_select <- ass_data %>% filter(Visit_Year >= 2003 & Visit_Year != 2022)

#ass_data_select <- ass_data %>% filter(Visit_Year == 2012)

### Vector of unique visit years

year_list <- sort(unique(ass_data_select$Visit_Year))
year_list
#year = 2005
#### Loop through a list of years
for (year in year_list){
  print(as.character(year))
  
  ass_data_year <- ass_data_select %>% filter(Visit_Year == year)
  ##Count NA values for each asset variable
  
  print(paste0("Total number of rows = ",as.character(nrow(ass_data_year)) ))
  
  print(paste0("Total number of rows with missing data = ",as.character(sum(!complete.cases(ass_data_year))) ))
  
  for (var in asset_list){
    print(paste0("Total number of rows with missing data for ",var," = ",as.character(sum(is.na(ass_data_year[var]))) ))
  }
  ### Drop any rows with NA values as this will led to problems with MCA calculation 
  
  print(paste0("Rows before dropping NA values = ",as.character(nrow(ass_data_year))))
  ass_data_tmp <-  na.omit(ass_data_year)
  print(paste0("Rows after dropping NA values = ",as.character(nrow(ass_data_tmp))))
  
  n_zero <- nrow(ass_data_tmp[rowSums(ass_data_tmp[ , c(5:(ncol(ass_data_tmp) - 1))], na.rm=TRUE) == 0,])
  print(paste0("Total number of rows with all asset values equal to zero =  ",as.character(n_zero)))
  
  ### Drop rows with Sum = 0 for all asset values (i.e. all 0 ) 
  ass_data_tmp$ses_sum <- rowSums(ass_data_tmp[ , c(5:(ncol(ass_data_tmp) - 1))], na.rm=TRUE)
  ass_data_tmp <-  dplyr::filter(ass_data_tmp, ses_sum > 0 )
  print(paste0("Rows after dropping all zero values = ",as.character(nrow(ass_data_tmp))))
  
  
  ### Keep first 4 columns then drop columns if all values for SES variables are 0 
  
  ass_data_tmp_head <- ass_data_tmp[c(1:4)]
  ass_data_tmp_tail <- ass_data_tmp[5:(ncol(ass_data_tmp) - 2)]
  
  print(paste0("Number of columns of asset data before dropping columns with all values as 0 = ",as.character(ncol(ass_data_tmp_tail))))
  
  ass_data_tmp_tail <- ass_data_tmp_tail[, colSums(ass_data_tmp_tail) > 0]
  
  print(paste0("Number of columns of asset data after dropping columns with all values as 0 = ",as.character(ncol(ass_data_tmp_tail))))
  
  ass_data_tmp2 <- cbind(ass_data_tmp_head,ass_data_tmp_tail)
  
    ### Convert all columns to factors
  
  ass_data_tmp_tail_fact <-data.frame(lapply(ass_data_tmp_tail,factor))
  
  #### Alternative for PCA 
  #https://stackoverflow.com/questions/67608438/using-r-how-can-i-create-and-index-using-principal-components
  # # run the PCA
  # PCA_results <- principal(df, nfactors = 1)
  # 
  # # add our PCA scores as an index
  # df$index <- PCA_results$scores 
  
  ### MCA alternative approaches 
  ### Fro multiple dimensions scale scores based on % of varianace explained ?? Need to check
  
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
  
  ind <- factoextra::get_mca_ind(res.mca)
  HH_coord <- as.data.frame(ind$coord)
  
  ### Using coord values (factor scores for the 1st component)
  ass_data_tmp2$mca_score <- HH_coord[,1]
  ### Generate Wealth Quintiles
  ass_data_tmp2$wealth_quantile_mca <-  schoRsch::ntiles(ass_data_tmp2, dv = "mca_score", bins=n_quant)
  # 
  # ## Keep HHId, Year , mca_score , wealth_quantile_mca
  # 
  ass_data_ses_year <- ass_data_tmp2[,c('HHIntId','Visit_Year','mca_score','wealth_quantile_mca')]
  # 
  ### Append to overall dataframe      
  if(exists("ass_ses_all")==FALSE) {
    ass_ses_all <- ass_data_ses_year
  } else{
    ass_ses_all <-  rbind(ass_ses_all,ass_data_ses_year)
  }
}