###
### This code takes the AHRI household data and calculates SES rankings for households and 
### individuals in those households 
### In this version SES quantiles are calculated within each year 
### Variables are only included which have been consistently collected over time
### in order to allow comparison of changes over time
### Interpolation is used to substitute for missing data 
### Two methods are use to generate the summary Wealth Index , Factor Analysis and Principal Components  Analysis 
### 

# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
# Ensure only the SA CRAN repository is accessed 
options(repos = getOption("repos")["CRAN"])

# Set file paths
## AHRI data
data_dir <- 'E:/PaulMee/HDSS'
output_dir <- 'E:/PaulMee/Outputs'


# Define vector of package names

package_names <- c('haven','dplyr','survival','psych','lubridate','schoRsch','data.table',
                   'imputeTS','DescTools','PerformanceAnalytics','qgraph','corrplot',
                   'ggplot2','Compind')


# This code installs all the other required packages if they are not currently installed and loads 
# all the libraries

pacman::p_load(char=package_names)


# Define Parameters 

n_quant = 5 # Number of quantiles for SES index 

### Loading Household Asset data 

stata_data_file <- '/RD06-99 ACDIS HSE-H All.dta'
ACDIS_hh <- haven::read_dta(paste0(data_dir,stata_data_file))

ACDIS_hh$Visit_Year <- lubridate::year(ACDIS_hh$VisitDate)

### Move Visit data column
ACDIS_hh  <- ACDIS_hh  %>% relocate(Visit_Year, .after=VisitDate)

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

### Create tab.df which shows the number of time each variable was collected in each year 

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
                "Visit_Year",
                "water_norm",
                "toilet_norm",
                "electric",
                "energy_norm",
                # "wall_mat_norm", 
                #  "roof_mat_norm",
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

### Create new dataframe with just required data 

ass_data <- ACDIS_hh[,asset_list]

### Rename variables 
ass_data <- dplyr::rename(ass_data, 'WAT' = 'water_norm')
ass_data <- dplyr::rename(ass_data, 'TOI' = 'toilet_norm')
ass_data <- dplyr::rename(ass_data, 'ENE' = 'energy_norm')
ass_data <- dplyr::rename(ass_data, 'ELC' = 'electric')

### Count number of visits per year per household

# ranking by HHID and Visit Date 

## Ranking by Individual Id , Year, year_diff
ass_data <- ass_data %>%
  group_by(HHIntId,Visit_Year) %>%
  dplyr::mutate(rank = order(order(VisitDate, decreasing=FALSE)))


### If two visits in a year just use first 
ass_data <- ass_data %>% filter(rank==1)


## Select range of years for data

ass_data_select <- ass_data %>% filter(Visit_Year >= 2003 & Visit_Year <= 2023)
#ass_data_select <- ass_data %>% filter(Visit_Year >= 2005 & Visit_Year <= 2006)

### Vector of unique visit years

year_list <- sort(unique(ass_data_select$Visit_Year))
year_list

#### Loop through a list of years
for (year in year_list){
  print(as.character(year))
  
  ass_data_year <- ass_data_select %>% filter(Visit_Year == year)
  ##Count NA values for each asset variable
  
  print(paste0("Total number of rows = ",as.character(nrow(ass_data_year)) ))
  
  print(paste0("Total number of rows with missing data = ",as.character(sum(!complete.cases(ass_data_year))) ))
  

  for (var in colnames(ass_data_select)){
    print(paste0("Total number of rows with missing data for ",var," = ",as.character(sum(is.na(ass_data_year[var]))) ))
  }
  ### Drop any rows with NA values as this will led to problems with PCA calculation 
  
  print(paste0("Rows before dropping NA values = ",as.character(nrow(ass_data_year))))
  ass_data_tmp <-  na.omit(ass_data_year)
  print(paste0("Rows after dropping NA values = ",as.character(nrow(ass_data_tmp))))
  
  n_zero <- nrow(ass_data_tmp[rowSums(ass_data_tmp[ , c(5:(ncol(ass_data_tmp) - 1))], na.rm=TRUE) == 0,])
  print(paste0("Total number of rows with all asset values equal to zero =  ",as.character(n_zero)))
  
  ### Drop rows with Sum = 0 for all asset values (i.e. all 0 ) 
  ass_data_tmp$ses_sum <- rowSums(ass_data_tmp[ , c(5:(ncol(ass_data_tmp) - 1))], na.rm=TRUE)
  ass_data_tmp <-  dplyr::filter(ass_data_tmp, ses_sum > 0 )
  print(paste0("Rows after dropping all zero values = ",as.character(nrow(ass_data_tmp))))
  
  ass_data_tmp<- as.data.frame(ass_data_tmp)
  rownames(ass_data_tmp) <- ass_data_tmp[,1]
  ass_data_tmp <- ass_data_tmp[5:(ncol(ass_data_tmp)-2)]
  
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
  
  CI_Factor_estimated <-  Compind::ci_factor(ass_data_tmp,
                                             indic_col = (1:ncol(ass_data_tmp)),
                                             method = "CH",
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
  
  ass_data_ses_year <- ass_data_year[,c('HHIntId','Visit_Year','pca_1d_score','wealth_quantile_pca', 'fa_score','wealth_quantile_fa' )]
  
  #ass_data_ses_year <- ass_data_year[,c('HHIntId','Visit_Year','pca_1d_score','wealth_quantile_pca' )]
  
  
  ### Append to overall dataframe      
  if(exists("ass_ses_all")==FALSE) {
    ass_ses_all <- ass_data_ses_year
  } else{ 
    ass_ses_all <-  rbind(ass_ses_all,ass_data_ses_year)
  }
}


### Interpolation for missing data
### Interpolate to get a value for each household for each year
### use locf - carry forward last observation
### If value missing in a particular year use most recent value
### This method will create interpolated values after the  first non-missing value

### Get a df with Each HHId and each Visit Year

all_HH.df <- as.data.frame(unique(ass_ses_all$HHIntId))
## Merge with list of all years from first to last year
min_year <- min(ass_ses_all$Visit_Year)
max_year <- max(ass_ses_all$Visit_Year)

years.df  = as.data.frame(seq(min_year, max_year, 1))
names(years.df)[1] <- "Year"

HH_years.df <- cross_join(years.df,all_HH.df)
names(HH_years.df)[1] <- "Visit_Year"
names(HH_years.df)[2] <- "HHIntId"

# ### Merge this with ass_data_ses
ass_ses_full <- merge(HH_years.df,ass_ses_all,by=c('Visit_Year','HHIntId'),all.x=TRUE)
#

# ### Count number of NA values by year
summary_dat <- ass_ses_full %>% group_by(Visit_Year) %>% summarise(NA_sum = sum(is.na(wealth_quantile_pca)),n_tot = n())
# ### Percentage NA values by year
summary_dat$percent_NA <- summary_dat$NA_sum/summary_dat$n_tot*100
#
#print(n=21,summary_dat)
#

ass_ses_full_imp  <- ass_ses_full %>%
  group_by(HHIntId) %>%
  arrange(Visit_Year) %>%
  mutate(wealth_quant_pca.imp1 = imputeTS::na_locf(wealth_quantile_pca)) %>%
  mutate(wealth_quant_fa.imp1 = imputeTS::na_locf(wealth_quantile_fa))

ass_ses_full_imp  <- ungroup(ass_ses_full_imp )


#### Load data-set with a row for each year an individual was resident in a household
#### To create this file need to run "O_Household_Residence_Episodes.R" once

epi_data_file <- "/House_Res_Episodes.RDS"
ACDIS_epi_full <- readRDS(paste0(output_dir,epi_data_file))


# 
# ## NB this includes years in which no household asset data was collected - hence NA values below are not really missing data 
# 
# ### Merge with asset data 
# 
ACDIS_epi_quant <- merge(ACDIS_epi_full,ass_ses_full_imp,by.x = (c("HouseholdId","Res_Year")),
                         by.y= (c("HHIntId","Visit_Year")),all.x=TRUE)
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

ACDIS_Ind_SES <- ACDIS_epi_quant[,c('Res_Year','IIntId','HouseholdId','pca_1d_score','wealth_quantile_pca',
                                    'wealth_quant_pca.imp1','fa_score','wealth_quantile_fa','wealth_quant_fa.imp1')]


#### Loading Education data 
### Loading Individual level data for Education variables 

stata_data_file <- '/RD07-99 ACDIS HSE-I All.dta'
ACDIS_ind <- haven::read_dta(paste0(data_dir,stata_data_file))

### Extract Visit Year , Id, Highest School level 

ACDIS_edu <- ACDIS_ind[c('IIntId', 'VisitDate', 'HighestSchoolLevel','HighestTertiaryLevel')]
ACDIS_edu$Visit_Year <- lubridate::year(ACDIS_edu$VisitDate)

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

### Getting mapping between HHId and BSId

HH_BS <- unique(ACDIS_hh[c('HHIntId','BSIntId')])

### Merge HH_BS with ACDIS_Ind_SES_edu
ACDIS_Ind_SES_edu_BS <- merge(ACDIS_Ind_SES_edu,HH_BS,by.x=c('HouseholdId'),  by.y = c('HHIntId') )

### Merge with ACDIS_bsi_tmp2

ACDIS_Ind_SES_edu_BS_full <- merge(ACDIS_Ind_SES_edu_BS,ACDIS_bsi_tmp2,by=c('BSIntId') )


### Save as RDS file 

R_fname_edu_bs <- paste0(output_dir,"/Ind_Edu_SES_BS_year.RDS")
### Saving as RDS file
saveRDS(ACDIS_Ind_SES_edu_BS_full  , file = R_fname_edu_bs)