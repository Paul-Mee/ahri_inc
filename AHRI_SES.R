###
### This code takes the AHRI household data and calculates SES rankings for each calendar year
### 

# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# Set file paths
## AHRI data
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'
## Local copy of AHRI R code
code_dir <- 'C:/github/ahri_inc/avdm_ahri_code/R/'

# Define vector of package names

package_names <- c('haven','dplyr','survival','psych','lubridate','schoRsch','data.table',
                   'imputeTS')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

# File paths


### list of R code to be sourced 
### Needs to be kept in sync between two files - better to use github 

file_list <- c("ahri.R","data.R","getARTData.R","getBSData.R","getEpisodes-PM.R",
               "getFiles.R","getHealthData.R","getHIV-PM.R","getIncidence-PM.R","imputeMethods.R",
               "intCens.R","setArgs.R","setData-PM.R","splitData-PM.R","test_ahri.R")

## Source the file in the list
for(i in 1:length(file_list)){
  source(paste0(code_dir,file_list[i]))
}

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



### Loading Household Asset data 

stata_data_file <- '/RD06-99 ACDIS HSE-H All.dta'
ACDIS_hh <- haven::read_dta(paste0(data_dir,stata_data_file))


### Extract year from visit date 

ACDIS_hh$Visit_Year <- lubridate::year(ACDIS_hh$VisitDate)

#ass_hh_tmp <- ACDIS_hh %>% filter(Visit_Year == 2022)

### Move Visit data column
ACDIS_hh  <- ACDIS_hh  %>% relocate(Visit_Year, .after=VisitDate)


### Recoding and normalising variables 
### For each variable recode such that 1 represents the most wealthy and 0 least wealthy 
### these definitions are subjective and could be changed based on local feedback
### For the next version functions could be developed to avoid repetitive lines of code

### Water Source 

ACDIS_hh$water <- NA

## Update codes based on wealth ranking for water sources 
## May need to update definitions 

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

### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$water_norm <- (max(ACDIS_hh$water,na.rm = TRUE) - ACDIS_hh$water)/(max(ACDIS_hh$water,na.rm = TRUE) - min(ACDIS_hh$water,na.rm = TRUE))


### Toilet Types

ACDIS_hh$toilet <- NA

## Update codes based on wealth ranking for toilets 
## May need to update definitions 

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

### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$toilet_norm <- (max(ACDIS_hh$toilet,na.rm = TRUE) - ACDIS_hh$toilet)/(max(ACDIS_hh$toilet,na.rm = TRUE) - min(ACDIS_hh$toilet,na.rm = TRUE))

# table(ACDIS_hh$toilet_norm)
# table(ACDIS_hh$toilet)

# Household electricity Supply

ACDIS_hh$electric <-  NA
ACDIS_hh$electric[as.integer(ACDIS_hh$IsElectrified)== 1] <- 1  # Yes
ACDIS_hh$electric[as.integer(ACDIS_hh$IsElectrified)== 2] <- 0  # No

table(ACDIS_hh$electric)

# Energy - Cooking Fuels 

ACDIS_hh$energy <-  NA

ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 2] <- 1  # Electricity from Generator
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 4] <- 1  # Electricity from Solar
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 3] <- 2  # Electricity from Grid
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 5] <- 3  # Gas(LPG)
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 6] <- 4  # Paraffin
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 1] <- 5  # Coal
ACDIS_hh$energy[as.integer(ACDIS_hh$MainCookingFuel)== 7] <- 6  # Wood

### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$energy_norm <- (max(ACDIS_hh$energy,na.rm = TRUE) - ACDIS_hh$energy)/(max(ACDIS_hh$energy,na.rm = TRUE) - min(ACDIS_hh$energy,na.rm = TRUE))
table(ACDIS_hh$energy_norm)


### Wall materials ### Only used in last 2 or 3 years 

ACDIS_hh$wall_mat <-  NA
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 1] <- 3  # Asbestos
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 2] <- 1  # Bricks
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 3] <- 9  # Cardboard
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 4] <- 2  # Cement Block
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 5] <- 4  # Corrugated iron
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 6] <- 5  # Asbestos
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 7] <- 6  # Mud & Cement
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 8] <- 8  # Mud
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 9] <- 9  # Plastic
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 10] <- 2  # Pre-Fabricated
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 11] <- 3  # Sail
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 12] <- 6  # Stone & Lath
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 13] <- 6  # Thatching
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 14] <- 2  # Tile
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 15] <- 6  # Wood
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 16] <- 6  # Wattle and daub
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 18] <- 2  # Other modern
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 19] <- 7  # Stabilised Mud
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 20] <- 8  # Traditional Mud
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 21] <- 9  # Other informal
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 22] <- 9  # Carpet
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 23] <- 9  # Dirt
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 24] <- 9  # Mat
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 25] <- 9  # Other traditional
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 26] <- 9  # Dung
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 27] <- 9  # Parquet
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 28] <- 9  # Vinyl


### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$wall_mat_norm <- (max(ACDIS_hh$wall_mat,na.rm = TRUE) - ACDIS_hh$wall_mat)/(max(ACDIS_hh$wall_mat,na.rm = TRUE) - min(ACDIS_hh$wall_mat,na.rm = TRUE))
table(ACDIS_hh$wall_mat_norm)

### data check 

test.df  <- ACDIS_hh %>% dplyr::select('VisitDate','Visit_Year','WallMaterial','wall_mat','wall_mat_norm')


### Roof materials

ACDIS_hh$roof_mat <-  NA
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 1] <- 3  # Asbestos
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 2] <- 1  # Bricks
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 3] <- 9  # Cardboard
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 4] <- 2  # Cement Block
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 5] <- 4  # Corrugated iron
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 6] <- 5  # Asbestos
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 7] <- 6  # Mud & Cement
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 8] <- 8  # Mud
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 9] <- 9  # Plastic
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 10] <- 2  # Pre-Fabricated
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 11] <- 3  # Sail
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 12] <- 6  # Stone & Lath
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 13] <- 6  # Thatching
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 14] <- 2  # Tile
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 15] <- 6  # Wood
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 16] <- 6  # Wattle and daub
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 18] <- 2  # Other modern
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 19] <- 7  # Stabilised Mud
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 20] <- 8  # Traditional Mud
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 21] <- 9  # Other informal
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 22] <- 9  # Carpet
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 23] <- 9  # Dirt
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 24] <- 9  # Mat
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 25] <- 9  # Other traditional
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 26] <- 9  # Dung
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 27] <- 9  # Parquet
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 28] <- 9  # Vinyl


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
                "VAC")

### Create new dataframe with just required data 

ass_data <- ACDIS_hh[,asset_list]

# ### Extract year from visit date 
# 
ass_data$Visit_Year <- lubridate::year(ass_data$VisitDate)
# 
# ### Move Visit data column
ass_data <- ass_data %>% relocate(Visit_Year, .after=VisitDate)


### Count number of visits per year per household

# ranking by HHID and Visit Date 

## Ranking by Individual Id , Year, year_diff
ass_data <- ass_data %>%
  group_by(HHIntId,Visit_Year) %>%
  dplyr::mutate(rank = order(order(VisitDate, decreasing=FALSE)))


### If two visits in a year just use first 
ass_data <- ass_data %>% filter(rank==1)


### Setting NA values for wall_mat_norm and roof_mat_norm to 0 

# ass_data$wall_mat_norm[is.na(ass_data$wall_mat_norm)] <- 0
# ass_data$roof_mat_norm[is.na(ass_data$roof_mat_norm)] <- 0

### Loop to calculate asset index for each year 

start_year = 2004
end_year = 2021
### No asset ownership data in 2022

n_fact = 3 # Number of PCA factors
n_quant = 5 # Number of quantiles

#i = 2021

for (i in start_year:end_year) {

  ## filter for year 
  ass_data_tmp <- ass_data %>% filter(Visit_Year == i)
  if(nrow(ass_data_tmp) > 1 ){
    print (i)
  }
  else {
    next
  }
  
  ### Drop any rows with NA values 
  print(paste0("Rows before dropping NA values = ",as.character(nrow(ass_data_tmp))))
  ass_data_tmp <-  na.omit(ass_data_tmp)
  print(paste0("Rows after dropping NA values = ",as.character(nrow(ass_data_tmp))))
  ### Drop columns with Sum = 0 for all asset values (i.e. all 0 ) 
  ass_data_tmp$ses_sum <- rowSums(ass_data_tmp[ , c(5:(ncol(ass_data_tmp) - 1))], na.rm=TRUE)
  ass_data_tmp <-  dplyr::filter(ass_data_tmp, ses_sum > 0 )
  print(paste0("Rows after dropping all zero values = ",as.character(nrow(ass_data_tmp))))
  
  ### Split data frame to asset data and identifiers
  # ass_data_tmp_head <- ass_data_tmp[c(1,4)]
  # ass_data_tmp_tail <- ass_data_tmp[5:(ncol(ass_data_tmp) - 2)]
  
  
  ### Keep first 4 columns then drop columns if all values for SES variables are 0 
  
  ass_data_tmp_head <- ass_data_tmp[c(1,4)]
  ass_data_tmp_tail <- ass_data_tmp[5:(ncol(ass_data_tmp) - 2)]
  
  ass_data_tmp_tail <- ass_data_tmp_tail[, colSums(ass_data_tmp_tail) > 0]

  ass_data_tmp <- cbind(ass_data_tmp_head,ass_data_tmp_tail)
  
  
  # Run PCA extract n_fact factors 
  prn<-psych::principal(ass_data_tmp[5:(ncol(ass_data_tmp))], rotate="varimax", nfactors=n_fact,covar=T, scores=TRUE)
  # Calculate Wealth quantiles 1 = poorest to n = richest
  ass_data_tmp$prn_score <- prn$scores[,1]
  ass_data_tmp$wealth_quantile <-  schoRsch::ntiles(ass_data_tmp, dv = "prn_score", bins=n_quant)
  
  ## Keep HHId, Year , prn_score , wealth_quantile
  
  ass_data_ses <- ass_data_tmp[,c('HHIntId','Visit_Year','prn_score','wealth_quantile')]
  
  ### Append to overall dataframe      
  if(exists("ass_ses_all")==FALSE) {
    ass_ses_all <- ass_data_ses
  } else{ 
    ass_ses_all <-  rbind(ass_ses_all,ass_data_ses)
    }
 
  i = i+ 1 
  }
 

### To get values for each Household for each  year 
### Get a df with Each HHId and each Visit Year

all_HH.df <- as.data.frame(unique(ass_ses_all$HHIntId))
## Merge with list of all years  
years.df  <- as.data.frame(seq(from=start_year, to=end_year, by=1 ))
HH_years.df <- cross_join(years.df,all_HH.df)
names(HH_years.df)[1] <- "Visit_Year"
names(HH_years.df)[2] <- "HHIntId"

### Merge this with ass_ses_all
ass_ses_full <- merge(HH_years.df,ass_ses_all,by=c('Visit_Year','HHIntId'),all.x=TRUE)

### Impute missing wealth quantiles
### Sort by HHIntId,Year
ass_ses_full <- ass_ses_full[order(ass_ses_full$HHIntId,ass_ses_full$Visit_Year),]


##https://stackoverflow.com/questions/60574665/impute-missing-with-interpolation-by-groups

### locf - where data missing carry over last observation

ass_ses_full$wealth_quant.imp <- with(ass_ses_full, ave(wealth_quantile
                                      ,FUN=imputeTS::na_locf))




#### Merge with Epi data 

stata_data_file <- "/SurveillanceEpisodesHIV.dta"
ACDIS_epi <- haven::read_dta(paste0(data_dir,stata_data_file))
# 
# ### Select Visit_Year as mid point in episode from epi file 
# ### Merge on HHIntId = HouseholdId and Visit_Year = Mid_Visit_year
# 
ACDIS_epi$Start_Year <- lubridate::year(ACDIS_epi$StartDate)
ACDIS_epi$End_Year <- lubridate::year(ACDIS_epi$EndDate)

ACDIS_epi$Mid_Year <- as.integer((ACDIS_epi$Start_Year + ACDIS_epi$End_Year)/2)


ACDIS_epi_quant <- merge(ACDIS_epi,ass_ses_full,by.x = (c("HouseholdId","Mid_Year")),
                                                 by.y= (c("HHIntId","Visit_Year")),all.x=TRUE)

## Now get data for each individual for each year 

ACDIS_Ind_SES <- ACDIS_epi_quant[,c('Mid_Year','IIntId','wealth_quant.imp')]

### Keep if Years > start_year

ACDIS_Ind_SES <-  ACDIS_Ind_SES %>% dplyr::filter(Mid_Year >= start_year)

all_Id.df <- as.data.frame(unique(ACDIS_Ind_SES$IIntId))
## Merge with list of all years  
Id_years.df <- cross_join(years.df,all_Id.df)
names(Id_years.df)[1] <- "Visit_Year"
names(Id_years.df)[2] <- "IIntId"


### Merge this with ACDIS_Ind_SES
ACDIS_Ind_SES_full <- merge(Id_years.df,ACDIS_Ind_SES,by.x=c('Visit_Year','IIntId'),
                                                      by.y=c('Mid_Year','IIntId'),all.x=TRUE)

### Impute missing wealth quantiles
### Sort by IIntId,Year
ACDIS_Ind_SES_full <- ACDIS_Ind_SES_full[order(ACDIS_Ind_SES_full$IIntId,ACDIS_Ind_SES_full$Visit_Year),]

##https://stackoverflow.com/questions/60574665/impute-missing-with-interpolation-by-groups

### locf - where data missing carry over last observation

ACDIS_Ind_SES_full$wealth_quant.imp2 <- with(ACDIS_Ind_SES_full, ave(wealth_quant.imp
                                                        ,FUN=imputeTS::na_locf))

ACDIS_Ind_SES_full <- ACDIS_Ind_SES_full[c('Visit_Year','IIntId','wealth_quant.imp2')]
names(ACDIS_Ind_SES_full)[3] <- "wealth_quantile"


R_fname_SES <- paste0(data_dir,"/Surv_SES_Data.RDS")
### Saving as RDS file
saveRDS(ACDIS_Ind_SES_full  , file = R_fname_SES)


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

ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(1,2)] <- 0
ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(3,4,5,6,7)] <- 1
ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(8,9,10)] <- 2
ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(11,12,13)] <- 3
ACDIS_edu$highest_edu[ACDIS_edu$HighestSchoolLevel %in% c(14,15)] <- 4
ACDIS_edu$highest_edu[ACDIS_edu$HighestTertiaryLevel %in% c(16,17,18,19,20)] <- 5

#table(ACDIS_edu$highest_edu)

### What to do when highest education reported decreases over time ? 

ACDIS_edu$highest_edu_fact <- as.factor(ACDIS_edu$highest_edu)

### Save as RDS file 

R_fname_edu <- paste0(data_dir,"/Ind_Edu_Data.RDS")
### Saving as RDS file
saveRDS(ACDIS_edu  , file = R_fname_edu)

### Interpolate missing data ?


