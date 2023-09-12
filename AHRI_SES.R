###
### This code takes the AHRI household data and calculates SES rankings for each calendar year
### 

# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# The first thing to do is set the file paths to the AHRI datasets. 


data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'

# Define vector of package names

package_names <- c('haven','dplyr','survival','psych','lubridate','schoRsch','plyr','data.table')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)


### Loading Household Asset data 

stata_data_file <- '/RD06-99 ACDIS HSE-H All.dta'
ACDIS_hh <- haven::read_dta(paste0(data_dir,stata_data_file))


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


### Wall materials 

ACDIS_hh$wall_mat <-  NA
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 1] <- 3  # Asbestos
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 2] <- 1  # Bricks
ACDIS_hh$wall_mat[as.integer(ACDIS_hh$WallMaterial)== 3] <- 9  # Cardboard
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



### Roof materials

ACDIS_hh$roof_mat <-  NA
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 1] <- 3  # Asbestos
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 2] <- 1  # Bricks
ACDIS_hh$roof_mat[as.integer(ACDIS_hh$RoofMaterial)== 3] <- 9  # Cardboard
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
ACDIS_hh$BED <- as.integer(ACDIS_hh$BED)
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

asset_list <- c("HHIntId",
                "BSIntId",
                "VisitDate",
                "water_norm",
                "toilet_norm",
                "electric",
                "energy_norm",
                # "wall_mat_norm", 
                # "roof_mat_norm",
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

### Create new dataframe with just identifier, year and asset data 

ass_data <- ACDIS_hh[,asset_list]

### Extract year from visit date 

ass_data$Visit_Year <- lubridate::year(ass_data$VisitDate)

### Move Visit data column
ass_data <- ass_data %>% relocate(Visit_Year, .after=VisitDate)


### Count number of visits per year per household

# ranking by HHID and Visit Date 

## Convert to data.table
ass_data <- data.table(ass_data)
## Use data.table::setkey to sort/index by State, then rate, then hospital name
data.table::setkey(ass_data,HHIntId,Visit_Year,VisitDate)
ass_data[,rank := seq_len(.N),by = .(HHIntId,VisitDate)]

### If two visits in a year just use first 
ass_data <- ass_data %>% filter(rank==1)

ass_data <- data.frame(ass_data)

### Loop to calculate asset index for each year 

start_year = 2004
end_year = 2023

#i = 2020

for (i in start_year:end_year) {

  ## filter for year 
  ass_data_tmp <- ass_data %>% filter(Visit_Year == i)
  if(nrow(ass_data_tmp > 1 )){
    print (i)
  ### Drop any rows with NA values 
  ass_data_tmp <-  na.omit(ass_data_tmp)
  ### Split data frame to asset data and identifiers
  ass_data_tmp_head <- ass_data_tmp[1:4]
  ass_data_tmp_tail <- ass_data_tmp[5:(ncol(ass_data_tmp) - 1)]
  ### Drop columns with Sum = 0 (i.e. all 0 ) 
  ass_data_tmp_tail <-  ass_data_tmp_tail[, colSums(ass_data_tmp_tail != 0) > 0]

  # Run PCA 
  prn<-psych::principal(ass_data_tmp_tail, rotate="varimax", nfactors=3,covar=T, scores=TRUE)
  #prn<-psych::principal(ass_data_tmp[,4:10], rotate="varimax", nfactors=3,covar=T, scores=TRUE)
  # Calculate Wealth quintiles 1 = poorest to 5 = richest
  ass_data_tmp_head$prn_score <- prn$scores[,1]
  ass_data_tmp_head$wealth_quintile <-  schoRsch::ntiles(ass_data_tmp_head, dv = "prn_score", bins=5)
  
  ### Append to overall dataframe      
  if(exists("ass_data_all")==FALSE) {
  ass_data_all <- ass_data_tmp_head}
    else{
  ass_data_all <-  rbind(ass_data_all,ass_data_tmp_head)}
 i = i+ 1 
  }
}  


  








