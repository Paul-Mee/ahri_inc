###
### This code takes the AHRI household data and calculates SES rankings for households and 
### individuals in those households 
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


### Define Function - cat_bin - Converts a categorical variable to a series of binary variables each 
### representing a level of the categorical variable as a yes/no
### Input variables -  df = dataframe name
### index_vars = vector of the index column names
### col_name = column name in df to be converted to binary
### max_val = maximum acceptable value of variable


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


# Function which takes a vector containing a list of variables 
# in a dataframe and converts all with a value of 9 to NA
convert_9_to_na <- function(df,vars){
  for (i in vars){
    df[[i]][df[[i]]==9] <- NA
  }
  df
}

# Define Parameters 

n_quant = 3 # Number of quantiles for SES indices 

### Loading Household Asset data 

stata_data_file <- '/RD06-99 ACDIS HSE-H All.dta'
ACDIS_hh <- haven::read_dta(paste0(data_dir,stata_data_file))

### Load Bounded Structures data
stata_data_file <- '/RD01-03 ACDIS BoundedStructures.dta'
ACDIS_BS <- haven::read_dta(paste0(data_dir,stata_data_file))
BS_PIP <- ACDIS_BS[c('BSIntId','PIPSA')]

## Merge with ACDIS_hh and select households with PIPSA = 1 (Southern PIPSA)
ass_data_PIP <- merge(ACDIS_hh,BS_PIP,by='BSIntId',all.x=TRUE)
ass_data_SPIP = ass_data_PIP[(ass_data_PIP$PIPSA %in% c(1)), ]


### Filter to drop rows in round 14 with visit date 2005-02-01 - misassigned dates 
ACDIS_hh <- ACDIS_hh[!(ACDIS_hh$DSRound==14 & ACDIS_hh$VisitDate=="2005-02-01"), ]

### Create a Visit_Year variable
ACDIS_hh$Visit_Year <- lubridate::year(ACDIS_hh$VisitDate)

## No data on ownership of individual assets in Rounds 45 and 46 - drop data for these rounds
ACDIS_hh <- ACDIS_hh %>% filter(DSRound!=45 & DSRound!=46)

### Generate a Round Year Variable which will be used to merge data with other datasets
round_years = ACDIS_hh %>% 
  group_by(DSRound) %>% 
  dplyr::summarise(earliest_visit_date =min(VisitDate), 
                   latest_visit_date = max(VisitDate), 
                   visit_count = n(),
                   m_year =mean.Date(as.Date(VisitDate), 
                                     format=c("%Y-%m-%d")), 
                   Round_Year = substr(m_year, 1,4))

### Keep required variables
round_years <- round_years[c('DSRound','Round_Year')] 

### Merge to household data file
ACDIS_hh <- merge(ACDIS_hh,round_years,by='DSRound')


### Count number of visits per Round Year per household
## Ranking by HH Id , Year, year_diff
ACDIS_hh <- ACDIS_hh %>%
  group_by(HHIntId,Round_Year) %>%
  dplyr::mutate(rank = order(order(VisitDate, decreasing=FALSE)))

### If two visits in a year just use first 
ACDIS_hh <- ACDIS_hh %>% filter(rank==1)

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


### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$water_norm <- (max(ACDIS_hh$water,na.rm = TRUE) - ACDIS_hh$water)/(max(ACDIS_hh$water,na.rm = TRUE) - min(ACDIS_hh$water,na.rm = TRUE))

### Create binary variables for MCA
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Round_Year'), 
                  col_name = 'DrinkWaterSource',
                  max_val=16 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Round_Year')))


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

### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$toilet_norm <- (max(ACDIS_hh$toilet,na.rm = TRUE) - ACDIS_hh$toilet)/(max(ACDIS_hh$toilet,na.rm = TRUE) - min(ACDIS_hh$toilet,na.rm = TRUE))

### Create binary variables for MCA
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Round_Year'), 
                  col_name = 'ToiletType',
                  max_val=14 )
AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Round_Year')))

# Household electricity Supply

ACDIS_hh$electric <-  NA
ACDIS_hh$electric[as.integer(ACDIS_hh$IsElectrified)== 1] <- 1  # Yes
ACDIS_hh$electric[as.integer(ACDIS_hh$IsElectrified)== 2] <- 0  # No


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

### Create binary variables for MCA
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Round_Year'),
                  col_name = 'MainCookingFuel',
                  max_val=7 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Round_Year')))

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

### Scale from 1 (most wealthy to 0 least wealthy)

ACDIS_hh$wall_mat_norm <- (max(ACDIS_hh$wall_mat,na.rm = TRUE) - ACDIS_hh$wall_mat)/(max(ACDIS_hh$wall_mat,na.rm = TRUE) - min(ACDIS_hh$wall_mat,na.rm = TRUE))


### Create binary variables for MCA
tmp.df <- cat_bin(df = 'ACDIS_hh', 
                  index_vars = c('HHIntId','Round_Year'),
                  col_name = 'WallMaterial',
                  max_val=28 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Round_Year')))

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
                  index_vars = c('HHIntId','Round_Year'),
                  col_name = 'RoofMaterial',
                  max_val=28 )

AC_1.df <- merge(AC_1.df,tmp.df,by=(c('HHIntId','Round_Year')))

#### Variables already coded as binary 

# Household electricity Supply

AC_1.df$electric <-  NA
AC_1.df$electric[as.integer(AC_1.df$IsElectrified)== 1] <- 1  # Yes
AC_1.df$electric[as.integer(AC_1.df$IsElectrified)== 2] <- 0  # No

item_list <-      c( "BED","BIC","BLM","CAR","CBE", "CTL", "ECO", "EHP", "EKT", "FRG", "GCK", "HSF", "KLT", "KTS",
                "MCS", "OLS", "PMC", "RAD", "SOF",  "SWM",  "TBC",  "TLL",  "TMB",  "TVS",  "VCR",   "WBR",
                "HWG",  "WSM", "EHT", "PHT", "SHF", "CPT", "VAN", "LOR", "TFV", "FRN","JWT",  "ACN", "DIS",
                "MIC", "PTV","SEC","SWP","TUM","VAC")

### Convert values of 9 to NA for all variables in item_list
AC_1.df <- convert_9_to_na(df = AC_1.df,vars = item_list)
ACDIS_hh <- convert_9_to_na(df = ACDIS_hh,vars = item_list)

### Vector of unique Round_Years
year_list <- sort(unique(as.numeric(ACDIS_hh$Round_Year)))
### year_min = starting Round_Year 
year_min = 2005 
### drop values from year_list if < year_min
filtered_year_list <- year_list[year_list >= year_min]

### Create a dataframe for pca with only asset and identifier data 
header_other_assets_pca <- c('HHIntId','Round_Year','Visit_Year','water_norm','toilet_norm','electric','energy_norm','wall_mat_norm','roof_mat')
### append header_other_assets to the list of items in item_list
analytic_vars_pca <- c(header_other_assets_pca,item_list)
### select only required variables 
analytic_data_pca.df  <- ACDIS_hh[analytic_vars_pca]


### Create a dataframe for mca with only asset and identifier data 
### include 'HHIntId','Round_Year','Visit_Year' and all variables with names starting 'mv_'
header_other_assets_mca <- c('HHIntId','Round_Year','Visit_Year')
header_other_assets_mca <- append(header_other_assets_mca, grep("mv_", names(AC_1.df), value = TRUE))
### append header_other_assets to the list of items in item_list
analytic_vars_mca <- c(header_other_assets_mca,item_list)
### select only required variables 
analytic_data_mca.df  <- AC_1.df[analytic_vars_mca]




#year = 2005
for (year in filtered_year_list){
  print(as.character(year))
  ### This stage carries out the PCA calculation for each year in the filtered_year_list
  print("PCA calculation")
  ass_data_year_pca <- analytic_data_pca.df %>% filter(Round_Year == year)

  ### Drop any columns with <= 5 NA values - 5 is slighlty abritrary
  ass_data_year_pca <- ass_data_year_pca[, colSums(!is.na(ass_data_year_pca)) > 5]
  
  ### Drop any rows with NA values as this will led to problems with PCA calculation 
  ass_data_year_pca <-  na.omit(ass_data_year_pca)
  
  ### Drop rows with Sum = 0 for all asset values (i.e. all 0 ) 
  ass_data_year_pca$ses_sum <- rowSums(ass_data_year_pca[ , c(4:ncol(ass_data_year_pca))], na.rm=TRUE)
  ass_data_year_pca<-  dplyr::filter(ass_data_year_pca, ses_sum > 0 )
  ### Drop ses_sum variable 
  ass_data_year_pca <- dplyr::select(ass_data_year_pca, -c("ses_sum"))
  
  ### Ungroup  and convert to dataframe
  ass_data_year_pca <- ungroup(as.data.frame(ass_data_year_pca))

  ### Use HHIntID as row name for later merge
  rownames(ass_data_year_pca) <- ass_data_year_pca$HHIntId
  
  ### Select columns of values used for PCA calculation
  ass_data_pca_tmp <- ass_data_year_pca[4:(ncol(ass_data_year_pca))]
  
  ### Drop columns with all values for asset data = 0 i.e. no useful asset data collected 
  ass_data_pca_tmp <- ass_data_pca_tmp[, colSums(ass_data_pca_tmp != 0, na.rm = TRUE) > 0]

  ### PCA analysis 
  res.pca <- FactoMineR::PCA(ass_data_pca_tmp, scale.unit = TRUE, ncp = 5, graph = FALSE)
  pca_score <- as.data.frame(res.pca$ind$coord[,1])
  pca_score$HHIntId <- row.names(pca_score)
  #### Add 1st dimension of PCA analysis to ass_data_tmp dataframe
  names(pca_score)[1] <- "pca_1d_score"
  
  #### Merge to original data 
  ass_data_year_pca <- merge(ass_data_year_pca,pca_score,by = 'HHIntId')
  
  check_pca <- ass_data_year_pca
  check_pca$ass_sum <- rowSums(check_pca[, 4:(ncol(check_pca)-1)], na.rm = TRUE)
  check_pca <- check_pca[c('HHIntId','Round_Year','pca_1d_score','ass_sum')]
  
  # Calculate Wealth quantiles 1 = poorest to n = richest
  ass_data_year_pca$wealth_quantile_pca <-  schoRsch::ntiles(ass_data_year_pca, dv = "pca_1d_score", bins=n_quant)
  ass_data_year_pca <- ass_data_year_pca[,c('HHIntId','Round_Year','pca_1d_score','wealth_quantile_pca')]
  
  ### Append PCA scores to overall dataframe      
  if(exists("ass_pca_all")==FALSE) {
    ass_pca_all <- ass_data_year_pca 
  } else{ 
    ass_pca_all <-  rbind(ass_pca_all,ass_data_year_pca)
  }
  
  if(exists("check_pca_all")==FALSE) {
    check_pca_all <- check_pca
  } else{ 
    check_pca_all <-  rbind(check_pca_all,check_pca)
  }
  
  ### This stage carries out the MCA calculation for each year in the filtered_year_list
  print("MCA calculation")
  
  ass_data_year_mca <- analytic_data_mca.df %>% filter(Round_Year == year)
  
  ### Drop any rows with NA values as this will led to problems with MCA calculation 
  ass_data_year_mca <-  na.omit(ass_data_year_mca)
  
  ### Drop rows with Sum = 0 for all asset values (i.e. all 0 ) 
  ass_data_year_mca$ses_sum <- rowSums(ass_data_year_mca[ , c(4:ncol(ass_data_year_mca))], na.rm=TRUE)
  ass_data_year_mca <-  dplyr::filter(ass_data_year_mca, ses_sum > 0 )

  ### Assign HHIntId row names for later merge
  rownames(ass_data_year_mca) <- ass_data_year_mca$HHIntId
  
  ### Create header and tail columns for analysis 
  ass_data_mca_head <- ass_data_year_mca[c('HHIntId','Round_Year')]
  ass_data_mca_tail <- ass_data_year_mca[4:ncol(ass_data_year_mca)]

  ### Convert all columns in ass_data_tmp_tail2 to factors keeping row labels
  ass_data_mca_tail_fact <- data.frame(lapply(ass_data_mca_tail, as.factor), row.names = rownames(ass_data_mca_tail))
  
  ### MCA analysis
  res.mca <- FactoMineR::MCA(ass_data_mca_tail_fact, graph = FALSE)
  
  ## The function get_mca_ind() [in factoextra] is used to extract the results for individuals. 
  ## This function returns a list containing the coordinates, the cos2 and the contributions of individuals:
  
  ind <- factoextra::get_mca_ind(res.mca)
  HH_coord <- as.data.frame(ind$coord)
  
  HH_coord$HHIntId <- row.names(HH_coord )
  
  ## Extract first dimension MCA score 
  HH_mca <- HH_coord[c('HHIntId', 'Dim 1')]
  names(HH_mca)[2] <- "mca_1d_score"
  
  ### To account for opposite directionality in MCA scores multiply scores by -1
  HH_mca$mca_1d_score <- -1*HH_mca$mca_1d_score 
  
  ### Generate Wealth Quintiles
  HH_mca$wealth_quantile_mca <-  schoRsch::ntiles(HH_mca, dv = "mca_1d_score", bins=n_quant)

  HH_mca$Round_Year <- as.character(year)
  
  ### Append to overall dataframe      
  if(exists("ass_mca_all")==FALSE) {
    ass_mca_all <- HH_mca 
  } else{ 
    ass_mca_all <-  rbind(ass_mca_all,HH_mca)
  }
}




### Interpolation for missing data
### For each HHIntID interpolate missing values for wealth quantile using Last Observation Carried Forward
### for Round Years after household asset data was first collected for the household

#### Interpolation for PCA data 
ass_pca_all <- ass_pca_all %>%
  group_by(HHIntId) %>%
  mutate(Min_Round_Year = as.numeric(min(Round_Year))) %>%
  ungroup()  # Ungroup after the operation

### Convert filtered_year_list to a dataframe 
year_list.df <- as.data.frame(filtered_year_list)

# Create all possible combinations of HHIntId and Round_Year
all_combinations <- expand.grid(
  HHIntId = unique(ass_pca_all$HHIntId),
  filtered_year_list
)
### Change variable names and types to allow a join
all_combinations <- rename(all_combinations, Round_Year = Var2)
ass_pca_all$Round_Year <- as.numeric(ass_pca_all$Round_Year)

# Identify missing combinations
missing_rows <- anti_join(all_combinations, ass_pca_all, by = c("HHIntId", "Round_Year"))

missing_rows$pca_1d_score <- NA
missing_rows$wealth_quantile_pca <- NA
missing_rows$Min_Round_Year <- 9999

ass_pca_all_complete <- bind_rows(ass_pca_all,missing_rows)

### Minimum of Round Years in orginal data
ass_pca_all_complete <- ass_pca_all_complete %>%
  group_by(HHIntId) %>%
  mutate(Min_Orig_Round_Year = as.numeric(min(Min_Round_Year))) %>%
  ungroup()  # Ungroup after the operation

### filter ass_ses_all_complete to drop rows where Round_Year < Min_Orig_Round_Year
ass_pca_all_complete <- ass_pca_all_complete %>% filter(Round_Year >= Min_Orig_Round_Year)

### Select required columns 
ass_pca_all_complete <- ass_pca_all_complete[c(1:4)]


#### Impute for all vlaues after first present values with LOCF

ass_pca_all_complete  <- ass_pca_all_complete %>%
  group_by(HHIntId) %>%
  arrange(Round_Year) %>%
  mutate(wealth_quant_pca.imp1 = imputeTS::na_locf(x=wealth_quantile_pca, option = "locf", na_remaining = "keep"))

ass_pca_imp  <- ungroup(ass_pca_all_complete)

#### save SES data file 
save(ass_pca_imp, file = paste0(output_dir,'/ass_pca_imp.RData'))


#### Interpolation for MCA data 
ass_mca_all <- ass_mca_all %>%
  group_by(HHIntId) %>%
  mutate(Min_Round_Year = as.numeric(min(Round_Year))) %>%
  ungroup()  # Ungroup after the operation

### Convert filtered_year_list to a dataframe 
year_list.df <- as.data.frame(filtered_year_list)

# Create all possible combinations of HHIntId and Round_Year
all_combinations <- expand.grid(
  HHIntId = unique(ass_mca_all$HHIntId),
  filtered_year_list
)
### Change variable names and types to allow a join
all_combinations <- rename(all_combinations, Round_Year = Var2)
ass_mca_all$Round_Year <- as.numeric(ass_mca_all$Round_Year)

# Identify missing combinations
missing_rows <- anti_join(all_combinations, ass_mca_all, by = c("HHIntId", "Round_Year"))

missing_rows$mca_1d_score <- NA
missing_rows$wealth_quantile_mca <- NA
missing_rows$Min_Round_Year <- 9999

ass_mca_all_complete <- bind_rows(ass_mca_all,missing_rows)

### Minimum of Round Years in orginal data
ass_mca_all_complete <- ass_mca_all_complete %>%
  group_by(HHIntId) %>%
  mutate(Min_Orig_Round_Year = as.numeric(min(Min_Round_Year))) %>%
  ungroup()  # Ungroup after the operation

### filter ass_ses_all_complete to drop rows where Round_Year < Min_Orig_Round_Year
ass_mca_all_complete <- ass_mca_all_complete %>% filter(Round_Year >= Min_Orig_Round_Year)

### Select required columns 
ass_mca_all_complete <- ass_mca_all_complete[c(1:4)]


#### Impute for all vlaues after first present values with LOCF

ass_mca_all_complete  <- ass_mca_all_complete %>%
  group_by(HHIntId) %>%
  arrange(Round_Year) %>%
  mutate(wealth_quant_mca.imp1 = imputeTS::na_locf(x=wealth_quantile_mca, option = "locf", na_remaining = "keep"))

ass_mca_imp  <- ungroup(ass_mca_all_complete)

#### save SES data file 
save(ass_mca_imp, file = paste0(output_dir,'/ass_mca_imp.RData'))


#### To check data create scatter plots of pca vs mca for each year 

all_data <- merge(ass_mca_imp,ass_pca_imp,by=c('HHIntId','Round_Year'),all.x =TRUE)

# Filter the dataframe for only the years in the filtered list
all_data_filtered <- all_data %>%
  filter(Round_Year %in% filtered_year_list)

# Create the scatter plot
scatter_plot <- ggplot(all_data_filtered, aes(x = mca_1d_score, y = pca_1d_score)) +
  geom_point(alpha = 0.7, color = "blue") + # Scatter plot points
  facet_wrap(~Round_Year, scales = "free") + # Create a tile for each year
  labs(
    title = "Scatter Plot of MCA vs PCA Scores by Year",
    x = "MCA 1D Score",
    y = "PCA 1D Score"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"), # Customize facet labels
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold") # Center title
  )

scatter_plot

#### Now check that PCA scores are positively associated with number of assets each year 

# Create the scatter plot for checking PCA data
scatter_plot_check_pca <- ggplot(check_pca_all, aes(x = pca_1d_score, y = ass_sum)) +
  geom_point(alpha = 0.7, color = "blue") + # Scatter plot points
  facet_wrap(~Round_Year, scales = "free") + # Create a tile for each year
  labs(
    title = "Scatter Plot of  PCA Scores vs Sum of assets by Year",
    x = "PCA 1D Score",
    y = "Asset sum"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"), # Customize facet labels
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold") # Center title
  )

print(scatter_plot_check_pca)


