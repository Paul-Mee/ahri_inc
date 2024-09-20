#### Plotting of date ranges for multiple AHRI data sets 

####
# Clear any existing data from the data set
rm(list = ls())
#####

# Define vector of package names

package_names <- c('haven','dplyr','reshape2','ggplot2')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023'
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'

#### Load rounds data 
#### HIV data - saved from Rounds-HIV.R
load(paste0(output_dir,"/hiv_rounds.RData"))
#### Asset data - saved from Rounds-asset-data.R
load(paste0(output_dir,"/hh_rounds.RData"))
#### Education data - saved from Rounds-education-data.R
load(paste0(output_dir,"/edu_rounds.RData"))

#### Select common variables
rounds_hiv <- rounds_hiv[c('Round_Year','earliest_visit','latest_visit')]
rounds_asset <- rounds_asset[c('Round_Year','earliest_visit_date','latest_visit_date')]
rounds_education <- rounds_asset[c('Round_Year','earliest_visit_date','latest_visit_date')]

#### Rename variables
rounds_hiv <- dplyr::rename(rounds_hiv,earliest_visit_date=earliest_visit)
rounds_hiv <- dplyr::rename(rounds_hiv,latest_visit_date=latest_visit)

### Add suffix to Round_Year fields
rounds_hiv$Round_Year <- paste0(rounds_hiv$Round_Year,"-HIV")
rounds_asset$Round_Year <- paste0(rounds_asset$Round_Year,"-ASSET")
rounds_education$Round_Year <- paste0(rounds_education$Round_Year,"-EDU")

### Stack rows from the two files
#rounds_all <- rbind(rounds_hiv,rounds_asset)

#### Plotting Date ranges for each Round_Year
###https://stackoverflow.com/questions/56164195/r-code-to-plot-a-date-range-as-a-bar-or-line-for-a-number-of-categorical-variabl

### melt to get the data in long format
rounds_hiv_long <- reshape2::melt(rounds_hiv, measure.vars = c("earliest_visit_date", "latest_visit_date"))
rounds_asset_long <- reshape2::melt(rounds_asset, measure.vars = c("earliest_visit_date", "latest_visit_date"))
rounds_education_long <- reshape2::melt(rounds_education, measure.vars = c("earliest_visit_date", "latest_visit_date"))

#### subset data before and after 2015

rounds_hiv_long_2015 <- subset(rounds_hiv_long, (as.integer(substr(rounds_hiv_long$Round_Year,1,4))) <= 2015)  
rounds_asset_long_2015 <- subset(rounds_asset_long, (as.integer(substr(rounds_asset_long$Round_Year,1,4))) <= 2015) 
rounds_education_long_2015 <- subset(rounds_education_long, (as.integer(substr(rounds_education_long$Round_Year,1,4))) <= 2015) 

## Plot in ggplot
rds_2015 <- ggplot(rounds_hiv_long_2015, aes(x=value, y=Round_Year)) +
  geom_line(size=3,colour = "red") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Round Years") +
  theme_minimal() +
  theme(aspect.ratio = 0.5, axis.text = element_text(size = 7)) +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  geom_line(data=rounds_asset_long_2015, size=3,colour = "black") +
  geom_line(data=rounds_education_long_2015, size=3,colour = "blue") 
rds_2015 
ggsave(paste0(output_dir,'/rounds_2015.jpg'), plot = rds_2015)  

#### subset data before and after 2015

rounds_hiv_long_2023 <- subset(rounds_hiv_long, (as.integer(substr(rounds_hiv_long$Round_Year,1,4))) > 2015)  
rounds_asset_long_2023 <- subset(rounds_asset_long, (as.integer(substr(rounds_asset_long$Round_Year,1,4))) > 2015) 
rounds_education_long_2023 <- subset(rounds_education_long, (as.integer(substr(rounds_education_long$Round_Year,1,4))) > 2015) 

## Plot in ggplot
rds_2023 <- ggplot(rounds_hiv_long_2023, aes(x=value, y=Round_Year)) +
  geom_line(size=3,colour = "red") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Round Years") +
  theme_minimal() +
  theme(aspect.ratio = 0.5, axis.text = element_text(size = 7)) +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  geom_line(data=rounds_asset_long_2023, size=3,colour = "black") +
  geom_line(data=rounds_education_long_2023, size=3,colour = "blue") 
rds_2023
ggsave(paste0(output_dir,'/rounds_2023.jpg'), plot = rds_2023)  