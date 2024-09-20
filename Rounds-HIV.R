####
# Clear any existing data from the data set
rm(list = ls())
#####

# Define vector of package names

package_names <- c('haven','dplyr')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023'
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'


code_dir <- 'C:/github/ahri_inc/avdm_ahri_code/R/'

## This is a download of the files from Alain Vandermael's AHRI R library 
## The files with suffix 'PM' have been updated to reflect changes in data file names 
## or R packages . 

### list of R code to be sourced

file_list <- c("ahri.R","data.R","getARTData.R","getBSData-PM.R","getEpisodes-PM.R",
               "getFiles.R","getHealthData.R","getHIV-PM.R","getIncidence-PM.R","imputeMethods.R",
               "intCens.R","setArgs.R","setData-PM.R","splitData-PM.R","test_ahri.R")

## Source the file in the list
for(i in 1:length(file_list)){
  source(paste0(code_dir,file_list[i]))
}

getFiles = setFiles("C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023")
setFiles(
  folder = "C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023",
  hivfile = "RD05-99 ACDIS HIV All-2024.dta",
)





hiv_all = readHIVData(dropTasP = TRUE, addVars = "DSRound")

hiv_all$DSRound = as.factor(hiv_all$DSRound)

###Filter Data to make temp dataset
hiv_all_tmp <- hiv_all[(hiv_all$IIntID %in% c(16,17)), ]

### Output as a csv file 
output_fname <- '/hiv_all_temp.csv'

write.csv(hiv_all_tmp,paste0(output_dir,output_fname),row.names=FALSE)



### Drop rounds 3,7 and 8 - preumably early years of the census
hiv_all = hiv_all[!(hiv_all$DSRound %in% c(3,7,8)), ]

#### Drop unused levels i.e. levels just dropped for rounds 3,7 and 8 
hiv_all$DSRound = droplevels(hiv_all$DSRound)
#### Keep only Southern PIPSA
hiv_all = hiv_all[hiv_all$PIPSA == "Southern PIPSA" | is.na(hiv_all$PIPSA),]

#### Check for multiple individual visits per year 

hiv_tmp <- hiv_all 
hiv_tmp$Visit_Year <- lubridate::year(hiv_tmp$VisitDate)
hiv_tmp_ind <- hiv_tmp[c('IIntID','DSRound','HIVPositive')]

tmp_count <- hiv_tmp_ind %>%
             group_by(IIntID,DSRound,HIVPositive) %>%
             dplyr::summarise(ind_visit_count = n(),)

rounds = hiv_all %>% 
         group_by(DSRound) %>% 
         dplyr::summarise(earliest_visit_date =min(VisitDate), 
                          latest_visit_date = max(VisitDate), 
                          m_year =mean.Date(as.Date(VisitDate), 
                          format=c("%Y-%m-%d")), 
                          Round_Year = substr(m_year, 1,4),
                          hiv_visit_count = n(),)

### Output as a csv file 
output_fname <- '/hiv_rounds.csv'

write.csv(rounds,paste0(output_dir,output_fname),row.names=FALSE)


##remove missing last row
rounds = rounds[complete.cases(rounds),]


### These steps combine 2003 and 2004 data and use the mid year for the original 2004 data
rounds$mid_year = rounds$m_year
rounds$mid_year[rounds$Round_Year == 2004]<-mean.Date(rounds$m_year[rounds$Round_Year==2004])

rounds$Round_Year = substr(rounds$mid_year, 1,4)

rounds$final_mid_year = rounds$mid_year
rounds$final_mid_year[rounds$Round_Year == 2003] <-unique(rounds$mid_year[rounds$Round_Year == 2004])

##change 2003  to 2004 
rounds$Round_Year = substr(rounds$final_mid_year, 1,4)

### Summarise revised round_year data 

rounds_hiv = rounds %>% 
  group_by(Round_Year) %>% 
  dplyr::summarise(earliest_visit =min(earliest_visit_date), 
                   latest_visit = max(latest_visit_date),
                   hiv_visit_count = sum(hiv_visit_count))

### Save file for later plotting
save(rounds_hiv, file = paste0(output_dir,'/hiv_rounds.RData'))

### Output as a csv file 
output_fname <- '/hiv_round_summary.csv'

write.csv(round_summ,paste0(output_dir,output_fname),row.names=FALSE)

##Assign roundyear to hiv_data

nrounds = rounds[, c(1,5,7)]

hiv = merge(hiv_all, nrounds, by = "DSRound")

### Count records per Round_Year

DS_round_year_counts <- table(hiv$Round_Year)
print.table(DS_round_year_counts)

#  2004  2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021  2022 
# 13490  9456  8470  9900  9790  8845 11100 10365  7882  9868  9447 13059 14683 11477 13229  9040  3155  8722 15539 



##writexl::write_xlsx(rounds, "nrounds.xlsx")


##hiv prevalence

#prev = hiv %>% group_by(BSIntID, Round_Year) %>% dplyr::summarise(Tot = n(), pos = sum(HIVResult), neg = Tot-pos,HIVprev = sum(HIVResult)/n())


