####
#### This script load the HIV , SES and education data and merges the three files
#### Before carrying out the Kaplen Meier plots and Cox Survival Analyses
####

####
# Clear any existing data from the data set
rm(list = ls())
#####

# Define vector of package names

package_names <- c('dplyr','ggplot2','ggsurvfit','survminer','survival','lubridate','ggpubr','grid',
                   'viridis', 'gt', 'flextable','officer')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023'
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'

#### Load rounds data 
#### HIV data - saved from 1_Load_AHRI_HIV_Data.R
load(paste0(output_dir,"/ACDIS_hiv_full.RData"))
#### Asset data - saved from 2_Generate_AHRI_SES_Index.R
load(paste0(output_dir,"/ass_pca_imp.RData"))
#### education data - saved from 3_Generate_AHRI_edu_Data.R
load(paste0(output_dir,"/ACDIS_edu_all.RData"))

#### Merge HIV episode data with education data on Round_Year and IIntID

HIV_edu <- merge(hiv_imput_full,ACDIS_edu_all,by.x = c('IIntID','Round_Year'),by.y=c('IIntId','Round_Year'),all.x=TRUE)

#### Merge this file with asset data on Household ID and Round_Year

HIV_edu_SES <- merge(HIV_edu,ass_pca_imp,by.x = c('HouseholdId_imp','Round_Year'),by.y=c('HHIntId','Round_Year'),all.x=TRUE)

#### Create Sex and Age Category variables

### Generate categorical age group variable
### age_cat 15-25,25-40,40-65, >65

HIV_edu_SES$age_cat <-  ifelse((HIV_edu_SES$Age >= 15 & HIV_edu_SES$Age < 25), "15-24",
                                          ifelse((HIV_edu_SES$Age >= 25 & HIV_edu_SES$Age < 40), "25-39",
                                                 ifelse((HIV_edu_SES$Age >= 40 & HIV_edu_SES$Age < 65), "40-64",
                                                        ifelse((HIV_edu_SES$Age >= 65 ), ">65",F))))




HIV_edu_SES$age_cat  <- factor(HIV_edu_SES$age_cat , levels = c("15-24", "25-39" , "40-64",   ">65"),
                                          labels = c("15-24", "25-39" , "40-64",   ">65"))

### Sex as a factor 
HIV_edu_SES$sex <- factor(HIV_edu_SES$Female,  levels = c(0, 1),
                                     labels = c("Male", "Female"))
# Save data to an RDS file in output_dir
saveRDS(HIV_edu_SES, file = paste0(data_dir, "/HIV_edu_SES.RDS"))


###
### Aggregate HIV_Edu_SES by Round_Year and wealth_quant_mca.imp1 and
### calculate the sum of time as total_PY and sero_event as  total_sero_events in each category 
### then calculate incidence 
### Then plot the raw data and smoothed curve through the data points 
### as crosses in ggplot with Round_Year on the X-axis and incidence on the Y-axis
### there will be one line for each strata of wealth_quant_mca.imp1
### use different colours for each strata

#### Original SES data 
aggregated_data_raw <- HIV_edu_SES %>%
  group_by(Round_Year, wealth_quantile_pca) %>%
  summarise(
    total_PY = sum(Time, na.rm = TRUE)/365.25,
    total_sero_events = sum(sero_event, na.rm = TRUE),
    incidence = total_sero_events / total_PY*100
  ) %>%
  ungroup()

### Drop NA values for incidence from aggregated_data
aggregated_data_raw <- na.omit(aggregated_data_raw)
### Create variable SES in which wealth_quant_mca.imp1 is a factor
aggregated_data_raw$SES <- factor(aggregated_data_raw$wealth_quantile_pca,
                                                levels = c("1","2","3"),
                                                labels = c("Poorest","Mid","Wealthiest"))



# Ensure Round_Year is numeric
aggregated_data_raw$Round_Year <- as.numeric(as.character(aggregated_data_raw$Round_Year))

# Plot with adjusted smoothing
# Filter aggregated_data to limit the data to Round_Year from  start_year to end_year

start_year <- 2005
end_year <- 2021

aggregated_data_raw <- dplyr::filter(aggregated_data_raw, 
                                 Round_Year >= start_year & Round_Year <= end_year)



# Plot with restricted x-axis range
ggplot(aggregated_data_raw, aes(x = Round_Year, y = incidence, color = SES, fill = SES)) +
  # Add original data points as small X symbols
  geom_point(shape = 4, size = 2, stroke = 1) + # Crosses for raw data
  #Add a smoothed curve through the data points with the same color as the data points
  stat_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE,
    alpha = 0.3
  ) + # Smoothed curve with shaded SE
  #scale_color_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Viridis palette for discrete data
  #scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Match fill colors to line colors
  scale_x_continuous(limits = c(start_year, end_year)) + # Set x-axis limits
  labs(
    title = "Incidence by Round Year and SES Quantile 
              (with 95% confidence intervals)",
    x = "Round Year",
    y = "Incidence (events per 100 person-years)",
    color = "SES Quantile",
    fill = "SES Quantile"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + # Move legend to bottom of plot
  theme(plot.title = element_text(size=12,hjust=0.5)) # Centre and decrease size of plot title


# Save the plot as a PNG file
ggsave(filename = paste0(output_dir,"/incidence_by_round_year_and_ses_quantile_raw.png"), width = 6, height = 4)


#### Imputed SES data 
aggregated_data_imp <- HIV_edu_SES %>%
  group_by(Round_Year, wealth_quant_pca.imp1) %>%
  summarise(
    total_PY = sum(Time, na.rm = TRUE)/365.25,
    total_sero_events = sum(sero_event, na.rm = TRUE),
    incidence = total_sero_events / total_PY*100
  ) %>%
  ungroup()

### Drop NA values for incidence from aggregated_data
aggregated_data_imp <- na.omit(aggregated_data_imp)
### Create variable SES as a factor
aggregated_data_imp$SES <- factor(aggregated_data_imp$wealth_quant_pca.imp1,
                                  levels = c("1","2","3"),
                                  labels = c("Poorest","Mid","Wealthiest"))



# Ensure Round_Year is numeric
aggregated_data_imp$Round_Year <- as.numeric(as.character(aggregated_data_imp$Round_Year))

# Plot with adjusted smoothing
# Filter aggregated_data to limit the data to Round_Year from  start_year to end_year

start_year <- 2005
end_year <- 2021

aggregated_data_imp <- dplyr::filter(aggregated_data_imp, 
                                     Round_Year >= start_year & Round_Year <= end_year)



# Plot with restricted x-axis range
ggplot(aggregated_data_imp, aes(x = Round_Year, y = incidence, color = SES, fill = SES)) +
  # Add original data points as small X symbols
  geom_point(shape = 4, size = 2, stroke = 1) + # Crosses for raw data
  #Add a smoothed curve through the data points with the same color as the data points
  stat_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE,
    alpha = 0.3
  ) + # Smoothed curve with shaded SE
  #scale_color_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Viridis palette for discrete data
  #scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Match fill colors to line colors
  scale_x_continuous(limits = c(start_year, end_year)) + # Set x-axis limits
  labs(
    title = "Incidence by Round Year and SES Quantile 
              (with 95% confidence intervals)",
    x = "Round Year",
    y = "Incidence (events per 100 person-years)",
    color = "SES Quantile",
    fill = "SES Quantile"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + # Move legend to bottom of plot
  theme(plot.title = element_text(size=12,hjust=0.5)) # Centre and decrease size of plot title


# Save the plot as a PNG file
ggsave(filename = paste0(output_dir,"/incidence_by_round_year_and_ses_quantile_imp.png"), width = 6, height = 4)



### Aggregate data for Education
aggregated_data_edu <- HIV_edu_SES %>%
  group_by(Round_Year, highest_edu_imp) %>%
  summarise(
    total_PY = sum(Time, na.rm = TRUE)/365.25,
    total_sero_events = sum(sero_event, na.rm = TRUE),
    incidence = total_sero_events / total_PY*100
  ) %>%
  ungroup()

### Drop NA values for incidence from aggregated_data
aggregated_data_edu <- na.omit(aggregated_data_edu)
### Filter to drop rows where highest_edu_imp is missing
aggregated_data_edu <- dplyr::filter(aggregated_data_edu, highest_edu_imp != "")
### Create education variable in which highest_edu_imp is a factor
aggregated_data_edu$education <- factor(aggregated_data_edu$highest_edu_imp,
                              levels = c("None","Primary","Secondary","Tertiary"),
                              labels = c("None","Primary","Secondary","Tertiary"))


# Ensure Round_Year is numeric
aggregated_data_edu$Round_Year <- as.numeric(as.character(aggregated_data_edu$Round_Year))

# Plot with adjusted smoothing
# Filter aggregated_data to limit the data to Round_Year from  start_year to end_year

start_year <- 2005
end_year <- 2021

aggregated_data_edu <- dplyr::filter(aggregated_data_edu, 
                                 Round_Year >= start_year & Round_Year <= end_year)



# Plot with restricted x-axis range
ggplot(aggregated_data_edu, aes(x = Round_Year, y = incidence, color = education, fill = education)) +
  # Add original data points as small X symbols
  geom_point(shape = 4, size = 2, stroke = 1) + # Crosses for raw data
  # Add a smoothed curve through the data points with the same color as the data points
  stat_smooth(
    method = "loess", 
    span = 0.5, 
    se = TRUE, 
    alpha = 0.3
  ) + # Smoothed curve with shaded SE
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Viridis palette for discrete data
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Match fill colors to line colors
  scale_x_continuous(limits = c(start_year, end_year)) + # Set x-axis limits
  labs(
    title = "Incidence by Round Year and Highest level of Education (with 95% confidence intervals)",
    x = "Round Year",
    y = "Incidence (events per 100 person-years)",
    color = "Highest Education Level",
    fill = "Highest Education Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + # Move legend to bottom of plot
  theme(plot.title = element_text(size=12,hjust=0.5)) # Centre and decrease size of plot title


# Save the plot as a PNG file
ggsave(filename = paste0(output_dir,"/incidence_by_round_year_and_education_quantile.png"), width = 6, height = 4)

### Aggregate data for Sex



# Summarise the data
aggregated_data_sex <- HIV_edu_SES %>%
  group_by(Round_Year, Female) %>%
  summarise(
    total_PY = sum(Time, na.rm = TRUE)/365.25,
    total_sero_events = sum(sero_event, na.rm = TRUE),
    incidence = (total_sero_events / total_PY) * 100,
    .groups = "drop"
  )



# Filter aggregated_data to limit the data to Round_Year from  start_year to end_year
aggregated_data_sex <- dplyr::filter(aggregated_data_sex, 
                                     Round_Year >= start_year & Round_Year <= end_year)
# Recode the sex variable and reorder columns
aggregated_data_sex <- aggregated_data_sex %>%
  mutate(
    sex = factor(Female, levels = c(0, 1), labels = c("Male", "Female"))
  ) %>%
  select(Round_Year, sex, total_PY, total_sero_events, incidence)  # Drop 'Female' and reorder




aggregated_data_sex <- aggregated_data_sex %>%
  mutate(
    total_PY = round(total_PY, 2),
    incidence = round(incidence, 2)
  )

# Create flextable
flextab <- flextable(aggregated_data_sex) %>%
  # Format total_sero_events as integer
  colformat_int(j = "total_sero_events") %>%
  colformat_num(j = c("total_PY", "incidence"), digits = 2) %>%
  set_header_labels(
    Round_Year = "Round Year",
    sex = "Sex",
    total_PY = "Total Person-Years",
    total_sero_events = "Total Seroconversions",
    incidence = "Incidence (per 100 PY)"
  ) %>%
  autofit()

# Write flextable to Word
read_docx() %>%
  body_add_par(paste0("Incidence by Sex and Year for individuals aged ",
                      as.character(age_min)," to ",as.character(age_max)), style = "heading 1") %>%
  body_add_par("Summary of Person-Years and Seroconversion Events", style = "heading 2") %>%
  body_add_flextable(flextab) %>%
  print(target = paste0(output_dir,"/incidence_summary.docx"))



# Ensure Round_Year is numeric
aggregated_data_sex$Round_Year <- as.numeric(as.character(aggregated_data_sex$Round_Year))

# Plot with adjusted smoothing



# Plot with restricted x-axis range
sex_plot <- ggplot(aggregated_data_sex, aes(x = Round_Year, y = incidence, color = sex, fill = sex)) +
  # Add original data points as small X symbols
  geom_point(shape = 4, size = 2, stroke = 1) + # Crosses for raw data
  # Add a smoothed curve through the data points with the same color as the data points
  stat_smooth(
    method = "loess", 
    span = 0.5, 
    se = TRUE, 
    alpha = 0.3
  ) + # Smoothed curve with shaded SE
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Viridis palette for discrete data
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Match fill colors to line colors
  scale_x_continuous(limits = c(start_year, end_year)) + # Set x-axis limits
  labs(
    title = paste0("Incidence by Round Year and Sex with 95% confidence intervals 
                   Age ", as.character(age_min)," to ",as.character(age_max)),
    x = "Round Year",
    y = "Incidence (events per 100 person-years)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + # Move legend to bottom of plot
  theme(plot.title = element_text(size=12,hjust=0.5)) # Centre and decrease size of plot title


# Save the plot as a PNG file
#ggsave(filename = paste0(output_dir,"/incidence_by_round_year_and_sex_quantile.png"), width = 6, height = 4)

landscape_one_column <- block_section(
  prop_section(
    page_size = page_size(orient = "landscape"), type = "continuous"
  )
)
  


doc <- read_docx() %>%
  # Add portrait content
  body_add_par("Incidence by Sex and Year", style = "heading 1") %>%
  body_add_par(paste0("Summary of Person-Years and Seroconversion Events (for those aged) "
               ,as.character(age_min)," to ", as.character(age_max)), style = "heading 2") %>%
  body_add_flextable(flextab) %>%
  
  # End the current (portrait) section and start a new one in landscape orientation
  body_end_section_portrait() %>%

  
  # Add heading and plot in landscape orientation
  body_add_gg(value = sex_plot, width = 10, height = 5, style = "centered") %>%
  body_end_block_section(value = landscape_one_column)

print(doc, target = paste0(output_dir,"/inc_sex_",as.character(age_min),"-"
                           ,as.character(age_max),".docx"))



### Set start and end dates for survival analysis 

start_date <- as.Date("2015-01-01")
end_date <- as.Date("2021-12-31")

### Create a cohort of all episodes for those under observation and known to be HIV negative at the start date 
### Episodes that start before the end date and finish after the start date are included 
### This version uses an open cohort in which individuals can join during the course of follow-up
### if not under observation at the start 

surv_dat <- dplyr::filter(HIV_edu_SES,((late_neg >= first_start_date) &
                                                    (late_neg >= start_date) &
                                                    (first_start_date <= end_date)))


#### Select which measure of SES quantile to use

#Wealth Quantile from MCA
###surv_dat$SES <- surv_dat$Wealth_Quant_MCA
### If using imputed values 
###surv_dat$SES <- surv_dat$wealth_quant_ca.imp1

#Wealth Quantile from Factor Analysis 
#surv_dat$SES <- surv_dat$Wealth_Quant_FA

#Wealth Quantile from PCA
surv_dat$SES <- surv_dat$wealth_quant_pca.imp1

### SES as a factor

surv_dat$SES <- factor(surv_dat$SES,
                       levels = c("1","2","3"),
                       labels = c("Poorest","Mid","Wealthiest"))


#### Select which education measure to use 

#surv_dat$education <- surv_dat$highest_edu
surv_dat$education <- surv_dat$highest_edu_imp

### Select relevant episodes for these individuals 
#### observation ends after start date and observations start before the end date

surv_dat <- dplyr::filter(surv_dat,((surv_dat$obs_end > start_date) &
                                      surv_dat$obs_start < end_date) )


### If observation spans the end date and date of seroconversion is after the end date then define as 
### seronegative at the end date 


surv_dat <- within(surv_dat, sero_event[(surv_dat$obs_start < end_date) &
                                          (surv_dat$obs_end > end_date) &
                                          (sero_event==1) &
                                          (sero_date > end_date)] <- 0)


### Set end date of observation to end date if end date is between the start and end of the observation

surv_dat <- within(surv_dat, obs_end[(surv_dat$obs_start < end_date) & 
                                       (surv_dat$obs_end > end_date)] <- end_date)


surv_dat$obs_end <- as.Date(surv_dat$obs_end)

#### Generate earliest observation date for each individual 

### Latest observation date 
surv_dat <- surv_dat %>%
  group_by(IIntID) %>%
  dplyr::mutate(first_obs_date = (min(obs_start)))


### Count number of individuals in cohort 
n_cohort <- dplyr::n_distinct(surv_dat$IIntID)

### Number of sero-conversions
n_sero <- sum(surv_dat$sero_event)

### list of those who sero convert 

# tmp <- dplyr::filter(surv_dat,(surv_dat$sero_event==1))
# tmp <- tmp[c('IIntID')]
# write.csv(tmp,file=paste0(output_dir,"/id_sero_open.csv"))

print(paste0("Number in starting cohort - HIV negative on ",as.character(start_date), " = ", as.character(n_cohort)))
print(paste0("Number of sero-conversions ",as.character(n_sero)))

### Create time variable 

### ntime length of time between start and end of observation

surv_dat$ntime <- surv_dat$obs_end - surv_dat$obs_start
py_tot <- as.integer(sum(surv_dat$ntime))/365.25
print(paste0("Total person years of observation =  ",as.character(py_tot)))

### Total incidence 

tot_inc <- n_sero/py_tot*100

### Events and PY by SES strata - by individual observations

sum_event_ses_obs  <- surv_dat %>% 
  group_by(SES) %>%                            
  summarise( sum_sero = sum(sero_event),sum_py = sum(as.integer(ntime)/365.25 )) 

sum_event_ses_obs$incid_100 <- 100*sum_event_ses_obs$sum_sero/sum_event_ses_obs$sum_py

sum_event_ses_obs

events_fname <- paste0(output_dir,"/events_ses_obs_open_",as.character(start_date),"_",as.character(end_date),".csv")
write.csv(sum_event_ses_obs,file = events_fname)

### Drop Age variable
surv_dat <- subset(surv_dat, select= -Age)

## Rename variables
surv_dat <- dplyr::rename(surv_dat, Education = education )
surv_dat <- dplyr::rename(surv_dat, Setting = urban_rural_fact)
surv_dat <- dplyr::rename(surv_dat, Clinic_Distance = km_clinic_fact)
surv_dat <- dplyr::rename(surv_dat, Age = age_cat)
surv_dat <- dplyr::rename(surv_dat, Sex = sex)
surv_dat <- dplyr::rename(surv_dat, SEP = SES)

### Select required variables 
tab_dat <- surv_dat[c('IIntID','Round_Year','Sex','Age','SEP','Education','Setting','Clinic_Distance','ntime','sero_event','obs_start')]

### Add total ntime column 
tab_dat <- tab_dat %>% 
  group_by(IIntID) %>% 
  mutate(total_ntime = sum(ntime))

### Add seroconversion during follow-up column

tab_dat <- tab_dat %>% 
  dplyr::group_by(IIntID) %>% 
  dplyr::mutate(seroconv=max(sero_event))

### Add count of observations by group and select 1st row (1st year of observation)

tab_dat <- tab_dat %>% dplyr::arrange(Round_Year)

tab_dat <- tab_dat %>% 
  dplyr::group_by(IIntID) %>% 
  dplyr::mutate(icount=row_number())

tab_dat <- tab_dat %>% dplyr::filter(icount==1)

rm(cohort_tab)


### summarise by variable 
### list of summary variables 
var_list <- c('Sex','Age','SEP','Education','Setting','Clinic_Distance')

for (var in var_list) {
  cat_dat <- tab_dat[c(var,'icount','seroconv','total_ntime')]
  colnames(cat_dat)[1] <- "category"
  cat_dat <- aggregate(.~category, cat_dat, sum)
  cat_dat$ntime <- cat_dat$total_ntime/365.25
  cat_dat$inc_100py <- as.character(round((cat_dat$seroconv/cat_dat$ntime*100),digits=2))
  tot_count <- sum(cat_dat$icount)
  cat_dat$percent_count <- cat_dat$icount/tot_count*100
  tot_ntime <- sum(cat_dat$ntime)
  cat_dat$percent_ntime <- cat_dat$ntime/tot_ntime*100
  cat_dat$count_n_percent <- paste0(as.character(round(cat_dat$icount,digits=0)),
                                    "(",
                                    as.character(round(cat_dat$percent_count,digits=1)),
                                    ")")
  cat_dat$ntime_percent <- paste0(as.character(round(cat_dat$ntime,digits=0)),
                                  "(",
                                  as.character(round(cat_dat$percent_ntime,digits=1)),
                                  ")")
  
  ### Create new columns for output table
  
  ### Add name column
  cat_dat <- cbind(var_name = var, cat_dat)
  
  ### Append to overall dataframe      
  if(exists("cohort_tab")==FALSE) {
    cohort_tab <- cat_dat
  } else{ 
    cohort_tab <-  rbind(cohort_tab,cat_dat)
  }
  
}

cohort_tab$ntime <- round(cohort_tab$ntime,digits = 0)
cohort_tab <- cohort_tab[c('var_name','category','icount','count_n_percent','seroconv','ntime','ntime_percent','inc_100py')]

### Write as a csv file
cohort_fname <- paste0(output_dir,"/cohort_summary_",as.character(start_date),"_",as.character(end_date),".csv")
write.csv(cohort_tab,file = cohort_fname)


#### Extended KM analysis allowinf for time varying covariates 

surv_dat$tstart_num <- as.integer(surv_dat$obs_start - surv_dat$first_obs_date)
surv_dat$tstop_num <- as.integer(surv_dat$obs_end - surv_dat$first_obs_date)


### Plot 1 SEP 
### Survival analysis taking account of time varying covariates

plot_title <- paste0(as.character(lubridate::year(start_date)),
                     " - ",
                     as.character(lubridate::year(end_date))
                      )

s1 <- survfit(Surv(time=tstart_num, time2=tstop_num, event=sero_event==1) ~ SEP, data=surv_dat, id=IIntID,cluster=HouseholdId)

p2 <- ggsurvplot(s1,
                 #fun="pct",
                 title=plot_title,
                  linetype="solid", 
                  risk.table = FALSE, conf.int = FALSE, 
                  break.x.by = 500, censor=FALSE,
                  ylab = "Probability of remaining seronegative",
                  xlab = "Follow-up time (days)",
                  legend.title="SEP")

p2$plot <- p2$plot + 
            ylim(c(0.75,1.0)) +
            xlim(c(0,4000)) +
            scale_color_manual(values = c('red', 'blue','green'),
            labels = c('Poorest', 'Mid','Wealthiest')) +
        theme(plot.title = element_text(hjust = 0.5,lineheight=.5),
        legend.text=element_text(size=12,face="bold"),
        axis.text.x = element_text(size=12, face="bold", color = "black"),
        axis.text.y = element_text(size=12, face="bold", color = "black"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(size=12, face="bold"),
        legend.position=c(0.7,0.65)) 
p2

### Log-rank test 

surv_diff <- survival::survdiff(Surv(time=ntime, event=sero_event==1) ~ SEP, data=surv_dat)
surv_diff

### # Perform pairwise comparisons
pairwise_test <- pairwise_survdiff(Surv(time=ntime, event=sero_event==1) ~ SEP, data=surv_dat)

# Extract the p-value matrix
p_matrix <- pairwise_test$p.value

# Format the p-value table, converting all p-values < 0.05 to "< 0.05"
formatted_p_matrix <- apply(p_matrix, c(1, 2), function(x) {
  if (!is.na(x) && x < 0.05) {
    "< 0.05"
  } else {
    format(x, digits = 2)
  }
})

# Convert the formatted matrix into a data frame for better printing
p_table <- as.data.frame(formatted_p_matrix)

# Convert the p-value table into a ggtexttable object
pval_table <- ggpubr::ggtexttable(p_table, rows = rownames(p_table), theme = ttheme("minimal"))

# Convert the table to a grob (graphical object) for positioning
table_grob <- ggplotGrob(pval_table)

p2_tab <- p2

# Add the table to the plot using annotation_custom
p2_tab$plot <- p2_tab$plot + 
  annotation_custom(table_grob, 
                    xmin = 100, xmax = 900,  # Adjust these values for horizontal placement
                    ymin = 0.80, ymax = 0.90)  # Adjust these values for vertical placement

# Define the title text and its position
title_text <- "p-values for pairwise comparisons of strata"
title_grob <- textGrob(title_text, gp = gpar(fontsize = 12, fontface = "plain"), just = "center")

# Add the title below the table
p2_tab$plot <- p2_tab$plot + 
  annotation_custom(title_grob, 
                    xmin = 300, xmax = 900,  # Adjust these values for horizontal placement
                    ymin = 0.80, ymax = 0.81)  # Adjust these values for vertical placement
p2_tab

plot_fname <- paste0(output_dir,"/km_all_open_sep_",as.character(start_date),"_",as.character(end_date),".png")
gg_obf_fname <- paste0(output_dir,"/km_all_open_sep_",as.character(start_date),"_",as.character(end_date),".RData")

save(p2_tab,file=gg_obf_fname)

ggpubr::ggexport(filename = plot_fname,width=694,height=420,
                 plot = p2_tab, device = "png")

### Plot 2  Education 

### Drop missing values for education and convert to factor
### Survival analysis taking account of time varying covariates

surv_dat_edu <- surv_dat

surv_dat_edu <- subset(surv_dat_edu,Education!="")
surv_dat_edu$Education <- as.factor(surv_dat_edu$Education)

s2 <- survfit(Surv(time=tstart_num, time2=tstop_num, event=sero_event) ~ Education, data=surv_dat_edu, id=IIntID,cluster=HouseholdId)

p3 <- ggsurvplot(s2,
                 linetype="solid",
                 title = plot_title,
                 #,fun="pct",
                 risk.table = FALSE, conf.int = FALSE, 
                 break.x.by = 500, censor=FALSE,
                 ylab = "Probability of remaining seronegative",
                 xlab = "Follow-up time (days)",
                 legend.title="Highest level of Education")

p3$plot <- p3$plot + 
  ylim(c(0.5, 1.0)) + 
  scale_color_manual(values = c('brown','red', 'blue','green'),
                     labels = c('None', 'Primary','Secondary','Tertiary')) +
  theme(plot.title = element_text(hjust = 0.5,lineheight=.5),
        legend.text=element_text(size=12,face="bold"),
        axis.text.x = element_text(size=12, face="bold", color = "black"),
        axis.text.y = element_text(size=12, face="bold", color = "black"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(size=12, face="bold"),
        legend.position=c(0.20,0.55))  
p3
### Log-rank test 

surv_diff <- survival::survdiff(Surv(time=ntime, event=sero_event==1) ~ Education, data=surv_dat)
surv_diff

### # Perform pairwise comparisons
pairwise_test <- pairwise_survdiff(Surv(time=ntime, event=sero_event==1) ~ Education, data=surv_dat_edu)

# Extract the p-value matrix
p_matrix <- pairwise_test$p.value

# Format the p-value table, converting all p-values < 0.05 to "< 0.05"
formatted_p_matrix <- apply(p_matrix, c(1, 2), function(x) {
  if (!is.na(x) && x < 0.05) {
    "< 0.05"
  } else {
    format(x, digits = 2)
  }
})

# Convert the formatted matrix into a data frame for better printing
p_table <- as.data.frame(formatted_p_matrix)

# Convert the p-value table into a ggtexttable object
pval_table <- ggpubr::ggtexttable(p_table, rows = rownames(p_table), theme = ttheme("minimal"))

# Convert the table to a grob (graphical object) for positioning
table_grob <- ggplotGrob(pval_table)

p3_tab <- p3

# Add the table to the plot using annotation_custom
p3_tab$plot <- p3$plot + 
  annotation_custom(table_grob, 
                    xmin = 100, xmax = 1000,  # Adjust these values for horizontal placement
                    ymin = 0.50, ymax = 0.75)  # Adjust these values for vertical placement

# Define the title text and its position
title_text <- "p-values for pairwise comparisons of strata"
title_grob <- textGrob(title_text, gp = gpar(fontsize = 12, fontface = "plain"), just = "center")

# Add the title below the table
p3_tab$plot <- p3_tab$plot + 
  annotation_custom(title_grob, 
                    xmin = 100, xmax = 1000,  # Adjust these values for horizontal placement
                    ymin = 0.35, ymax = 0.75)  # Adjust these values for vertical placement
p3_tab

plot_fname <- paste0(output_dir,"/km_all_open_edu_",as.character(start_date),"_",as.character(end_date),".png")
gg_obf_fname <- paste0(output_dir,"/km_all_open_edu_",as.character(start_date),"_",as.character(end_date),".RData")

save(p3_tab,file=gg_obf_fname)

ggpubr::ggexport(filename = plot_fname,width=1000,height=1000,
                 plot = p3_tab, device = "png")


### tabulating results 
### https://argoshare.is.ed.ac.uk/healthyr_book/cox-proportional-hazards-regression.html
### https://finalfit.org/articles/finalfit.html

#### Summary analysis using finalfit
#### SEP

covariates <- c( 'SEP','Age','Sex','Setting','Clinic_Distance','cluster(HouseholdId)')
dependent <- "Surv(time=ntime, event=sero_event==1)"


surv_dat %>%
  finalfit::finalfit.coxph(dependent=dependent ,explanatory = covariates,
                           add_dependent_label = FALSE) -> t1 
# rename("Overall survival" = label) %>% 
# rename(" " = levels) %>% 
# rename("  " = all) -> t1
knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

### Use r markdown to get nicely formatted tables in Word

### https://argoshare.is.ed.ac.uk/healthyr_book/ms-word-via-knitrr-markdown.html


#### Summary analysis using finalfit
#### Education 

covariates <- c( 'Education','Age','Sex','Setting','Clinic_Distance','cluster(HouseholdId)')
dependent <- "Surv(time=ntime, event=sero_event==1)"


surv_dat_edu %>%
  finalfit::finalfit.coxph(dependent=dependent ,explanatory = covariates,
                           add_dependent_label = FALSE) -> t1 
# rename("Overall survival" = label) %>% 
# rename(" " = levels) %>% 
# rename("  " = all) -> t1
knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

### Use r markdown to get nicely formatted tables in Word

### https://argoshare.is.ed.ac.uk/healthyr_book/ms-word-via-knitrr-markdown.html

