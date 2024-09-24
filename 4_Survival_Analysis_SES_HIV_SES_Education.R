####
#### This script load the HIV , SES and education data and merges the three files
#### Before carrying out the Kaplen Meier plots and Cox Survival Analyses
####

####
# Clear any existing data from the data set
rm(list = ls())
#####

# Define vector of package names

package_names <- c('dplyr','ggplot2','ggsurvfit','survminer')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023'
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'

#### Load rounds data 
#### HIV data - saved from 1_Load_AHRI_HIV_Data.R
load(paste0(output_dir,"/ACDIS_hiv_full.RData"))
#### Asset data - saved from 2_Generate_AHRI_SES_Index.R
load(paste0(output_dir,"/ass_ses_full_imp.RData"))
#### education data - saved from 3_Generate_AHRI_edu_Data.R
load(paste0(output_dir,"/ACDIS_edu_all.RData"))

#### Merge HIV episode data with education data on Round_Year and IIntID

HIV_edu <- merge(hiv_imput_full,ACDIS_edu_all,by.x = c('IIntID','Round_Year'),by.y=c('IIntId','Round_Year'),all.x=TRUE)

#### Merge this file with asset data on Household ID and Round_Year

HIV_edu_SES <- merge(HIV_edu,ass_ses_full_imp,by.x = c('HouseholdId_imp','Round_Year'),by.y=c('HHIntId','Round_Year'),all.x=TRUE)

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
surv_dat$SES <- surv_dat$wealth_quant_mca.imp1

#Wealth Quantile from Factor Analysis 
#surv_dat$SES <- surv_dat$Wealth_Quant_FA

#Wealth Quantile from PCA
#surv_dat$SES <- surv_dat$Wealth_Quant_PCA

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


### Count number of individuals in cohort 
n_cohort <- dplyr::n_distinct(surv_dat$IIntID)

### Number of sero-conversions
n_sero <- sum(surv_dat$sero_event)

### list of those who sero convert 

tmp <- filter(surv_dat,(surv_dat$sero_event==1))
tmp <- tmp[c('IIntID')]
write.csv(tmp,file=paste0(output_dir,"/id_sero_open.csv"))

print(paste0("Number in starting cohort - HIV negative on ",as.character(start_date), " = ", as.character(n_cohort)))
print(paste0("Number of sero-conversions ",as.character(n_sero)))

### Create time variable 

### ntime length of time between start and end of observation

surv_dat$ntime <- surv_dat$obs_end - surv_dat$obs_start
py_tot <- as.integer(sum(surv_dat$ntime))/365.25
print(paste0("Total person years of observation =  ",as.character(py_tot)))

### Events and PY by SES strata - by individual observations

sum_event_ses_obs  <- surv_dat %>% 
  group_by(SES) %>%                            
  summarise( sum_sero = sum(sero_event),sum_py = sum(as.integer(ntime)/365.25 )) 

sum_event_ses_obs$incid_100 <- 100*sum_event_ses_obs$sum_sero/sum_event_ses_obs$sum_py

sum_event_ses_obs

events_fname <- paste0(output_dir,"/events_ses_obs_open_",as.character(start_date),"_",as.character(end_date),".csv")
write.csv(sum_event_ses_obs,file = events_fname)


surv_dat <- dplyr::rename(surv_dat, 'Urban_Rural' = 'urban_rural_fact')
surv_dat <- dplyr::rename(surv_dat, 'Km_Clinic' = 'km_clinic_fact')

### Select required variables 
tab_dat <- surv_dat[c('IIntID','Year','sex','age_cat','SES','education','Urban_Rural','Km_Clinic','ntime','sero_event','obs_start')]

### Add total ntime column 
tab_dat <- tab_dat %>% 
  group_by(IIntID) %>% 
  mutate(total_ntime = sum(ntime))

### Add seroconversion during follow-up column

tab_dat <- tab_dat %>% 
  dplyr::group_by(IIntID) %>% 
  dplyr::mutate(seroconv=max(sero_event))

### Add count of observations by group and select 1st row (1st year of observation)

tab_dat <- tab_dat %>% dplyr::arrange(Year)

tab_dat <- tab_dat %>% 
  dplyr::group_by(IIntID) %>% 
  dplyr::mutate(icount=row_number())

tab_dat <- tab_dat %>% filter(icount==1)

rm(cohort_tab)


### summarise by variable 
### list of summary variables 
var_list <- c('sex','age_cat','SES','education','Urban_Rural','Km_Clinic')

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




### Sero Events allowing for time varying covariates - analyses within each episode



plot_title <- paste0("Kaplan-Meier plot  \n 
Probability of seroconversion within each year of observation from  ",lubridate::year(start_date),
                     " to ",lubridate::year(end_date))


p2 <-  survfit(Surv(time=ntime, event=sero_event==1) ~ SES, data=surv_dat,id=IIntID,cluster=HouseholdId)%>% 
  ggsurvfit(type = "survival") +
  labs(
    x = "Days to serconversion",
    y = "Survival Probability",
    title = plot_title,
    colour = "SES"
  )  +
  scale_color_manual(values = c('red', 'blue','green'),
                     labels = c('Poorest', 'Mid','Wealthiest')) +
  theme(plot.title = element_text(hjust = 0.5,lineheight=.5),
        legend.text=element_text(size=12,face="bold"),
        axis.text.x = element_text(size=12, face="bold", color = "black"),
        axis.text.y = element_text(size=12, face="bold", color = "black"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        legend.position=c(.75,.75)) 
p2

### Log-rank test 

surv_diff <- survdiff(Surv(time=ntime, event=sero_event==1) ~ SES, data=surv_dat)
surv_diff

plot_fname <- paste0(output_dir,"/km_all_open_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(plot_fname,p2,  width=20, height=15, units="cm")


### New version where each individual has one row and we use starting SES 

### Calculate starting SES value by individual

surv_dat <- surv_dat %>% arrange(obs_start)

surv_dat <- surv_dat  %>% 
  group_by(IIntID) %>% 
  mutate(SES_start = SES[1])

### Final status
# surv_dat <- surv_dat %>%
#   group_by(IIntID) %>%
#   dplyr::mutate(mean_SES = round((mean(Wealth_Quant_PCA))))

### First obs_start
surv_dat <- surv_dat %>%
  group_by(IIntID) %>%
  dplyr::mutate(first_obs_start = min(obs_start))

### Last obs_end
surv_dat <- surv_dat %>%
  group_by(IIntID) %>%
  dplyr::mutate(last_obs_end = max(obs_end))

surv_dat <- ungroup(surv_dat)


surv_dat_total <- unique(surv_dat[c('IIntID','first_obs_start',
                                              'last_obs_end','final_sero_status','SES_start')])

surv_dat_total$SES <- surv_dat_total$SES_start


surv_dat_total$ntime <- surv_dat_total$last_obs_end - surv_dat_total$first_obs_start

### maximum ntime
max_ntime = as.integer(max(surv_dat_total$ntime))

### Sero Events by starting SES group 

plot_title <- paste0("Kaplan-Meier plot  \n 
Probability of seroconversion from  ",lubridate::year(start_date),
                     " to ",lubridate::year(end_date))


p3 <-  survfit(Surv(time=ntime, event=final_sero_status==1) ~ SES, data=surv_dat_total,id=IIntID)%>% 
  ggsurvfit(type = "survival") +
  labs(
    x = "Days to serconversion",
    y = "Survival Probability",
    title = plot_title,
    colour = "SES"
  )  +
  scale_color_manual(values = c('red', 'blue','green'),
                     labels = c('Poorest', 'Mid','Wealthiest')) +
  theme(plot.title = element_text(hjust = 0.5,lineheight=.5),
        legend.text=element_text(size=12,face="bold"),
        axis.text.x = element_text(size=12, face="bold", color = "black"),
        axis.text.y = element_text(size=12, face="bold", color = "black"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        legend.position=c(.75,.75)) +
  scale_x_continuous(limits=c(0,max_ntime-10 ))
p3


plot_fname <- paste0(output_dir,"/km_start_open_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(plot_fname,p3,  width=20, height=15, units="cm")



### Cox Regression  - Age - SES fixed at start of period
#### Survival Analysis (https://cran.r-project.org/web/packages/survivalAnalysis/vignettes/multivariate.html)
#### http://www.sthda.com/english/wiki/cox-proportional-hazards-model


#### Univariable analysis 


covariates <- c('age_cat', 'sex',  'SES', 'education','Urban_Rural','Km_Clinic')
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(ntime, sero_event)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = surv_dat,id=IIntID,cluster=HouseholdId_imp)})

cox_fname <- paste0(data_dir,"/cox_univ_open_",as.character(start_date),"_",as.character(end_date),".txt")
for (i in seq(1,length(covariates),1)) {
  res.cox <- univ_models[[i]]
  if(i == 1){
    sink(cox_fname,append=FALSE)
    print(summary(res.cox))
  } else {
    sink(cox_fname,append=TRUE)
    print(summary(res.cox))
  }
  sink(file=NULL)
  print(summary(res.cox))
}

### tabulating results
### https://argoshare.is.ed.ac.uk/healthyr_book/cox-proportional-hazards-regression.html

#### Plotting Forest plot for multiple Univariable regressions

### https://github.com/kassambara/survminer/issues/459
### Or this 
### https://rdrr.io/cran/survivalAnalysis/man/forest_plot.html

#### Multivariable analysis

surv_dat <- subset(surv_dat, select = -c(Age) ) 

## Rename variables
surv_dat <- dplyr::rename(surv_dat, Education = education )
surv_dat <- dplyr::rename(surv_dat, Setting = Urban_Rural)
surv_dat <- dplyr::rename(surv_dat, Clinic_Distance = Km_Clinic)
surv_dat <- dplyr::rename(surv_dat, Age = age_cat)
surv_dat <- dplyr::rename(surv_dat, Sex = sex)

cox_fname <- paste0(data_dir,"/cox_multi_open_",as.character(start_date),"_",as.character(end_date),".txt")
#res.cox <- coxph(Surv(ntime, sero_event) ~  SES + Age + Sex + Education + Setting + Clinic_Distance, data =  as.data.frame(surv_dat),cluster=HouseholdId)
res.cox <- coxph(Surv(ntime, sero_event) ~  SES,data =  as.data.frame(surv_dat),cluster=HouseholdId)
sink(cox_fname,append=FALSE)
summary(res.cox)
sink(file=NULL)
summary(res.cox)

forest_title <- paste0("Multivariate Cox PH regression model  
Hazard Ratio for serconversion ",year(start_date)," to ",year(end_date))

p4 <- ggforest(model=res.cox,
               main = forest_title,
               cpositions = c(0.02, 0.12, 0.3),
               fontsize = 1.0) 

p4


forest_fname <- paste0(output_dir,"/forest_multi_open_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(forest_fname,p4, width=30, height=18, units="cm")


### Forest plot SES univariable

cox_fname <- paste0(data_dir,"/cox_uni_open_",as.character(start_date),"_",as.character(end_date),".txt")
#res.cox <- coxph(Surv(ntime, sero_event) ~  SES + Age + Sex , data =  as.data.frame(surv_dat),cluster=HouseholdId)
#res.cox <- coxph(Surv(ntime, sero_event) ~  SES , data =  as.data.frame(surv_dat),cluster=HouseholdId)
res.cox <- coxph(Surv(ntime, sero_event) ~  SES , data =  as.data.frame(surv_dat))
sink(cox_fname,append=FALSE)
summary(res.cox)
sink(file=NULL)
summary(res.cox)

forest_title <- paste0("Univariable Cox PH regression model  
Hazard Ratio for serconversion ",year(start_date)," to ",year(end_date))

p5 <- ggforest(model=res.cox,
               main = "",
               cpositions = c(0.02, 0.1, 0.35),
               fontsize = 1.1) 

p5
forest_fname <- paste0(output_dir,"/forest_uni_open_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(forest_fname,p5, width=20, height=8, units="cm")

### Forest plot SES control for Age and Sex

cox_fname <- paste0(output_dir,"/cox_multi_open_",as.character(start_date),"_",as.character(end_date),".txt")
res.cox <- coxph(Surv(ntime, sero_event) ~  SES + Age + Sex , data =  as.data.frame(surv_dat),cluster=HouseholdId)

sink(cox_fname,append=FALSE)
summary(res.cox)
sink(file=NULL)
summary(res.cox)

forest_title <- paste0("Univariable Cox PH regression model  
Hazard Ratio for serconversion ",year(start_date)," to ",year(end_date))

p6 <- ggforest(model=res.cox,
               main = "",
               cpositions = c(0.02, 0.1, 0.35),
               fontsize = 1.1) 

p6
forest_fname <- paste0(output_dir,"/forest_multi_open_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(forest_fname,p6, width=20, height=8, units="cm")


#### Testing the proportional Hazards assumption 
### http://www.sthda.com/english/wiki/cox-model-assumptions
### "From the output above, the test is not statistically significant for each of the covariates, 
### and the global test is also not statistically significant. Therefore, we can assume the proportional hazards."

test.ph <- survival::cox.zph(res.cox)
test.ph


### tabulating results 
### https://argoshare.is.ed.ac.uk/healthyr_book/cox-proportional-hazards-regression.html
### https://finalfit.org/articles/finalfit.html

#### Summary analysis using finalfit

#covariates <- c('Age', 'Sex',  'SES', 'Education' , 'Setting','cluster(HouseholdId)')
covariates <- c('Age', 'Sex', 'SES', 'Education','Setting','Clinic_Distance','gini_quant_fact','cluster(HouseholdId)')
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

### Subdivide by gender

surv_dat_male <- filter(surv_dat,(surv_dat$Sex=="Male"))
surv_dat_female <- filter(surv_dat,(surv_dat$Sex=="Female"))

#covariates <- c( 'Age','Education','cluster(HouseholdId)')
# covariates <- c('Age', 'Sex', 'SES', 'Education','Setting','Clinic_Distance','cluster(HouseholdId)')
# dependent <- "Surv(time=ntime, event=sero_event==1)"



surv_dat %>%
  finalfit::finalfit.coxph(dependent=dependent ,explanatory = covariates,
                           add_dependent_label = FALSE) -> t1 
# rename("Overall survival" = label) %>% 
# rename(" " = levels) %>% 
# rename("  " = all) -> t1
knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))



surv_dat_male %>%
  finalfit::finalfit.coxph(dependent=dependent ,explanatory = covariates,
                           add_dependent_label = FALSE) -> t1 
# rename("Overall survival" = label) %>% 
# rename(" " = levels) %>% 
# rename("  " = all) -> t1
knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))


surv_dat_female %>%
  finalfit::finalfit.coxph(dependent=dependent ,explanatory = covariates,
                           add_dependent_label = FALSE) -> t1 
# rename("Overall survival" = label) %>% 
# rename(" " = levels) %>% 
# rename("  " = all) -> t1
knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

