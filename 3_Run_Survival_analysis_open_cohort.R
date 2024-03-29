### This file runs the Survival analysis 

# Clear any existing data from the data set
rm(list = ls())


# Define vector of package names

package_names <- c('haven','dplyr','ggplot2','ggthemes','zoo','stringr','survival',
                   'ggsurvfit','survivalAnalysis','NCmisc','devtools','finalfit','survminer')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

# Set file paths
## AHRI data
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'


### Read saved RDS file with SES quantiles and education 
R_fname_survdat <- paste0(data_dir,"/Survdata.RDS")
sero_data_imput_ses.df <- readRDS(R_fname_survdat)

### Set start and end dates for survival analysis 

start_date <- as.Date("2018-01-01")
end_date <- as.Date("2021-12-31")

### Create a cohort of all episodes for those under observation and known to be HIV negative at the start date 
### Episodes that start before the end date and finish after the start date are included 

### This version uses an open cohort in which individuals can join during the course of follow-up
### if not under observation at the start 


### Individuals to be included in the cohort 
### Closed cohort 
### latest HIV negative on or after start date AND
### Earliest observation on or before start date AND
### Latest observation after start date 
### Plus 
### First start date > start date and < end_date 
### and late_neg > first_start_date

surv_dat_1 <- dplyr::filter(sero_data_imput_ses.df,((sero_data_imput_ses.df$late_neg >= start_date) &
                                                    (sero_data_imput_ses.df$first_start_date <= start_date) &
                                                    sero_data_imput_ses.df$last_end_date > start_date))

surv_dat_2 <- dplyr::filter(sero_data_imput_ses.df,((sero_data_imput_ses.df$late_neg >= first_start_date) &
                                                      (sero_data_imput_ses.df$first_start_date > start_date) &
                                                      sero_data_imput_ses.df$first_start_date < end_date))

surv_dat <- rbind(surv_dat_1,surv_dat_2)

#### Select relevant episodes for these individuals 
#### observation ends after start date and observations start before the end date

surv_dat <- dplyr::filter(surv_dat,((surv_dat$obs_end > start_date) &
                                      surv_dat$obs_start < end_date) )


### Count number of individuals in cohort 
n_cohort <- dplyr::n_distinct(surv_dat$IIntID)

### Number of sero-conversions
n_sero <- sum(surv_dat$sero_event)

### list of those who sero convert 

tmp <- filter(surv_dat,(surv_dat$sero_event==1))
tmp <- tmp[c('IIntID')]
write.csv(tmp,file=paste0(data_dir,"/id_sero_open.csv"))

print(paste0("Number in starting cohort - HIV negative on ",as.character(start_date), " = ", as.character(n_cohort)))
print(paste0("Number of sero-conversions ",as.character(n_sero)))


#### Selecting whether to use imputed data for wealth quantile and household

#surv_dat$SES <- surv_dat$wealth_quantile # 1) Those with complete SES data 
surv_dat$SES <- surv_dat$wealth_quant.imp1 # 2) those with imputed SES based on household 
#surv_dat$SES <- surv_dat$wealth_quant.imp2 # 3) those with imputed SES based on household and individual 

### SES as a factor
surv_dat$SES <- factor(surv_dat$SES,
                        levels = c("1","2","3","4","5"))

## If using imputed Household ID's 
#surv_dat$HouseholdId <- surv_dat$HouseholdId_imp

#### Missing data analysis 
#### Generate a table of missing data by episode for age and sex

surv_dat$included = 1 
surv_dat$included[is.na(surv_dat$SES)] <-0

table(surv_dat$age_cat,surv_dat$included)
prop.table(table(surv_dat$age_cat,surv_dat$included),1)
chisq.test(table(surv_dat$age_cat,surv_dat$included),correct=FALSE)

#### Missing data analysis 
#### Generate a table of missing data by episode for age and sex


table(surv_dat$sex,surv_dat$included)
prop.table(table(surv_dat$sex,surv_dat$included),1)
chisq.test(table(surv_dat$sex,surv_dat$included),correct=FALSE)

### Create time variable 
### Set end date of observation to censor date if censor date is between the start and end of the observation
surv_dat$obs_end[ (surv_dat$obs_start < surv_dat$censor_date)   & (surv_dat$obs_end > surv_dat$censor_date)] <- surv_dat$censor_date

### Set end date of observation to end date if end date is between the start and end of the observation
surv_dat$obs_end[ (surv_dat$obs_start < end_date)   & (surv_dat$obs_end > end_date)] <- end_date

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
events_fname <- paste0(data_dir,"/events_ses_obs_open_",as.character(start_date),"_",as.character(end_date),".csv")

write.csv(sum_event_ses_obs,file = events_fname)

### Keep required variables for analysis

surv_dat_anal <- surv_dat[,c('IIntID','HouseholdId','Year','sex','age_cat','SES','highest_edu_fact',
                             'urban_rural_fact','pipsa_fact','km_clinic_fact','obs_start','obs_end','ntime','sero_event')]



### Sero Events by starting SES group 



plot_title <- paste0("Kaplan-Meier plot (failure = seroconversion) \n Open cohort from  ",start_date,
                     " - Censored on ",end_date,
                     "\n SES at start of period")



p2 <-  survfit(Surv(time=ntime, event=sero_event==1) ~ SES, data=surv_dat_anal,id=IIntID,cluster=HouseholdId)%>% 
  ggsurvfit(type = "survival") +
  labs(
    x = "Days to serconversion",
    y = "Survival Probability",
    title = plot_title
  ) 
p2

plot_fname <- paste0(data_dir,"/km_all_open_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(paste0(plot_fname),p2,  width=20, height=15, units="cm")



### Cox Regression  - Age - SES fixed at start of period
#### Survival Analysis (https://cran.r-project.org/web/packages/survivalAnalysis/vignettes/multivariate.html)
#### http://www.sthda.com/english/wiki/cox-proportional-hazards-model


#### Univariable analysis 


covariates <- c('age_cat', 'sex',  'SES', 'highest_edu_fact','urban_rural_fact','pipsa_fact')
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(ntime, sero_event)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = surv_dat_anal,id=IIntID,cluster=HouseholdId)})

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

cox_fname <- paste0(data_dir,"/cox_multi_open_",as.character(start_date),"_",as.character(end_date),".txt")
res.cox <- coxph(Surv(ntime, sero_event) ~ age_cat + sex + SES + highest_edu_fact, data =  as.data.frame(surv_dat_anal),cluster=HouseholdId)
sink(cox_fname,append=FALSE)
summary(res.cox)
sink(file=NULL)
summary(res.cox)

ggforest(model=res.cox)
forest_fname <- paste0(data_dir,"/forest_multi_open_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(forest_fname, width=30, height=12, units="cm")

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

covariates <- c('age_cat', 'sex',  'SES', 'highest_edu_fact','urban_rural_fact')
dependent <- "Surv(time=ntime, event=sero_event==1)"

surv_dat_anal %>%
  finalfit::finalfit(dependent=dependent ,explanatory = covariates,add_dependent_label = FALSE) -> t1 
  # rename("Overall survival" = label) %>% 
  # rename(" " = levels) %>% 
  # rename("  " = all) -> t1
knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

### Use r markdown to get nicely formatted tables in Word

### https://argoshare.is.ed.ac.uk/healthyr_book/ms-word-via-knitrr-markdown.html
