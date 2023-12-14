### This file runs the Survival analysis 

# Clear any existing data from the data set
rm(list = ls())


# Define vector of package names

package_names <- c('haven','dplyr','ggplot2','ggthemes','zoo','stringr','survival',
                   'ggsurvfit','survivalAnalysis','NCmisc','devtools')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

# Set file paths
## AHRI data
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'


### Read saved RDS file with SES quantiles
R_fname_survdat <- paste0(data_dir,"/Survdata.RDS")
sero_data_imput_ses.df <- readRDS(R_fname_survdat)

### Set start and end dates for survival analysis 

start_date <- as.Date("2018-01-01")
end_date <- as.Date("2022-12-31")

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


### Analysis on 
### 1) Those with complete SES data - SES_1
### 2) those with imputed SES based on household -  SES_2
### 3) those with imputed SES based on individual -  SES_3


surv_dat$SES <- surv_dat$SES_1
# surv_dat$SES <- surv_dat$SES_2
# surv_dat$SES <- surv_dat$SES_3

### Create time variable 
### Set end date of observation to earliest of obs_end, censor_date and end_date

surv_dat    <- surv_dat %>%   
  mutate(obs_end = min(obs_end,censor_date,end_date))

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




names(surv_dat_km)[2] <- "HouseholdId"
names(surv_dat_km)[6] <- "sero_event"
names(surv_dat_km)[7] <- "ntime"

surv_dat_km$SES_start <- as.factor(surv_dat_km$SES_start)

### Sero Events by starting SES group 

sum_event_ses_all  <- surv_dat_km %>% 
  group_by(SES_start) %>%                            
  summarise( sum_sero = sum(sero_event),sum_py = sum(as.integer(ntime)/365.25 )) 

sum_event_ses_all$incid_100 <- 100*sum_event_ses_all$sum_sero/sum_event_ses_all$sum_py

sum_event_ses_all

events_fname <- paste0(data_dir,"/events_ses_open_",as.character(start_date),"_",as.character(end_date),".csv")

write.csv(sum_event_ses_all,file = events_fname)

plot_title <- paste0("Kaplan-Meier plot (failure = seroconversion) \n Open cohort from  ",start_date,
                     " - Censored on ",end_date,
                     "\n SES at start of period")

### Converting time to years 

surv_dat_km$time_years = surv_dat_km$ntime/365.25


p2 <-  survfit(Surv(time=time_years, event=sero_event==1) ~ SES_start, data=surv_dat_km,id=IIntID)%>% 
  ggsurvfit(type = "survival") +
  labs(
    x = "Years to serconversion",
    y = "Survival Probability",
    title = plot_title
  ) 
p2

plot_fname <- paste0(data_dir,"/km_all_open_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(paste0(plot_fname),p2,  width=20, height=15, units="cm")


### Time variant covariates 


### Converting time to years 

plot_title <- paste0("Kaplan-Meier plot (failure = seroconversion) \n open cohort starting on ",start_date,
                     " - Censored on ",end_date,
                     "\n Time varying covariates")


p2 <-  survfit(Surv(time=ntime, event=sero_event==1) ~ SES, data=surv_dat,id=IIntID)%>% 
  ggsurvfit(type = "survival") +
  labs(
    x = "Days to serconversion",
    y = "Survival Probability",
    title = plot_title
  ) 
p2

plot_fname <- paste0(data_dir,"/km_obs_open_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(paste0(plot_fname),p2,  width=20, height=15, units="cm")


### Cox Regression  - Age - SES fixed at start of period
#### Survival Analysis (https://cran.r-project.org/web/packages/survivalAnalysis/vignettes/multivariate.html)
#### http://www.sthda.com/english/wiki/cox-proportional-hazards-model


#### Univariable analysis 


covariates <- c("Age_start", "sex_start",  "SES_start")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(ntime, sero_event)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = surv_dat_km,id=IIntID)})

cox_fname <- paste0(data_dir,"/cox_univ_all_",as.character(start_date),"_",as.character(end_date),".txt")
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


#### Multivariable analysis 

cox_fname <- paste0(data_dir,"/cox_multi_all_",as.character(start_date),"_",as.character(end_date),".txt")
res.cox <- coxph(Surv(ntime, sero_event) ~ Age_start + sex_start + SES_start , data =  surv_dat_km,id=IIntID)
sink(cox_fname,append=FALSE)
summary(res.cox)
sink(file=NULL)
summary(res.cox)





### Cox Regression for individual observations - Age - SES as time varying covariates
#### Survival Analysis (https://cran.r-project.org/web/packages/survivalAnalysis/vignettes/multivariate.html)
#### http://www.sthda.com/english/wiki/cox-proportional-hazards-model


#### Univariable analysis 


covariates <- c("Age", "sex",  "SES")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(ntime, sero_event)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = surv_dat,id=IIntID)})

cox_fname <- paste0(data_dir,"/cox_univ_",as.character(start_date),"_",as.character(end_date),".txt")
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


#### Multivariable analysis 

cox_fname <- paste0(data_dir,"/cox_multi_",as.character(start_date),"_",as.character(end_date),".txt")
res.cox <- coxph(Surv(ntime, sero_event) ~ Age + sex + SES , data =  surv_dat,id=IIntID)
sink(cox_fname,append=FALSE)
summary(res.cox)
sink(file=NULL)
summary(res.cox)


