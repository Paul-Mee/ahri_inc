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


### Individuals to be included in the cohort 
### latest HIV negative on or after start date AND
### Earliest observation on or before start date AND
### Latest observation after start date 

surv_dat <- dplyr::filter(sero_data_imput_ses.df,((sero_data_imput_ses.df$late_neg >= start_date) &
                                                    (sero_data_imput_ses.df$first_start_date <= start_date) &
                                                    sero_data_imput_ses.df$last_end_date > start_date))

#### Select relevant episodes for these individuals 
#### observation ends after start date and observations start before the end date

surv_dat <- dplyr::filter(surv_dat,((surv_dat$obs_end > start_date) &
                                      surv_dat$obs_start < end_date) )

### If obs_end greater than end date then censor at end date

surv_dat <- within(surv_dat, obs_end[obs_end > end_date ] <- end_date)

### If obs_end greater than last negative then censor at last observation 
### check not needed ?



### Count number of individuals in cohort 
n_cohort <- dplyr::n_distinct(surv_dat$IIntID)

### Number of sero-conversions
n_sero <- sum(surv_dat$sero_event)

### list of those who sero convert 

tmp <- filter(surv_dat,(surv_dat$sero_event==1))
tmp <- tmp[c('IIntID')]
write.csv(tmp,file=paste0(data_dir,"/id_sero_closed.csv"))



print(paste0("Number in starting cohort - HIV negative on ",as.character(start_date), " = ", as.character(n_cohort)))
print(paste0("Number of sero-conversions ",as.character(n_sero)))

### Keep required variables for KM analysis

surv_dat <- surv_dat[c('IIntID','Year','sex','obs_start','obs_end','sero_event','Age','SES')]

### Create time variable 

surv_dat$ntime <- surv_dat$obs_end - surv_dat$obs_start

py_tot <- as.integer(sum(surv_dat$ntime))/365.25

print(paste0("Total person years of observation =  ",as.character(py_tot)))

### Events and PY by SES strata - by individual observations

sum_event_ses_obs  <- surv_dat %>% 
  group_by(SES) %>%                            
  summarise( sum_sero = sum(sero_event),sum_py = sum(as.integer(ntime)/365.25 )) 

sum_event_ses_obs$incid_100 <- 100*sum_event_ses_obs$sum_sero/sum_event_ses_obs$sum_py

sum_event_ses_obs
events_fname <- paste0(data_dir,"/events_ses_obs",as.character(start_date),"_",as.character(end_date),".csv")

write.csv(sum_event_ses_obs,file = events_fname)


### For KM plots reduce to a single row per individual 

surv_dat$ntime_days <- as.integer(surv_dat$ntime)
surv_dat$SES_int <- as.integer(surv_dat$SES)

### Max sero_event
surv_dat <- surv_dat %>%
  group_by(IIntID) %>%
  dplyr::mutate(max_sero = (max(sero_event)))

### Sum ntime
surv_dat <- surv_dat %>%
  group_by(IIntID) %>%
  dplyr::mutate(sum_ntime = (sum(ntime_days)))

#### SES varies over time - use SES at start of period under observation 
### Max SES 
## Ranking by Individual Id , Mid_year
surv_dat <- surv_dat %>%
  group_by(IIntID) %>%
  dplyr::mutate(rank = order(order(obs_start, decreasing=FALSE)))

tmp_ses <- surv_dat[c('IIntID','Age','sex','SES_int','rank')]
tmp_ses <- tmp_ses %>% dplyr::filter(rank == 1)

names(tmp_ses)[2] <- "Age_start"
names(tmp_ses)[3] <- "sex_start"
names(tmp_ses)[4] <- "SES_start"

tmp_ses <- tmp_ses[c('IIntID','Age_start','sex_start','SES_start')]

surv_dat_km <- merge(surv_dat,tmp_ses,by="IIntID")
surv_dat_km <- surv_dat_km %>% dplyr::filter(rank == 1)

### Keep required variables for KM analysis

surv_dat_km <- surv_dat_km[c('IIntID','Age_start','sex_start','SES_start',  'max_sero','sum_ntime')]

names(surv_dat_km)[5] <- "sero_event"
names(surv_dat_km)[6] <- "ntime"

surv_dat_km$SES_start <- as.factor(surv_dat_km$SES_start)

### Sero Events by starting SES group 

sum_event_ses_all  <- surv_dat_km %>% 
  group_by(SES_start) %>%                            
  summarise( sum_sero = sum(sero_event),sum_py = sum(as.integer(ntime)/365.25 )) 

sum_event_ses_all$incid_100 <- 100*sum_event_ses_all$sum_sero/sum_event_ses_all$sum_py

sum_event_ses_all

events_fname <- paste0(data_dir,"/events_ses_",as.character(start_date),"_",as.character(end_date),".csv")

write.csv(sum_event_ses_all,file = events_fname)

plot_title <- paste0("Kaplan-Meier plot (failure = seroconversion) \n Cohort of individuals HIV negative on  ",start_date,
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

plot_fname <- paste0(data_dir,"/km_all_",as.character(start_date),"_",as.character(end_date),".png")
ggsave(paste0(plot_fname),p2,  width=20, height=15, units="cm")


### Time variant covariates 


### Converting time to years 

plot_title <- paste0("Kaplan-Meier plot (failure = seroconversion) \n Cohort of individuals HIV negative on  ",start_date,
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

plot_fname <- paste0(data_dir,"/km_obs_",as.character(start_date),"_",as.character(end_date),".png")
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


