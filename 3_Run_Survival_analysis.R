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

surv_dat <- dplyr::filter(sero_data_imput_ses.df,((sero_data_imput_ses.df$late_neg >= start_date) &
                                                    (sero_data_imput_ses.df$obs_start < end_date) &
                                                    sero_data_imput_ses.df$obs_end > start_date))

### Count number of individuals in cohort 
n_cohort <- dplyr::n_distinct(surv_dat$IIntID)

### Number of sero-conversions
n_sero <- sum(surv_dat$sero_event)



print(paste0("Number in starting cohort - HIV negative on ",as.character(start_date), " = ", as.character(n_cohort)))
print(paste0("Number of sero-conversions ",as.character(n_sero)))

### Keep required variables for KM analysis

surv_dat <- surv_dat[c('IIntID','Year','sex','obs_start','obs_end','sero_event','Age','SES')]

### Create time variable 

surv_dat$ntime <- surv_dat$obs_end - surv_dat$obs_start

## Incidence 
incid_100py = 100*(n_sero/n_cohort)*(as.integer(end_date - start_date)/365.25)/(as.integer(end_date - start_date)/365.25)

print(paste0("Calculate overall incidence rate =  ",as.character(incid_100py)," per 100 person years" ))

### Plot KM curves with multiple episodes per individual 

### For KM plots reduce to a single row per individual 

surv_dat_km <- surv_dat %>% 
  group_by(IIntID) %>%                            # multiple group columns
  summarise( sero_end = max(sero_event), sum_ntime = sum(ntime),ses_max = max(as.integer(SES)) ) 

surv_dat_km$SES <- as.character(surv_dat_km$ses_max)

surv_dat_km$SES <- as.factor(surv_dat_km$SES)

names(surv_dat_km)[2] <- "sero_event"
names(surv_dat_km)[3] <- "ntime"

plot_title <- paste0("Kaplan-Meier plot (failure = seroconversion) \n Cohort of individuals HIV negative on  ",start_date,
                     "\n Censored on ",end_date)


p2 <-  survfit(Surv(time=ntime, event=sero_event==1) ~ SES, data=surv_dat_km,id=IIntID)%>% 
  ggsurvfit(type = "survival") +
  labs(
    x = "Days to serconversion",
    y = "Survival Probability",
    title = plot_title
  ) 
p2


### Cox Regression 
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


