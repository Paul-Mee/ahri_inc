# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# The first thing to do is set the file paths to the AHRI datasets. 


data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'
code_dir <- 'C:/github/avdm_code_download/R/'

## Running R files 

source(paste0(code_dir,"ahri.R"))
source(paste0(code_dir,"data.R"))
source(paste0(code_dir,"getARTData.R"))
source(paste0(code_dir,"getBSData.R"))
source(paste0(code_dir,"getEpisodes-PM.R"))
source(paste0(code_dir,"getFiles.R"))
source(paste0(code_dir,"getHealthData.R"))
source(paste0(code_dir,"getHIV.R"))
source(paste0(code_dir,"getIncidence.R"))
source(paste0(code_dir,"imputeMethods.R"))
source(paste0(code_dir,"intCens.R"))
source(paste0(code_dir,"setArgs.R"))
source(paste0(code_dir,"setData.R"))
source(paste0(code_dir,"splitData-PM.R"))
source(paste0(code_dir,"test_ahri.R"))


# Define vector of package names

package_names <- c('haven','dplyr','ggplot2','ggthemes','zoo','stringr','survival',
                   'ggsurvfit','survivalAnalysis')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

#### Need to first run AHRI_SES.R and create a set of stata file with SES data for each quantile in each year 
#### Loop through number of quantiles to calcuate incidence for each


### Load filenames 


hiv_fname="RD05-99 ACDIS HIV All.dta"
wgh_fname="RD03-99 ACDIS WGH ALL.dta"
mgh_fname="RD04-99 ACDIS MGH ALL.dta" 
bsi_fname="RD01-03 ACDIS BoundedStructures.dta"
epi_fname="SurveillanceEpisodesHIV.dta"




### Set parameters for setting Args

start_year = 2005
end_year = 2022
#n_fact = 3 # Number of quantiles in SES
sim_num = 2 # Multiple imputation simulations 
age_min = 15 # minimum age for incidence calculation
age_max = 54 # maximum age for incidence calculation
gender = "all" # Include Males (male) Females (female) or both (all)
plot_title = "Incidence by wealth quantile - All"
plot_fname = "/Inc_SES_all.png"

getFiles <- setFiles(folder=data_dir,
                     hivfile=hiv_fname,
                     epifile=epi_fname,
                     wghfile=wgh_fname, 
                     mghfile=mgh_fname, 
                     bsifile=bsi_fname)
getFiles()[1:5]

#Set Args 
if(gender=="all") {
  Args <- setArgs(Years=c(start_year:end_year), 
                  Age=list(All=c(age_min,age_max)),
                  imputeMethod=imputeRandomPoint, nSim=sim_num)
} else if(gender=="male") {
  Args <- setArgs(Years=c(start_year:end_year), 
                  Age=list(Mal=c(age_min,age_max)),
                  imputeMethod=imputeRandomPoint, nSim=sim_num)
} else if(gender=="female") {
  Args <- setArgs(Years=c(start_year:end_year), 
                  Age=list(Fem=c(age_min,age_max)),
                  imputeMethod=imputeRandomPoint, nSim=sim_num)
} else {
  print("Gender not specified - using All") 
  Args <- setArgs(Years=c(start_year:end_year), 
                  Age=list(All=c(age_min,age_max)),
                  imputeMethod=imputeRandomPoint, nSim=sim_num)
}      

hiv <- setHIV(Args)

# 2020 round       
hiv$Year[hiv$VisitDate >= "2020-01-21" & hiv$VisitDate <= "2021-07-02"] <- 2020
# # 2021 round 
hiv$Year[hiv$VisitDate >= "2021-07-03" & hiv$VisitDate <=  "2022-04-20"] <- 2021
# # 2022 round 
hiv$Year[hiv$VisitDate >= "2022-04-21" & hiv$VisitDate <= "2023-05-05"] <- 2022
# # 2023 round 
hiv$Year[hiv$VisitDate >= "2023-05-06"] <- 2023

AggByYearSES = AggFunc("Year + wealth_quantile")
AggBySESYear = AggFunc("wealth_quantile + Year")

#aggFun = AggByYear
#aggFun = AggByYearSES
aggFun = AggBySESYear

# make multiple imputed datasets, run model on each
rtdat <- getRTData(hiv)
mdat <- MIdata(rtdat, Args)

#temp.df <- mdat[[1]]

# temp11.df <- mdat[[1]]
# temp12.df <- mdat[[2]]
# temp13.df <- mdat[[3]]

# Load data giving SES for each year of HIV data 
R_fname_SES <- paste0(data_dir,"/Vis_SES_Data.RDS")
Vis_SES <- readRDS(R_fname_SES)

Vis_SES$Year <- lubridate::year(Vis_SES$VisitDate)
## Get Year for each Visit Date
Yr_SES <- unique(Vis_SES[c('IIntID','Year','wealth_quantile')])

### Merge each dataframe in mdat with actual SES for that year and replace 
### wealth quantile with mean of other values if no SES for that year 
#tmp_m_1.df <- mdat[[1]]

for (i in 1:sim_num) {
print(i)
mdat[[i]]  <- merge(mdat[[i]] ,Yr_SES,by=c('IIntID','Year'),all.x=TRUE)
mdat[[i]]  <- mdat[[i]]  %>% 
  group_by(IIntID) %>% 
  mutate_at(c('wealth_quantile'), zoo::na.aggregate)
  mdat[[i]]$wealth_quantile <- as.integer(mdat[[i]]$wealth_quantile)
  # Calculating the mean serodate - updating for each iteration 
  m_tmp.df <- mdat[[i]] 
  if (i==1) {
    m_all.df <- m_tmp.df
    m_all.df$mean_sero_date  <- m_all.df$sero_date
  } else {
    m_tmp2.df <- m_tmp.df[c('IIntID','Year','sero_date')]
    names(m_tmp2.df)[3] <- "sero_date2"
    m_all.df <- merge(m_all.df,m_tmp2.df,by=(c('IIntID','Year')))
    #m_all.df$mean_sero_date <- (m_all.df$mean_sero_date + ((m_all.df$sero_date2 - m_all.df$mean_sero_date) / 2))
    #m_all.df$mean_sero_date <- mean.Date(m_all.df$mean_sero_date,m_all.df$sero_date2)
    m_all.df$mean_sero_date <- m_all.df$mean_sero_date  + floor((m_all.df$sero_date2-m_all.df$mean_sero_date)/2)
    ## Drop sero_date2
    m_all.df <- select(m_all.df, -c("sero_date2"))
  }
}

m_all.df$sero_date <- m_all.df$mean_sero_date
m_all.df <- select(m_all.df, -c("mean_sero_date"))




#sformula = "sero_event ~ -1 + as.factor(wealth_quantile) +  as.factor(Year) +   offset(log(tscale))"
 
#sformula = "sero_event ~ -1 +   as.factor(Year) + as.factor(wealth_quantile)   + offset(log(tscale))"

sformula = "sero_event ~ -1 +    as.factor(Year):as.factor(wealth_quantile) + as.factor(Year) + as.factor(wealth_quantile)   + offset(log(tscale))"
## first imputation used for KM
tmp_m_1.df <- mdat[[1]]

agg_inc <- lapply(mdat, aggFun)
agg_inc <- MIaggregate(agg_inc)


mdat <- mitools::imputationList(mdat)



mods <- with(mdat, stats::glm(as.formula(sformula), family=poisson))

# used in predict step to estimate by year
# newdata <- temp2.df %>% 
#            group_by (Year) %>% 
#            summarize(wealth_quantile = as.factor(as.integer(mean(wealth_quantile,na.rm=TRUE)))) %>%
#            mutate(Year = factor(Year), tscale = 1)

# used in predict step to estimate by wealth quantile
# newdata <- temp2.df %>%
#   group_by (wealth_quantile) %>%
#   summarize(Year = as.factor(as.integer(mean(Year,na.rm=TRUE)))) %>%
#   mutate(wealth_quantile = factor(wealth_quantile), tscale = 1)

newdata <- unique(m_all.df[c('Year','wealth_quantile')])
newdata <- dplyr::filter(newdata,!is.na(newdata$wealth_quantile))
newdata <- newdata %>% 
           dplyr::arrange(Year,wealth_quantile)
newdata$year_ses <- paste0(as.character(newdata$Year),"-",as.character(newdata$wealth_quantile))
newdata <- newdata[c('year_ses','Year','wealth_quantile')]
newdata$year_ses <- as.factor(newdata$year_ses)
newdata$Year <- as.factor(newdata$Year)
newdata$wealth_quantile <- as.factor(newdata$wealth_quantile)
newdata$tscale <- 1
 
 #factor
 
#MIpredict(mods,newdata)

### MIPredict code

object <- mods

if ("list" %in% class(object)) {
  res <- mitools::MIcombine(object)
  object <- object[[1]]
} else {
  res = object
}
Terms <- stats::delete.response(stats::terms(object))
m <- stats::model.frame(Terms, newdata, xlev = object$xlevels)
mat <- stats::model.matrix(Terms, m, contrasts.arg = object$contrasts)
pred <- mat %*% stats::coef(res)
se <-  sqrt(diag(mat %*% stats::vcov(res) %*% t(mat)))
fit <- exp(pred)
se.fit <- se * abs(exp(pred))
Qt <- c(-1, 1) * stats::qnorm((1 - 0.95)/2, lower.tail = FALSE)
CI <- sapply(Qt, "*", se.fit)
out <- data.frame(fit=fit, se.fit=se.fit, 
                  lci=fit+CI[, 1], uci=fit+CI[, 2])
out <- data.frame(lapply(out, "*", 100))
#rownames(out) <- object$xlevels[[1]]
rownames(out) <- newdata$year_ses # If using the interaction terms

pois_inc <- out

# pois_inc <- MIpredict(mods, newdata)
# list(agg=agg_inc, pois_inc=pois_inc)

### Plotting the data 

inc_plot_data <- pois_inc 

inc_plot_data$terms <- rownames(inc_plot_data)
inc_plot_data[c('Year', 'SES')] <- stringr::str_split_fixed(inc_plot_data$terms, '-', 2)
inc_plot_data <- inc_plot_data[c('Year','SES','fit','lci','uci')]
names(inc_plot_data )[3] <- "Incidence"


max_year <- 2022

SES_inc_plot <- inc_plot_data %>% filter(Year <= max_year)
SES_inc_plot$Year <- as.integer(SES_inc_plot$Year)
SES_inc_plot$SES <- as.factor(SES_inc_plot$SES)

# Make the plot

p1 <- ggplot(data=SES_inc_plot, aes(x=Year, y=Incidence, group=SES,
                             color=SES,ymin=lci, ymax=uci)) +
geom_line() +
geom_errorbar( width=.2) +
  theme_classic() +
  theme_igray() + scale_colour_colorblind() +
  scale_x_continuous(name ="Year",breaks = seq(2004,2022, by = 1)) +
  labs(color='SES',
       title = plot_title)
p1


ggsave(paste0(data_dir,plot_fname),p1,  width=20, height=15, units="cm")

###  KM curve code 

km_start_year <- 2015
km_end_year <- 2022

#Inc <- m_all.df[m_all.df$Year %in% c(km_start_year:km_end_year),]
Inc <- tmp_m_1.df[tmp_m_1.df$Year %in% c(km_start_year:km_end_year),] # Use first imputation 

### Keep those included in start_year

#Inc <- Inc[Inc$first_start_yr == km_start_year,] # closed cohort starting at beginning



Inc <- Inc %>%
  group_by(IIntID) %>%
  dplyr::mutate(first_start_yr = lubridate::year(min(obs_start)))

### Earliest start date

Inc <- Inc %>%
  group_by(IIntID) %>%
  dplyr::mutate(first_start_date = (min(obs_start)))





# datee <- as.Date("2017-12-31") ## Event = dead or end of observation
# Inc$Event_Date <- ifelse(is.na(Inc$sero_date), datee, Inc$sero_date)
# Inc$Event_Date <- as.Date(Inc$Event_Date)

Sero <- Inc[Inc$sero_event == 1, ]
Sero$ntime <- Sero$sero_date - Sero$first_start_date

### Drop those sero converting after end of follow-up
Sero <- Sero[(lubridate::year(Sero$sero_date) <= km_end_year),]

#### Aggregate incidence by group 




Non_Sero <- Inc[Inc$sero_event == 0, ]

##Keep distinct with earliest start date
NNon_Sero <- Non_Sero %>% group_by(IIntID) %>% dplyr::slice(which.max(obs_end))
NNon_Sero$ntime <- NNon_Sero$obs_end - NNon_Sero$first_start_date

##Rbind with sero data

T_Inc <- rbind(NNon_Sero, Sero)

### Two records for those who convert
### Ranking by sero-status
## Ranking by Individual Id , Year, year_diff
T_Inc <- T_Inc %>%
  group_by(IIntID) %>%
  dplyr::mutate(rank = order(sero_event, decreasing=TRUE))

### For sero converters remove duplicate
T_Inc <- T_Inc[T_Inc$rank==1,]

### Remove NA 
T_Inc <- T_Inc[!is.na(T_Inc$wealth_quantile),]



### Add label for wealth_quantile

T_Inc$SES <- ""

T_Inc <- within(T_Inc, SES[wealth_quantile==1] <- "Wealthiest")
T_Inc <- within(T_Inc, SES[wealth_quantile==2] <- "Medium")
T_Inc <- within(T_Inc, SES[wealth_quantile==3] <- "Least Wealthy")

T_Inc$SES <- factor(T_Inc$SES,
                      levels = c("Wealthiest", "Medium", "Least Wealthy"))

T_Inc$SES_char <- as.character(T_Inc$SES)

#### Aggregate incidence by group 

T_Inc_sum <- T_Inc %>% 
  group_by(SES) %>%                            # multiple group columns
  summarise(sum_event = sum(sero_event), sum_ntime = sum(ntime)) 

T_Inc_sum$int_ntime <- as.integer(T_Inc_sum$sum_ntime)

T_Inc_sum$incidence <- (T_Inc_sum$sum_event/T_Inc_sum$int_ntime)*365.25*100

Inc_fname <- paste0(data_dir,"/incidence_",km_start_year,"_",km_end_year,".txt")

sink(Inc_fname)
T_Inc_sum
sink()
T_Inc_sum



plot_title <- paste0("Kaplan-Meier plot (failure = seroconversion) \n Open cohort of individuals first tested between  ",km_start_year," and ",km_end_year)


#legend.labs= c("Wealthiest", "Medium", "Least Wealthy")

tmp <- survfit(Surv(ntime, sero_event) ~ SES, data = T_Inc)

p2 <-  survfit(Surv(ntime, sero_event) ~ SES, data = T_Inc)%>% 
  ggsurvfit() +
  labs(
    x = "Days to serconversion",
    y = "Probability of remaining HIV negative",
    title = plot_title
     ) +
   add_confidence_interval() +
   theme(plot.title = element_text(hjust = 0.5,),
        legend.direction='vertical',
        legend.position = c(0.2, 0.3),
        legend.text=element_text(size=13))

         
p2
surv_diff <- survdiff(Surv(ntime, sero_event) ~ SES, data = T_Inc)

survdiff_fname <- paste0(data_dir,"/surv_diff_",km_start_year,"_",km_end_year,".txt")
sink(survdiff_fname)
surv_diff
sink()
surv_diff





plot_fname <- paste0("/km_",km_start_year,"_",km_end_year,".png")

ggsave(paste0(data_dir,plot_fname),p2,  width=20, height=15, units="cm")


#### Survival Analysis (https://cran.r-project.org/web/packages/survivalAnalysis/vignettes/multivariate.html)
T_Inc$sex <- as.factor(T_Inc$Female)


res.cox <- coxph(Surv(ntime, sero_event) ~ Age + sex + SES , data =  T_Inc)

cox_fname <- paste0(data_dir,"/cox_",km_start_year,"_",km_end_year,".txt")
sink(cox_fname)
summary(res.cox)
sink()
summary(res.cox)
