# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# Define vector of package names

package_names <- c('haven','dplyr','ggplot2','ggthemes','zoo','stringr','survival',
                   'ggsurvfit','survivalAnalysis','NCmisc','devtools')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

# Set file paths
## AHRI data
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'
## Local copy of AHRI R code
code_dir <- 'C:/github/ahri_inc/avdm_ahri_code/R/'

## This is a download of the files from Alain Vandermael's AHRI R library 
## The files with suffix 'PM' have been updated to reflect changes in data file names 
## or R packages . 

### list of R code to be sourced

file_list <- c("ahri.R","data.R","getARTData.R","getBSData.R","getEpisodes-PM.R",
               "getFiles.R","getHealthData.R","getHIV-PM.R","getIncidence-PM.R","imputeMethods.R",
               "intCens.R","setArgs.R","setData-PM.R","splitData-PM.R","test_ahri.R")

## Source the file in the list
for(i in 1:length(file_list)){
  source(paste0(code_dir,file_list[i]))
}

### Set parameters for setting Args values 

start_year = 2005
end_year = 2023
#n_fact = 3 # Number of quantiles in SES
sim_num = 2 # Multiple imputation simulations 
age_min = 15 # minimum age for incidence calculation
age_max = 54 # maximum age for incidence calculation
gender = "all" # Include Males (male) Females (female) or both (all)


### Current default AHRI filenames 
hiv_fname="RD05-99 ACDIS HIV All.dta"
wgh_fname="RD03-99 ACDIS WGH ALL.dta"
mgh_fname="RD04-99 ACDIS MGH ALL.dta" 
bsi_fname="RD01-03 ACDIS BoundedStructures.dta"
epi_fname="SurveillanceEpisodesHIV.dta"


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

### Load HIV surveillance data 
### All data is in Southern PIPSA area 

hiv <- setHIV(Args)

### Rounds are labelled by Year but the dates have overlapped multiple years
### This code updates the year definitions
### Probably can be dropped 

# 2020 round       
hiv$Year[hiv$VisitDate >= "2020-01-21" & hiv$VisitDate <= "2021-07-02"] <- 2020
# # 2021 round 
hiv$Year[hiv$VisitDate >= "2021-07-03" & hiv$VisitDate <=  "2022-04-20"] <- 2021
# # 2022 round 
hiv$Year[hiv$VisitDate >= "2022-04-21" & hiv$VisitDate <= "2023-05-05"] <- 2022
# # 2023 round 
hiv$Year[hiv$VisitDate >= "2023-05-06"] <- 2023


# make multiple imputed datasets, run model on each
rtdat <- getRTData(hiv)
mdat <- MIdata(rtdat, Args)


# Load data giving SES for each year of HIV data for each Individual 
# Look at SES generation code to check this

#### Merge SES data with HIV data from AHRI_SES.R

### Read saved RDS file with SES quantiles
R_fname_SES <- paste0(data_dir,"/Surv_SES_Data.RDS")
Vis_SES <- readRDS(R_fname_SES)





### Merge each dataframe in mdat with actual SES for that year and replace 
### wealth quantile with mean of other values if no SES for that year 
### Better to generate a file with a real or imputed SES value before the merge step in the loop

# tmp.df <- mdat[[1]]
# i=1

for (i in 1:sim_num) {
print(i)
mdat[[i]]  <- merge(mdat[[i]] ,Vis_SES,by.x=c('IIntID','Year'),by.y=c('IIntId','Visit_Year'),all.x=TRUE)
### 
# mdat[[i]]  <- mdat[[i]]  %>% 
#   group_by(IIntID) %>% 
#   mutate_at(c('wealth_quantile'), zoo::na.aggregate)
#   mdat[[i]]$wealth_quantile <- as.integer(mdat[[i]]$wealth_quantile)
  ### For KM analysis - need a summary serodate from all the iterations
  
  # # Calculating the mean serodate - updating for each iteration 
  # m_tmp.df <- mdat[[i]] 
  # if (i==1) {
  #   m_all.df <- m_tmp.df
  #   m_all.df$mean_sero_date  <- m_all.df$sero_date
  # } else {
  #   m_tmp2.df <- m_tmp.df[c('IIntID','Year','sero_date')]
  #   names(m_tmp2.df)[3] <- "sero_date2"
  #   m_all.df <- merge(m_all.df,m_tmp2.df,by=(c('IIntID','Year')))
  #   #m_all.df$mean_sero_date <- (m_all.df$mean_sero_date + ((m_all.df$sero_date2 - m_all.df$mean_sero_date) / 2))
  #   #m_all.df$mean_sero_date <- mean.Date(m_all.df$mean_sero_date,m_all.df$sero_date2)
  #   m_all.df$mean_sero_date <- m_all.df$mean_sero_date  + floor((m_all.df$sero_date2-m_all.df$mean_sero_date)/2)
  #   ## Drop sero_date2
  #   m_all.df <- select(m_all.df, -c("sero_date2"))
  #}
}

# m_all.df$sero_date <- m_all.df$mean_sero_date
# m_all.df <- select(m_all.df, -c("mean_sero_date"))


## first imputation used for KM
tmp_m_1.df <- mdat[[1]]

table(tmp_m_1.df$Year,tmp_m_1.df$sero_event)

tmp_sero.df <- filter(tmp_m_1.df,sero_event==1)
table(tmp_sero.df$Year,tmp_sero.df$sero_event)


#### Incidence Prediction Code

## Definitions of additional aggregation functions used in the Poisson regression 
AggByYearSES = AggFunc("Year + wealth_quantile")
AggBySESYear = AggFunc("wealth_quantile + Year")

#aggFun = AggByYear
aggFun = AggBySESYear

agg_inc <- lapply(mdat, aggFun)
agg_inc <- MIaggregate(agg_inc)

#sformula = "sero_event ~ -1 + as.factor(wealth_quantile) +  as.factor(Year) +   offset(log(tscale))"

#sformula = "sero_event ~ -1 +   as.factor(Year) + as.factor(wealth_quantile)   + offset(log(tscale))"

sformula = "sero_event ~ -1 +    as.factor(Year):as.factor(wealth_quantile) + as.factor(Year) + as.factor(wealth_quantile)   + offset(log(tscale))"


mdat_l <- mitools::imputationList(mdat)

mods <- with(mdat_l, stats::glm(as.formula(sformula), family=poisson))

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

### Setting up data for prediction 

newdata <- unique(tmp_m_1.df[c('Year','wealth_quantile')])
newdata <- dplyr::filter(newdata,!is.na(newdata$wealth_quantile))
newdata <- newdata %>% 
  dplyr::arrange(Year,wealth_quantile)
newdata$year_ses <- paste0(as.character(newdata$Year),"-",as.character(newdata$wealth_quantile))
newdata <- newdata[c('year_ses','Year','wealth_quantile')]
newdata$year_ses <- as.factor(newdata$year_ses)
newdata$Year <- as.factor(newdata$Year)
newdata$wealth_quantile <- as.factor(newdata$wealth_quantile)
newdata$tscale <- 1



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


SES_inc_plot <- inc_plot_data %>% filter(Year <= end_year)
SES_inc_plot$Year <- as.integer(SES_inc_plot$Year)
SES_inc_plot$SES <- as.factor(SES_inc_plot$SES)

# Make the plot

plot_title <- paste0("Smoothed Incidence stratified by wealth quantile ", 
                    as.character(start_year)," to ", as.character(end_year))


p1 <- ggplot(data=SES_inc_plot, aes(x=Year, y=Incidence, group=SES,
                                    color=SES,ymin=lci, ymax=uci)) +
  geom_line() +
  geom_errorbar( width=.2) +
  theme_classic() +
  theme_igray() + scale_colour_colorblind() +
  scale_x_continuous(name ="Year",breaks = seq(2005,2021, by = 1),limits = c(2005, 2021)) +
  labs(color='SES',
       title = plot_title) 

p1

#ggsave(paste0(data_dir,plot_fname),p1,  width=20, height=15, units="cm")

### Ribbon plot

# p1 <- ggplot(data=SES_inc_plot) +
#   theme_bw() +
#       geom_ribbon(mapping = aes(x=Year, y=Incidence, group=SES, fill=SES,
#                            color=SES,ymin=lci, ymax=uci), alpha = 0.3) + guides(color = "none") +
#       geom_smooth( mapping = aes(x=Year, y=Incidence,group=SES,color=SES,fill=SES), se = FALSE ) +
#       scale_x_continuous(name ="Year",breaks = seq(2005,2021, by = 1),limits = c(2005, 2021)) +
#       labs(color='SES',
#        title = plot_title) 
# 
# p1

### Smoothed SE lines

## https://stackoverflow.com/questions/19643234/fill-region-between-two-loess-smoothed-lines-in-r-with-ggplot

# create plot object with loess regression lines
g1 <- ggplot(data=SES_inc_plot) + 
  stat_smooth(aes(x=Year, y = Incidence, colour = SES), method = "loess", se = FALSE) +
  stat_smooth(aes(x=Year, y = uci, colour = SES), method = "loess", se = FALSE) +
  stat_smooth(aes(x=Year, y = lci, colour = SES), method = "loess", se = FALSE)
#g1

# build plot object for rendering 
gg1 <- ggplot_build(g1)

gg1_dat <- data.frame(Year = gg1$data[[1]]$x,
                  Incidence = gg1$data[[1]]$y, 
                  lci_sm = gg1$data[[2]]$y,
                  uci_sm = gg1$data[[3]]$y,
                  SES = as.factor(gg1$data[[1]]$group))

gg1_dat$Wealth_Status <- as.character(gg1_dat$SES)

# gg1_dat <- within(gg1_dat, Wealth_Status[SES==1] <- "Wealthiest")
# gg1_dat <- within(gg1_dat, Wealth_Status[SES==2] <- "Medium")
# gg1_dat <- within(gg1_dat, Wealth_Status[SES==3] <- "Least Wealthy")

### Add 'ribbon' to plot

plot_fname = "/inc_smooth_ses.png"



p3 <- ggplot() + 
   theme_bw() +
   geom_ribbon(data = gg1_dat,aes(x = Year , ymin=lci_sm, ymax=uci_sm,
                                    group=Wealth_Status, fill=Wealth_Status),alpha=0.2) +
   geom_smooth(data=gg1_dat,aes(x=Year, y=Incidence,group=Wealth_Status,colour=Wealth_Status), se = FALSE, linetype = "dotted" ) +
   scale_x_continuous(name ="Year",breaks = seq(start_year,2021, by = 1),limits = c(start_year,2021)) +
  # change name of legend here 
  labs(title = plot_title)
p3



## Need to fix legend

ggsave(paste0(data_dir,plot_fname),p3,  width=20, height=15, units="cm")



###  KM curve code 

km_start_year <- 2015

km_start_date <- as.Date(paste0(as.character(km_start_year),"-01-01"))

km_end_year <- 2023


### Use first imputed data set
### Keep those known to be HIV negative in the start year 
Inc <- tmp_m_1.df[(lubridate::year(tmp_m_1.df$late_neg) >= km_start_year),]
### Keep observations after start year 
Inc <- Inc[(lubridate::year(Inc$obs_start) >= km_start_year),]

### Right Censor data at end of km_end_year
Inc <- Inc[(lubridate::year(Inc$obs_end) <= km_end_year),]

### Earliest start date

Inc <- Inc %>%
  group_by(IIntID) %>%
  dplyr::mutate(first_start_date = (min(obs_start)))


### Latest observation date 

Inc <- Inc %>%
  group_by(IIntID) %>%
  dplyr::mutate(last_end_date = (max(obs_end)))

#### Correct this based on Alans Nature Communications paper
# Assuming a
# uniform distribution, we imputed a single random infection date between the latest
# HIV-negative and earliest HIV-positive dates (the censoring interval). We then
# right censored the data at the latest HIV-negative date (if uninfected) or at the
# imputed date (if infected). 

Sero <- Inc[Inc$sero_event == 1, ]
Sero$ntime <- Sero$sero_date - Sero$first_start_date
## Defining start and end of interval

### Drop those sero converting after end of follow-up
Sero <- Sero[(lubridate::year(Sero$sero_date) <= km_end_year),]

## For non sero-converters select first observation ending after last negative test
## negative date

Inc$obs_to_ln <- as.integer(Inc$obs_end - Inc$late_neg)
## Drop observations ending before last negative date
Non_Sero <- Inc[(Inc$obs_to_ln >= 0 & Inc$sero_event == 0),]

Non_Sero <- Non_Sero %>% 
  group_by(IIntID) %>%
  slice(which.max(obs_to_ln))




## Calculate ntime

Non_Sero$ntime <- Non_Sero$late_neg - Non_Sero$first_start_date

### alternative assuming negative at end of last observation unless sero-converted used to create 
### KM plots which assume those lost to follow-up remain HIV negative

### Non_Sero$ntime <- max(Non_Sero$last_end_date) - Non_Sero$first_start_date



##Rbind with sero data

T_Inc <- rbind(Non_Sero, Sero)

### Two records for those who convert
### Keeping the one with highest sero-stautus 
T_Inc <- T_Inc %>%
  group_by(IIntID) %>%
  slice(which.max(sero_event))

### Remove NA 
### Need to fix this there should be a wealth quantile for every observation 
T_Inc <- T_Inc[!is.na(T_Inc$wealth_quantile),]



### Add label for wealth_quantile

T_Inc$SES <- ""

# T_Inc <- within(T_Inc, SES[wealth_quantile==1] <- "Wealthiest")
# T_Inc <- within(T_Inc, SES[wealth_quantile==2] <- "Medium")
# T_Inc <- within(T_Inc, SES[wealth_quantile==3] <- "Least Wealthy")
# 
# T_Inc$SES <- factor(T_Inc$SES,
#                     levels = c("Wealthiest", "Medium", "Least Wealthy"))

T_Inc$SES_char <- as.character(T_Inc$wealth_quantile)

T_Inc$SES <- as.factor(T_Inc$SES_char)

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

tmp <- survfit(Surv(time=ntime, event=sero_event==1) ~ SES, data=T_Inc)

dat <- as.data.frame(tmp$time)
dat$event <- tmp$n.event
dat$censor <- tmp$n.censor
dat$n.risk<- tmp$n.risk
dat$surv <- tmp$surv

p2 <-  survfit(Surv(time=ntime, event=sero_event==1) ~ SES, data=T_Inc)%>% 
  ggsurvfit(type = "survival") +
  labs(
    x = "Days to serconversion",
    y = "Survival Probability",
    title = plot_title
  ) +
  add_confidence_interval() +
  theme(plot.title = element_text(hjust = 0.5,),
        legend.direction='vertical',
        legend.position = c(0.2, 0.3),
        legend.text=element_text(size=13)) 
#adjust for required ranges
# scale_x_continuous(limits = c(0, 2500)) +
# scale_y_continuous(limits = c(0.5, 1.0)) 
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