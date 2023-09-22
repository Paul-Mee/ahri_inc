#### Alain example 

library(dplyr)
library(ahri)
getFiles <- setFiles('~/Documents/AHRI_Data/2020')
Args <- setArgs(Years = c(2008:2018),
                Age = list(Mal = c(15, 54)), nSim = 3)
dat <- setHIV(Args)
# simulate SES column
dat$SES <- sample(5, nrow(dat), replace = TRUE)
# only 1 SES value per IIntID
sdat <- distinct(dat, IIntID, .keep_all = TRUE)

# make multiple imputed datasets, run model on each
sformula = "sero_event ~ -1 + as.factor(Year) + SES + as.factor(Year):SES + offset(log(tscale))"
rtdat <- getRTData(dat)
rtdat <- inner_join(rtdat, select(sdat, IIntID, SES), by = "IIntID")
mdat <- MIdata(rtdat, Args)
mdat <- mitools::imputationList(mdat)
mods <- with(mdat, stats::glm(as.formula(sformula), family=poisson))

# used in predict step to estimate by year
newdata <- group_by(sdat, Year) %>% summarize(SES = mean(SES)) %>%
  mutate(Year = factor(Year), tscale = 1)
pois_inc <- MIpredict(mods, newdata)
