
setwd("/Users/elphasokango/R/Personal/SA/HIV-INCIDENCE/2023_Data")

#install.packages("remotes")
#remotes::install_github('vando026/ahri')

library(ahri)
library(dplyr)
library(readstata13)


readEpi <- readEpisodes()

getFiles = setFiles("/Users/elphasokango/R/Personal/SA/HIV-INCIDENCE/2023_Data/Trial")
setFiles(
  folder = "/Users/elphasokango/R/Personal/SA/HIV-INCIDENCE/2023_Data/Trial",
  hivfile = "RD05-99 ACDIS HIV All.dta",
  epifile = "SurveillanceEpisodes.dta",
  wghfile = "RD03-99 ACDIS WGH ALL.dta",
  mghfile = "RD04-99 ACDIS MGH ALL.dta",
  bsifile = "RD01-03 ACDIS BoundedStructures.dta",
  hiv_rda = "ACDIS_HIV_All.Rda",
  epi_rda = "SurveillanceEpisodes.Rda",
  wgh_rda = "ACDIS_WGH_ALL.Rda",
  mgh_rda = "ACDIS_MGH_ALL.Rda",
  bsc_rda = "ACDIS_BoundedStructures.Rda"
)


Args <- setArgs(Years=c(2000:2023), 
                Age=list(All=c(15, 54)))


readHIVData()
hiv  <- getHIV()
rtdat <- getRTData(hiv)
rtdat
View(rtdat)


##getIncData
# Start from scratch
set.seed(100500)
Args <- setArgs(Years=c(2004:2023), 
                Age=list(All=c(15, 54)),
                imputeMethod=imputeRandomPoint)
hiv <- getHIV()
rtdat <- getRTData(hiv)
nsplitAtSeroDate <- function(
    dat=NULL,  splitYears=NULL) {
  dat <- rename(dat, event = .data$sero_event)
  dat <- mutate(dat, obs_end=ifelse(.data$event==1, .data$sero_date, .data$late_neg))
  edat <- splitData2(dat, years=splitYears)
  edat <- mutate(edat, Time = as.numeric(.data$obs_end - .data$obs_start))
  #if(any(edat$Time>366)) stop("Days in Year > 366")
  edat <- rename(edat, sero_event = .data$event)
  tibble::as_tibble(edat)
}

ngetIncData <- function(rtdat, bdat = NULL, Args, func = identity) {
  if (is.null(bdat)) bdat = getBirthDate()
  dat <- Args$imputeMethod(rtdat)
  edat <- nsplitAtSeroDate(dat) 
  edat <- setData(edat, Args, time2 = "obs_end", birthdate = bdat)
  edat <- mutate(edat, ntscale = .data$Time/365.25)
  func(edat)
}
idat <- ngetIncData(rtdat, bdat=getBirthDate(), Args)
idat

idat$tscale = ifelse(idat$Time>366, idat$Time/1215, idat$Time/365.25)





###-----------------------------------------------
###Incidence

nn = idat[idat$Year %in% c(2005:2023),]

nn$Year = as.factor(nn$Year)
sformula <- "sero_event ~ - 1 + Year + offset(log(tscale))"
mod <- stats::glm(as.formula(sformula), data=nn[nn$Female ==1, ], family=poisson)
mod


# Make the new data.frame
yr_dat <- data.frame(Year=unique(nn$Year), tscale=1)

# Show the new data.frame
yr_dat

# Get incidence rates and CIs
Incc = MIpredict(mod, newdata=yr_dat)
Incc$Year = 2005:2022

Incc$Year = 2005:2022

plot(Incc$Year, Incc$fit, ylim = c(0,5), pch = 4) 


fit <- smooth.spline(Incc$fit~Incc$Year, df = 6)
fit2 <- smooth.spline(Incc$uci~Incc$Year, df = 6)
fit3 <- smooth.spline(Incc$lci~Incc$Year, df = 6)



mod2 <- stats::glm(as.formula(sformula), data=nn[nn$Female ==0, ], family=poisson)
mod2


# Make the new data.frame
yr_dat <- data.frame(Year=unique(nn$Year), tscale=1)

# Show the new data.frame
yr_dat

# Get incidence rates and CIs
Inccm = MIpredict(mod2, newdata=yr_dat)
Inccm$Year = 2005:2022


fitm <- smooth.spline(Inccm$fit~Inccm$Year, df = 6)
fitm2 <- smooth.spline(Inccm$uci~Inccm$Year, df = 6)
fitm3 <- smooth.spline(Inccm$lci~Inccm$Year, df = 6)


plot(Incc$Year~Incc$fit, type="l", lty=1, col="white", 
     ylab="Incidence rate per 100 person-years", xlab="Year", ylim=c(0.0,7.0), xlim=c(2005,2022), cex.lab=1.8, cex.axis=2)

abline(h=c(0,1,2,3,4,5,6,7), col="gray70", lty=2)
abline(v=c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022), col="gray70", lty=2)

polygon(c(Incc$Year, rev(Incc$Year)), c(fit2$y, rev(fit3$y)),
        col = adjustcolor("palegreen3",alpha.f = 0.65), border = NA)

lines(fit, col = "seagreen", lwd = 1.5)

points(Incc$Year, Incc$fit, pch = 4)


polygon(c(Inccm$Year, rev(Inccm$Year)), c(fitm2$y, rev(fitm3$y)),
        col = adjustcolor("lightgoldenrod1",alpha.f = 0.65), border = NA)

lines(fitm, col = "darkgoldenrod1", lwd = 1.5)

points(Inccm$Year, Inccm$fit, pch = 3)



Nidat = idat %>% subset(idat$AgeCat == "[15,20)" | idat$AgeCat == "[20,25)")

## Clculating HIV Incidence

idat_yr <- AggByYear(idat)
idat_yr

idat_age <- AggByAge(idat)
idat_age


Nidat_yr <- AggByYear(Nidat)
Nidat_yr

### Crude Incidence

calcPoisExact(idat_yr, byVar="Year")

ii = calcPoisExact(idat_age, byVar="Age")
###Cummulative incidence for females


##Poison regression
# Make year a factor variable
idat <- dplyr::mutate(idat, Year = as.factor(Year))
sformula <- "sero_event ~ - 1 + Year + offset(log(tscale))"

# Run the Poisson model

idat$Female = as.factor(idat$Female)
mod <- stats::glm(as.formula(sformula), data=idat, family=poisson)
mod



# Make the new data.frame
yr_dat <- data.frame(Year=unique(idat$Year), tscale=1)

# Show the new data.frame
yr_dat

# Get incidence rates and CIs
Incc = MIpredict(mod, newdata=yr_dat)
Incc$Year = 2004:2022


##Female

modf <- stats::glm(as.formula(sformula), data=idat[idat$Female == 1,], family=poisson)
modf

# Get incidence rates and CIs
Inccf = MIpredict(modf, newdata=yr_dat)
Inccf$Year = 2004:2022


##Male

modm <- stats::glm(as.formula(sformula), data=idat[idat$Female == 0,], family=poisson)
modm

# Get incidence rates and CIs
Inccm = MIpredict(modm, newdata=yr_dat)
Inccm$Year = 2004:2022
## Plots

plot(Incc$Year,Incc$fit, col="white", 
     xlab="Year", ylab="HIV Incidence", ylim=c(0,10), xlim=c(2004,2022), cex.lab=1.3, cex.axis=2)



abline(h=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10), col="gray70", lty=2)
abline(v=c(2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022), col="gray70", lty=2)

polygon(c(Incc$Year, rev(Incc$Year)), c(Incc$uci, rev(Incc$lci)),
        col = adjustcolor("palegreen3", alpha.f = 0.65), border = NA)

lines(Incc$fit~Incc$Year, type="l", lty=1, lwd=2, col="seagreen", ylim=c(0.0, 10.0))


polygon(c(Inccf$Year, rev(Inccf$Year)), c(Inccf$uci, rev(Inccf$lci)),
        col = adjustcolor("lightgoldenrod1", alpha.f = 0.65), border = NA)


lines(Inccf$fit~Inccf$Year, type="l", lty=1, lwd=2, col="darkgoldenrod1", ylim=c(0.0, 10.0))



polygon(c(Inccm$Year, rev(Inccm$Year)), c(Inccm$uci, rev(Inccm$lci)),
        col = adjustcolor("lightblue2", alpha.f = 0.65), border = NA)


lines(Inccm$fit~Inccm$Year, type="l", lty=1, lwd=2, col="dodgerblue2", ylim=c(0.0, 10.0))

legend("topleft", legend=c("Females", "Population", "Males"), 
       col=c("darkgoldenrod1","seagreen", "dodgerblue2"), lty=1, lwd = 5,cex = 1, bty="n")


##Age
##Poison regression
# Make age a factor variable
idat <- dplyr::mutate(idat, Age = as.factor(Age))
nsformula <- "sero_event ~ - 1 + Age + offset(log(tscale))"


mod_age <- stats::glm(as.formula(nsformula), data=idat, family=poisson)
mod_age
# Make the new data.frame
Age_dat <- data.frame(Age=unique(idat$Age), tscale=1)

# Show the new data.frame
Age_dat

##Predictions
Ageall = MIpredict(mod_age, newdata=Age_dat)
Ageall$Age = 15:54
Ageall = Ageall[1:39, ]

#Females
mod_agef <- stats::glm(as.formula(nsformula), data=idat[idat$Female==1,], family=poisson)
mod_agef

##Predictions
Ageallf = MIpredict(mod_agef, newdata=Age_dat)
Ageallf$Age = 15:54
Ageallf = Ageallf[1:39,]
##Predictions
Ageallf = MIpredict(mod_agef, newdata=Age_dat)


#Males
mod_agem <- stats::glm(as.formula(nsformula), data=idat[idat$Female==0,], family=poisson)
mod_agem

##Predictions
Ageallm = MIpredict(mod_agem, newdata=Age_dat)
Ageallm$Age = 15:54
Ageallm = Ageallm[1:39,]



plot(Ageall$Age,Ageall$fit, col="white", 
     xlab="Age", ylab="HIV Incidence", ylim=c(0,10), xlim=c(15,53), cex.lab=1.3, cex.axis=2)



abline(v=c(15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53), col="gray70", lty=2)
abline(h=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10), col="gray70", lty=2)

polygon(c(Ageall$Age, rev(Ageall$Age)), c(Ageall$uci, rev(Ageall$lci)),
        col = adjustcolor("palegreen3", alpha.f = 0.65), border = NA)

lines(Ageall$fit~Ageall$Age, type="l", lty=1, lwd=2, col="seagreen", ylim=c(0.0, 10.0))


polygon(c(Ageallf$Age, rev(Ageallf$Age)), c(Ageallf$uci, rev(Ageallf$lci)),
        col = adjustcolor("lightgoldenrod1", alpha.f = 0.35), border = NA)


lines(Ageallf$fit~Ageallf$Age, type="l", lty=1, lwd=2, col="darkgoldenrod1", ylim=c(0.0, 10.0))



polygon(c(Ageallm$Age, rev(Ageallm$Age)), c(Ageallm$uci, rev(Ageallm$lci)),
        col = adjustcolor("lightblue2", alpha.f = 0.35), border = NA)


lines(Ageallm$fit~Ageallm$Age, type="l", lty=1, lwd=2, col="dodgerblue2", ylim=c(0.0, 10.0))

legend("topleft", legend=c("Females", "Population", "Males"), 
       col=c("darkgoldenrod1","seagreen", "dodgerblue2"), lty=1, lwd = 5,cex = 1, bty="n")





Inccf$Year = 2004:2022
NNidat = idat %>% subset(Female == "1")

Incidence_by_age = NNidat %>% group_by(Age)  %>%  summarise(total = n(), Incidence = sum(sero_event)) 


Incidence_by_age$Cumm = cumsum(Incidence_by_age$Incidence)
Incidence_by_age$Cumm_Total = cumsum(Incidence_by_age$total)

Incidence_by_age$CI = round(Incidence_by_age$Cumm/Incidence_by_age$Cumm_Total, 2)

Incidence_by_age$Inc = round(Incidence_by_age$Incidence/ Incidence_by_age$total,4)

sum(Incidence_by_age$Inc)

#### New Incidence for females 15-25
NNidat = idat %>% subset(Female == "1") %>% filter(Age >=15 & Age <=25)



#Aggregate by age

AggByAge = AggFunc("Age")
inc_age <- AggByAge(NNidat)
inc_age

## Calculate crude incidence

inc = calcPoisExact(inc_age, byVar="Age")
inc$Age = 15:25

### https://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_diseasefrequency/ep713_diseasefrequency4.html
New_fem = idat %>% subset(Female == "1")
Agg_Fem = New_fem %>% group_by(Age) %>% summarise(Inc = sum(sero_event))
gg = idat %>%  group_by(Age) %>% count()
Agg_Fem$n = gg$n

Agg_Fem$Ninc = Agg_Fem$Inc/Agg_Fem$n

Agg_Fem$cumm = cumsum(Agg_Fem$Ninc)


Agg_Fem$lower = Agg_Fem$cumm -1.6849*(0.2143455/sqrt(40))



Fplot = ggplot() +
  theme_bw() +
  labs(x = "Age (Years)", y = "HIV Cummulative Incidence", colour = "Legend", fill = "") +
  #geom_point(mapping = aes(x = z, y = y, colour = "data")) +
  geom_line(mapping = aes(x = Agg_Fem$Age, y = Agg_Fem$cumm), size = 1, alpha = 1, linetype = "dotdash", color = "red") +
  geom_line(mapping = aes(x = Agg_mal$Age, y = Agg_mal$cumm), size = 1, alpha = 1, linetype = "twodash", color = "blue") +theme_classic()
  
###Fplot by year

fim = idat %>% filter(idat$Female == "1")


#idat04 = fim %>% filter(fim$Year == 2004)
idat04 = fim %>% filter(fim$Year %in% c(2004:2009))

idat04 = idat04 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

#gg = idat %>% filter(idat$Year == 2004) %>% group_by(Age) %>% count()
gg = idat %>% filter(idat$Year %in% c(2004:2009)) %>% group_by(Age) %>% count()

idat04$n = gg$n

idat04$Ninc = idat04$Inc/idat04$n
idat04$cumm = cumsum(idat04$Ninc)


#idat12 = fim %>% filter(fim$Year == 2012)
idat10 = fim %>% filter(fim$Year %in% c(2010:2014))

#idat12 = idat12 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

idat10 = idat10 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

#gg = idat %>% filter(idat$Year == 2012) %>% group_by(Age) %>% count()

gg = idat %>% filter(idat$Year %in% c(2010:2014)) %>% group_by(Age) %>% count()

#gg = gg[-1,]
idat10$n = gg$n

idat10$Ninc = idat10$Inc/idat10$n
idat10$cumm = cumsum(idat10$Ninc)


#idat18 = fim %>% filter(fim$Year == 2018)
idat15 = fim %>% filter(fim$Year %in% c(2015:2019))

#idat18 = idat18 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

idat15 = idat15 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

#gg = idat %>% filter(idat$Year == 2018) %>% group_by(Age) %>% count()

gg = idat %>% filter(idat$Year %in% c(2015:2019)) %>% group_by(Age) %>% count()

idat15$n = gg$n

idat15$Ninc = idat15$Inc/idat15$n
idat15$cumm = cumsum(idat15$Ninc)

cum_Fem = ggplot() +
  theme_bw() +
  labs(x = "Age (Years)", y = "HIV Cummulative Incidence", colour = "Legend", fill = "") +
  #geom_point(mapping = aes(x = z, y = y, colour = "data")) +
  geom_line(mapping = aes(x = idat04$Age, y = idat04$cumm), size = 1, alpha = 1, linetype = "dotdash", color = "red") +
  geom_line(mapping = aes(x = idat10$Age, y = idat10$cumm), size = 1, alpha = 1, linetype = "twodash", color = "blue") +
  geom_line(mapping = aes(x = idat15$Age, y = idat15$cumm), size = 1, alpha = 1, linetype = "twodash", color = "orange") +
  scale_colour_manual(name="Years",values=cols) + theme(legend.position = c(0.19, 0.87))


plot(idat04$cumm~idat04$Age, col="white", 
     xlab="Male age (years)", ylab="Cummulative HIV Incidence", ylim=c(0.0,0.9), xlim=c(15,55), cex.lab=1.3, cex.axis=2)



abline(h=c(0.1,0.2,0.3,0.4,0.5,0.6, 0.7, 0.8, 0.9), col="gray70", lty=2)
abline(v=c(15,20,25,30,35,40,45,50,55), col="gray70", lty=2)

lines(idat04$cumm~idat04$Age, type="b", pch=0, col="dodgerblue2", lwd=2) 

lines(idat10$cumm~idat10$Age, type="b", pch=5, col="seagreen", lwd=2, cex=1.5)

lines(idat15$cumm~idat15$Age, type="b", pch=19, col="darkgoldenrod1", lwd=2, cex=1.5)

legend("topleft", legend=c("2010-2014","2004-2009", "2015-2019"), 
       col=c("seagreen","dodgerblue2", "darkgoldenrod1"), 
       pch=c(0,5,19), lty=1, bty="n")

###MALES

mim = idat %>% filter(idat$Female == "0")


#midat04 = mim %>% filter(mim$Year == 2004)
midat04 = mim %>% filter(mim$Year %in% c(2004:2009))

midat04 = midat04 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

#mgg = midat %>% filter(midat$Year == 2004) %>% group_by(Age) %>% count()
mgg = idat %>% filter(idat$Year %in% c(2004:2009)) %>% group_by(Age) %>% count()

midat04$n = mgg$n

midat04$Ninc = midat04$Inc/midat04$n
midat04$cumm = cumsum(midat04$Ninc)


#midat12 = mim %>% filter(mim$Year == 2012)
midat10 = mim %>% filter(mim$Year %in% c(2010:2014))

#midat12 = midat12 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

midat10 = midat10 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

#mgg = midat %>% filter(midat$Year == 2012) %>% group_by(Age) %>% count()

mgg = idat %>% filter(idat$Year %in% c(2010:2014)) %>% group_by(Age) %>% count()

#mgg = mgg[-1,]
midat10$n = mgg$n

midat10$Ninc = midat10$Inc/midat10$n
midat10$cumm = cumsum(midat10$Ninc)


#midat18 = mim %>% filter(mim$Year == 2018)
midat15 = mim %>% filter(mim$Year %in% c(2015:2019))

#midat18 = midat18 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

midat15 = midat15 %>% group_by(Age) %>% summarise(Inc = sum(sero_event))

#mgg = midat %>% filter(midat$Year == 2018) %>% group_by(Age) %>% count()

mgg = idat %>% filter(idat$Year %in% c(2015:2019)) %>% group_by(Age) %>% count()

midat15$n = mgg$n

midat15$Ninc = midat15$Inc/midat15$n
midat15$cumm = cumsum(midat15$Ninc)

cum_Mal = ggplot() +
  theme_bw() +
  labs(x = "Age (Years)", y = "HIV Cummulative Incidence", colour = "Legend", fill = "") +
  #geom_point(mapping = aes(x = z, y = y, colour = "data")) +
  geom_line(mapping = aes(x = midat04$Age, y = midat04$cumm), size = 1, alpha = 1, linetype = "dotdash", color = "palegreen3") +
  geom_line(mapping = aes(x = midat10$Age, y = midat10$cumm), size = 1, alpha = 1, linetype = "twodash", color = "seagreen") +
  geom_line(mapping = aes(x = midat15$Age, y = midat15$cumm), size = 1, alpha = 1, linetype = "twodash", color = "darkgoldenrod1") + ylim(0,0.9)


gridExtra::grid.arrange(cum_Mal, cum_Fem, nrow = 1)

## Agg Female

New_mal = idat %>% subset(Female == "0")
Agg_mal = idat %>% subset(Female == "0") %>%  group_by(Age) %>% summarise(Inc = sum(sero_event))
gg = idat %>%  group_by(Age) %>% count()
Agg_mal$n = gg$n

Agg_mal$Ninc = Agg_mal$Inc/Agg_mal$n

Agg_mal$cumm = cumsum(Agg_mal$Ninc)

p <- ggplot(Agg_mal, aes(Age, cumm))
p + geom_smooth(method = "loess")
## Cummulative incidence


fidat = idat %>% filter(Female == 1)

finc_age <- AggByAge(fidat)
finc_age

finc = calcPoisExact(finc_age, byVar="Age")
finc

finc$Age = 15:54

finc$cumm = cumsum(finc$rate)

sum(finc$rate)


##Male

Midat = idat %>% filter(Female == 0)

Minc_age <- AggByAge(Midat)
Minc_age

Minc = calcPoisExact(Minc_age, byVar="Age")
Minc

Minc$Age = 15:54

Minc$cumm = cumsum(Minc$rate)

sum(Minc$rate)
###Poisson

library("tidycmprsk")
library(ggsurvfit)
library(survival)

names = c("status", "time")
cumm_dat = fidat %>% select(sero_event, Age) 
  
colnames(cumm_dat) = names 
cumm_dat$status = as.factor(cumm_dat$status)

cumm_mod = cuminc(Surv(time, status) ~ 1, data = cumm_dat)

cumm_mod %>%  ggcuminc() + 
  labs(
    x = "Days"
  ) + 
  add_confidence_interval() +
  add_risktable()


cumm_mod1 = survfit(Surv(time, status) ~ 1, data = cumm_dat)

plot(cumm_mod1, fun="cumhaz")

# Run the model
sformula <- "sero_event ~ - 1 + Year + Age+ Year:Age + offset(log(tscale))"
mod <- stats::glm(as.formula(sformula), data=fidat, family=poisson)


# Make the new data.frame
age_dat <- data.frame(Age=unique(fidat$Age), tscale=1)

# Show the new data.frame
colnames(age_dat) = c("Year","tscale")



MIpredict(mod, newdata=age_dat)


getAgeAge<- function(dat) {
  group_by(dat, .data$Age) %>% 
    summarize(Age = mean(.data$Age)) %>% 
    mutate(Age = factor(.data$Age), tscale=1)
}


# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/forest.html
### prevalence
#setwd("/Users/elphasokango/R/Personal/HIV-INCIDENCE")
HIVALL <- read.dta13("RD05-99 ACDIS HIV All.dta", 
                     convert.factors = T, generate.factors = T,
                     encoding = NULL, fromEncoding = NULL, convert.underscore = FALSE,
                     missing.type = TRUE, convert.dates = TRUE, replace.strl = FALSE,
                     add.rownames = FALSE, nonint.factors = TRUE) 


NHIVALL = HIVALL %>% select(IIntId, VisitDate, HIVResult, AgeAtVisit, Sex)

NHIVALL$Year =  as.numeric(substr(NHIVALL$VisitDate, 1,4))


NHIVALL = NHIVALL %>% subset(NHIVALL$HIVResult == "Negative" | NHIVALL$HIVResult == "Positive") 


New_HIV = NHIVALL %>% group_by(IIntId, Year) %>% filter(VisitDate == max(VisitDate)) %>% distinct(IIntId, VisitDate, .keep_all = TRUE)

#New_HIV = New_HIV %>% filter(Year >= "2008")

New_HIV$Status = ifelse(New_HIV$HIVResult == "Positive", 1,0)
New_HIV = New_HIV |> subset(New_HIV$Sex == "Male" | New_HIV$Sex == "Female")
New_HIV$Gender = ifelse(New_HIV$Sex =="Female", 0, 1)

New_HIV$Status = as.factor(New_HIV$Status)
New_HIV$Gender = as.factor(New_HIV$Gender)
New_HIV$Year = as.factor(New_HIV$Year)
New_HIV$AgeAtVisit = as.numeric(New_HIV$AgeAtVisit)


New_HIV = New_HIV |> subset(New_HIV$AgeAtVisit>=15) 

New_HIV = New_HIV |> subset(New_HIV$AgeAtVisit < 56)

New_HIV = New_HIV |> mutate(yg = interaction(Year, Gender))

New_HIV = New_HIV %>% subset(New_HIV$Year %in% c(2003:2023))


New_HIV$Year = droplevels(New_HIV$Year)
library(mgcv)

dat = head(New_HIV, 50000)
dat = New_HIV
dat$Gender = as.factor(dat$Gender)
dat$Year = as.factor(dat$Year)


library(gratia)
Mod2 = gam(Status ~ Gender + Year + s(AgeAtVisit,by = Year) + s(AgeAtVisit, by = Gender), family = "binomial",   data = dd)

draw(Mod2)

Mod3 = gam(Status ~ Gender + Year + s(AgeAtVisit, Year, bs = "fs", by = Gender), family = "binomial",   data = dd)

draw(Mod3, labeller = ggplot::label_both())
##https://stackoverflow.com/questions/75239262/plot-gam-results-with-original-x-values-not-scaled-and-centred
#https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.terms.html

b = sample(1:169082, 50000)
dd = New_HIV[b,] #initial 

#Mod33 = gam(Status ~ s(Year)
             #+s(AgeAtVisit, by = Year) + s(AgeAtVisit, by = Gender), family = "binomial",   data = dd)


dat = dd
model_p <- predict(Mod3, newdata = dat, se.fit = TRUE)

New_mode_p = dat %>% ungroup() %>% 
  mutate(
    model1 = model_p$fit,
    lower1 = model_p$fit - 1.96 * model_p$se.fit, # 95% C.I.
    upper1 = model_p$fit + 1.96 * model_p$se.fit,
  )

## Add fitted values

With_fitted = add_fitted(New_mode_p, Mod3)

CI = fitted_values(Mod3, ci_level = 0.95)

With_fitted$lower = CI$lower
With_fitted$upper = CI$upper


#--------------------------------------------------
##split by Year

Year = split(New_mode_p, f = New_mode_p$Year)

Year04 = Year$'2004'
Year12 = Year$'2012'
Year19 = Year$'2023'


##Split by gender

Gender04 = split(Year04, f = Year04$Sex)
Year04M = Gender04$Male
Year04F = Gender04$Female

Gender12 = split(Year12, f = Year12$Sex)
Year12M = Gender12$Male
Year12F = Gender12$Female

Gender19 = split(Year19, f = Year19$Sex)
Year19M = Gender19$Male
Year19F = Gender19$Female

Male_Data = rbind(Year04M,Year12M, Year19M)

Male_Data$Year = as.factor(Male_Data$Year)
Female_Data = rbind(Year04F, Year12F, Year19F)


New_Male = Male_Data %>% group_by(Year, AgeAtVisit) %>% summarise(Meanfit = mean(model1), Meanupper = mean(upper1),
                                                                  Meanlower = mean(lower1))

New_Female = Female_Data %>% group_by(Year, AgeAtVisit) %>% summarise(Meanfit = mean(model1), Meanupper = mean(upper1),
                                                                      Meanlower = mean(lower1))

bb = New_Male[New_Male$Year == 2004, ]

cc = New_Male[New_Male$Year == 2012, ]

dd = New_Male[New_Male$Year == 2023, ]

fbb = New_Female[New_Female$Year == 2004, ]
fcc =  New_Female[New_Female$Year == 2012, ]
fdd =  New_Female[New_Female$Year == 2023, ]

library(ggplot2)
Males = ggplot() +
  theme_bw() +
  labs(x = "Age (Years)", y = "Effect", colour = "Legend", fill = "") +
  #geom_point(mapping = aes(x = z, y = y, colour = "data")) +
  geom_line(mapping = aes(x = Male_Data$AgeAtVisit, y = Male_Data$model1, colour = "2004"), size = 1, alpha = 1) +
  geom_ribbon(mapping = aes(x = Male_Data$AgeAtVisit, ymin = Male_Data$lower1, ymax = Male_Data$upper1, fill = "2004"), alpha = 0.3) + guides(color = "none")


Male = ggplot() +
  theme_bw() +
  labs(x = "Age (Years)", y = "Effect", colour = "Legend", fill = "") +
  #geom_point(mapping = aes(x = z, y = y, colour = "data")) +
  geom_line(mapping = aes(x = bb$AgeAtVisit, y = bb$Meanfit, color = "2004"), size = 1, alpha = 1, color = "seagreen", show.legend = FALSE) +
  geom_ribbon(mapping = aes(x = bb$AgeAtVisit, ymin = bb$Meanlower, ymax = bb$Meanupper,  fill = "2004"), alpha = 0.3, show.legend = FALSE) + 
  geom_line(mapping = aes(x = cc$AgeAtVisit, y = cc$Meanfit, color = "2012"), size = 1, alpha = 1, show.legend = FALSE) +
  geom_ribbon(mapping = aes(x = cc$AgeAtVisit, ymin = cc$Meanlower, ymax = cc$Meanupper, fill = "2012"), alpha = 0.3, show.legend = FALSE) + 
  geom_line(mapping = aes(x = dd$AgeAtVisit, y = dd$Meanfit, color = "2019"), size = 1, alpha = 1, show.legend = FALSE) +
  geom_ribbon(mapping = aes(x = dd$AgeAtVisit, ymin = dd$Meanlower, ymax = dd$Meanupper, fill = "2019"), alpha = 0.3, show.legend = FALSE) + ylim(0,0.7) + theme_classic()


Female = ggplot() +
  theme_bw() +
  labs(x = "Age (Years)", y = "Effect", colour = "Legend", fill = "") +
  #geom_point(mapping = aes(x = z, y = y, colour = "data")) +
  geom_line(mapping = aes(x = fbb$AgeAtVisit, y = fbb$Meanfit, color = "2004"), size = 1, alpha = 1, show.legend = FALSE) +
  #scale_y_continuous(sec.axis = sec_axis(~ . *1, name = "Effect"))+
  geom_ribbon(mapping = aes(x = fbb$AgeAtVisit, ymin = fbb$Meanlower, ymax = fbb$Meanupper, fill = "2004"), alpha = 0.3) + 
  geom_line(mapping = aes(x = fcc$AgeAtVisit, y = fcc$Meanfit, color = "2012"), size = 1, alpha = 1, show.legend = FALSE) +
  geom_ribbon(mapping = aes(x = fcc$AgeAtVisit, ymin = fcc$Meanlower, ymax = fcc$Meanupper, fill = "2012"), alpha = 0.3) + 
  geom_line(mapping = aes(x = fdd$AgeAtVisit, y = fdd$Meanfit, color = "2019"), size = 1, alpha = 1, show.legend = FALSE) +
  geom_ribbon(mapping = aes(x = fdd$AgeAtVisit, ymin = fdd$Meanlower, ymax = fdd$Meanupper, fill = "2019"), alpha = 0.3) + ylim(0,0.7) + theme_classic()


gridExtra::grid.arrange(Male, Female, nrow = 1)

## Males

p = par(mfrow = c(1,2))
plot(bb$AgeAtVisit~bb$Meanfit, type="l", lty=1, col="white", 
     xlab="Male age (years)", ylab="HIV Prevalence", ylim=c(-5,0), xlim=c(15,55), cex.lab=1.8, cex.axis=2)

abline(h=c(-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5), col="gray70", lty=2)
abline(v=c(15,20,25,30,35,40,45,50,55), col="gray70", lty=2)

polygon(c(bb$AgeAtVisit, rev(bb$AgeAtVisit)), c(bb$Meanupper, rev(bb$Meanlower)),
        col = adjustcolor("palegreen3",alpha.f = 0.65), border = NA)

lines(bb$Meanfit~bb$AgeAtVisit, type="l", lty=1, lwd=2, col="seagreen", ylim=c(-5,0))


polygon(c(cc$AgeAtVisit, rev(cc$AgeAtVisit)), c(cc$Meanupper, rev(cc$Meanlower)),
        col = adjustcolor("lightgoldenrod1",alpha.f = 0.65), border = NA)

lines(cc$Meanfit~cc$AgeAtVisit, type="l", lty=1, lwd=2, col="darkgoldenrod1", ylim=c(-5,0))



polygon(c(dd$AgeAtVisit, rev(dd$AgeAtVisit)), c(dd$Meanupper, rev(dd$Meanlower)),
        col = adjustcolor("lightblue2",alpha.f = 0.55), border = NA)

lines(dd$Meanfit~dd$AgeAtVisit, type="l", lty=1, lwd=2, col="dodgerblue2", ylim=c(0.0,0.1))

legend("topleft", legend=c("2004", "2012", "2019"), 
       col=c("seagreen", "darkgoldenrod1", "dodgerblue2"), lty=1, lwd = 5,cex = 1, bty="n")



plot(fbb$AgeAtVisit~fbb$Meanfit, type="l", lty=1, col="white", 
     xlab="Female age (years)", ylab="", ylim=c(0.0,0.7), xlim=c(15,55), cex.lab=1.8, cex.axis=2)

abline(h=c(0.1,0.2,0.3,0.4,0.5,0.6, 0.7), col="gray70", lty=2)
abline(v=c(15,20,25,30,35,40,45,50,55), col="gray70", lty=2)

polygon(c(fbb$AgeAtVisit, rev(fbb$AgeAtVisit)), c(fbb$Meanupper, rev(fbb$Meanlower)),
        col = adjustcolor("palegreen3", alpha.f = 0.65), border = NA)

lines(fbb$Meanfit~fbb$AgeAtVisit, type="l", lty=1, lwd=2, col="seagreen", ylim=c(0.0,0.1))


polygon(c(fcc$AgeAtVisit, rev(fcc$AgeAtVisit)), c(fcc$Meanupper, rev(fcc$Meanlower)),
        col = adjustcolor("lightgoldenrod1",alpha.f = 0.65), border = NA)

lines(fcc$Meanfit~fcc$AgeAtVisit, type="l", lty=1, lwd=2, col="darkgoldenrod1", ylim=c(0.0,0.1))



polygon(c(fdd$AgeAtVisit, rev(fdd$AgeAtVisit)), c(fdd$Meanupper, rev(fdd$Meanlower)),
        col = adjustcolor("lightblue2", alpha.f = 0.65), border = NA)

lines(fdd$Meanfit~fdd$AgeAtVisit, type="l", lty=1, lwd=2, col="dodgerblue2", ylim=c(0.0,0.1))
#https://github.com/HopkinsIDD/rakaimigration/blob/master/source/Figure1_code.r




#--------------------------------------------------
##split by Year
#https://towardsdatascience.com/a-comprehensive-guide-on-model-calibration-part-1-of-4-73466eb5e09a
Year = split(With_fitted, f = With_fitted$Year)

Year04 = Year$'2004'
Year12 = Year$'2012'
Year19 = Year$'2019'


##Split by gender

Gender04 = split(Year04, f = Year04$Sex)
Year04M = Gender04$Male
Year04F = Gender04$Female

Gender12 = split(Year12, f = Year12$Sex)
Year12M = Gender12$Male
Year12F = Gender12$Female

Gender19 = split(Year19, f = Year19$Sex)
Year19M = Gender19$Male
Year19F = Gender19$Female

Male_Data = rbind(Year04M,Year12M, Year19M)

Male_Data$Year = as.factor(Male_Data$Year)
Female_Data = rbind(Year04F, Year12F, Year19F)


New_Male = Male_Data %>% group_by(Year, AgeAtVisit) %>% summarise(Meanfit = mean(.value), Meanupper = mean(upper),
                                                                  Meanlower = mean(lower))

New_Female = Female_Data %>% group_by(Year, AgeAtVisit) %>% summarise(Meanfit = mean(.value), Meanupper = mean(upper),
                                                                  Meanlower = mean(lower))

bb = New_Male[New_Male$Year == 2004, ]

cc = New_Male[New_Male$Year == 2012, ]

dd = New_Male[New_Male$Year == 2019, ]

fbb = New_Female[New_Female$Year == 2004, ]
fcc =  New_Female[New_Female$Year == 2012, ]
fdd =  New_Female[New_Female$Year == 2019, ]

Males = ggplot() +
  theme_bw() +
  labs(x = "Age (Years)", y = "Effect", colour = "Legend", fill = "") +
  #geom_point(mapping = aes(x = z, y = y, colour = "data")) +
  geom_line(mapping = aes(x = Male_Data$AgeAtVisit, y = Male_Data$model1, colour = "2004"), size = 1, alpha = 1) +
  geom_ribbon(mapping = aes(x = Male_Data$AgeAtVisit, ymin = Male_Data$lower1, ymax = Male_Data$upper1, fill = "2004"), alpha = 0.3) + guides(color = "none")


Male = ggplot() +
  theme_bw() +
  labs(x = "Age (Years)", y = "HIV Prevalence", colour = "Legend", fill = "") +
  #geom_point(mapping = aes(x = z, y = y, colour = "data")) +
  geom_line(mapping = aes(x = bb$AgeAtVisit, y = bb$Meanfit, color = "2004"), size = 1, alpha = 1) +
  geom_ribbon(mapping = aes(x = bb$AgeAtVisit, ymin = bb$Meanlower, ymax = bb$Meanupper, fill = "2004"), alpha = 0.3) + 
  geom_line(mapping = aes(x = cc$AgeAtVisit, y = cc$Meanfit, color = "2012"), size = 1, alpha = 1) +
  geom_ribbon(mapping = aes(x = cc$AgeAtVisit, ymin = cc$Meanlower, ymax = cc$Meanupper, fill = "2012"), alpha = 0.3) + 
  geom_line(mapping = aes(x = dd$AgeAtVisit, y = dd$Meanfit, color = "2019"), size = 1, alpha = 1) +
  geom_ribbon(mapping = aes(x = dd$AgeAtVisit, ymin = dd$Meanlower, ymax = dd$Meanupper, fill = "2019"), alpha = 0.3) + ylim(0,0.75)


Female = ggplot() +
  theme_bw() +
  labs(x = "Age (Years)", y = "HIV Prevalence", colour = "Legend", fill = "") +
  #geom_point(mapping = aes(x = z, y = y, colour = "data")) +
  geom_line(mapping = aes(x = fbb$AgeAtVisit, y = fbb$Meanfit, color = "2004"), size = 1, alpha = 1) +
  geom_ribbon(mapping = aes(x = fbb$AgeAtVisit, ymin = fbb$Meanlower, ymax = fbb$Meanupper, fill = "2004"), alpha = 0.3) + 
  geom_line(mapping = aes(x = fcc$AgeAtVisit, y = fcc$Meanfit, color = "2012"), size = 1, alpha = 1) +
  geom_ribbon(mapping = aes(x = fcc$AgeAtVisit, ymin = fcc$Meanlower, ymax = fcc$Meanupper, fill = "2012"), alpha = 0.3) + 
  geom_line(mapping = aes(x = fdd$AgeAtVisit, y = fdd$Meanfit, color = "2019"), size = 1, alpha = 1) +
  geom_ribbon(mapping = aes(x = fdd$AgeAtVisit, ymin = fdd$Meanlower, ymax = fdd$Meanupper, fill = "2019"), alpha = 0.3) + ylim(0,0.75)

  
  gridExtra::grid.arrange(Male, Female, nrow = 1)






model_p$Year = floor(model_p$Year)

tufo = model_p %>% subset(model_p$Year == 2005)



New_fit = model_p %>% subset(model_p$Year %in% c("2004", "2012","2019"))

p <- ggplot(New_fit, aes(AgeAtVisit, fit, color = Year))  +
  geom_smooth(aes(group = Year))

p

library(plotly)
ggplotly(p)
#install.packages("remotes")
#remotes::install_github("egenn/rtemis")
library(rtemis)

ggplot()

plot(Mod2$fitted.values, residuals = TRUE, col = "blue", cex = .5)

library(broom)
n = tidy(Mod3)
draw(Mod3)

sm <- smooth_estimates(Mod3, data = dat) %>%
  add_confint()

ggplot(sm, aes(x = Year, y = est, colour = group)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, colour = NULL, fill = group),
              alpha = 0.2) +
  geom_line() +
  facet_wrap(~ group)

#https://cran.r-project.org/web/packages/tidymv/vignettes/plot-smooths.html
library(tidymv)
library(ggplot2)
theme_set(theme_bw())

plot_smooths(
  model = Mod2,
  series = AgeAtVisit,
  comparison = Year
) +
  theme(legend.position = "top")

library(gratia)

p_obj <- plot(Mod1, residuals = TRUE)


Prev = New_HIV %>% group_by(Year) %>% summarise(total = n(),Prev = sum(Status))

Prev$Cumm_prev = cumsum(Prev$Prev)
head(Prev)


### Incidence 15-25

New_idat = idat %>% group_by(Year) %>% filter(Age>=15 & Age <=25) %>% ungroup()



nn = AggByYear(New_idat)
nn
calcPoisExact(nn, byVar="Year")


### Cummulative incidence for females

NNidat = idat %>% subset(Female == "1")
NAggByAge = AggFunc("Age")

Age = NAggByAge(NNidat)

Cumulative_Incidence = calcPoisExact(Age, byVar="Age")
Cumulative_Incidence$Cumm = cumsum(Cumulative_Incidence$rate)

Cumulative_Incidence$Age = 15:54
Cumulative_Incidence$lower = cumsum(Cumulative_Incidence$lci)


##Poisson regression

# Run the model
sformula <- "sero_event ~ - 1 + Year + Year:Age + offset(log(tscale))"
mod <- stats::glm(as.formula(sformula), data=idat, family=poisson)


## Forest plot
## https://www.khstats.com/blog/forest-plots/

library(gt)
library(forcats)
library(tidyverse)


##Female
finc$Age = as.factor(finc$Age)
p <- 
  finc |>
  ggplot(aes(y = fct_rev(Age))) + 
  theme_classic()
p


p <- p +
  geom_point(aes(x=rate), shape=15, size=3, color = "seagreen") +
  geom_linerange(aes(xmin=lci, xmax=uci)) + xlim(0,8)
p

p <- p +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Incidence Rate", y="")
p



p <- p +
  coord_cartesian(xlim=c(0, 8))
p


p_mid <- p + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
p_mid


# wrangle results into pre-plotting table form
res_plot <- finc |>
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(
    c(rate, lci, uci),
    ~ str_pad(
      round(.x, 2),
      width = 4,
      pad = "0",
      side = "right"
    )
  ),
  # add an "-" between HR estimate confidence intervals
   estimate_lab = paste0(rate, " (", lci, "-", uci, ")")) 
  # # round p-values to two decimal places, except in cases where p < .001
  # mutate(p.value = case_when(
  #   p.value < .001 ~ "<0.001",
  #   round(p.value, 2) == .05 ~ as.character(round(p.value,3)),
  #   p.value < .01 ~ str_pad( # if less than .01, go one more decimal place
  #     as.character(round(p.value, 3)),
  #     width = 4,
  #     pad = "0",
  #     side = "right"
  #   ),
  #   TRUE ~ str_pad( # otherwise just round to 2 decimal places and pad string so that .2 reads as 0.20
  #     as.character(round(p.value, 2)),
  #     width = 4,
  #     pad = "0",
  #     side = "right"
  #   )
  # )) 
  # add a row of data that are actually column names which will be shown on the plot in the next step
  # bind_rows(
  #   data.frame(
  #     Age = "Age",
  #     estimate_lab = "Incidence rate (95% CI)",
  #     lci = "",
  #     uci = "",
  #   
  #   )
  # ) |>
  # mutate(model = fct_rev(fct_relevel(Age, "Age")))

#glimpse(res_plot)



p_left <-
  res_plot  |>
  ggplot(aes(y = fct_rev(Age)))
p_left


p_left <- 
  p_left +
  geom_text(aes(x = 0, label = Age), hjust = 0, fontface = "bold")
p_left


p_left <- 
  p_left +
  geom_text(
    aes(x = 1, label = estimate_lab),
    hjust = 0,
    fontface = ifelse(res_plot$estimate_lab == "Hazard Ratio (95% CI)", "plain", "bold")
  )

p_left


p_left <-
  p_left +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

p_left

library(patchwork)
layout <- c(
  area(t = 0, l = 0, b = 30, r = 3), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 1, l = 4, b = 30, r = 30) # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  #area(t = 0, l = 9, b = 30, r = 11) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)
# final plot arrangement
fem = p_left + p_mid + plot_layout(design = layout)


### Cummulative values

qt(p=.05, df=39, lower.tail=TRUE)

finc$cumm_low = finc$cumm - 1.685*(41.27295/sqrt(40))

finc$cumm_upper = finc$cumm + 1.685*(41.27295/sqrt(40))



### Male

Minc$Age = as.factor(Minc$Age)
mp <- 
  Minc |>
  ggplot(aes(y = fct_rev(Age))) + 
  theme_classic()
mp


mp <- mp +
  geom_point(aes(x=rate), shape=15, size=3, color = "seagreen") +
  geom_linerange(aes(xmin=lci, xmax=uci)) + xlim(0,8)
mp

mp <- mp +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Incidence Rate", y="")
mp



mp <- mp +
  coord_cartesian(xlim=c(0, 8))
mp


mp_mid <- mp + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
mp_mid


# wrangle results into pre-plotting table form
mres_plot <- Minc |>
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(
    c(rate, lci, uci),
    ~ str_pad(
      round(.x, 2),
      width = 4,
      pad = "0",
      side = "right"
    )
  ),
  # add an "-" between HR estimate confidence intervals
  estimate_lab = paste0(rate, " (", lci, "-", uci, ")")) 
# # round p-values to two decimal places, except in cases where p < .001
# mutate(p.value = case_when(
#   p.value < .001 ~ "<0.001",
#   round(p.value, 2) == .05 ~ as.character(round(p.value,3)),
#   p.value < .01 ~ str_pad( # if less than .01, go one more decimal place
#     as.character(round(p.value, 3)),
#     width = 4,
#     pad = "0",
#     side = "right"
#   ),
#   TRUE ~ str_pad( # otherwise just round to 2 decimal places and pad string so that .2 reads as 0.20
#     as.character(round(p.value, 2)),
#     width = 4,
#     pad = "0",
#     side = "right"
#   )
# )) 
# add a row of data that are actually column names which will be shown on the plot in the next step
# bind_rows(
#   data.frame(
#     Age = "Age",
#     estimate_lab = "Incidence rate (95% CI)",
#     lci = "",
#     uci = "",
#   
#   )
# ) |>
# mutate(model = fct_rev(fct_relevel(Age, "Age")))

#glimpse(res_plot)



mp_left <-
  mres_plot  |>
  ggplot(aes(y = fct_rev(Age)))
mp_left


mp_left <- 
  mp_left +
  geom_text(aes(x = 0, label = Age), hjust = 0, fontface = "bold")
mp_left


mp_left <- 
  mp_left +
  geom_text(
    aes(x = 1, label = estimate_lab),
    hjust = 0,
    fontface = ifelse(res_plot$estimate_lab == "Hazard Ratio (95% CI)", "plain", "bold")
  )

mp_left


mp_left <-
  mp_left +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

mp_left

library(patchwork)
layout <- c(
  area(t = 0, l = 0, b = 30, r = 3), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 1, l = 4, b = 30, r = 30) # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  #area(t = 0, l = 9, b = 30, r = 11) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)
# final plot arrangement
mal = mp_left + mp_mid + plot_layout(design = layout)

gridExtra::grid.arrange(mal, fem,  nrow = 2)

## Wrangle

cumm_res_plot <- finc |>
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(
    c(cumm, cumm_low, cumm_upper),
    ~ str_pad(
      round(.x, 2),
      width = 4,
      pad = "0",
      side = "right"
    )
  ),
  # add an "-" between HR estimate confidence intervals
  cumm_estimate_lab = paste0(cumm, " (", cumm_low, "-", cumm_upper, ")")) 


cumm_p_left <-
  cumm_res_plot  |>
  ggplot(aes(y = fct_rev(Age)))
cumm_p_left


cumm_p_left <-
  cumm_p_left +
  geom_text(aes(x = 0, label = Age), hjust = 0, fontface = "bold")
cumm_p_left


cumm_p_left <- 
  cumm_p_left +
  geom_text(
    aes(x = 1, label = cumm_estimate_lab),
    hjust = 0,
    fontface = ifelse(cumm_res_plot$cumm_estimate_lab == "Hazard Ratio (95% CI)", "bold", "plain")
  )

cumm_p_left


cumm_p_left <-
  cumm_p_left +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

cumm_p_left


layout <- c(
  area(t = 0, l = 0, b = 30, r = 3), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 1, l = 4, b = 30, r = 9), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  area(t = 0, l = 9, b = 30, r = 11), # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
  area(t = 0, l = 9, b = 30, r = 11)
  )
# final plot arrangement
p_left + p_mid + cumm_p_left+ cumm_p_mid + plot_layout(design = layout)

p_left + p_mid + plot_layout(design = layout)


### Tables

library(haven)
SurveillanceEpisodes <- read_dta("SurveillanceEpisodes.dta")

length(unique(Female_Data$IIntId))


New_Surv = SurveillanceEpisodes %>% subset(SurveillanceEpisodes$IndividualId %in% Female_Data$IIntId) %>% distinct(.,IndividualId, .keep_all = TRUE)


Fem_25 = Female_Data %>% filter(Female_Data$AgeAtVisit <= 25 )
SurveillanceEpisodes$Year = substr(SurveillanceEpisodes$StartDate, 1,4)
SurveillanceEpisodes$IIntId = SurveillanceEpisodes$IndividualId

New_Female_Data = merge(Fem_25, SurveillanceEpisodes, by = c("IIntId", "Year")) %>% distinct(.,IIntId, Year, .keep_all = TRUE)

NNew_Surv = New_Surv %>% subset(New_Surv$IndividualId %in% Fem_25$IIntId) %>% distinct(., IndividualId, .keep_all = TRUE)

vars = c("IIntId", "Year" , "VisitDate", "HIVResult" , "AgeAtVisit", "Sex.x" , "InMigration" , "Resident" , 
         "OutMigration")



#### https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

library(table1)
table1(~AgeAtVisit + Year + ModerntAssetIdx + PowerSupplyIdx + FatherCoResident + MotherCoResident| HIVResult, data = New_Female_Data, topclass="Rtable1-grid")



Fem1 = dat %>% subset(dat$Sex %in% c("Female"))

Fem1$Sex = droplevels(Fem1$Sex)

Fem1 = Fem1 %>% group_by(IIntId, Year) %>%  distinct(., IIntId, .keep_all = TRUE) %>% ungroup()


Fem125 = Fem1 %>% filter(Fem1$AgeAtVisit <= 25)



Women_health = RD03_99_ACDIS_WGH_ALL %>% subset(RD03_99_ACDIS_WGH_ALL$IIntId %in% Fem125$IIntId)

Women_health$Year = substr(Women_health$VisitDate, 1,4)



varrs = c("IIntId","AgeAtVisit","TBLast12m","TBCurrTmt" , "HospitalAdmissionLast12m","CurrentMaritalStatus","AgeFirstMarried","CurrentlyLivingWithPartner",
"PolygamousCoWives","EverPregnant","MaleCondomCurrently" ,"MaleCondomLastSex","EverHadSex" , "AgeAtFirstSex", "MRPOlder","MRPAgeDifference" , "MRPEverUsedCondoms", 
"LifetimePartners", "Year")

Nwom = Women_health[, varrs]

Nwom = Nwom %>% filter(Nwom$AgeAtVisit <=25)

gg = Nwom %>% subset(Nwom$IIntId %in% Fem125$IIntId)

vals = c(92,93,94,95,96,97,98,99,992,993,994,995,996,998,999)



writexl::write_xlsx(gg, "gg.xlsx")




gg$MRPOlder = as.factor(gg$MRPOlder)
gg$LifetimePartners = as.factor(gg$LifetimePartners)
gg <- read_excel("gg.xlsx")

gg[gg$EverHadSex == "NA", ] = NA
gg[gg$LifetimePartners == "NA", ] = NA
gg[gg$AgeAtFirstSex =0,] = NA


table1(~AgeAtVisit + EverPregNAnt + EverHadSex + MRPOlder + MRPAgeDifference + LifetimePartners | Year, data = gg, topclass="Rtable1-grid")

library(crosstable)
library(flextable)
tab11 = crosstable(gg, c(AgeAtVisit, AgeAtFirstSex, EverPregnant,EverHadSex, MRPOlder, MRPAgeDifference),label=FALSE,total = 'both',crosstable_options(scientific_log=20)) %>% 
  as_flextable(compact=TRUE, header_show_n=1:2)%>% set_table_properties(width = .5,layout = "autofit")


exportxlsx(tab11, path = "/Users/elphasokango/R/Personal/SA/HIV-INCIDENCE")

sex1 = gg[gg$EverHadSex ==1, ]
length(unique(sex1$IIntId))

sex2 = gg[gg$EverHadSex ==2, ]
length(unique(sex2$IIntId))


preg1 = gg[gg$EverPregnant == 1, ]
length(unique(preg1$IIntId))

preg2 = gg[gg$EverPregnant == 2, ]
length(unique(preg2$IIntId))

mro1 = gg[gg$MRPOlder ==1, ]
length(unique(mro1$IIntId))

mro2 = gg[gg$MRPOlder ==2, ]
length(unique(mro2$IIntId))

