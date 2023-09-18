# 
# This version calculates overall incidence
#
# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)




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

package_names <- c('haven','dplyr','ggplot2','ggthemes')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

#### Need to first run AHRI_SES.R and create a set of stata file with SES data for each quantile in each year 
#### Loop through number of quantiles to calcuate incidence for each


### Load filenames 


hiv_fname="RD05-99 ACDIS HIV All.dta"
wgh_fname="RD03-99 ACDIS WGH ALL.dta"
mgh_fname="RD04-99 ACDIS MGH ALL.dta" 
bsi_fname="RD01-03 ACDIS BoundedStructures.dta"
epi_name = "SurveillanceEpisodesHIV.dta"

start_year = 2005
end_year = 2022

sim_num = 10 # Multiple imputation simulations 
age_min = 15 # minimum age for incidence calculation
age_max = 49 # maximum age for incidence calculation
gender = "Fem" # Include Males (Mal) Females (Fem) or both (All)
plot_title = "Incidence by year- Female (Age 15 to 49)"
plot_fname = "/Inc_SES_fem.png"

    getFiles <- setFiles(folder=data_dir,
                         hivfile=hiv_fname,
                         epifile=epi_name,
                         wghfile=wgh_fname, 
                         mghfile=mgh_fname, 
                         bsifile=bsi_fname)
    getFiles()[1:5]
    readEpisodes()
    getBirthDate()

    #Read the HIV data and set the Years of interest, and set gender and  age groups
    if(gender=="All") {
        Args <- setArgs(Years=c(start_year:end_year), 
                        Age=list(All=c(age_min,age_max)),
                        imputeMethod=imputeRandomPoint, nSim=sim_num)
        } else if(gender=="Mal") {
        Args <- setArgs(Years=c(start_year:end_year), 
                      Age=list(Mal=c(age_min,age_max)),
                      imputeMethod=imputeRandomPoint, nSim=sim_num)
        } else if(gender=="Fem") {
        Args <- setArgs(Years=c(start_year:end_year), 
                        Age=list(Fem=c(age_min,age_max)),
                        imputeMethod=imputeRandomPoint, nSim=sim_num)
        } else {
        print("Gender not specified - using All")
        Args <- setArgs(Years=c(start_year:end_year), 
                         Age=list(All=c(age_min,age_max)),
                        imputeMethod=imputeRandomPoint, nSim=sim_num)
        }
    hiv <- getHIV()
    #### Update Year based on actual census rounds
    ##Fixing Year values for rounds effected by Covid
    
    # 2020 round       
    hiv$Year[hiv$VisitDate >= "2020-01-21" & hiv$VisitDate >= "2021-04-20"] <- 2020
    # 2021 round 
    hiv$Year[hiv$VisitDate >= "2021-04-21" & hiv$VisitDate >= "2022-05-18"] <- 2021
    # 2022 round 
    hiv$Year[hiv$VisitDate >= "2022-05-19" & hiv$VisitDate >= "2023-05-05"] <- 2022
    # 2023 round 
    hiv$Year[hiv$VisitDate >= "2023-05-06"] <- 2023
    
    rtdat <- getRTData(hiv)
    idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
    levels(idat$AgeCat)
    idat <- dplyr::mutate(idat, Year = as.factor(Year))

    sformula = "sero_event ~ -1 + as.factor(Year) + Age + as.factor(Year):Age + offset(log(tscale))"
    mod <- stats::glm(as.formula(sformula), data=idat, family=poisson)

    age_dat <- getAgeYear(dat=setHIV(Args))

    # Calculate the age-adjusted HIV incidence rates from n_sim imputed datasets
    # Check where 10 imputations is set 

   SES_inc <- MIpredict(mod, newdata=age_dat)


### Reorder columns
   SES_inc$Year <- row.names(SES_inc)
   row.names(SES_inc) <- NULL

SES_inc<- SES_inc[, c("Year", "fit","se.fit","lci","uci")]    
names(SES_inc)[2] <- "Incidence"
names(SES_inc)[3] <- "se.inc"

SES_inc$Year_num <- as.integer(SES_inc$Year)

## Output data
R_fname <- paste0(data_dir,"/Overall_Inc.RDS")

### Saving as RDS file
saveRDS(SES_inc, file = R_fname) 


### Plotting 

## select data between min and  max year



inc_plot <- SES_inc %>% filter(Year_num >= start_year  & Year_num <= end_year )

# Make the plot

p1 <- ggplot(data=inc_plot, aes(x=Year_num, y=Incidence, 
                             ymin=lci, ymax=uci)) +
geom_line() +
geom_errorbar( width=.2) +
  theme_classic() +
  #scale_color_manual(values=c('red','green','blue')) +
  #scale_color_brewer(palette="BrBG") +
  theme_igray() + scale_colour_colorblind() +
  scale_x_continuous(name ="Year",breaks = seq(2004,2022, by = 1)) +
  labs(title = plot_title)
p1


ggsave(paste0(data_dir,plot_fname),p1,  width=20, height=15, units="cm")


  






