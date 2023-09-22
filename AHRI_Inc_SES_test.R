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
#source(paste0(code_dir,"splitData.R"))
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
epi_fname="SurveillanceEpisodesHIV.dta"

### test loading HIV file 

### Loading Household Asset data 

# stata_data_file <- '/RD05-99 ACDIS HIV All.dta'
# hiv_all <- haven::read_dta(paste0(data_dir,stata_data_file))
# 
# stata_data_file <- '/SurveillanceEpisodesHIV.dta'
# epi_all <- haven::read_dta(paste0(data_dir,stata_data_file))


start_year = 2005
end_year = 2022
#n_fact = 3 # Number of quantiles in SES
sim_num = 10 # Multiple imputation simulations 
age_min = 15 # minimum age for incidence calculation
age_max = 54 # maximum age for incidence calculation
gender = "male" # Include Males (male) Females (female) or both (all)
# plot_title = "Incidence by wealth quantile - Men"
# plot_fname = "/Inc_SES_male.png"


    print(paste0("Reading Stata file - ",epi_fname))

    getFiles <- setFiles(folder=data_dir,
                         hivfile=hiv_fname,
                         epifile=epi_fname,
                         wghfile=wgh_fname, 
                         mghfile=mgh_fname, 
                         bsifile=bsi_fname)
    getFiles()[1:5]
   

    
    
    
    #getBirthDate()

    #Read the HIV data and set the Years of interest, and set gender and  age groups
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
    
    # Load data giving SES for each year of HIV data 
    R_fname_SES <- paste0(data_dir,"/Yr_SES_Data.RDS")
    Year_SES <- readRDS(R_fname_SES)

    ##Fixing Year values for rounds effected by Covid
    hiv <- getHIV()
    # 2020 round
    hiv$Year[hiv$VisitDate >= "2020-01-21" & hiv$VisitDate <= "2021-04-20"] <- 2020
    # 2021 round
    hiv$Year[hiv$VisitDate >= "2021-04-21" & hiv$VisitDate <=  "2022-05-18"] <- 2021
    # 2022 round
    hiv$Year[hiv$VisitDate >= "2022-05-19" & hiv$VisitDate <= "2023-05-05"] <- 2022
    # 2023 round
    hiv$Year[hiv$VisitDate >= "2023-05-06"] <- 2023
    
    rtdat <- getRTData(hiv)
    idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
    ## Merge with a file including SES for each HIV episode
    idat <- merge(idat,Year_SES,by=c('IIntID','Year'))
    levels(idat$AgeCat)
    idat <- dplyr::mutate(idat, Year = as.factor(Year),wealth_quantile = as.factor(wealth_quantile))
    sformula = "sero_event ~ -1 + as.factor(Year) + wealth_quantile + as.factor(Year):wealth_quantile + offset(log(tscale))"
    mod <- stats::glm(as.formula(sformula), data=idat, family=poisson)
    summary(mod)
    

    age_dat <- getAgeYear(dat=setHIV(Args))
    

    

    # Calculate the age-adjusted HIV incidence rates from n_sim imputed datasets
    # Check where 10 imputations is set 

   SES_inc <- MIpredict(mod, newdata=age_dat)
   #SES_inc$quantile <- i
   
# SES_inc$Year <- row.names(SES_inc)
# row.names(SES_inc) <- NULL


# ### Reorder rows 
# SES_inc <- SES_inc[, c("Year", "fit","se.fit","lci","uci")] 
# # 
# names(SES_inc)[2] <- "Incidence"
# names(SES_inc)[3] <- "se.inc"
# 
# fnam_csv <- paste0("/inc_",as.character(age_min),"-",as.character(age_max),"-",gender,
#                    as.character(start_year),"-",as.character(end_year),".csv")



write.csv(SES_inc,paste0(data_dir,fnam_csv),row.names = FALSE)

# 
# SES_inc_all$quant_fact <- as.factor(SES_inc_all$quantile)
# SES_inc_all$Year_num <- as.integer(SES_inc_all$Year)

# ## Output data
# R_fname <- paste0(data_dir,"/SEP_Inc.RDS")
# 
# ### Saving as RDS file
# saveRDS(SES_inc_all, file = R_fname) 


### Plotting 

## select data up to max year

# max_year <- 2022
# 
# SES_inc_plot <- SES_inc_all %>% filter(Year_num <= max_year)
# 
# # Make the plot
# 
# p1 <- ggplot(data=SES_inc_plot, aes(x=Year_num, y=Incidence, group=quant_fact,
#                              color=quant_fact,ymin=lci, ymax=uci)) +
# geom_line() +
# geom_errorbar( width=.2) +
#   theme_classic() +
#   #scale_color_manual(values=c('red','green','blue')) +
#   #scale_color_brewer(palette="BrBG") +
#   theme_igray() + scale_colour_colorblind() +
#   scale_x_continuous(name ="Year",breaks = seq(2004,2022, by = 1)) +
#   labs(color='Wealth Quantile',
#        title = plot_title)
# p1
# 
# 
# ggsave(paste0(data_dir,plot_fname),p1,  width=20, height=15, units="cm")


  






