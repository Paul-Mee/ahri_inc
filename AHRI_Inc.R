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

package_names <- c('haven','dplyr','ggplot2')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

#### Need to first run AHRI_SES.R and create a set of stata file with SES data for each quantile in each year 
#### Loop through number of quantiles to calcuate incidence for each


### Load filenames 


hiv_fname="RD05-99 ACDIS HIV All.dta"
wgh_fname="RD03-99 ACDIS WGH ALL.dta"
mgh_fname="RD04-99 ACDIS MGH ALL.dta" 
bsi_fname="RD01-03 ACDIS BoundedStructures.dta"

start_year = 2004
end_year = 2023
n_fact = 3 # Number of quantiles in SES
sim_num = 10 # Multiple imputation simulations 
age_min = 15 # minimum age for incidence calculation
age_max = 65 # maximum age for incidence calculation
gender = "Fem" # Include Males (Mal) Females (Fem) or both (All)
plot_title = "Incidence by wealth quantile - Female"
plot_fname = "/Inc_SES_fem.png"

for (i in 1:n_fact) {
    
    ST_fname_SES_q <- paste0("q",as.character(i),"_SES_Data.dta")
    print(i) 
    print(paste0("Reading Stata file - ",ST_fname_SES_q))

    getFiles <- setFiles(folder=data_dir,
                         hivfile=hiv_fname,
                         epifile=ST_fname_SES_q,
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
   SES_inc$quantile <- i
   
   SES_inc$Year <- row.names(SES_inc)
   row.names(SES_inc) <- NULL
   
   ### Append to overall dataframe      
   if(exists("SES_inc_all")==FALSE) {
     SES_inc_all <- SES_inc}
   else{
     SES_inc_all <-  rbind(SES_inc_all ,SES_inc)}
   i = i+ 1 
}

### Reorder rows 
SES_inc_all <- SES_inc_all[, c("Year", "quantile", "fit","se.fit","lci","uci")] 

names(SES_inc_all)[3] <- "Incidence"
names(SES_inc_all)[4] <- "se.inc"

SES_inc_all$quant_fact <- as.factor(SES_inc_all$quantile)
SES_inc_all$Year_num <- as.integer(SES_inc_all$Year)

## Output data
R_fname <- paste0(data_dir,"/SEP_Inc.RDS")

### Saving as RDS file
saveRDS(SES_inc_all, file = R_fname) 


### Plotting 

## select data up to max year

max_year <- 2022

SES_inc_plot <- SES_inc_all %>% filter(Year_num <= max_year)

# Make the plot

p1 <- ggplot(data=SES_inc_plot, aes(x=Year_num, y=Incidence, group=quant_fact,
                             color=quant_fact,ymin=lci, ymax=uci)) +
geom_line() +
geom_errorbar( width=.2) +
  theme_classic() +
  scale_color_manual(values=c('red','green','blue')) +
  scale_x_continuous(name ="Year",breaks = seq(2004,2022, by = 1)) +
  labs(color='Wealth Quantile',
       title = plot_title)
p1


ggsave(paste0(data_dir,plot_fname),p1,  width=20, height=15, units="cm")


  






