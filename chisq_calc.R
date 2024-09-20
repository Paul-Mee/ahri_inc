#### Utility to calculate Chi-Sqaured p values by group 

# Clear any existing data from the data set
rm(list = ls())

# Set file paths
## AHRI data
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/2023'

dat_fname <- '/cohort_table_chisq_dat.csv' 


chi_dat.df <- read.csv(paste0(data_dir,dat_fname))

### Loop through variables 

### summarise by variable 
### list of summary variables 
var_list <- c('sex','age_cat','SES','Highest_Education','Urban_Rural','Km_Clinic')

#var='sex'

for (var in var_list) {
  chi_rows <- filter(chi_dat.df,var_name==var)
  cont_tab <- as.matrix(chi_rows[c('n_ind.2005','n_ind.2015')])
  chi_rows$pval_ind <- chisq.test(x=cont_tab)$p.value
  cont_tab <- as.matrix(chi_rows[c('ntime.2005','ntime.2015')])
  chi_rows$pval_ntime <- chisq.test(x=cont_tab)$p.value
  
  ### Append to overall dataframe      
  if(exists("chi_tab")==FALSE) {
    chi_tab <- chi_rows
  } else{ 
    chi_tab <-  rbind(chi_tab,chi_rows)
  }
  
}

#reformat p value columns 

tmp_chi_ind <- chi_tab[c('pval_ind')]
chi_tab$pval_ind_char <- apply(tmp_chi_ind, 1, function(x)
{
  if(x[1] >= 0.001) result <- as.character( round(x[1],digits=3))
  else if(x[1] <= 0.001 ) result <- "<0.001"
  else result <- 0
  return(result)
})

tmp_chi_ntime <- chi_tab[c('pval_ntime')]
chi_tab$pval_ntime_char <- apply(tmp_chi_ntime, 1, function(x)
{
  if(x[1] >= 0.001) result <- as.character( round(x[1],digits=3))
  else if(x[1] <= 0.001 ) result <- "<0.001"
  else result <- 0
  return(result)
})

### Write as a csv file
chi_fname <- paste0(data_dir,"/chi_calc.csv")

write.csv(chi_tab,file = chi_fname)


