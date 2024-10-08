#### Utility to calculate Chi-Sqaured p values by group 
#### Reads data derived from cohort summary table and calculates Chi-Sqaured values 
#### for the distributions of strata for each variable 
#####

# Clear any existing data from the data set
rm(list = ls())

# Set file paths
## AHRI data
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'
dat_fname <- '/cohort_table_chisq_dat.csv' 

### Read data cut and pasted from the cohort summary table
chi_dat.df <- read.csv(paste0(output_dir,dat_fname))

### Loop through variables 

### summarise by variable 
### list of summary variables 
var_list <- c('sex','age_cat','SES','Highest_Education','Urban_Rural','Km_Clinic')

#var='sex'
rm(chi_tab)

for (var in var_list) {
  print(var)
  chi_rows <- filter(chi_dat.df,var_name==var)
  
  cont_tab <- as.data.frame(chi_rows[c('n_ind.2005','n_ind.2015')])
  ind_test <- chisq.test(as.matrix(cont_tab$n_ind.2005,cont_tab$n_ind.2015))

  cont_tab <- as.data.frame(chi_rows[c('ntime.2005','ntime.2015')])
  ntime_test <- chisq.test(as.matrix(cont_tab$ntime.2005,cont_tab$ntime.2015))
  
  chi_summ <- as.data.frame(t(c(var,ind_test$p.value,ntime_test$p.value)))
  
  ### Append to overall dataframe      
  if(exists("chi_tab")==FALSE) {
    chi_tab <- chi_summ
  } else{ 
    chi_tab <-  rbind(chi_tab,chi_summ)
  }
  
}

colnames(chi_tab)[1] = 'var'
colnames(chi_tab)[2] = 'pval_ind'
colnames(chi_tab)[3] = 'pval_ntime'

chi_tab$pval_ind <- as.numeric(chi_tab$pval_ind)
chi_tab$pval_ntime <- as.numeric(chi_tab$pval_ntime)

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
chi_fname <- paste0(output_dir,"/chi_calc.csv")

write.csv(chi_tab,file = chi_fname)


