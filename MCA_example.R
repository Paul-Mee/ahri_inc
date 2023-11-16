#######
### This code provides a worked example of an application of MCA analysis using the example given in this link 
### http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials
#######

# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# Define vector of package names

package_names <- c('')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)