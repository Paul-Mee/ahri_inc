#######
### This code provides a worked example of an application of MCA analysis using the example given in this link 
### http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials
#######
### This article references simple CA
##### Correspondence Analysis in R: Essentials
### http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/113-ca-correspondence-analysis-in-r-essentials/

# Clear any existing data from the data set
rm(list = ls())
# Increase R studio columns viewed
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# Define vector of package names

package_names <- c("FactoMineR", "factoextra")


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

# We’ll use the demo data sets poison available in FactoMineR package:
data(poison, package="FactoMineR")
head(poison[, 1:7], 3)

#Subset only active individuals and variables for multiple correspondence analysis:
poison.active <- poison[1:55, 5:15]
head(poison.active[, 1:6], 3)

# Summary of the 4 first variables
summary(poison.active)[, 1:4]

#It’s also possible to plot the frequency of variable categories. The R code below, plots the first 4 columns:
  
  for (i in 1:4) {
    plot(poison.active[,i], main=colnames(poison.active)[i],
         ylab = "Count", col="steelblue", las = 2)
  }

# In the R code below, the MCA is performed only on the active individuals/variables :

res.mca <- MCA(poison.active, graph = FALSE)

# The output of the MCA() function is a list including :

print(res.mca)

# The proportion of variances retained by the different dimensions (axes) can be extracted using the function get_eigenvalue() [factoextra package] as follow:

eig.val <- factoextra::get_eigenvalue(res.mca)

# To visualize the percentages of inertia explained by each MCA dimensions, use the function fviz_eig() or fviz_screeplot() [factoextra package]:

factoextra::fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

# The function fviz_mca_biplot() [factoextra package] is used to draw the biplot of individuals and variable categories:

factoextra::fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

# The function get_mca_var() [in factoextra] is used to extract the results for variable categories. 
# This function returns a list containing the coordinates, the cos2 and the contribution of variable categories:

var <- get_mca_var(res.mca)
var

# To visualize the correlation between variables and MCA principal dimensions, type this:

fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# The R code below displays the coordinates of each variable categories in each dimension (1, 2 and 3):

head(round(var$coord, 2), 4)

# It’s possible to change the color and the shape of the variable points using the arguments col.var and shape.var as follow:

fviz_mca_var(res.mca, col.var="black", shape.var = 15,
             repel = TRUE)

# The plot above shows the relationships between variable categories. It can be interpreted as follow:
#   Variable categories with a similar profile are grouped together.
# Negatively correlated variable categories are positioned on opposite sides of the plot origin (opposed quadrants).
# The distance between category points and the origin measures the quality of the variable category on the factor map. 
# Category points that are away from the origin are well represented on the factor map.


# The quality of the representation is called the squared cosine (cos2) 
# which measures the degree of association between variable categories and a particular axis. 
# The cos2 of variable categories can be extracted as follow:
  
head(var$cos2, 4)

# If a variable category is well represented by two dimensions, 
#the sum of the cos2 is closed to one. For some of the row items, 
#more than 2 dimensions are required to perfectly represent the data.

# Color by cos2 values: quality on the factor map
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

## The function get_mca_ind() [in factoextra] is used to extract the results for individuals. 
## This function returns a list containing the coordinates, the cos2 and the contributions of individuals:
  
  ind <- factoextra::get_mca_ind(res.mca)
  
# Coordinates of column points
  head(ind$coord)
# Quality of representation
  head(ind$cos2)
# Contributions
  head(ind$contrib)  
  
### coord is synonymous with the factor scores
