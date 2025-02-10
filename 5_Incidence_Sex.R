####
#### 
#### Plot smoothed curves and plot for incidence aggregated by sex 
####

####
# Clear any existing data from the data set
rm(list = ls())
#####

# Define vector of package names

package_names <- c('dplyr','ggplot2','ggsurvfit','survminer','survival','lubridate','ggpubr','grid',
                   'viridis', 'gt', 'flextable','officer')


# This code installs all the other required packages if they are not currently installed and load all the libraries

pacman::p_load(char=package_names)

data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/AHRI_data/AHRI_2023'
output_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Changing_face_hiv/HIV_SES/Outputs'


### Load the data 
HIV_edu_SES <- readRDS(file = paste0(data_dir,"/HIV_edu_SES.rds"))

#### Define range of ages to include in the analysis
age_min = 15
age_max = 54

### Define range of years to include in the analysis
start_year <- 2005
end_year <- 2021


# Filter the data to limit the data to age_min to age_max
HIV_edu_SES <- dplyr::filter(HIV_edu_SES, (HIV_edu_SES$Age >= age_min & HIV_edu_SES$Age <= age_max))
# Filter the data to limit the data to Round_Year from  start_year to end_year
HIV_edu_SES <- dplyr::filter(HIV_edu_SES, 
                                     Round_Year >= start_year & Round_Year <= end_year)

### Aggregate data for Sex

# Summarise the data
aggregated_data_sex <- HIV_edu_SES %>%
  group_by(Round_Year, Female) %>%
  summarise(
    total_PY = sum(Time, na.rm = TRUE)/365.25,
    total_sero_events = sum(sero_event, na.rm = TRUE),
    incidence = (total_sero_events / total_PY) * 100,
    .groups = "drop"
  )

# Recode the sex variable and reorder columns
aggregated_data_sex <- aggregated_data_sex %>%
  mutate(
    sex = factor(Female, levels = c(0, 1), labels = c("Male", "Female"))
  ) %>%
  select(Round_Year, sex, total_PY, total_sero_events, incidence)  # Drop 'Female' and reorder

# Round the total PY and incidence columns to 2 decimal places
aggregated_data_sex <- aggregated_data_sex %>%
  mutate(
    total_PY = round(total_PY, 2),
    incidence = round(incidence, 2)
  )

# Create flextable
flextab <- flextable(aggregated_data_sex) %>%
  # Format total_sero_events as integer
  colformat_int(j = "total_sero_events") %>%
  colformat_num(j = c("total_PY", "incidence"), digits = 2) %>%
  set_header_labels(
    Round_Year = "Round Year",
    sex = "Sex",
    total_PY = "Total Person-Years",
    total_sero_events = "Total Seroconversions",
    incidence = "Incidence (per 100 PY)"
  ) %>%
  autofit()

# Write flextable to Word
read_docx() %>%
  body_add_par(paste0("Incidence by Sex and Year for individuals aged ",
                      as.character(age_min)," to ",as.character(age_max)), style = "heading 1") %>%
  body_add_par("Summary of Person-Years and Seroconversion Events", style = "heading 2") %>%
  body_add_flextable(flextab) %>%
  print(target = paste0(output_dir,"/incidence_summary.docx"))



# Convert Round_Year is numeric
aggregated_data_sex$Round_Year <- as.numeric(as.character(aggregated_data_sex$Round_Year))

# Plot with  smoothing

# Plot with restricted x-axis range
sex_plot <- ggplot(aggregated_data_sex, aes(x = Round_Year, y = incidence, color = sex, fill = sex)) +
  # Add original data points as small X symbols
  geom_point(shape = 4, size = 2, stroke = 1) + # Crosses for raw data
  # Add a smoothed curve through the data points with the same color as the data points
  stat_smooth(
    method = "loess", 
    span = 0.5, 
    se = TRUE, 
    alpha = 0.3
  ) + # Smoothed curve with shaded SE
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Viridis palette for discrete data
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9) + # Match fill colors to line colors
  scale_x_continuous(limits = c(start_year, end_year)) + # Set x-axis limits
  labs(
    title = paste0("Incidence by Round Year and Sex with 95% confidence intervals 
                   Age ", as.character(age_min)," to ",as.character(age_max)),
    x = "Round Year",
    y = "Incidence (events per 100 person-years)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + # Move legend to bottom of plot
  theme(plot.title = element_text(size=12,hjust=0.5)) # Centre and decrease size of plot title


# Save the plot as a PNG file
#ggsave(filename = paste0(output_dir,"/incidence_by_round_year_and_sex_quantile.png"), width = 6, height = 4)


### Save table and plot to a word document

landscape_one_column <- block_section(
  prop_section(
    page_size = page_size(orient = "landscape"), type = "continuous"
  )
)



doc <- read_docx() %>%
  # Add portrait content
  body_add_par("Incidence by Sex and Year", style = "heading 1") %>%
  body_add_par(paste0("Summary of Person-Years and Seroconversion Events (for those aged) "
                      ,as.character(age_min)," to ", as.character(age_max)), style = "heading 2") %>%
  body_add_flextable(flextab) %>%
  
  # End the current (portrait) section and start a new one in landscape orientation
  body_end_section_portrait() %>%
  
  
  # Add heading and plot in landscape orientation
  body_add_gg(value = sex_plot, width = 10, height = 5, style = "centered") %>%
  body_end_block_section(value = landscape_one_column)

print(doc, target = paste0(output_dir,"/inc_sex_",as.character(age_min),"-"
                           ,as.character(age_max),".docx"))
