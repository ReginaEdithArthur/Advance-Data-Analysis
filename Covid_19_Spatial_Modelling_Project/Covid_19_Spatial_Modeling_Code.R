#Set working directory
setwd("C:/Users/LENOVO/Desktop/Advance Data Analysis Exam/Covid_19_Spatial_Modelling_Project")

#Package Installations
install.packages("bit") #readr depends on bit, you don't need to load it
install.packages("readr")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyr")

# Load the Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Read the CSV file
ghana_covid <- read_delim("gha_subnational_covid19_hera.csv", delim = ";")
spec(ghana_covid) #to retrieve the full column specification for this data

# Check the first few rows
head(ghana_covid) #Viewing column heads
View(ghana_covid) #Viewing entire column in a different tab

#Data Cleaning

#Renaming Columns
ghana_covid <- ghana_covid %>%
  rename(
    country = PAYS,
    country_id = ID_PAYS,
    region = REGION,
    region_id = ID_REGION,
    cases = CONTAMINES,
    deaths = DECES,
    recovered = GUERIS,
    female_cases = CONTAMINES_FEMME,
    male_cases = CONTAMINES_HOMME,
    unspecified_gender_cases = CONTAMINES_GENRE_NON_SPECIFIE
  )

colnames(ghana_covid) #check column names

#Removing Column
ghana_covid <- ghana_covid %>%
  select(-...15)

colnames(ghana_covid) #check column names
View(ghana_covid) #Viewing entire column in a different tab

#Removing All Rows with Regions Labeled Non spécifiér
n_before <- nrow(ghana_covid)# Count rows before

ghana_covid <- ghana_covid %>%
  filter(region != "Non spécifié") 

n_after <- nrow(ghana_covid)# Count rows after

cat("Initial number of rows were ", n_before)
cat("Current number of rows are ", n_after)
cat("Removed", n_before - n_after, "rows.\n")# Count how many were removed

View(ghana_covid) #Viewing entire column in a different tab

#Changing "Service de la Santé du Ghana" to "Ghana Health Service" in SOURCE column 
ghana_covid <- ghana_covid %>%
  mutate(SOURCE = ifelse(SOURCE == "Service de la Santé du Ghana", "Ghana Health Service", SOURCE))

View(ghana_covid) #Viewing entire column in a different tab

# I am leaving N/A in the dataset because it won't affect the spatial modeling task

#Viewing the dataset from different angle using head()
options(tibble.width = Inf)  # Show all columns when using tibbles
head(ghana_covid) #Viewing column heads

#SPATIAL MODELING TASK

#Installation of spatial packages
install.packages("logger")#tmap depends on logger, you don't need to load it
install.packages("sf")
install.packages("tmap")

#Load Spatial Packages
library(sf)
library(tmap)

# Load Ghana Regions shapefile
ghana_shapefile <- st_read("C:/Users/LENOVO/Desktop/Advance Data Analysis Exam/Covid_19_Spatial_Modelling_Project/geoBoundaries-GHA-ADM1_simplified.shp")

head(ghana_shapefile) #Viewing column heads

# Check the region names
ghana_shapefile$shapeName

# Unique region names in COVID data
sort(unique(ghana_covid$region))

# Unique region names in shapefile
sort(unique(ghana_shapefile$shapeName))

# Remove " Region" from shapefile names
ghana_shapefile <- ghana_shapefile %>%
  mutate(region_clean = str_replace(shapeName, " Region$", ""),
         region_clean = ifelse(region_clean == "Ahafo", "Brong Ahafo", region_clean))

sort(unique(ghana_shapefile$region_clean)) # cleaned unique region names in shapefile

ghana_covid$deaths <- as.numeric(ghana_covid$deaths) #Make deaths numeric in order to sum it, because it was char

#Making covid data compact in order to display it
covid_summary <- ghana_covid %>%
  group_by(region) %>%
  summarise(
    total_cases = sum(cases, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE)
  )

View(covid_summary)

#Merging the covid data with the shapefile
ghana_merged <- ghana_shapefile %>%
  left_join(covid_summary, by = c("region_clean" = "region"))


tmap_mode("view")  #Making the map interactive

#Styling and plotting the map
tm_shape(ghana_merged) +
  tm_polygons(
    fill = "total_cases",                        # What to color by
    fill.scale = tm_scale(values = "brewer.yl_or_rd"),    # Color palette
    fill.legend = tm_legend(title = "Total Cases")  # Legend title
  ) +
  tm_title("Ghana COVID-19 Cases by Region")

combine_data <- ghana_merged

ghana_population <- read_delim("gha_population_adm1_2021.csv", delim = ",")

View(ghana_population)
View(ghana_merged)

ghana_merged_population <- combine_data %>%
  left_join(ghana_population, by = c("region_clean" = "region"))

View(ghana_merged_population)

#Styling and plotting the map
tm_shape(ghana_merged_population) +
  tm_polygons(
    fill = "total_cases",                        # What to color by
    fill.scale = tm_scale(values = "brewer.yl_or_rd"),    # Color palette
    fill.legend = tm_legend(title = "Total Cases")  # Legend title
  ) +
  tm_title("Ghana COVID-19 Cases by Region and Population")


#Spatial Regression in R
install.packages("spdep")
install.packages("spatialreg")

library(spdep)
library(spatialreg)

# Create neighbors list based on region boundaries
ghana_nb <- poly2nb(ghana_merged_population)

# Convert to spatial weights list
ghana_weights <- nb2listw(ghana_nb, style = "W", zero.policy = TRUE)

# Fit a spatial lag model (lagsarlm)
lag_model <- lagsarlm(total_cases ~ `Total Population`, 
                      data = ghana_merged_population, 
                      listw = ghana_weights, 
                      zero.policy = TRUE)

summary(lag_model)

ghana_merged_population$residuals <- residuals(lag_model)

tm_shape(ghana_merged_population) +
  tm_polygons("residuals", palette = "-RdBu", title = "Model Residuals")







