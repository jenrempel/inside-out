# Author: Kristin Dobbin, with edits by Jenny Rempel
# 
# Purpose:
#   - Data compilation
# 
# Overview: 
#   - This script compiles all of the data used in the paper analyses.
#
# Inputs:
#   - "outputs/acr_df_for_analysis.csv" 
#       (violation data, system type, and other covariates; created in script 02)
#   - "raw_data/CASDWIS_WaterSystemSummary_20250812.csv" 
#       (CA SDWIS data downloaded 08-12-2025 from 
#       https://sdwis.waterboards.ca.gov/PDWW/)
#   - "raw_data/i03_Hydrologic_Regions/i03_Hydrologic_Regions.shp"
#       (CA DWR hydrologic region data; downloaded 7/17/25;
#       https://data.ca.gov/dataset/i03-hydrologic-regions)
#   - "raw_data/ca_boundaries_072325/California_Drinking_Water_System_Area_Boundaries.geojson"
#       (CA SWRCB system boundary data downloaded 072325 from the CA State Geoportal; 
#       https://gis.data.ca.gov/datasets/waterboards::california-drinking-water-system-area-boundaries/about)
#   - "raw_data/2025riskassessment_categorysummarylevels.csv"
#       (2025 CA SWRCB risk assessment data downloaded in 2025 from 
#       https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/saferdashboard.html)
#   - "raw_data/i07_Water_Shortage_Vulnerability_Small_Water_Systems.csv"
#       (DWR water shortage vulnerability tool data; downloaded 08/12/25 from 
#       https://data.ca.gov/dataset/i07-water-shortage-vulnerability-small-water-systems)
#   - "processed_data/NCCR_full_list.csv"
#       (CCR data compiled by web scraping, created in script 03)
#
# Key outputs: 
#   - "outputs/fullpaperdata.csv"
#   - "outputs/fullpaperdata.rds" (same thing, rds format)


# Install pacman if not present
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Check, install missing, and load all packages in one call
pacman::p_load(tidyverse, 
               sf,
               forcats)

#Read in base data 
System_types <- read_csv("outputs/acr_df_for_analysis.csv") 
  # ^ MCL and monitoring violation outcome variables and carceral water system 
  # typology data for full universe of systems 
System_types <- System_types[,c(2,13:19,10,5,12,7,6,11)] #Reduce to only needed variables
System_types <- System_types %>% mutate(across(c(pwsid, carceral_sys), as.factor))

#Rename and reduce attributes from CA public drinking water water (CA SDWIS)
CASDWIS <- read_csv("raw_data/CASDWIS_WaterSystemSummary_20250812.csv") 
  #CA Public drinking water watch data for all public water systems downloaded Aug 12 2025
CASDWIS <- CASDWIS %>% rename(pwsid = 'Water System No.')
CASDWIS <- CASDWIS[,c(1,3,6,7)]
CASDWIS <- CASDWIS %>% rename(CASDWIS_County = 'Principal County Served')
CASDWIS <- CASDWIS %>% rename(CASDWIS_SourceType = 'Primary Source Water Type')
CASDWIS <- CASDWIS %>% mutate(across(c(pwsid, CASDWIS_County, CASDWIS_SourceType), as.factor))
CASDWIS <- CASDWIS %>% rename(CASDWIS_Type = 'Type')

#Join base data with CA SDWIS attributes
Data <- left_join(System_types, CASDWIS)

#Get rid of 42 NA systems no longer in SDWIS (assume closed) 
Data <- Data %>% filter(!is.na(CASDWIS_SourceType))

#Create binary variables for source type and whether source purchased or not
Data$watersource <- as.factor(ifelse(Data$CASDWIS_SourceType == "GU" | 
                                       Data$CASDWIS_SourceType == "GUP" |
                                       Data$CASDWIS_SourceType == "GW" | 
                                       Data$CASDWIS_SourceType == "GWP", 
                                     "GW", 
                                     "SW"))

Data$purchased <- as.factor(ifelse(Data$CASDWIS_SourceType == "GUP" | 
                                     Data$CASDWIS_SourceType == "GWP" |
                                     Data$CASDWIS_SourceType == "SWP", 
                                   "Purchased", 
                                   "Not purchased"))

Data$pws_type_code <- as.factor(Data$pws_type_code) #make factor
Data$CASDWIS_Type <- as.factor(Data$CASDWIS_Type) #make factor

#Remove NC system types (Non-community state designation)
Data <- Data %>% filter(CASDWIS_Type != "NC")
Data$pwsid <- droplevels(Data$pwsid) #Make sure all unique systems

summary(Data$carceral_sys)

#create variable for hydro region
Systemboundarypolygons <- st_read("raw_data/ca_boundaries_072325/California_Drinking_Water_System_Area_Boundaries.geojson")
Systemboundarypolygons <- st_make_valid(Systemboundarypolygons)
hydroregionpolygons <- st_read("raw_data/i03_Hydrologic_Regions/i03_Hydrologic_Regions.shp")
hydroregionpolygons <- st_transform(hydroregionpolygons, crs = 3310)
Systemboundarypolygons <- st_transform(Systemboundarypolygons, crs = 3310)

intersection <- st_intersection(Systemboundarypolygons, hydroregionpolygons) #perform intersection
intersection$area <- st_area(intersection) #calculate intersection area
majority_overlap_join <- intersection %>% #assign to hydro region based on majority overlap
  group_by(SABL_PWSID) %>% 
  slice_max(order_by = area, n = 1) %>%
  ungroup() %>%
  select(SABL_PWSID, HR_NAME, REGULATING_AGENCY) 
majority_overlap_join <- st_drop_geometry(majority_overlap_join) #get rid of spatial data

#Join hydro region data with main data
Data <- left_join(Data, majority_overlap_join, by = c("pwsid" = "SABL_PWSID"))
Data$HR_NAME <- as.factor(Data$HR_NAME)
summary(Data$HR_NAME) #811 don't have boundaries so are NA

#Add in additional output measures

#Needs assessment 2025

#read in and ready data
NA2025 <- read_csv("raw_data/2025riskassessment_categorysummarylevels.csv") #downloaded from the data dashboard September 24th
NA2025$SAFER_STATUS <- as.factor(NA2025$SAFER_STATUS)
NA2025$Water_Quality_Risk_Level_Display <- as.factor(NA2025$Water_Quality_Risk_Level_Display)
NA2025$Accessability_Risk_Level_Display <- as.factor(NA2025$Accessability_Risk_Level_Display)
NA2025$TMF_Capacity_Risk_Level_Display <- as.factor(NA2025$TMF_Capacity_Risk_Level_Display)
NA2025$Affordability_Risk_Level_Display <- as.factor(NA2025$Affordability_Risk_Level_Display)

#Join main data with needs assessment
Data <- left_join(Data, NA2025, join_by("pwsid" == "PWSID"))

#Change "not assessed" for needs assessment data into NA
Data <- Data %>%
  mutate(across(c(Water_Quality_Risk_Level_Display, SAFER_STATUS, 
                  Accessability_Risk_Level_Display, Affordability_Risk_Level_Display, 
                  TMF_Capacity_Risk_Level_Display), 
                ~replace_na(., "Not Assessed")))

#collapse risk assessment levels into a smaller number of categories
Data$Water_Quality_Risk_Level_Display <- fct_collapse(Data$Water_Quality_Risk_Level_Display,
                                 "High to medium risk" = c("High Risk", "Medium Risk"),
                                 "Low to no risk" = c("Low Risk", "No Risk"),
                                 "Not Assessed" = "Not Assessed")
Data$Accessability_Risk_Level_Display <- fct_collapse(Data$Accessability_Risk_Level_Display,
                                                      "High to medium risk" = c("High Risk", "Medium Risk"),
                                                      "Low to no risk" = c("Low Risk", "No Risk"),
                                                      "Not Assessed" = "Not Assessed")
Data$TMF_Capacity_Risk_Level_Display <- fct_collapse(Data$TMF_Capacity_Risk_Level_Display,
                                                      "High to medium risk" = c("High Risk", "Medium Risk"),
                                                      "Low to no risk" = c("Low Risk", "No Risk"),
                                                      "Not Assessed" = "Not Assessed")

#DWR water shortage vulnerability tool

#read in and ready data
DWRvulnerability <- read_csv("raw_data/i07_Water_Shortage_Vulnerability_Small_Water_Systems.csv")
DWRvulnerability <- DWRvulnerability[,c(2,43,57)]
DWRvulnerability$Distributiondis_any <- ifelse(DWRvulnerability$SC3i_Count_Distribution > 0, 1, 0)

#Join with main data
Data <- left_join(Data, DWRvulnerability,  join_by("pwsid" == "WATER_SYSTEM_NUMBER"))

#Mising CCR report data

#read in data
CCR <- read_csv("processed_data/NCCR_full_list.csv")

#Join with main data
Data <- left_join(Data, CCR, join_by("pwsid" == "id"))
# remove one duplicated row
Data <- Data %>% distinct(pwsid, .keep_all = TRUE)

# save full info on regulating agency (some got dropped during hydro region intersection)
Data_w_reg_agency <- Data |> 
  dplyr::select(-Regulating_Agency) 

#Save full paper data
write_csv(Data_w_reg_agency, "outputs/fullpaperdata.csv")
saveRDS(Data_w_reg_agency, "outputs/fullpaperdata.rds")