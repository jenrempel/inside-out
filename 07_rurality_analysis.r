# Author: Code by Kristin Dobbin and Jenny Rempel
# 
# Purpose:
#   - Assess the rurality (i.e. remoteness) of analyzed systems, per reviewer 
#      feedback
#   - Create final analytical dataset for reproducibility purposes
# 
# Overview: 
#   - The script assesses the rurality of the three types of water systems 
#      analyzed in the manuscript. It then creates the final analytical dataset.
#
# Inputs:
#   - "raw_data/caltrans/Adjusted_Urban_Area.geojson" (CalTrans urbanized areas 
#       list, downloaded from 
#       https://gisdata-caltrans.opendata.arcgis.com/datasets/71930cd25e6f4683ba2ee18511ac21ef_0/about 
#       on January 3rd. File last updated Dec 11, 2024)
#   - "raw_data/ca_boundaries_072325/California_Drinking_Water_System_Area_Boundaries.geojson"
#       (CA SWRCB system boundary data downloaded 072325 from the CA State Geoportal; 
#       https://gis.data.ca.gov/datasets/waterboards::california-drinking-water-system-area-boundaries/about)
#   - "outputs/fullpaperdata.csv" (created in script 04)
# 
# Key outputs: 
#   - "processed_data/rurality_both_options.csv" (used for last rows in Table 1)
#   - "outputs/final_analytical_dataset.csv" 


##############
##############
# setup
# Install pacman if not present
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Check, install missing, and load all packages in one call
pacman::p_load(tidyverse, 
               sf,
               ggmap,
               nngeo)

##############
##############
# useful functions
# %notin%: operator that returns the negation of %in%
`%notin%` <- Negate(`%in%`)

# count_df_na: takes a dataframe and returns counts of NA values in each column
count_df_na <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
}

# view_flat: quick look at sf objects w/o geometry
view_flat <- function(df) {
  st_drop_geometry(df) %>% 
    View()
}

##############
##############
# download CalTrans urbanized areas list
## Downloaded from https://gisdata-caltrans.opendata.arcgis.com/datasets/71930cd25e6f4683ba2ee18511ac21ef_0/about 
# on January 3rd. Last updated Dec 11, 2024. 
urbanized_areas <- st_read("raw_data/caltrans/Adjusted_Urban_Area.geojson") 

# explore that
str(urbanized_areas)
st_crs(urbanized_areas) #  "EPSG",4326; degrees lat/lon
#plot(st_geometry(urbanized_areas)) 

# load PWS boundaries
# ca pws service area boundaries - downloaded 7/23/25
pws_boundary <- read_sf("raw_data/ca_boundaries_072325/California_Drinking_Water_System_Area_Boundaries.geojson")
dim(pws_boundary) # 4804   35
st_crs(pws_boundary) # "EPSG",4326; degrees lat/lon 

# load main paper data
df1 <- read_csv("outputs/fullpaperdata.csv")

##############
##############
# filter service area boundaries for final list of PWSs included in study
sabl_boundaries <- pws_boundary %>% 
  janitor::clean_names() %>% 
  filter(sabl_pwsid %in% df1$pwsid)  

# if there are 2 boundaries for a given system, select the water service area
  # boundary, not the jurisdictional one
final_pws_boundaries <- sabl_boundaries %>% 
  group_by(sabl_pwsid) %>% 
  slice_min(order_by = factor(boundary_type, 
                              levels = c("Water Service Area", 
                                         "Jurisdictional")), 
            n = 1) %>% 
  ungroup()

# confirm there are no duplicates
final_pws_boundaries %>% 
  group_by(sabl_pwsid) %>% 
  filter(n() >1) # 0, good no duplicates

# save
saveRDS(final_pws_boundaries, "processed_data/final_pws_boundaries.rds")

# transform
final_pws_boundaries_t <- st_transform(final_pws_boundaries, crs = "EPSG:3310")
urbanized_areas_t <- st_transform(urbanized_areas, crs = "EPSG:3310")

# pull centroid of PWSs
pws_centroids <- st_centroid(final_pws_boundaries_t) 

#mapview(final_pws_boundaries_t) + 
#  mapview(pws_centroids)

# calculate distance to nearest neighbor (and time the operation)
system.time(
  distance_to_5kurban <- st_nn(pws_centroids, 
                                  urbanized_areas_t$geometry, 
                                  k=1, 
                                  returnDist = T) %>% 
    # NN = nearest neighbor
    set_names("nearest_neighbor", "nn_distance_meters") %>% 
    map_df(., unlist))

# double check that that worked - checked 3
#nearest_urban <- urbanized_areas_t[156,]

# plot
#mapview(urbanized_areas_t) + 
#  mapview(nearest_urban, col.regions = "red", col = "red") + 
#  mapview(pws_boundary_centroids[7,], col.regions = "green", col = "green")

# pull pwsids (column 2)
pws_ids <- final_pws_boundaries_t[2]

# add pwsids to distances df
nn_5kurban <- cbind(pws_ids, distance_to_5kurban)

# save PWSs that are >0 miles from a 5k urban area
pws_not_in_5kurban <- nn_5kurban %>% 
  filter(nn_distance_meters !=0)
# convert to miles
pws_not_in_5kurban$nn_distance_miles <- 
  pws_not_in_5kurban$nn_distance_meters/1609.34

# save PWSs that are 0 miles from a 5k urban area
pws_in_5kurban <- nn_5kurban %>% 
  filter(nn_distance_meters == 0)
# manually set to 0
pws_in_5kurban$nn_distance_miles <- 0

all_pws_w_rural5k <- rbind(pws_not_in_5kurban, pws_in_5kurban)

# using 5 miles as a threshold for rurality. Corresponds with census designation 
# for rural- distant (although definition of urban area for measuring to is 
# different, theirs is 50k people plus whereas the cal trans layer here is all 
# urbanized areas with more thank 5k people. 
all_pws_w_rural5k$remote_from_5kurban <- 
  ifelse(all_pws_w_rural5k$nn_distance_miles > 5, 
                                               "Yes", "No") 
saveRDS(all_pws_w_rural5k, "processed_data/all_pws_w_rural5k.rds")


##############
##############
# repeat with another definition of rural as >5miles from a 50k urban area
large_urbans <- urbanized_areas_t %>% 
  filter(UrbanAreas == 2)

# double check
st_crs(large_urbans) == st_crs(pws_centroids)

# calculate distance to nearest neighbor (and time the operation)
system.time(
  distance_to_large_urban <- st_nn(pws_centroids, 
                                   large_urbans$geometry, 
                               k=1, 
                               returnDist = T) %>% 
    # NN = nearest neighbor
    set_names("nearest_neighbor", "nn_distance_meters") %>% 
    map_df(., unlist))

# double check that that worked - checked 3
#nearest_urban <- urbanized_areas_t[156,]

# plot
#mapview(urbanized_areas_t) + 
#  mapview(nearest_urban, col.regions = "red", col = "red") + 
#  mapview(pws_boundary_centroids[7,], col.regions = "green", col = "green")

# pull pwsids (column 2)
pws_ids <- final_pws_boundaries_t[2]

# add pwsids to distances df
nn_large_urban <- cbind(pws_ids, distance_to_large_urban)

# save PWSs that are >0 miles from a 5k urban area
pws_not_in_large_urban <- nn_large_urban %>% 
  filter(nn_distance_meters !=0)
# convert to miles
pws_not_in_large_urban$nn_distance_miles <- 
  pws_not_in_large_urban$nn_distance_meters/1609.34

# save PWSs that are 0 miles from a 5k urban area
pws_in_large_urban <- nn_large_urban %>% 
  filter(nn_distance_meters == 0)
# manually set to 0
pws_in_large_urban$nn_distance_miles <- 0

all_pws_distance_to_lg_urban <- rbind(pws_not_in_large_urban, 
                                      pws_in_large_urban)

# using 5 miles as a threshold for rurality, but with 50k as the definition for 
# urban
all_pws_distance_to_lg_urban$remote_from_large_urban <- 
  ifelse(all_pws_distance_to_lg_urban$nn_distance_miles > 5, 
         "Yes", "No") 
saveRDS(all_pws_distance_to_lg_urban, 
        "processed_data/.all_pws_distance_to_lg_urban.rds")

##############
##############
# save both

rurality_option1_5k <- all_pws_w_rural5k %>% 
  st_drop_geometry() 

rurality_option2_50k <- all_pws_distance_to_lg_urban %>% 
  st_drop_geometry() 
  
rurality_both_options <- rurality_option1_5k %>% 
  left_join(rurality_option2_50k, by = "sabl_pwsid")

# simplify
rurality_final <- rurality_both_options %>% 
  dplyr::select(sabl_pwsid, remote_from_5kurban, remote_from_large_urban)

# save
saveRDS(rurality_both_options, "processed_data/rurality_both_options.rds")

# export for analysis
write_csv(rurality_both_options, "processed_data/rurality_both_options.csv") 

##############
##############
# load main paper data
df1 <- read_csv("outputs/fullpaperdata.csv")
table(df1$carceral_sys)

# load rurality data
rural <- readRDS("processed_data/rurality_both_options.rds")
rural_cl <- rural %>%
  janitor::clean_names() %>%
  rename(pwsid = sabl_pwsid) %>%
  mutate(remote_from_large_urban = as_factor(remote_from_large_urban),
         remote_from_5kurban = as_factor(remote_from_5kurban)) %>%
  dplyr::select(pwsid, remote_from_large_urban, remote_from_5kurban)

# join the two df's
rural_df <- df1 %>%
  left_join(rural_cl, by = "pwsid")

# check if any that should have info are not there
rural_df %>% filter(is.na(remote_from_large_urban) &
                      !is.na(HR_NAME)) %>% nrow() # none, good

# check the inverse
rural_df %>% filter(!is.na(remote_from_large_urban) &
                      is.na(HR_NAME)) %>% view()
# 3 systems have distance data but lack hydrologic data, bc they're on islands
# and DWR hydro regions don't cover those areas


##############
##############
# add to table 1, by group:
tbl1_add_5k_urban <- rural_df %>%
  # #&% of systems that are beyond 5 miles of a 5k urban area
  group_by(carceral_sys, remote_from_5kurban) %>%
  summarize (count = n(), .groups = "drop")

tbl1_additions_lg_urban <- rural_df %>%
  # #&% of systems that are beyond 5 miles of a 50k urban area
  group_by(carceral_sys, remote_from_large_urban) %>%
  summarize (count = n(), .groups = "drop")


##############
##############
# save final analytical dataset
df <- rural_df

# recreate missing ccr variable (originally created and used in script 05)
df$Missing_ccr_any <- as.factor(ifelse(df$n_ccr_missing > 0, 
                                         "Yes", 
                                         "No"))

# rename and relocate variable names for ease
final_analytical_dataset <- df |> 
  dplyr::select(-c(pws_type_code,
                   service_connections_count,
                   health_5yr_count_no_2ndary_or_lsl,
                   mon_only_5yr_count,
                   mr_5yr_binary,
                   mr_5yr_count,
                   gw_sw_code,
                   primary_source_code,
                   CASDWIS_SourceType,
                   System_Name,
                   County,
                   Affordability_Risk_Level_Display,
                   POPULATION,
                   `Distinct count of PWSID`,
                   `Funding Since 2017`,
                   SC3i_Count_Distribution,
                   SC5e_Drought_Impact,
                   Distributiondis_any,
                   n_ccr_downloaded,
                   n_ccr_missing)) |> 
  rename(cws_or_ntnc = CASDWIS_Type,
         system_type = carceral_sys,
         health_5yr_binary = health_5yr_no2ndary_or_lsl_binary,
         monitoring_5yr_binary = mon_only_5yr_binary,
         water_source = watersource,
         water_purchasing = purchased,
         hydrologic_region = HR_NAME,
         regulating_agency = REGULATING_AGENCY,
         water_quality_risk_level = Water_Quality_Risk_Level_Display,
         water_accessibility_risk_level = Accessability_Risk_Level_Display,
         tmf_capacity_risk_level = TMF_Capacity_Risk_Level_Display,
         safer_overall_status = SAFER_STATUS,
         rurality_remote_from_50k_urban = remote_from_large_urban,
         rurality_remote_from_5kurban = remote_from_5kurban,
         county = CASDWIS_County,
         missing_ccrs = Missing_ccr_any) |> 
  relocate(pwsid,
           pws_name,
           system_type,
           population_served_count,
           cws_or_ntnc,
           water_source,
           water_purchasing,
           hydrologic_region,
           rurality_remote_from_5kurban,
           rurality_remote_from_50k_urban,
           health_5yr_binary,
           monitoring_5yr_binary,
           water_quality_risk_level,
           water_accessibility_risk_level,
           tmf_capacity_risk_level,
           safer_overall_status,
           missing_ccrs
  )

write_csv(final_analytical_dataset, "outputs/final_analytical_dataset.csv")
