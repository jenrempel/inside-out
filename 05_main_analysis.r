# Author: Kristin Dobbin, with edits by Jenny Rempel
# 
# Purpose:
#   - Data analysis
# 
# Overview: 
#   - This script performs the analyses to develop Figure 1, Table 1, Table 2, 
#       chi-squared tests (Table 3 and Table S5), and Figure S1. 
#
# Inputs:
#   - "outputs/fullpaperdata.csv" (created in script 04)
#   - "outputs/fullpaperdata.csv" (same data in rds format, from script 04)
#   - "raw_data/ca_boundaries_072325/California_Drinking_Water_System_Area_Boundaries.geojson"
#       (CA SWRCB system boundary data downloaded 072325 from the CA State Geoportal; 
#       https://gis.data.ca.gov/datasets/waterboards::california-drinking-water-system-area-boundaries/about)
# 
# Outputs:
#   - "outputs/Figure1.eps"
#   - "outputs/Appendixplots.png"
#   - "outputs/Appendixplots_nolegend.png"
#   - "outputs/table_s4.csv"

# Install pacman if not present
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Check, install missing, and load all packages in one call
pacman::p_load(tidyverse, 
               gtsummary,
               kableExtra,
               ggmap,
               sf,
               sp,
               tigris, 
               jgsbook,
               ggpubr)

#Read in and prep full paper data
Data <- read_csv("outputs/fullpaperdata.csv")
Data <- Data %>% 
  mutate(across(c(pwsid, CASDWIS_Type, carceral_sys, 
                  health_5yr_no2ndary_or_lsl_binary, mon_only_5yr_binary, 
                  mr_5yr_binary, CASDWIS_County, watersource, purchased, 
                  SAFER_STATUS, SC5e_Drought_Impact, Distributiondis_any, 
                  HR_NAME, Water_Quality_Risk_Level_Display, 
                  Accessability_Risk_Level_Display, 
                  Affordability_Risk_Level_Display, 
                  TMF_Capacity_Risk_Level_Display), as.factor))
Data$Missing_ccr_any <- as.factor(ifelse(Data$n_ccr_missing > 0, 
                                         "Yes", 
                                         "No"))
Data$Missing_ccr_twoplus <- as.factor(ifelse(Data$n_ccr_missing >= 2, 
                                             "Yes", 
                                             "No"))


# Table 1 -----------------------------------------------------------------

#Provide basic descriptive on systems across typology
System_descriptives <- Data[, c(6,8,15,18,19,20)]
Table <- System_descriptives %>% tbl_summary(by = carceral_sys)
kable_Table <- Table %>% as_kable_extra(); kable_Table
  # note: rurality rows created in separate analysis


# Figure 1 (map) ----------------------------------------------------------

#Map water systems by type

#load in boundary polygons for CWS
PWS_boundary <- st_read("raw_data/ca_boundaries_072325/California_Drinking_Water_System_Area_Boundaries.geojson")
str(PWS_boundary)
PWS_boundary <- st_make_valid(PWS_boundary)
PWS_boundary$SABL_PWSID <- as.factor(PWS_boundary$SABL_PWSID)
PWS_boundary$WATER_SYSTEM_NAME <- as.factor(PWS_boundary$WATER_SYSTEM_NAME)
#remove jurisdictional boundaries to get rid of duplicates
PWS_boundary <- PWS_boundary %>% 
  filter(BOUNDARY_TYPE != "Jurisdictional") 
  

#Create a new version of the main data with geo attributes
Geodata <- Data
Geodata <- left_join(Geodata, PWS_boundary, by = c("pwsid" = "SABL_PWSID"))
Geodata <- Geodata %>% distinct(pwsid, .keep_all = TRUE) #get rid of one duplicate

#Get centroids to service area boundaries
Geodata$Centroid <- NA
Geodata$Centroid <- st_centroid(Geodata$geometry) 

#Basemaps
caCountiesTmp <- tigris::counties(state = 06) %>%
  st_as_sf()

Geodata <- st_as_sf(Geodata, sf_column_name = "Centroid")
Geodata$carceral_sys <- factor(Geodata$carceral_sys, 
                               levels = c("carceral_water_system", 
                                          "carceral_serving_water_system", 
                                          "other_water_systems"))
# arrange before mapping
Geodata_sorted <- Geodata |> 
  arrange(carceral_sys == "carceral_water_system" | 
            carceral_sys == "carceral_serving_water_system")

#map
figure1 <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), 
          size = 1, data = Geodata_sorted, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "#d95f02", 
                                "other_water_systems" = "azure3", 
                                "carceral_serving_water_system" = "#7570b3"),
                     labels = c("carceral_water_system" = "Carceral water system", 
                                "other_water_systems" = "Other water system", 
                                "carceral_serving_water_system" = 
                                  "Partially carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Helvetica"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

figure1

#save map
ggsave('outputs/Figure1.eps', dpi = 800, bg='#ffffff',
       width = 175, units = "mm")



# Table 2 -----------------------------------------------------------------

#Provide basic descriptive for outcome measures by typology
Outcomes <- Data[,c(8,9,11,24,25,27,29,37)]
Table2 <- Outcomes %>% tbl_summary(by = carceral_sys, missing = "no")
kable_Table2 <- Table2 %>% as_kable_extra(); kable_Table2


# Chi-squared tests -------------------------------------------------------

### Table S5

#Run Chi-squared tests for outcomes
MCL <- xtabs(~ health_5yr_no2ndary_or_lsl_binary + carceral_sys , data = Data)
chisq.test(MCL, correct = FALSE) #No sig differences between any groups
p.adjust(0.2046, method = "bonferroni", n = 7) #adjusted p value

MON <- xtabs(~ mon_only_5yr_binary + carceral_sys , data = Data); MON
chisq.test(MON, correct = FALSE) #sig 
p.adjust(0.00359, method = "bonferroni", n = 7) #adjusted p value

CCR <- xtabs(~ Missing_ccr_any + carceral_sys , data = Data); CCR
chisq.test(CCR, correct = FALSE) # Sig differences 
p.adjust(2.445e-05, method = "bonferroni", n = 7) #adjusted p value

# create dataset with NAs (not asssessed) removed to do the same for the 
# needs assessment data
Data_notassessedremoved <- Data
Data_notassessedremoved$Water_Quality_Risk_Level_Display[Data$Water_Quality_Risk_Level_Display == "Not Assessed"] <- NA
Data_notassessedremoved$Accessability_Risk_Level_Display[Data$Accessability_Risk_Level_Display == "Not Assessed"] <- NA
Data_notassessedremoved$TMF_Capacity_Risk_Level_Display[Data$TMF_Capacity_Risk_Level_Display == "Not Assessed"] <- NA
Data_notassessedremoved$SAFER_STATUS[Data$SAFER_STATUS == "Not Assessed"] <- NA
Outcomes_notassessedremoved <- Data_notassessedremoved[,c(8,9,11,24,25,27,29,37)]
Outcomes_notassessedremoved <- na.omit(Outcomes_notassessedremoved)
Outcomes_notassessedremoved <- droplevels(Outcomes_notassessedremoved)
nrow(Data) # 4250
nrow(Outcomes_notassessedremoved) # 3024

QR <- xtabs(~ Water_Quality_Risk_Level_Display + carceral_sys , 
            data = Outcomes_notassessedremoved); QR
chisq.test(QR, correct = FALSE) # not sig different 
p.adjust(0.4525, method = "bonferroni", n = 7) #adjusted p value

AR <- xtabs(~ Accessability_Risk_Level_Display + carceral_sys , 
            data = Outcomes_notassessedremoved); AR
chisq.test(AR, correct = FALSE) # there are sig differences, p<0.001
p.adjust(2.212e-11, method = "bonferroni", n = 7) #adjusted p value

TMFR <- xtabs(~ TMF_Capacity_Risk_Level_Display + carceral_sys , 
              data = Outcomes_notassessedremoved); TMFR
chisq.test(TMFR, correct = FALSE) # there are sig differences, p<0.001
p.adjust(2.2e-16, method = "bonferroni", n = 7) #adjusted p value

Outcomes_notassessedremoved$failingoratrisk <- as.factor(ifelse(Outcomes_notassessedremoved$SAFER_STATUS == "At-Risk" | 
                                                                  Outcomes_notassessedremoved$SAFER_STATUS == "Failing", 
                                                                "Yes", "No"))

FAILORRISK <- xtabs(~ failingoratrisk + carceral_sys, 
                    data = Outcomes_notassessedremoved); FAILORRISK
chisq.test(FAILORRISK, correct = FALSE) # sig difference p=0.02
p.adjust(0.02107, method = "bonferroni", n = 7) #adjusted p value 


###### Table 3 
# Pairwise chi-squared table 
mcl <- pairwise.chisq.test(Data$health_5yr_no2ndary_or_lsl_binary, 
                           Data$carceral_sys)
mcl$Outcome <- "One or more health-based violation 2020-2024"
mcl <- mcl %>% relocate(Outcome, .before = group1)

mon <- pairwise.chisq.test(Data$mon_only_5yr_binary, Data$carceral_sys)
mon$Outcome <- "One or more monitoring violation 2020-2024"
mon <- mon %>% relocate(Outcome, .before = group1)

qr <- pairwise.chisq.test(Data$Water_Quality_Risk_Level_Display, Data$carceral_sys)
qr$Outcome <- "Water quality risk level"
qr <- qr %>% relocate(Outcome, .before = group1)

ar <- pairwise.chisq.test(Outcomes_notassessedremoved$Accessability_Risk_Level_Display, 
                          Outcomes_notassessedremoved$carceral_sys)
ar$Outcome <- "Water accessibility risk level"
ar <- ar %>% relocate(Outcome, .before = group1)

tmfr <-  pairwise.chisq.test(Outcomes_notassessedremoved$TMF_Capacity_Risk_Level_Display, 
                             Outcomes_notassessedremoved$carceral_sys)
tmfr$Outcome <- "Technical, Managerial Financial risk level"
tmfr <- tmfr %>% relocate(Outcome, .before = group1)

status <- pairwise.chisq.test(Outcomes_notassessedremoved$failingoratrisk, 
                              Outcomes_notassessedremoved$carceral_sys)
status$Outcome <- "Designated failing or at-risk"
status <- status %>% relocate(Outcome, .before = group1)

ccr <- pairwise.chisq.test(Outcomes_notassessedremoved$Missing_ccr_any, 
                           Outcomes_notassessedremoved$carceral_sys)
ccr$Outcome <- "Missing one or more Consumer Confidence Report 2020-2023"
ccr <- ccr %>% relocate(Outcome, .before = group1)

pairwise <- rbind(mcl, mon, qr, ar, tmfr, status, ccr)

pairwise <- pairwise %>% mutate(across(c(xsquare, pvalue, padjust), as.numeric))

pairwise_rounded <- data.frame(lapply(pairwise, function(x) {
  if (is.numeric(x)) {
    round(x, digits = 2)
  } else {
    x 
  }
}))

#view(pairwise_rounded)


# Supplemental Figure S1 ---------------------------------------------------

#Make several versions of Figure 1 (statewide map of typology) showing only 
# systems by type that are at risk for each outcome measure

Geodata_MCL <- Geodata %>% filter(health_5yr_no2ndary_or_lsl_binary == 1)

Map_MCL <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), 
          size = .35, data = Geodata_MCL, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", 
                                "other_water_systems" = "azure3", 
                                "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", 
                                "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = 
                                  "Partially carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

Geodata_MON <- Geodata %>% filter(mon_only_5yr_binary == 1)

Map_MON <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), 
          size = .35, data = Geodata_MCL, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", 
                                "other_water_systems" = "azure3", 
                                "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", 
                                "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = 
                                  "Partially carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

Geodata_QR <- Geodata %>% 
  filter(Water_Quality_Risk_Level_Display == "High to medium risk")

Map_QR <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), 
          size = .35, data = Geodata_QR, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", 
                                "other_water_systems" = "azure3", 
                                "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", 
                                "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = 
                                  "Partially carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))


Geodata_AR <- Geodata %>% filter(Accessability_Risk_Level_Display == "High to medium risk")

Map_AR <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), 
          size = .35, data = Geodata_AR, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", 
                                "other_water_systems" = "azure3", 
                                "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", 
                                "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = 
                                  "Partially carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

Geodata_TMFR <- Geodata %>% 
  filter(TMF_Capacity_Risk_Level_Display == "High to medium risk")

Map_TMFR <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), 
          size = .35, data = Geodata_TMFR, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", 
                                "other_water_systems" = "azure3", 
                                "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", 
                                "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = 
                                  "Partially carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

Geodata_STATUS <- Geodata %>% 
  filter(SAFER_STATUS == "Failing" | SAFER_STATUS == "At-Risk")

Map_STATUS <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), 
          size = .35, data = Geodata_STATUS, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", 
                                "other_water_systems" = "azure3", 
                                "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", 
                                "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = 
                                  "Partially carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

Appendixplots <- ggarrange(Map_MCL, Map_MON, Map_QR, Map_AR, Map_TMFR, Map_STATUS,
                           nrow = 3, ncol = 2, common.legend = TRUE, 
                           legend = "right", labels = "auto")
ggsave('outputs/Appendixplots.png', dpi = 720, width = 7.5, height = 10, 
       bg='#ffffff')

AppendixplotsB <- ggarrange(Map_MCL, Map_MON, Map_QR, Map_AR, Map_TMFR, Map_STATUS, 
                            nrow = 2, ncol = 3, common.legend = TRUE, 
                            legend = "none", labels = "auto", align='hv', 
                            hjust=-4.5, vjust=3)
ggsave('outputs/Appendixplots_nolegend.png', dpi = 720, width = 8.5, 
       height = 10, bg='#ffffff')


# Supplemental Table S4 -------------------------------------------------------
df <- readRDS("outputs/fullpaperdata.rds")

# format data for table s4
df_carceral_sys <- df |> 
  filter(carceral_sys == "carceral_water_system") |> 
  mutate(one_or_more_health = ifelse(health_5yr_no2ndary_or_lsl_binary == 1, 
                                     "Yes",
                                     ""),
         one_or_more_mon = ifelse(mon_only_5yr_binary == 1, 
                                     "Yes",
                                     ""),
         wq_risk = case_when(
           Water_Quality_Risk_Level_Display == "Not Assessed" ~ "NA",
           Water_Quality_Risk_Level_Display == "High to medium risk" ~ "Yes",
           Water_Quality_Risk_Level_Display == "Low to no risk" ~ ""),
         access_risk = case_when(
           Accessability_Risk_Level_Display == "Not Assessed" ~ "NA",
           Accessability_Risk_Level_Display == "High to medium risk" ~ "Yes",
           Accessability_Risk_Level_Display == "Low to no risk" ~ ""),
         tmf_risk = case_when(
           TMF_Capacity_Risk_Level_Display == "Not Assessed" ~ "NA",
           TMF_Capacity_Risk_Level_Display == "High to medium risk" ~ "Yes",
           TMF_Capacity_Risk_Level_Display == "Low to no risk" ~ ""),
         risk_assess_status = ifelse(
           SAFER_STATUS == "Not Assessed", "NA", SAFER_STATUS),
         missing_ccrs = ifelse(
           n_ccr_missing > 0, "Yes", "")) |> 
  dplyr::select(REGULATING_AGENCY,
                pws_name,
                pwsid,
                CASDWIS_County,
                one_or_more_health,
                one_or_more_mon,
                wq_risk,
                access_risk,
                tmf_risk,
                risk_assess_status,
                missing_ccrs) 

# manually update SDWA regulating agency for the 4 NTNC systems that are missing
# it (due to not being included in the system area boundaries dataset)
table_s4 <- df_carceral_sys |> 
  mutate(sdwa_regulating_agency = case_when(
    pwsid == "CA1900089" ~ "DISTRICT 16 - CENTRAL",
    pwsid == "CA1900904" ~ "DISTRICT 16 - CENTRAL",
    pwsid == "CA5210800" ~ "DISTRICT 21 - VALLEY",
    pwsid == "CA5210801" ~ "DISTRICT 21 - VALLEY",
    TRUE ~ "REGULATING_AGENCY"
  )) |> 
  dplyr::select(-REGULATING_AGENCY)


write_csv(table_s4, "outputs/table_s4.csv")

