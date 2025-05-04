
rm(list=ls())
library(readxl)
library(dplyr)
library(usmap)
library(sf)
library(ggplot2)

sac_data <- read.csv("data/sac_intake_outcome_data_3.2025.csv")
state_regions <- read.csv("metadata/State_Region.csv")
colnames(state_regions)[4] <- "location_state_us"
state_regions$State_Name <- NULL

populous_counties <- countypop %>% filter(pop_2022 > 2e5)
populous_counties$fips_int <- as.integer(populous_counties$fips)

# retain counties whose population is larger than 2e5
sac_data <- sac_data %>% filter(us_fips_code %in% populous_counties$fips) %>%
  filter(species_name == "Dog")

## retain locations which has complete monthly record between 2021 and 2024
sac_timepoints <- sac_data %>% select(location_id, year_of_record) %>%
  group_by(location_id, year_of_record) %>%
  summarise(months_recorded=n())

num_years_recorded <- sac_timepoints %>% filter(year_of_record >= 2021) %>%
  group_by(location_id) %>% summarise(years_recorded=n())
subset_sac <- num_years_recorded$location_id[num_years_recorded$years_recorded == 4]
sac_timepoints_subset <- sac_timepoints %>% filter(location_id %in% subset_sac) %>%
  arrange(location_id, year_of_record)
sac_timepoints_subset_summary <- sac_timepoints_subset %>%
  group_by(location_id) %>% summarise(mean_month = mean(months_recorded))

subset_locations <- sac_timepoints_subset_summary$location_id[sac_timepoints_subset_summary$mean_month == 12]

sac_subset <- sac_data %>% filter(location_id %in% subset_locations) %>% filter(year_of_record >= 2021) %>%
  arrange(location_id, year_of_record, month_of_record)

sac_subset <- sac_subset %>% left_join(state_regions, by="location_state_us")



# # show where the counties are
# county_data <- data.frame(fips=unique(countypop$fips))
# # county_data$fips_int <- as.integer(county_data$fips)
# county_data$avail <- ifelse((county_data$fips) %in% unique(sac_subset$us_fips_code),
#                                 "Available", "NA")
# options(usmap.crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +rf=298.257222101 +units=m +no_defs")
# plot_usmap(data = county_data, regions = "counties", values = "avail") +
#   scale_fill_manual(values = c("NA" = "lightgray", "Available" = "tomato1"),
#                     name = "Availability") +
#   labs(title = "Counties of available animal shelters") +
#   theme(legend.position = "right")

write.table(sac_subset, "data/subset_complete_2124.tsv", row.names=FALSE, sep='\t',
            quote=FALSE)

