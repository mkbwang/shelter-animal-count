
rm(list=ls())
library(readxl)
library(dplyr)
library(usmap)
# library(sf)
library(ggplot2)

sac_data <- read.csv("data/sac_intake_outcome_data_3.2025.csv")
state_regions <- read.csv("metadata/State_Region.csv")
colnames(state_regions)[4] <- "location_state_us"
state_regions$State_Name <- NULL

# populous_counties <- countypop %>% filter(pop_2022 > 2e5)
# populous_counties$fips_int <- as.integer(populous_counties$fips)

# retain counties whose population is larger than 2e5
sac_data <- sac_data %>%
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
sac_subset$completion_status_split <- NULL
sac_subset$species_name <- NULL


# retain locations where the number of monthly gross intake and outcome are at least 10
sac_subset$intakegt10 <- sac_subset$gross_intakes >= 10
sac_subset$outcomegt10 <- sac_subset$gross_outcomes >= 10
sac_subset_consistency <- sac_subset %>% group_by(location_id) %>%
  summarise(mean_intakegt10=mean(intakegt10), mean_outcomegt10=mean(outcomegt10))
sac_subset_consistency <- sac_subset_consistency %>% filter(mean_intakegt10 == 1 & mean_outcomegt10 == 1)
sac_subset <- sac_subset %>% filter(location_id %in% sac_subset_consistency$location_id)

# only retain organizations that are government animal services or shelters (with or without government contract)
sac_subset <- sac_subset %>%
  filter(org_type %in% c("Animal Shelter with a Government Contract", "Government Animal Services", "Shelter without a Government Contract"))

# merge with populations
colnames(countypop)[1] <- "us_fips_code"
sac_subset <- countypop %>% select(us_fips_code, pop_2022) %>%
  right_join(sac_subset, by="us_fips_code")

sac_subset$intakegt10 <- NULL
sac_subset$outcomegt10 <- NULL
sac_subset <- sac_subset %>% select(organization_id, location_id, org_type, location_state_us, us_county_name, Region, Division,
                                    us_fips_code, everything())



# carry out data cleaning and extract out key variables of interest
source("preprocess.R")
sac_subset_cleaned <- clean_data(sac_subset)


# I also want to confirm that the intakes and outcomes are well classified
sac_subset_cleaned_summary <- sac_subset_cleaned %>% select(location_id, net_intakes, owner_total, stray_at_large_total,
                                                    adult_community_total, youth_community_total,
                                                    live_outcomes, adoption_rto, transferred_out_total,
                                                    adult_live_outcomes, youth_live_outcomes)  %>%
  mutate(known_net_intake_source = (owner_total + stray_at_large_total)/net_intakes,
                                                    known_net_intake_age = (youth_community_total + adult_community_total)/net_intakes,
                                                    known_live_outcome_source = (adoption_rto + transferred_out_total)/live_outcomes,
                                                    known_live_outcome_age = (adult_live_outcomes + youth_live_outcomes)/live_outcomes)


sac_subset_cleaned_summary_mean <- sac_subset_cleaned_summary %>% group_by(location_id) %>%
  summarise(net_intakes=mean(net_intakes > 0),
            live_outcomes=mean(live_outcomes > 0),
            known_net_intake_source=min(known_net_intake_source, na.rm=T),
            known_net_intake_age = min(known_net_intake_age, na.rm=T),
            known_live_outcome_source = min(known_live_outcome_source, na.rm=T),
            known_live_outcome_age = min(known_live_outcome_age, na.rm=T))

sac_subset_cleaned_summary_mean <- sac_subset_cleaned_summary_mean %>%
  filter(live_outcomes == 1 & net_intakes == 1 & known_net_intake_source > 0.5 & known_net_intake_age > 0.5 &
           known_live_outcome_source >0.5 & known_live_outcome_age > 0.5)

sac_subset_cleaned <- sac_subset_cleaned %>% filter(location_id %in%
                                                      sac_subset_cleaned_summary_mean$location_id)
sac_subset_cleaned <- na.omit(sac_subset_cleaned)
write.table(sac_subset_cleaned, "data/subset_complete_2124.tsv", row.names=FALSE, sep='\t',
            quote=FALSE)

