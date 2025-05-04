library(dplyr)
library(ggplot2)
rm(list=ls())

data_of_interest <- read.table("data/subset_complete_2124.tsv",
                               sep='\t', header=T)

col_metadata <- c("organization_id", "location_id",
                  "org_type", "location_state_us",
                  "us_county_name", "us_fips_code", "Region", "Division")

unique_sites <- unique(data_of_interest[, col_metadata])


# select a random site
site_example <- sample(unique_sites$location_id, 1)

example_data <- data_of_interest %>% filter(location_id == site_example) %>%
  arrange(year_of_record, month_of_record)


# intake_columns
col_intake <- c("gross_intakes", "net_intakes", "stray_at_large_total",
                "relinquished_by_owner_total", "seized_total",
                "other_intakes_total", "transferred_in_undesignated_total",
                "transferred_in_instate_total", "transferred_in_outstate_total",
                "transferred_in_international_total", "transferred_in_total")

example_data_intake <- example_data[, c("year_of_record", "month_of_record", col_intake)]
## relationship between columns
all(example_data_intake[, "gross_intakes"] + example_data_intake[, c("transferred_in_total",
                                                                   "net_intakes")])
all(example_data_intake[, "net_intakes"] == rowSums(example_data_intake[, c("stray_at_large_total",
                                                                            "relinquished_by_owner_total",
                                                                            "seized_total",
                                                                            "other_intakes_total")]))
all(example_data_intake[, "transferred_in_total"] == rowSums(example_data_intake[, c("transferred_in_instate_total",
                                                                                     "transferred_in_outstate_total",
                                                                                     "transferred_in_international_total")]))

# outcome columns
col_outcomes <- c("gross_outcomes", "other_outcomes", "live_outcomes",
                  "adoption_total", "returned_to_owner_total", "transferred_out_undesignated_total",
                  "transferred_out_instate_total", "transferred_out_outstate_total",
                  "transferred_out_international_total", "transferred_out_total",
                  "returned_to_field_total", "other_live_outcome_total", "died_in_care_total",
                  "lost_in_care_total", "shelter_euthanasia_total")


example_data_outcome <- example_data[, c("year_of_record", "month_of_record", col_outcomes)]

all(example_data_outcome[, "gross_outcomes"] == rowSums(example_data_outcome[, c("other_outcomes",
                                                                                 "live_outcomes")]))
all(example_data_outcome[, "live_outcomes"] == rowSums(example_data_outcome[, c("adoption_total",
                                                                                "returned_to_owner_total",
                                                                                "transferred_out_total",
                                                                                "returned_to_field_total",
                                                                                "other_live_outcome_total")]))
all(example_data_outcome[, "other_outcomes"] == rowSums(example_data_outcome[, c("died_in_care_total",
                                                                                 "lost_in_care_total",
                                                                                 "shelter_euthanasia_total")]))










