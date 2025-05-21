
clean_data <- function(count_data){

  govaffi <- c("Yes", "No")
  gov_indicator <- grepl("without", count_data$org_type)
  count_data$Gov <- govaffi[gov_indicator+1]

  # preprocess the variables
  count_data[, "owner_total"] <- rowSums(count_data[, c("relinquished_by_owner_total", "seized_total")])
  count_data[, "youth_community_total"] <- rowSums(count_data[, c("youth_stray_at_large_count", "youth_relinquished_by_owner_count",
                                                                  "youth_other_intakes_count", "youth_seized_count")])
  count_data[, "adult_community_total"] <- rowSums(count_data[, c("adult_stray_at_large_count", "adult_relinquished_by_owner_count",
                                                                  "adult_other_intakes_count", "adult_seized_count")])

  count_data[, "adoption_rto"] <- rowSums(count_data[, c("adoption_total", "returned_to_owner_total")])
  count_data[, "adult_adoption_rto"] <- rowSums(count_data[, c("adult_adoption_count", "adult_returned_to_owner_count")])
  count_data[, "adult_live_outcomes"] <- rowSums(count_data[, c("adult_adoption_count", "adult_returned_to_owner_count",
                                                                "adult_transferred_out_total_count",
                                                                "adult_returned_to_field_count", "adult_other_live_outcome_count")])
  count_data[, "adult_other_outcomes"] <- rowSums(count_data[, c("adult_died_in_care_count",
                                                                  "adult_lost_in_care_count",
                                                                  "adult_shelter_euthanasia_count")])


  count_data[, "youth_adoption_rto"] <- rowSums(count_data[, c("youth_adoption_count", "youth_returned_to_owner_count")])
  count_data[, "youth_live_outcomes"] <- rowSums(count_data[, c("youth_adoption_count", "youth_returned_to_owner_count",
                                                                "youth_transferred_out_total_count",
                                                                "youth_returned_to_field_count", "youth_other_live_outcome_count")])
  count_data[, "youth_other_outcomes"] <- rowSums(count_data[, c("youth_died_in_care_count",
                                                                 "youth_lost_in_care_count",
                                                                 "youth_shelter_euthanasia_count")])

  # only select columns that we are interested in

  count_data <- count_data %>% select(location_id, year_of_record,  month_of_record, Gov, Division, location_state_us, # metadata
                                      gross_intakes, # total number of intakes
                                      net_intakes, transferred_in_total, # community intake vs transferred in
                                      owner_total, stray_at_large_total, # surrendered/seized from owner vs stray
                                      youth_community_total, adult_community_total, # youth vs adult in community intake
                                      youth_transferred_in_total_count, adult_transferred_in_total_count, # youth vs adult in transfered intake
                                      gross_outcomes, # total number of animals which left shelter
                                      live_outcomes, other_outcomes, # positive vs negative outcomes
                                      adoption_rto, transferred_out_total, # go directly to a human owner vs transferred out
                                      adult_live_outcomes, adult_other_outcomes, # adult positive vs negative outcomes
                                      adult_adoption_rto, adult_transferred_out_total_count, # adult go directly to a human owner vs transferred out
                                      youth_live_outcomes, youth_other_outcomes, # youth positive vs negative outcomes
                                      youth_adoption_rto, youth_transferred_out_total_count) # youth go directly to a human owner vs transferred out


  return(count_data)

}


