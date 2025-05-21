library(dplyr)
library(ggplot2)
rm(list=ls())

# source("preprocess.R")
data_of_interest <- read.table("data/subset_complete_2124.tsv",
                               sep='\t', header=T)



state_regions <- read.csv("metadata/State_Region.csv")

# data_of_interest$species_name <- NULL
# data_of_interest$completion_status_split <- NULL
# govaffi <- c("Yes", "No")
# gov_indicator <- grepl("without", data_of_interest$org_type)
# data_of_interest$Gov <- govaffi[gov_indicator+1]



col_metadata <- c("location_id", "Division", "Gov")
shelter_metadata <- data_of_interest[, col_metadata] |> unique()

View(shelter_metadata %>% group_by(Division, Gov) %>% summarise(count=n()))



colors = c("#000080", "#008080", "#014421", "#301934",
          "#36454F", " #2F4F4F", "#4B0082", "#800000", "#556B2F",
          "#5C4033")


library(lme4)


glmm_subset <- function(input_df, division){

  # select a subset of data
  counts_subset <- input_df %>% filter(Division == division)


  # sum all the 12 months by years
  counts_subset_by_year <- counts_subset %>% select(-Gov) %>% group_by(location_id, year_of_record, Division) %>%
    summarise(across(everything(), sum))


  # set up the backward difference coding
  t1_coding <- c(1/4, -3/4)
  t1 <- t1_coding[1+(counts_subset_by_year$year_of_record == 2021)]
  t2_coding <- c(0.5, -0.5)
  t2 <- t2_coding[1+(counts_subset_by_year$year_of_record <= 2022)]
  t3_coding <- c(-1/4, 3/4)
  t3 <- t3_coding[1+(counts_subset_by_year$year_of_record == 2024)]

  counts_subset_by_year$t1 <- t1
  counts_subset_by_year$t2 <- t2
  counts_subset_by_year$t3 <- t3


  intake_trend_coefs <- list()

  # total numbers of intake
  total_intake <- glmer(gross_intakes ~ t1+t2+t3+(1|location_id),
                        family=poisson, data=counts_subset_by_year) |> summary()
  intake_trend_coefs$total_intake <- total_intake$coefficients[2:4, c(1,4)]


  # community intake vs transfer
  community_vs_transfer <- glmer(cbind(net_intakes, transferred_in_total) ~ t1+t2+t3+(1|location_id),
                                 family=binomial, data=counts_subset_by_year) |> summary()
  intake_trend_coefs$community_vs_transferin <- community_vs_transfer$coefficients[2:4, c(1,4)]



  # surrender vs stray among community intakes
  surrender_vs_stray <- glmer(cbind(owner_total, stray_at_large_total) ~ t1+t2+t3+(1|location_id),
                              family=binomial, data=counts_subset_by_year) |> summary()
  intake_trend_coefs$surrender_vs_stray <- surrender_vs_stray$coefficients[2:4, c(1,4)]




  # youth vs adult in community intakes
  youth_vs_adult_community <- glmer(cbind(youth_community_total, adult_community_total) ~ t1+t2+t3+(1|location_id),
                                    family=binomial, data=counts_subset_by_year) |> summary()
  intake_trend_coefs$youth_vs_adult_community <- youth_vs_adult_community$coefficients[2:4, c(1,4)]



  # youth vs adult in transfer-ins
  youth_vs_adult_transferin <- glmer(cbind(youth_transferred_in_total_count, adult_transferred_in_total_count) ~ t1+t2+t3+(1|location_id),
                                     family=binomial, data=counts_subset_by_year) |> summary()
  intake_trend_coefs$youth_vs_adult_transferin <- youth_vs_adult_transferin$coefficients[2:4, c(1,4)]


  ## outcome trends

  outcome_trend_coefs <- list()

  # total number of outcomes
  total_outcome <- glmer(gross_outcomes ~ t1+t2+t3+(1|location_id),
                         family=poisson, data=counts_subset_by_year) |> summary()
  outcome_trend_coefs$total_outcome <- total_outcome$coefficients[2:4, c(1,4)]


  # total live vs nonlive outcomes
  live_vs_nonlive <- glmer(cbind(live_outcomes, other_outcomes) ~ t1+t2+t3+(1|location_id),
                           family=binomial, data=counts_subset_by_year) |> summary()
  outcome_trend_coefs$live_vs_nonlive <- live_vs_nonlive$coefficients[2:4, c(1,4)]



  # total adoption vs transfer out

  adoption_vs_transfer <- glmer(cbind(adoption_rto, transferred_out_total) ~ t1+t2+t3+(1|location_id),
                                family=binomial, data=counts_subset_by_year) |> summary()
  outcome_trend_coefs$adoption_vs_transfer <- adoption_vs_transfer$coefficients[2:4, c(1,4)]


  ## Next step is to break down to adult and youth


  # adult live vs nonlive outcomes
  adult_live_vs_nonlive <- glmer(cbind(adult_live_outcomes, adult_other_outcomes) ~ t1+t2+t3+(1|location_id),
                                 family=binomial, data=counts_subset_by_year) |> summary()
  outcome_trend_coefs$adult_live_vs_nonlive <- adult_live_vs_nonlive$coefficients[2:4, c(1,4)]



  # adult adoption vs transfer
  adult_adoption_vs_transfer <- glmer(cbind(adult_adoption_rto, adult_transferred_out_total_count) ~ t1+t2+t3+(1|location_id),
                                      family=binomial, data=counts_subset_by_year) |> summary()
  outcome_trend_coefs$adult_adoption_vs_transfer <- adult_adoption_vs_transfer$coefficients[2:4, c(1,4)]


  # youth live vs nonlive outcomes
  youth_live_vs_nonlive <- glmer(cbind(youth_live_outcomes, youth_other_outcomes) ~ t1+t2+t3+(1|location_id),
                                 family=binomial, data=counts_subset_by_year) |> summary()
  outcome_trend_coefs$youth_live_vs_nonlive <- youth_live_vs_nonlive$coefficients[2:4, c(1,4)]



  # youth adoption vs transfer outcomes
  youth_adoption_vs_transfer <- glmer(cbind(youth_adoption_rto, youth_transferred_out_total_count) ~ t1+t2+t3+(1|location_id),
                                      family=binomial, data=counts_subset_by_year) |> summary()
  outcome_trend_coefs$youth_adoption_vs_transfer <- youth_adoption_vs_transfer$coefficients[2:4, c(1,4)]

  return(list(intake=intake_trend_coefs, outcome=outcome_trend_coefs))

}



all_divisions <- unique(data_of_interest$Division)

all_results <- list()
for (division in all_divisions){

  all_results[[division]] <- glmm_subset(input_df = data_of_interest,
                                           division=division)

}


# ## select all the columns that are involved in the glmms
# count_subset_gov <- count_subset_gov %>% select(location_id, year_of_record, Gov, net_intakes, gross_intakes,
#                                                 relinquished_by_owner_total, stray_at_large_total,
#                                                 youth_community_total, adult_community_total,)
#

# plot total number of intakes
# ggplot(counts_subset_by_year, aes(x=year_of_record, y=gross_intakes, group=location_id)) +
#   geom_point(aes(color=Type), size=1.2, alpha=0.5) + geom_line(aes(color=Type, linetype=Gov), size=1, alpha=0.5) +
#   xlab("Year") + ylab("Total Number of Annual Intake") +
#   scale_x_continuous(breaks=seq(2021, 2024))+
#   scale_y_log10(breaks=c(seq(300, 1000, 100), seq(1000,5000, 500)))+
#   scale_color_manual(values = c("Shelter" = "#000080", "Rescue" = "#800000"), name = "Organization Type")+
#   scale_linetype_manual(values = c("Yes" = "solid", "No" = "dotdash"), name = "Government Affiliated")+
#   theme(
#     legend.position = "bottom",
#     legend.justification = "top"
#   )



# table(counts_subset$org_type)


