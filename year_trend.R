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



col_metadata <- c("location_id", "Division", "Gov", "location_state_us")
shelter_metadata <- data_of_interest[, col_metadata] |> unique()
View(shelter_metadata %>% group_by(Division, location_state_us) %>% summarise(count=n()))



library(lme4)


model_summary <- function(glmm){

  confint_mat <- confint(glmm, method="Wald")[6:8, ]
  colnames(confint_mat) <- c("lower", "upper")
  significance <- summary(glmm)
  significance_mat <- significance$coefficients[2:4, ]
  colnames(significance_mat) <- c("Estimate", "SE", "Z", "Pval")
  output_mat <- cbind(significance_mat, confint_mat)
  return(output_mat)

}


glmm_subset <- function(input_df, division){

  # select a subset of data
  counts_subset <- input_df %>% filter(Division == division)


  # sum all the 12 months by years
  counts_subset_by_year <- counts_subset %>% select(-c(Gov, location_state_us)) %>% group_by(location_id, year_of_record, Division) %>%
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
  total_intake <- glmer(gross_intakes ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                        family=poisson, data=counts_subset_by_year)
  intake_trend_coefs$total_intake <- model_summary(total_intake)


  # community intake vs transfer
  community_vs_transfer <- glmer(cbind(net_intakes, transferred_in_total) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                                 family=binomial, data=counts_subset_by_year)
  intake_trend_coefs$community_vs_transferin <- model_summary(community_vs_transfer)



  # surrender vs stray among community intakes
  surrender_vs_stray <- glmer(cbind(owner_total, stray_at_large_total) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                              family=binomial, data=counts_subset_by_year)
  intake_trend_coefs$surrender_vs_stray <- model_summary(surrender_vs_stray)




  # youth vs adult in community intakes
  youth_vs_adult_community <- glmer(cbind(youth_community_total, adult_community_total) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                                    family=binomial, data=counts_subset_by_year)
  intake_trend_coefs$youth_vs_adult_community <- model_summary(youth_vs_adult_community)



  # youth vs adult in transfer-ins
  youth_vs_adult_transferin <- glmer(cbind(youth_transferred_in_total_count, adult_transferred_in_total_count) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                                     family=binomial, data=counts_subset_by_year)
  intake_trend_coefs$youth_vs_adult_transferin <- model_summary(youth_vs_adult_transferin)


  ## outcome trends
  outcome_trend_coefs <- list()

  # total number of outcomes
  total_outcome <- glmer(gross_outcomes ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                         family=poisson, data=counts_subset_by_year)
  outcome_trend_coefs$total_outcome <- model_summary(total_outcome)


  # total live vs nonlive outcomes
  live_vs_nonlive <- glmer(cbind(live_outcomes, other_outcomes) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                           family=binomial, data=counts_subset_by_year)
  outcome_trend_coefs$live_vs_nonlive <- model_summary(live_vs_nonlive)



  # total adoption vs transfer out

  adoption_vs_transfer <- glmer(cbind(adoption_rto, transferred_out_total) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                                family=binomial, data=counts_subset_by_year)
  outcome_trend_coefs$adoption_vs_transfer <- model_summary(adoption_vs_transfer)


  ## Next step is to break down to adult and youth


  # adult live vs nonlive outcomes
  adult_live_vs_nonlive <- glmer(cbind(adult_live_outcomes, adult_other_outcomes) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                                 family=binomial, data=counts_subset_by_year)
  outcome_trend_coefs$adult_live_vs_nonlive <- model_summary(adult_live_vs_nonlive)



  # adult adoption vs transfer
  adult_adoption_vs_transfer <- glmer(cbind(adult_adoption_rto, adult_transferred_out_total_count) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                                      family=binomial, data=counts_subset_by_year)
  outcome_trend_coefs$adult_adoption_vs_transfer <- model_summary(adult_adoption_vs_transfer)


  # youth live vs nonlive outcomes
  youth_live_vs_nonlive <- glmer(cbind(youth_live_outcomes, youth_other_outcomes) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                                 family=binomial, data=counts_subset_by_year)
  outcome_trend_coefs$youth_live_vs_nonlive <- model_summary(youth_live_vs_nonlive)



  # youth adoption vs transfer outcomes
  youth_adoption_vs_transfer <- glmer(cbind(youth_adoption_rto, youth_transferred_out_total_count) ~ t1+t2+t3+(1+t1+t2+t3||location_id),
                                      family=binomial, data=counts_subset_by_year)
  outcome_trend_coefs$youth_adoption_vs_transfer <- model_summary(youth_adoption_vs_transfer)

  return(list(intake=intake_trend_coefs, outcome=outcome_trend_coefs))

}



all_divisions <- unique(data_of_interest$Division)

intake_criteria <- c("total_intake", "community_vs_transferin", "surrender_vs_stray",
                     "youth_vs_adult_community", "youth_vs_adult_transferin")

outcome_criteria <- c("total_outcome", "live_vs_nonlive", "adoption_vs_transfer",
                      "adult_live_vs_nonlive", "adult_adoption_vs_transfer",
                      "youth_live_vs_nonlive", "youth_adoption_vs_transfer")


criteria <- c(intake_criteria, outcome_criteria)

all_results <- list()
for (division in all_divisions){

  all_results[[division]] <- glmm_subset(input_df = data_of_interest,
                                           division=division)

}




intake_estimate_list <- list()
intake_pval_list <- list()
intake_lower_bound_list <- list()
intake_upper_bound_list <- list()

for (crt in intake_criteria){

  estimate_mat <- matrix(0, nrow=length(all_divisions), ncol=3)
  pval_mat <- matrix(0, nrow=length(all_divisions), ncol=3)
  lower_bound_mat <- matrix(0, nrow=length(all_divisions), ncol=3)
  upper_bound_mat <- matrix(0, nrow=length(all_divisions), ncol=3)

  rownames(estimate_mat) <- rownames(pval_mat) <- rownames(lower_bound_mat) <- rownames(upper_bound_mat) <- all_divisions
  colnames(estimate_mat) <- colnames(pval_mat) <- colnames(lower_bound_mat) <- colnames(upper_bound_mat) <- c("21-22", "22-23", "23-24")

  for(j in 1:length(all_divisions)){

    division <- all_divisions[j]
    result <- all_results[[division]][["intake"]][[crt]]
    estimate_mat[j, ] <- result[, "Estimate"]
    pval_mat[j, ] <- result[, "Pval"]
    lower_bound_mat[j, ] <- result[, "lower"]
    upper_bound_mat[j, ] <- result[, "upper"]

  }

  intake_estimate_list[[crt]] <- estimate_mat
  intake_pval_list[[crt]] <- pval_mat
  intake_lower_bound_list[[crt]] <- lower_bound_mat
  intake_upper_bound_list[[crt]] <- upper_bound_mat

}


outcome_estimate_list <- list()
outcome_pval_list <- list()
outcome_lower_bound_list <- list()
outcome_upper_bound_list <- list()

for (crt in outcome_criteria){

  estimate_mat <- matrix(0, nrow=length(all_divisions), ncol=3)
  pval_mat <- matrix(0, nrow=length(all_divisions), ncol=3)
  lower_bound_mat <- matrix(0, nrow=length(all_divisions), ncol=3)
  upper_bound_mat <- matrix(0, nrow=length(all_divisions), ncol=3)

  rownames(estimate_mat) <- rownames(pval_mat) <- rownames(lower_bound_mat) <- rownames(upper_bound_mat) <- all_divisions
  colnames(estimate_mat) <- colnames(pval_mat) <- colnames(lower_bound_mat) <- colnames(upper_bound_mat) <- c("21-22", "22-23", "23-24")

  for(j in 1:length(all_divisions)){

    division <- all_divisions[j]
    result <- all_results[[division]][["outcome"]][[crt]]
    estimate_mat[j, ] <- result[, "Estimate"]
    pval_mat[j, ] <- result[, "Pval"]
    lower_bound_mat[j, ] <- result[, "lower"]
    upper_bound_mat[j, ] <- result[, "upper"]

  }

  outcome_estimate_list[[crt]] <- estimate_mat
  outcome_pval_list[[crt]] <- pval_mat
  outcome_lower_bound_list[[crt]] <- lower_bound_mat
  outcome_upper_bound_list[[crt]] <- upper_bound_mat

}




# adjust p values
for (j in 1:length(intake_pval_list)){
  for (k in 1:3){
    intake_pval_list[[j]][, k] <- p.adjust(intake_pval_list[[j]][, k], method="BH")
  }
}

for (j in 1:length(outcome_pval_list)){
  for (k in 1:3){
    outcome_pval_list[[j]][, k] <- p.adjust(outcome_pval_list[[j]][, k], method="BH")
  }
}


saveRDS(list(estimate=intake_estimate_list,
             pval=intake_pval_list,
             lower_bound=intake_lower_bound_list,
             upper_bound=intake_upper_bound_list),
         file="data/intake_result.rds")


saveRDS(list(estimate=outcome_estimate_list,
             pval=outcome_pval_list,
             lower_bound=outcome_lower_bound_list,
             upper_bound=outcome_upper_bound_list),
        file="data/outcome_result.rds")



# visualize significance and examples

library(circlize)
library(ComplexHeatmap)

source("visualization.R")

data_by_years <- data_of_interest %>% select(-Gov) %>%
  group_by(location_id, year_of_record, Division, location_state_us) %>%
  summarise_all(sum)
data_by_years$year_of_record <- data_by_years$year_of_record - 2000



col_fun <- colorRamp2(c(1, 0, -1), c("#A00000", "white", "#0000A0"))

# total intake
total_intake_directions <- 2*((intake_estimate_list$total_intake > 0) - 0.5) *
  (intake_pval_list$total_intake < 0.05)
total_intake_heatmap <- save_heatmap(X=total_intake_directions,
             entry_name="direction",
             title_name="Total Intake Number",
             cmap=col_fun,
             border_col="black",
             legend=F)



# community intake proportion
community_vs_transferin_directions <- 2*((intake_estimate_list$community_vs_transferin > 0) - 0.5) *
  (intake_pval_list$community_vs_transferin < 0.05)
community_vs_transferin_heatmap <- save_heatmap(X=community_vs_transferin_directions,
                                                entry_name="direction",
                                                title_name="Community Intake vs. Transfer-in Ratio",
                                                cmap=col_fun,
                                                border_col="black",
                                                legend=F)


# surrender proportion
surrender_vs_stray_directions <- 2*((intake_estimate_list$surrender_vs_stray > 0) - 0.5) *
  (intake_pval_list$surrender_vs_stray < 0.05)
surrender_vs_stray_heatmap <- save_heatmap(X=surrender_vs_stray_directions,
                                                entry_name="direction",
                                                title_name="Surrender vs. Stray Ratio",
                                                cmap=col_fun,
                                                border_col="black",
                                                legend=F)


# youth proportion in community
youth_vs_adult_community <- 2*((intake_estimate_list$youth_vs_adult_community > 0) - 0.5) *
  (intake_pval_list$youth_vs_adult_community < 0.05)

youth_vs_adult_community_heatmap <- save_heatmap(X=youth_vs_adult_community,
                                           entry_name="direction",
                                           title_name="Youth vs. Adult among Community Intakes",
                                           cmap=col_fun,
                                           border_col="black",
                                           legend=F)



# youth proportion in transferin animals
youth_vs_adult_transferin <- 2*((intake_estimate_list$youth_vs_adult_transferin > 0) - 0.5) *
  (intake_pval_list$youth_vs_adult_transferin < 0.05)
youth_vs_adult_transferin_heatmap <- save_heatmap(X=youth_vs_adult_transferin,
                                                 entry_name="direction",
                                                 title_name="Youth vs. Adult among Transfer-ins",
                                                 cmap=col_fun,
                                                 border_col="black",
                                                 legend=F)



# total outcome
total_outcome_directions <- 2*((outcome_estimate_list$total_outcome > 0) - 0.5) *
  (outcome_pval_list$total_outcome < 0.05)
total_outcome_heatmap <- save_heatmap(X=total_outcome_directions,
                                                  entry_name="direction",
                                                  title_name="Total Outcome Number",
                                                  cmap=col_fun,
                                                  border_col="black",
                                                  legend=F)

# live vs nonlive outcomes
live_vs_nonlive_directions <- 2*((outcome_estimate_list$live_vs_nonlive > 0) - 0.5) *
  (outcome_pval_list$live_vs_nonlive < 0.05)
live_vs_nonlive_heatmap <- save_heatmap(X=live_vs_nonlive_directions,
                                      entry_name="direction",
                                      title_name="Live vs. Nonlive Outcomes Ratio",
                                      cmap=col_fun,
                                      border_col="black",
                                      legend=F)


# adoption vs transfer-outs
adoption_vs_transfer_directions <- 2*((outcome_estimate_list$adoption_vs_transfer > 0) - 0.5) *
  (outcome_pval_list$adoption_vs_transfer < 0.05)
adoption_vs_transfer_heatmap <- save_heatmap(X=adoption_vs_transfer_directions,
                                        entry_name="direction",
                                        title_name="Adoptions vs. Transfer-out",
                                        cmap=col_fun,
                                        border_col="black",
                                        legend=F)


# adult live vs nonlive
adult_live_vs_nonlive_directions <- 2*((outcome_estimate_list$adult_live_vs_nonlive > 0) - 0.5) *
  (outcome_pval_list$adult_live_vs_nonlive < 0.05)
adult_live_vs_nonlive_heatmap <- save_heatmap(X=adult_live_vs_nonlive_directions,
                                        entry_name="direction",
                                        title_name="Adult Live vs. Nonlive Outcomes Ratio",
                                        cmap=col_fun,
                                        border_col="black",
                                        legend=F)


# adult adoption vs transfer
adult_adoption_vs_transfer_directions <- 2*((outcome_estimate_list$adult_adoption_vs_transfer > 0) - 0.5) *
  (outcome_pval_list$adult_adoption_vs_transfer < 0.05)
adult_adoption_vs_transfer_heatmap <- save_heatmap(X=adult_adoption_vs_transfer_directions,
                                              entry_name="direction",
                                              title_name="Adult Adoptions vs. Transfer-out",
                                              cmap=col_fun,
                                              border_col="black",
                                              legend=F)


# youth live vs nonlive
youth_live_vs_nonlive_directions <- 2*((outcome_estimate_list$youth_live_vs_nonlive > 0) - 0.5) *
  (outcome_pval_list$youth_live_vs_nonlive < 0.05)
youth_live_vs_nonlive_heatmap <- save_heatmap(X=youth_live_vs_nonlive_directions,
                                              entry_name="direction",
                                              title_name="Youth Live vs. Nonlive Outcomes Ratio",
                                              cmap=col_fun,
                                              border_col="black",
                                              legend=F)

# youth adoption vs transfer-out
youth_adoption_vs_transfer_directions <- 2*((outcome_estimate_list$youth_adoption_vs_transfer > 0) - 0.5) *
  (outcome_pval_list$youth_adoption_vs_transfer < 0.05)
youth_adoption_vs_transfer_heatmap <- save_heatmap(X=youth_adoption_vs_transfer_directions,
                                                   entry_name="direction",
                                                   title_name="Youth Adoptions vs. Transfer-out",
                                                   cmap=col_fun,
                                                   border_col="black",
                                                   legend=F)


