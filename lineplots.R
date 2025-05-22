
library(ggplot2)
library(dplyr)



data_of_interest <- read.table("data/subset_complete_2124.tsv",
                               sep='\t', header=T)

data_by_years <- data_of_interest %>% select(-Gov) %>%
  group_by(location_id, year_of_record, Division, location_state_us) %>% summarise(across(everything(), sum))
data_by_years$year_of_record <- data_by_years$year_of_record - 2000

# total intakes
total_intake_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=gross_intakes, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_log10(breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Total Intake Number")

# proportion of community intakes among all intakes
data_by_years$Community_Intake_Proportion <- data_by_years$net_intakes / data_by_years$gross_intakes
community_intake_proportion_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=Community_Intake_Proportion, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Community Intake among All Intakes")


# proportion of owner surrendered dogs among all the community intake
data_by_years$surrender_proportion<- data_by_years$owner_total /
  (data_by_years$owner_total + data_by_years$stray_at_large_total)
surrender_proportion_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=surrender_proportion, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Owner Surrender among Community Intakes")



# youth proportion of community intake
data_by_years$youth_intake_proportion_community <- data_by_years$youth_community_total/
  (data_by_years$youth_community_total + data_by_years$adult_community_total)
youth_intake_proportion_community_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=youth_intake_proportion_community, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Youth Dogs among Community Intakes")



# youth proportion of transfer
data_by_years$youth_intake_proportion_transferin <- data_by_years$youth_transferred_in_total_count/
  (data_by_years$youth_transferred_in_total_count + data_by_years$adult_transferred_in_total_count)
youth_intake_proportion_transferin_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=youth_intake_proportion_transferin , group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Youth Dogs among Transfer-in")


# gross outcomes
total_outcomes_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=gross_outcomes, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_log10(breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Total Outcome Number")



# proportion of live outcomes
data_by_years$live_outcome_proportion <- data_by_years$live_outcomes /
  (data_by_years$live_outcomes + data_by_years$other_outcomes)
live_outcome_proportion_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=live_outcome_proportion, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Live Outcomes")



# proportion of adoption among live outcomes
data_by_years$adoption_proportion <- data_by_years$adoption_rto /
  (data_by_years$adoption_rto + data_by_years$transferred_out_total)

adoption_proportion_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=adoption_proportion, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Direct Adoptions among Live Outcomes")



# proportion of live outcomes among adults
data_by_years$adult_live_outcome_proportion <- data_by_years$adult_live_outcomes /
  (data_by_years$adult_live_outcomes + data_by_years$adult_other_outcomes)
adult_live_outcome_proportion_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=adult_live_outcome_proportion, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Live Outcomes among Adults")



# proportion of direct adoptions among adults
data_by_years$adult_adoption_proportion <- data_by_years$adult_adoption_rto /
  (data_by_years$adult_adoption_rto + data_by_years$adult_transferred_out_total_count)
adult_adoption_proportion_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=adult_adoption_proportion, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Direct Adoptions among Adult Live Outcomes")


# proportion of live outcomes among youth
data_by_years$youth_live_outcome_proportion <- data_by_years$youth_live_outcomes /
  (data_by_years$youth_live_outcomes + data_by_years$youth_other_outcomes)
youth_live_outcome_proportion_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=youth_live_outcome_proportion, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Live Outcomes among Youth")



# proportion of adoption among youth live outcomes
data_by_years$youth_adoption_proportion <- data_by_years$youth_adoption_rto /
  (data_by_years$youth_adoption_rto + data_by_years$youth_transferred_out_total_count)
youth_adoption_proportion_lineplot <- ggplot(data_by_years, aes(x=year_of_record, y=youth_adoption_proportion, group=location_id)) +
  geom_point(size=1.2, alpha=0.6) +
  geom_line(linewidth=1, alpha=0.6) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_x_continuous(breaks=c(21, 22, 23, 24))+
  facet_wrap(vars(Division), nrow=2)+
  xlab("Year") + ylab("Proportion of Direct Adoptions among Youth Live Outcomes")





