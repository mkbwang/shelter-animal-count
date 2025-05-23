library(dplyr)
library(ggplot2)
library(usmap)
rm(list=ls())

data_of_interest <- read.table("data/subset_complete_2124.tsv",
                               sep='\t', header=T)
state_regions <- read.csv("metadata/State_Region.csv")


# plot US map colored by divisions

# Create a data frame with state abbreviations and their corresponding division.
# The built-in vectors 'state.abb' and 'state.division' are in matching order.
df <- data.frame(
  state = state.abb,          # US state abbreviations (e.g., "TX" for Texas)
  division = state.division,  # Census division (e.g., "West South Central")
  stringsAsFactors = FALSE
)



myColors <- c(
  "New England"        = "#A6CEE3",
  "Middle Atlantic"    = "#B2DF8A",
  "East North Central" = "#FDBF6F",
  "West North Central" = "#FB9A99",
  "South Atlantic"     = "#CAB2D6",
  "East South Central" = "#FFFF99",
  "West South Central" = "#B3E2CD",
  "Mountain"           = "#BBBBBB",
  "Pacific"            = "#FBB4AE"
)

p <- plot_usmap(data = df, values = "division") +
  scale_fill_manual(values = myColors,
                    name = "Census Division") + xlab("") + ylab("")+
  theme(legend.position = "none")

print(p)


col_metadata <- c("location_id", "location_state_us", "Division")
unique_sites <- unique(data_of_interest[, col_metadata])

site_counts <- unique_sites %>% group_by(Division, location_state_us) %>%
  summarise(count=n())


intake_outcome_sum <- data_of_interest %>%
  group_by(Division, location_id) %>%
  summarise(total_intakes=sum(gross_intakes),
            total_outcomes=sum(gross_outcomes))


ggplot(intake_outcome_sum, aes(x=total_intakes, y=total_outcomes)) +
  geom_point(size=0.8, alpha=0.7)+ facet_wrap(vars(Division), nrow=3)+
  scale_y_log10(breaks=c(1e3, 2e3, 5e3, 1e4, 2e4, 5e4, 1e5), labels=c("1", "2", "5", "10", "20", "50", "100"))+
  scale_x_log10(breaks=c(1e3, 2e3, 5e3, 1e4, 2e4, 5e4, 1e5), labels=c("1", "2", "5", "10", "20", "50", "100"))+
  xlab("Total Intake Number (Thousand)") + ylab("Total Outcome Number (Thousand)")


