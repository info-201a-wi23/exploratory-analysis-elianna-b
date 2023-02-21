# Load necessary packages
library("dplyr")
library("ggplot2")

# Get the original data from Tidy Tuesday
tuesdata <- tidytuesdayR::tt_load("2021-01-26")

# Extract the plastics data
plastics <- tuesdata$plastics

# Group all countries and sum the # of events and volunteers
events_and_volunteers_per_country <- plastics %>%
  group_by(country) %>%
  summarize(num_events = sum(num_events, na.rm = TRUE), num_volunteers = sum(volunteers, na.rm = TRUE)) %>%
  filter(!country %in% c("EMPTY"))

# Create a scatter plot of events and volunteers to show specific countries as outliers
ggplot(events_and_volunteers_per_country, aes(x = num_events, y = num_volunteers)) +
  geom_point(aes(color = country)) +
  geom_text(aes(label = country), hjust = -0.2, vjust = 0.5) +
  labs(title = "Number of Events vs Number of Volunteers by Country", x = "Number of Events", y = "Number of Volunteers") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_events), by = 10000)) +
  scale_y_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_volunteers), by = 300000))

