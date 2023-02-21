library("dplyr")
# Get the original data from Tidy Tuesday
install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2021-01-26')
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics

# Summarize information
summary_info <- list()
summary_info$num_of_countries <- n_distinct(plastics$country)
summary_info$max_total_count <- plastics %>% summarize(value = max(grand_total, na.rm = TRUE)) %>% pull(value)
# high density polyethylene is the material of plastic milk containers, plastic bags, bottle caps, trash cans, oil cans, plastic lumber, toolboxes, supplement containers)
summary_info$max_high_density_polyethylene_count <- plastics %>% summarize(value = max(hdpe, na.rm = TRUE)) %>% pull(value)
# polyester plastic is the material of polyester fibers, soft drink bottles, food containers and some plastic bottles. 
summary_info$max_polyester_plastic_count <- plastics %>% summarize(value = max(pet, na.rm = TRUE)) %>% pull(value)
summary_info$max_num_volunteer <- plastics %>% summarize(value = max(volunteers, na.rm = TRUE)) %>% pull(value)

#Chart 1
# Load necessary packages
library("dplyr")
library("ggplot2")
install.packages("scales")
library("scales")
# Get the original data from Tidy Tuesday
tuesdata <- tidytuesdayR::tt_load("2021-01-26")

# Extract the plastics data
plastics <- tuesdata$plastics

# Group all parent_companies and summarize the plastic emitted per company.
top_20_companies <- plastics %>%
  group_by(parent_company) %>%
  summarize(company_total_plastics = sum(grand_total, na.rm = TRUE)) %>%
  arrange(desc(company_total_plastics)) %>% # Order companies by their total plastic emissions in descending order
  filter(!parent_company %in% c("null", "NULL")) %>% # Remove rows where parent_company is "null" or "NULL"
  filter(row_number() <= 20) # Keep only the top 20 companies

# Create the stacked bar chart with the fill variable ordered by descending company_total_plastics
ggplot(data = top_20_companies) +
  geom_col(mapping = aes(x = reorder(parent_company, +company_total_plastics), y = company_total_plastics, fill = parent_company)) +
  coord_flip() +
  labs(title = "Top 20 Plastic-Emitting Companies", x = "Parent Company", y = "Total Plastic Emitted (million metric tons)") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = label_number_si())

#Chart 2
# Load necessary packages
library("dplyr")
library("ggplot2")
library("scales")
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

#Chart 3
# Load necessary packages
library("dplyr")
library("ggplot2")
library("scales")
# Get the original data from Tidy Tuesday
tuesdata <- tidytuesdayR::tt_load("2021-01-26")

# Extract the plastics data
plastics <- tuesdata$plastics

# Group all countries and sum the # of HDPE and PET Plastics emitted
hdpe_and_pet_plastics <- plastics %>%
  group_by(country) %>%
  summarize(num_hdpe = sum(hdpe, na.rm = TRUE), num_pet = sum(pet, na.rm = TRUE))

# Create a line graph of HDPE and PET emissions to show their relationship and dangerous of the amount emitted
ggplot(hdpe_and_pet_plastics, aes(x = num_hdpe, y = num_pet)) +
  geom_line() +
  labs(title = "Number of HDPE and PET Plastic Emissions", x = "High Density Polyethylene Count", y = "Polyester Plastic Count") +
  scale_x_continuous(breaks = seq(0, max(hdpe_and_pet_plastics$num_hdpe), by = 1000)) +
  scale_y_continuous(breaks = seq(0, max(hdpe_and_pet_plastics$num_pet), by = 10000))