# Load necessary packages
library("dplyr")
library("ggplot2")

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
