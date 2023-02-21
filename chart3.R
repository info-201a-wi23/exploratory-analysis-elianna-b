# Load necessary packages
library("dplyr")
library("ggplot2")

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

