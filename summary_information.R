library("dplyr")
# Get the original data from Tidy Tuesday
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

# Summarize information
summary_info <- list()
summary_info$num_of_countries <- n_distinct(plastics$country)
summary_info$max_total_count <- plastics %>% summarize(value = max(grand_total, na.rm = TRUE)) %>% pull(value)
# high density polyethylene is the material of plastic milk containers, plastic bags, bottle caps, trash cans, oil cans, plastic lumber, toolboxes, supplement containers)
summary_info$max_high_density_polyethylene_count <- plastics %>% summarize(value = max(hdpe, na.rm = TRUE)) %>% pull(value)
# polyester plastic is the material of polyester fibers, soft drink bottles, food containers and some plastic bottles. 
summary_info$max_polyester_plastic_count <- plastics %>% summarize(value = max(pet, na.rm = TRUE)) %>% pull(value)
summary_info$max_num_volunteer <- plastics %>% summarize(value = max(volunteers, na.rm = TRUE)) %>% pull(value)
