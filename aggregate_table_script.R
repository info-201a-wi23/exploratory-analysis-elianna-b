library("dplyr")
# get the data
tuesdata <- tidytuesdayR::tt_load('2021-01-26')
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics

aggregated <- plastics %>% group_by(country) %>% select(country, empty, hdpe, ldpe, pet, pp, ps, pvc, o, grand_total)
aggregated <- aggregated %>% rename(empty_count = empty, 
                                    high_density_polyethylene_count = hdpe, 
                                    low_density_polyethylene_count = ldpe,
                                    polyester_plastic_count = pet, 
                                    polypropylene_count = pp, 
                                    polystyrene_count = ps,
                                    PVC_plastic_count = pvc,
                                    other_count = o,
                                    total_count = grand_total)

