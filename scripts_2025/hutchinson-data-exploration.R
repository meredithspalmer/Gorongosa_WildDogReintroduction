library(dplyr)
library(tidyr)
library(ggplot2)

# Explore Matt Hutchinson data

group_size <- read.csv("data/hutchinson/Raw Data/group_size_road_counts_all_years_raw.csv") 
group_size_ungulate <- group_size %>% 
    dplyr::filter(Species %in% c("Impala", "Oribi", "Warthog", "Waterbuck", "Reedbuck",
                                 "Bushbuck", "Nyala", "Kudu"))

# doesn't have information on dates of the surveys; assume same as vigilance?
unique(group_size$Year)

# sample size 
sample_size <- count(group_size, Year, Species)
sample_size_wide <- pivot_wider(sample_size, names_from = "Year", values_from = "n")

# Notes from Matt's README
# The transects are: FP1 & FP2 (Road 4; floodplain transects) and S1 & S2 (Road 3; savanna transects). 
# FP transects will likely have bigger group sizes as an artifact of greater visibility so you may want to be selective about which data you use to represent group size (though the floodplain is also a part of the ecosystem so perhaps averaging across all is a good idea).  
# 'road_count_transects.kml' has the start (S) and finish (F) points of each transect, which runs along the respective roads between those two points. Each was designed to be ~2km

ggplot(group_size_ungulate, aes(x = as.factor(Year), y = N_individuals)) +
    geom_boxplot() + 
    facet_wrap(~Species, scales = "free") +
    theme_bw()


vigilance <- read.csv("data/hutchinson/Raw Data/vigilance_per_survey_all_years.csv") %>% 
    mutate(Date = as.Date(Date, format = "%m/%d/%y"))
unique(vigilance$Date) 
# 2017: June 23 to July 12
# 2018: June 17 to July 14
# 2019: June 8 to June 20

# Notes from Matt's README
# has the proportion of heads up ('Prop_up') per survey 
# the average number of individuals visible ('Visible') across a 20-minute survey where we recorded N_head_up and N_visible every two minutes. 
# Habitat (floodplain/savanna)
# time of day ('Time' records the starting time of the survey; these were done between 0600-1000 and 1400-1800 each day)
# distance between us and the animals ('Distance' in meters, recorded with a range finder)

ggplot(vigilance, aes(x = as.factor(Year), y = Prop_up)) +
    geom_boxplot() + 
    facet_wrap(~Species, scales = "free") +
    theme_bw()
