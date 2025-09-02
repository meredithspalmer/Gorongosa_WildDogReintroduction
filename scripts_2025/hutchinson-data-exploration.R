library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Explore Matt Hutchinson data

group_size_2017 <- read.csv("data/hutchinson/rawData171819/herbivore_road_counts_2017.csv") %>% 
    mutate(Year = "2017", Date = as.Date(Date, format = "%m/%d/%y"),
           Species = fct_recode(Species, IM = "Impala", WB = "Waterbuck", NY = "Nyala", OR = "Oribi",
                                RB = "Reedbuck", BB = "Bushbuck", LI = "Lion", KU = "Kudu", BP = "Bushpig",
                                WH = "Warthog", SGH = "GroundHornbill", WI = "Wildebeest", ML = "Monitor"))
group_size_2018 <- read.csv("data/hutchinson/rawData171819/herbivore_road_counts_2018.csv") %>% 
    mutate(Year = "2018", Date = as.Date(Date, format = "%m/%d/%y"))
group_size_2019 <- read.csv("data/hutchinson/rawData171819/herbivore_road_counts_2019.csv") %>% 
    mutate(Year = "2019", Date = as.Date(Date, format = "%m/%d/%y"))
group_size <- bind_rows(group_size_2017, group_size_2018, group_size_2019)


# Notes from Matt's README
# The transects are: FP1 & FP2 (Road 4; floodplain transects) and S1 & S2 (Road 3; savanna transects). 
# FP transects will likely have bigger group sizes as an artifact of greater visibility so you may want to be selective about which data you use to represent group size (though the floodplain is also a part of the ecosystem so perhaps averaging across all is a good idea).  
# 'road_count_transects.kml' has the start (S) and finish (F) points of each transect, which runs along the respective roads between those two points. Each was designed to be ~2km

# Create habitat column
group_size <- group_size %>%
    mutate(Habitat = case_when(
        Transect %in% c("FP1", "FP2") ~ "floodplain",
        Transect %in% c("S1", "S2")  ~ "savanna"
    ))

# How many of each species?
count(group_size, Species, Year)
sample_size <- count(group_size, Year, Species)
pivot_wider(sample_size, names_from = "Year", values_from = "n")

# Filter to just ungulates
group_size_ungulate <- group_size %>% 
    dplyr::filter(Species %in% c("BB", "IM", "NY", "OR", "RB", "WB", "WH"))


# Basic visualization
ggplot(group_size_ungulate, aes(x = as.factor(Year), y = N_individuals)) +
    geom_boxplot() + 
    facet_wrap(~Species, scales = "free") +
    theme_bw()
ggplot(group_size_ungulate, aes(x = as.factor(Year), y = N_individuals, fill = Habitat)) +
    geom_boxplot() + 
    facet_wrap(~Species, scales = "free") +
    theme_bw()


vigilance_2017 <- read.csv("data/hutchinson/rawData171819/herbivore_vigilance_group_2017.csv") %>% 
    mutate(Year = "2017")
vigilance_2018 <- read.csv("data/hutchinson/rawData171819/herbivore_vigilance_group_2018.csv") %>% 
    mutate(Year = "2018")
vigilance_2019 <- read.csv("data/hutchinson/rawData171819/herbivore_vigilance_group_2019.csv") %>% 
    mutate(Year = "2019")

vigilance <- bind_rows(vigilance_2017, vigilance_2018, vigilance_2019) %>% 
    mutate(Date = as.Date(Date, format = "%m/%d/%y"),
           Prop_up = N_head_up / N_visible)


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


# TIME FRAME

unique(group_size$Date)
# 2017: July 13 to July 31
# 2018: June 17 to July 14
# 2019: July 13 to July 15

unique(vigilance$Date) 
# 2017: June 23 to July 12
# 2018: June 17 to July 14
# 2019: June 8 to June 20
