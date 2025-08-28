library(dplyr)

all_collar <- read.csv("data/collar/cleaned-carnivore-data-2025-08-28.csv")
awd_collar <- all_collar %>% dplyr::filter(SpeciesCollar == "Wild Dog")
