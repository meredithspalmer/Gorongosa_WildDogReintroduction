diet <- read.csv("data/hallie-diet/African_Wild_Dog_Diet_Data_Filtered.csv")

# Comma-delimited file contains 42 rows (wild dog fecal sampling events) and 18 columns (reporting the 
# composition of those fecal samples and when they were collected). The first column ('Date') reports time 
# to/since Cyclone Idai landfall when the fecal-sampling event occurred. The remaining columns 
# (Tragelaphus.sylvaticus to Loxodonta.africana) report the proportion of reads from each fecal-sampling 
# event attributed to each prey species.

# Exclude likely mis-IDed civet sample from analysis
diet_filter <- diet %>% filter(Civettictis.civetta < 0.95)
