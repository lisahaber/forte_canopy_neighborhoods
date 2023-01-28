###########################################################################
## Lisa T. Haber                                                         ##
## January 2023                                                          ##
## FoRTE Canopy Neighborhood Spatial Analysis Manuscript Prep            ##
## 2018-2021 LMA data analysis                                           ##
###########################################################################

## Working up LMA data for analysis

## load required packages
library(tidyverse)

## load data, 2018-2021
morph2018 <- read.csv("data/morphology_RAW/FoRTE_canopy_leaf_morphology_2018.csv", header = TRUE) %>%
  as_tibble()


morph2018 <- morph2018 %>%
  mutate(Year = 2018) %>%
  select(Filename_2018, leaf_area_cm2, leaf_mass_g, LMA_gm2, Year)

morph2019 <- read.csv("data/morphology_RAW/FoRTE_canopy_leaf_morphology_2019.csv", header = TRUE) %>%
  as_tibble()

morph2019 <- morph2019 %>%
  mutate(Year = 2019) %>%
  select(final_ID, Sample, leaf_area_cm2, leaf_mass_g, LMA_gm2, Year)

morph2020 <- read.csv("data/morphology_RAW/FoRTE_canopy_leaf_morphology_2020.csv", header = TRUE) %>%
  as_tibble()

morph2020 <- morph2020 %>%
  mutate(Year = 2020)  %>%
  select(final_ID, Sample, leaf_area_cm2, leaf_mass_g, LMA_gm2, Year)

morph2021 <- read.csv("data/morphology_RAW/FoRTE_canopy_leaf_morphology_2021.csv", header = TRUE) %>%
  as_tibble()

morph2021 <- morph2021 %>%
  mutate(Year = 2021) %>%
  select(final_ID, Sample, leaf_area_cm2, leaf_mass_g, LMA_gm2, Year)

## add columns for Species, Subplot to years where these variables are missing; clean and tidy the data for binding
ID_lookup <- read.csv("data/canopy_tree_ID_lookup.csv") %>%
  as_tibble()

#2018
morph2018 <- morph2018 %>%
  left_join(ID_lookup, by = "Filename_2018") %>%
  select(final_ID, Sample, Subplot, Species, girdled, LMA_gm2, Year, Filename_2018) 

#2019
morph2019 <- morph2019 %>%
  left_join(ID_lookup, by = c("final_ID", "Sample")) %>%
  select(final_ID, Sample, Subplot, Species, girdled, LMA_gm2, Year, Filename_2018)

#2020
morph2020 <- morph2020 %>%
  left_join(ID_lookup, by = c("final_ID", "Sample")) %>%
  select(final_ID, Sample, Subplot, Species, girdled, LMA_gm2, Year, Filename_2018) 

#2021
morph2021 <- morph2021 %>%
  left_join(ID_lookup, by = c("final_ID", "Sample")) %>%
  select(final_ID, Sample, Subplot, Species, girdled, LMA_gm2, Year, Filename_2018)

allmorph <- rbind(morph2018, morph2019, morph2020, morph2021) %>%
  as_tibble() %>%
  na.omit()

## visualize overall samples
# note, this is not very helpful; need to get to subplot scale first as experimental unit
library(ggplot2)
# Basic density
p1 <- ggplot(allmorph, aes(x=LMA_gm2)) + 
  geom_density()
p1

p2 <- ggplot(allmorph, aes(x=LMA_gm2)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm,
                args = list(mean = mean(allmorph$LMA_gm2),
                            sd = sd(allmorph$LMA_gm2)),
                col = "#1b98e0",
                size = 3)
p2

## find NA values in dataset
which(is.na(allmorph$LMA_gm2))

range(allmorph$LMA_gm2) 

length(allmorph$LMA_gm2[allmorph$LMA_gm2 > 150])
# 1 samples with LMA over 150

# allmorph_cleaned <- allmorph %>%
#   filter(LMA_gm2 <= 150)

p3 <- ggplot(allmorph, aes(x=LMA_gm2)) + 
  geom_density()
p3

counts <- allmorph %>% group_by(Year, Subplot) %>% summarize(n = n())
tibble::view(counts)

# write.csv(allmorph, "data/cleaned_data_for_analyses/lma_canopy_all_years.csv", row.names=FALSE)
