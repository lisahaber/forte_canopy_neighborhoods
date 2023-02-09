###########################################################################
## Lisa T. Haber                                                         ##
## February 2023                                                          ##
## FoRTE Canopy Tree Spatial Neighborhood Analysis Manuscript Prep       ##
## 2018-2021 canopy tree dendroband NPP calculation                      ##
###########################################################################

## note that this code pulls directly from code written by Kerstin Niedermaier,
## former Gough Lab member and FoRTEan, which in turn built off work from
## Maxim Grigri and Jeff Atkins.

## load dependencies
library(lubridate)
library(tidyverse)
library(dplyr)

## load data
dendro_data <- read.csv("C:/github/forte_canopy_neighborhoods/data/raw_dendro.csv")
head(dendro_data)
