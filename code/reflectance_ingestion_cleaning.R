###########################################################################
## Lisa T. Haber                                                         ##
## January 2023                                                          ##
## FoRTE Canopy Neighborhoods Spatial Analysis Manuscript Prep           ##
## 2018-2021 reflectance data integration & cleaning                     ##
###########################################################################

## This code will extract and utilize subcanopy leaf spectrometry (CI-910) files from the shared FoRTE data drive.
## There are four separate data folders for the 2018-2021 field seasons' LICOR data on Google Drive. 

## load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)

##################################################
### 2018:
## DOWNLOADED
##################################################
### 1. Bring in data
# # Direct Google Drive link to "FoRTE/data/fd_leaf_physiology/canopy_leaf_CID_reflectance/2018"
# as_id("https://drive.google.com/drive/u/0/folders/1SFy1wiYvw5mfq2HG-8rWFjynll6I5ej9") %>%
#   drive_ls ->
#   gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/CID_2018_RAW"
# if(!dir.exists(data_dir)) dir.create(data_dir)

# # Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

# Get a (fresh) list of the downloaded data we're working with
## Calculations and Indices from CID
require(data.table)
# Filenames we want end with "_Calculations.csv"
# this way imports the filenames
files <- list.files(path = data_dir, pattern = "*_Calculations.csv", full.names = TRUE)
#custom read
read_data <- function(z){
  data <- fread(z, skip = 32)
}
#read the files from the list
l <- lapply( files, read_data)

#names the list using the basename from `l`
# this also is the step to manipulate the filenames to whatever you like
names(l) <- basename( files )
#bind the rows from the list together, putting the filenames into the column "id"
dt <- rbindlist( l, idcol = "filename" )
z <- dt

#get rid of infinite values, since these are screwing things up
z <- do.call(data.frame,lapply(z, function(x) replace(x, is.infinite(x),NA)))

# Combine data into a single data frame for analysis
z %>%
  bind_rows %>%
  as_tibble %>%
  separate(filename, into = c("Subplot", "Species", "Leaf_Sample", "Filename_date", "CID_Gibberish"),
           remove = FALSE) %>%
  data.frame() ->   indices2018

# filter out garbage from filename and weird CID file format
indices2018 %>%
  select(-filename, -Layer, -CID_Gibberish) %>%
  data.frame() -> indices2018

# replace all non-alphanumeric characters
indices2018$Value <- gsub("[^0-9.-]", NA, indices2018$Value)


#preparing N-associated indices data set
indices2018$Value <- as.numeric(indices2018$Value)

NIndices2018 <- indices2018 %>%
  filter(Calculation == "NDVI" | Calculation == "PRI" | Calculation == "RENDVI" | Calculation == "NPCI") %>%
  mutate(Date = mdy(Filename_date)) ## note that files where date was left out in naming used the CID gibberish for the Filename_date column data generation...need a different identifier column for join with leaf ID lookup table, leave out date

## count number of observations by subplot
obs_count <- NIndices2018 %>%
  group_by(Subplot) %>%
  summarize( n = n())

tibble::view(obs_count) ## SEE METADATA about 2018 CID files. D04W data for ACRU & QURU may be impacted.

# clean subplot names that contain "O" instead of "0" (All of them for canopy CID 2018)
NIndices2018$Subplot[NIndices2018$Subplot == "DO1E"] <- "D01E"
NIndices2018$Subplot[NIndices2018$Subplot == "DO1W"] <- "D01W"
NIndices2018$Subplot[NIndices2018$Subplot == "DO2E"] <- "D02E"
NIndices2018$Subplot[NIndices2018$Subplot == "DO2W"] <- "D02W"
NIndices2018$Subplot[NIndices2018$Subplot == "DO3E"] <- "D03E"
NIndices2018$Subplot[NIndices2018$Subplot == "DO3W"] <- "D03W"
NIndices2018$Subplot[NIndices2018$Subplot == "DO4E"] <- "D04E"
NIndices2018$Subplot[NIndices2018$Subplot == "DO4W"] <- "D04W"


tibble::view(NIndices2018)

## use lookup to get final_ID variable and species
ID_lookup <- read.csv("data/canopy_tree_ID_lookup.csv") %>%
  as_tibble() 

## need to convert CID names from upper case to lower case to match with lookup.
NIndices2018$Species <- tolower(NIndices2018$Species)
NIndices2018$Leaf_Sample <- tolower(NIndices2018$Leaf_Sample) 

## now, make a lookup column called Filename_2018, rejoining the elements of the original CID filename to match lookup.
NIndices2018 <- NIndices2018 %>%
  mutate(Filename_2018 = paste0(Subplot,'_',Species,'_',Leaf_Sample,'_',Filename_date))

tibble::view(NIndices2018)

d2018 <- left_join(NIndices2018, ID_lookup, by = "Filename_2018")

d2018 <- d2018 %>%
  select(Subplot.y, Species.y, Calculation, Value, Date, Filename_2018, final_ID, Sample, girdled) %>%
  rename(Subplot = Subplot.y,
         Species = Species.y) %>%
  mutate(Year = 2018)

tibble::view(d2018)


##################################################
### 2019:
## DOWNLOADED
##################################################
### 1. Bring in data
# # Direct Google Drive link to "FoRTE/data/subcanopy_leaf_CID_reflectance/2019"
# as_id("https://drive.google.com/drive/u/0/folders/1fXFgqE1kQV_k3NeJwS8ZNmI6vThNCJHf") %>% 
#   drive_ls ->
#   gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/CID_2019_RAW"
# if(!dir.exists(data_dir)) dir.create(data_dir)

# # Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

## Calculations and Indices from CID, 2019
require(data.table)
# Filenames we want end with "_Calculations.csv"
# this way imports the filenames
files <- list.files(path = data_dir, pattern = "*_Calculations.csv", full.names = TRUE)
#custom read
read_data <- function(z){
  data <- fread(z, skip = 32)
}
#read the files from the list
l <- lapply( files, read_data)

#names the list using the basename from `l`
# this also is the step to manipulate the filesnames to whatever you like
names(l) <- basename( files )
#bind the rows from the list together, putting the filenames into the column "id"
dt <- rbindlist( l, idcol = "filename" )
z <- dt

#get rid of infinite values, since these are screwing things up
z <- do.call(data.frame,lapply(z, function(x) replace(x, is.infinite(x),NA)))

# Combine data into a single data frame for analysis
z %>%
  bind_rows %>%
  as_tibble %>%
  separate(filename, into = c("final_ID", "Sample", "Filename_date", "CID_Gibberish"),
           remove = FALSE) %>%
  data.frame() ->   indices2019

# filter
indices2019 %>%
  select(-filename, -Layer, -CID_Gibberish) %>%
  data.frame() -> indices2019

# replace all non-alphanumeric characters
indices2019$Value <- gsub("[^0-9.-]", NA, indices2019$Value)


#preparing NDVI data set
indices2019$Value <- as.numeric(indices2019$Value)

NIndices2019 <- indices2019 %>%
  filter(Calculation == "NDVI" | Calculation == "PRI" | Calculation == "RENDVI" | Calculation == "NPCI") %>%
  mutate(Date = ymd(Filename_date)) %>%
  as_tibble()

## count number of observations by subplot
obs_count <- NIndices2019 %>%
  summarize( n = n())

tibble::view(obs_count) 

## adding additional info from lookup table
NIndices2019$final_ID <- as.character(NIndices2019$final_ID)
NIndices2019$Sample <- as.numeric(NIndices2019$Sample)

d2019 <- left_join(NIndices2019, ID_lookup, by = c("final_ID", "Sample"), keep = TRUE)

d2019 <- d2019 %>%
  select(Subplot, Species, Calculation, Value, Date, Filename_2018, final_ID.y, Sample.y, girdled) %>%
  rename(final_ID = final_ID.y,
         Sample = Sample.y) %>%
  mutate(Year = 2019)

tibble::view(d2019)

##################################################
### 2020:
## DOWNLOADED
##################################################
### 1. Bring in data
# # Direct Google Drive link to "FoRTE/data/subcanopy_leaf_CID_reflectance/2020"
# as_id("https://drive.google.com/drive/u/0/folders/1UDCPV1Rj_BbyYmmEUrK9HuhioXjjySCi") %>% 
#   drive_ls ->
#   gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/CID_2020_RAW"
# if(!dir.exists(data_dir)) dir.create(data_dir)

# # Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

## ## Calculations and Indices from CID, 2019
require(data.table)
# Filenames we want end with "_Calculations.csv"
# this way imports the filenames
files <- list.files(path = data_dir, pattern = "*_Calculations.csv", full.names = TRUE)
#custom read
read_data <- function(z){
  data <- fread(z, skip = 32)
}
#read the files from the list
l <- lapply( files, read_data)

#names the list using the basename from `l`
# this also is the step to manipulate the filesnames to whatever you like
names(l) <- basename( files )
#bind the rows from the list together, putting the filenames into the column "id"
dt <- rbindlist( l, idcol = "filename" )
z <- dt

#get rid of infinite values, since these are screwing things up
z <- do.call(data.frame,lapply(z, function(x) replace(x, is.infinite(x),NA)))

# Combine data into a single data frame for analysis
z %>%
  bind_rows %>%
  as_tibble %>%
  separate(filename, into = c("final_ID", "Sample", "Filename_date", "CID_Gibberish"),
           remove = FALSE) %>%
  data.frame() ->   indices2020

# filter
indices2020 %>%
  select(-filename, -Layer, -CID_Gibberish) %>%
  data.frame() -> indices2020

# replace all non-alphanumeric characters
indices2020$Value <- gsub("[^0-9.-]", NA, indices2020$Value)

NIndices2020 <- indices2020 %>%
  filter(Calculation == "NDVI" | Calculation == "PRI" | Calculation == "RENDVI" | Calculation == "NPCI") %>%
  mutate(Date = ymd(Filename_date)) %>%
  as_tibble()

## count number of observations by subplot
obs_count <- NIndices2020 %>%
  summarize( n = n())

tibble::view(obs_count) 

## adding additional info from lookup table
NIndices2020$final_ID <- as.character(NIndices2020$final_ID)
NIndices2020$Sample <- as.numeric(NIndices2020$Sample)

d2020 <- left_join(NIndices2020, ID_lookup, by = c("final_ID", "Sample"), keep = TRUE)

d2020 <- d2020 %>%
  select(Subplot, Species, Calculation, Value, Date, Filename_2018, final_ID.y, Sample.y, girdled) %>%
  rename(final_ID = final_ID.y,
         Sample = Sample.y) %>%
  mutate(Year = 2020)

tibble::view(d2020)

#################################################
### 2021:
## DOWNLOADED
##################################################
### 1. Bring in data
# # Direct Google Drive link to "FoRTE/data/subcanopy_leaf_CID_reflectance/2021"
# as_id("https://drive.google.com/drive/u/0/folders/1sarztVnC2wIsYoR_9bkgubH8C2_uUGoz") %>% 
#   drive_ls ->
#   gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/CID_2021_RAW"
# if(!dir.exists(data_dir)) dir.create(data_dir)

# # Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

## ## Calculations and Indices from CID, 2019
require(data.table)
# Filenames we want end with "_Calculations.csv"
# this way imports the filenames
files <- list.files(path = data_dir, pattern = "*_Calculations.csv", full.names = TRUE)
#custom read
read_data <- function(z){
  data <- fread(z, skip = 32)
}
#read the files from the list
l <- lapply( files, read_data)

#names the list using the basename from `l`
# this also is the step to manipulate the filesnames to whatever you like
names(l) <- basename( files )
#bind the rows from the list together, putting the filenames into the column "id"
dt <- rbindlist( l, idcol = "filename" )
z <- dt

#get rid of infinite values, since these are screwing things up
z <- do.call(data.frame,lapply(z, function(x) replace(x, is.infinite(x),NA)))

# Combine data into a single data frame for analysis
z %>%
  bind_rows %>%
  as_tibble %>%
  separate(filename, into = c("final_ID", "Sample", "Filename_date", "CID_Gibberish"),
           remove = FALSE) %>%
  data.frame() ->   indices2021

# filter
indices2021 %>%
  select(-filename, -Layer, -CID_Gibberish) %>%
  data.frame() -> indices2021

# replace all non-alphanumeric characters
indices2021$Value <- gsub("[^0-9.-]", NA, indices2021$Value)

NIndices2021 <- indices2021 %>%
  filter(Calculation == "NDVI" | Calculation == "PRI" | Calculation == "RENDVI" | Calculation == "NPCI") %>%
  mutate(Date = ymd(Filename_date)) %>%
  as_tibble()

## count number of observations by subplot
obs_count <- NIndices2021 %>%
  summarize( n = n())

tibble::view(obs_count) 

## adding additional info from lookup table
NIndices2021$final_ID <- as.character(NIndices2021$final_ID)
NIndices2021$Sample <- as.numeric(NIndices2021$Sample)

d2021 <- left_join(NIndices2021, ID_lookup, by = c("final_ID", "Sample"), keep = TRUE)

d2021 <- d2021 %>%
  select(Subplot, Species, Calculation, Value, Date, Filename_2018, final_ID.y, Sample.y, girdled) %>%
  rename(final_ID = final_ID.y,
         Sample = Sample.y) %>%
  mutate(Year = 2021)

tibble::view(d2021)

###############################################################
## now join all data into a single data frame for export
## go ahead and exclude the girdled trees from 2019, 2020, 2021

Nindices_all <- rbind(d2018, d2019, d2020, d2021) %>%
  filter(girdled == "N")

counts <- Nindices_all %>% group_by(Year, Subplot, final_ID) %>% summarize(n = n())
tibble::view(counts)

print(which(counts$n > 12))

# write.csv(Nindices_all, "data/cleaned_data_for_analyses/Reflectance_indices_canopy_all_years.csv", row.names=FALSE)
