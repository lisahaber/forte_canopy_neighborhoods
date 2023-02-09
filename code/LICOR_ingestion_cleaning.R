###########################################################################
## Lisa T. Haber                                                         ##
## January 2023                                                         ##
## FoRTE Canopy Tree Spatial Neighborhood Analysis Manuscript Prep      ##
## 2018-2021 data integration & cleaning                                 ##
###########################################################################

## This code will extract and utilize canopy physiology (LICOR 6400XT) files from the shared FoRTE data drive.
## There are four separate data folders for the 2018-2021 field seasons' LICOR data on Google Drive. 

## load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)

#### I. Download successive years' data into data directories (only need to do this once)

###########NOTE: 12/6/2021 ALL data drives have changed location in migration of Google Drive to a university maintained share drive. Need to updat all links before running those download scripts again.

##################################################
### 2018:
## COMPLETED
##################################################
### 1. Bring in data
# Direct Google Drive link to "FoRTE/data/subcanopy_leaf_physiology/2018"
as_id("https://drive.google.com/drive/u/0/folders/1B35H7R2fMa--zFSVtD4MCkXfhfFrggrW") %>% 
  drive_ls ->
  gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/LICOR_2018_RAW"
if(!dir.exists(data_dir)) dir.create(data_dir)

# # Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

# Get a (fresh) list of the downloaded data we're working with
# Filenames we want end with eight digits and no file extension
files <- list.files(data_dir, pattern = "[0-9]{8}$", full.names = TRUE)
HEADER_PATTERN <- "\"OPEN \\d\\.\\d\\.\\d"
DATA_PATTERN <- "\\$STARTOFDATA\\$"

# Scan through all the data files and read data into list structure
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  first_comment <- text_raw[data_start - 1] # there's always a comment on this line
  
  if(length(data_start)) {
    # What makes this tricky is that there can be additional comments WITHIN the data frame
    # Who on earth thought that was a good idea?!?
    data_raw <- text_raw[data_start+1:length(text_raw)] %>% na.omit
    line_lengths <- lapply(strsplit(data_raw, "\t"), length) %>% unlist
    data_rows <- line_lengths == line_lengths[1]
    comments <- paste(which(!data_rows), data_raw[!data_rows], sep = ". ") %>%
      paste(first_comment, ., sep = "; ") %>% 
      gsub('\"', "", .)
    
    # OK, now read the data into a data frame and add the 'comments'
    con <- textConnection(data_raw[data_rows])
    read.table(con, header = TRUE, stringsAsFactors = FALSE) %>% 
      mutate(Filename = basename(f),
             Timestamp = text_raw[grep(HEADER_PATTERN, text_raw) + 1],
             Comments = paste(comments, collapse = "; ")) ->
      filedata[[f]]
    close(con)
  }
}

# Combine data into a single data frame for analysis
filedata %>% 
  bind_rows %>% 
  as_tibble %>% 
  mutate(Timestamp = mdy_hms(Timestamp)) %>%  # change to a POSIXct object
  separate(Filename, into = c("Plot", "Species", "Sample", "Filename_date"), remove = FALSE) ->
  licordata2018

unique(licordata2018$Filename_date)

### 2: exclude excessive observations
# figure out which dates/files have more than 5 observations
excessobs <- licordata2018 %>%
  subset(Obs >= 6)

dates <- excessobs$Filename
# [1] "D01E_quru_can04_07222018" "D01E_quru_can05_07222018" "D01W_pogr_can01_07232018"
# [4] "D01W_pogr_can02_07232018" "D02E_pogr_can06_07192018" "D02E_pogr_can09_07192018"
# [7] "D02E_quru_can12_07192018" "D02W_acru_can11_07182018" "D02W_pogr_can10_07182018"
# [10] "D02W_quru_can07_07172018" "D03E_pogr_can08_07202018" "D03E_pogr_can09_07202018"
# [13] "D03E_quru_can07_07202018" "D03W_pogr_can01_07222018" "D04W_pogr_can07_07202018"
# [16] "D04W_quru_can02_07202018" "D04W_quru_can04_07202018" "D04W_quru_can05_07202018"

tibble::view(excessobs)
print(unique(dates))

# remove excess observations
# figure out which rows in dataframe need removal (easiest to just do this by hand)
# these are observations ("logged" measurements) on the LICOR that I either: accidentally made before a leaf had fully stabilized, intentionally made because the leaf had not actually stabilized in the first 5 measurements, or just from hitting the log button too many times by accident. I cross-checked with field notes from 2018. 2018 was an exceptionally dry and stressful year and many leaves did not stabilize immediately.
tibble::view(licordata2018)

# now, exclude them. Note that this means the affected files will have observations beginning at "2" or "6" or "11" or "16", not "1"
drop <- c(106:108, 114:118, 189:193, 199:208, 374, 390:399, 470:479, 540:554,
          625:629, 695, 816:820, 826:830, 866:870, 931:935, 1236:1240, 
          1261:1265, 1281:1282, 1283:1287)

licordata2018_cleaned <- licordata2018[-drop, ] #this subsets the original dataframe to exclude unwanted measurements


### 3: attach unique subcanopy tree IDs to all trees from 2018
# use lookup table

ID_lookup <- read.csv("data/canopy_tree_ID_lookup.csv") %>%
  as_tibble()

ID_lookup_2018 <- ID_lookup %>%
  rename(Filename = Filename_2018)

licordata2018_cleaned <- left_join(licordata2018_cleaned, ID_lookup_2018, by = "Filename", keep = TRUE) 
tibble::view(licordata2018_cleaned)

# drop variables we don't want retained in final data set
# this includes erroneous subplot information in column "Plot" deriving from the Licor filename itself, which was sometimes incorrectly entered in 2018

head(licordata2018_cleaned)

licordata2018_cleaned <- licordata2018_cleaned %>%
  select(-Plot, -Species.x, -Sample.x, -Filename.x) %>%
  rename(Species = Species.y) %>%
  rename(Filename = Filename.y) %>%
  rename(Sample = Sample.y) %>%
  mutate(Year = 2018)

# check to see what overall non-negative sample size is
licordata2018_means <- licordata2018_cleaned %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo)) %>%
  filter(MeanPhoto >= 0) # I will leave the negative values in the "raw" data for fortedata; my analyses will ultimately exclude them

# we exclude negative photosynthetic values because they are not usable measurements

nrow(licordata2018_means)#242 crown-level samples represented in the data set from 72 trees


# write.csv(licordata2018_cleaned, "data/cleaned_data_for_analyses/LICOR_canopy_leaf_phys_2018.csv", row.names = FALSE)

#######################################################
### 2019:
## COMPLETED
#######################################################
# # Direct Google Drive link to "FoRTE/data/subcanopy_leaf_physiology/2018"
# as_id("https://drive.google.com/drive/u/0/folders/1w2a00MNQwh9c3HO04PDYm3V5gblbBSYG") %>% 
#   drive_ls ->
#   gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/LICOR_2019_RAW"
# if(!dir.exists(data_dir)) dir.create(data_dir)

# # Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

## Get a (fresh) list of the downloaded data we're working with
# Filenames we want end with four digits and no file extension
files <- list.files(data_dir, pattern = "[0-9]{4}$", full.names = TRUE)
HEADER_PATTERN <- "\"OPEN \\d\\.\\d\\.\\d"
DATA_PATTERN <- "\\$STARTOFDATA\\$"

# Scan through all the data files and read data into list structure
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  first_comment <- text_raw[data_start - 1] # there's always a comment on this line
  
  if(length(data_start)) {
    # What makes this tricky is that there can be additional comments WITHIN the data frame
    # Who on earth thought that was a good idea?!?
    data_raw <- text_raw[data_start+1:length(text_raw)] %>% na.omit
    line_lengths <- lapply(strsplit(data_raw, "\t"), length) %>% unlist
    data_rows <- line_lengths == line_lengths[1]
    comments <- paste(which(!data_rows), data_raw[!data_rows], sep = ". ") %>%
      paste(first_comment, ., sep = "; ") %>% 
      gsub('\"', "", .)
    
    # OK, now read the data into a data frame and add the 'comments'
    con <- textConnection(data_raw[data_rows])
    read.table(con, header = TRUE, stringsAsFactors = FALSE) %>% 
      mutate(Filename = basename(f),
             Timestamp = text_raw[grep(HEADER_PATTERN, text_raw) + 1],
             Comments = paste(comments, collapse = "; ")) ->
      filedata[[f]]
    close(con)
  }
}

# Combine data into a single data frame for analysis
filedata %>% 
  bind_rows %>% 
  as_tibble %>% 
  mutate(Timestamp = mdy_hms(Timestamp)) %>%  # change to a POSIXct object
  separate(Filename, into = c("Tree_ID", "Sample", "Filename_date"), remove = FALSE) ->
  licordata2019

## code to generate the clean 2019 phys data 
excessobs <- licordata2019 %>%
  subset(Obs >= 6)

dates <- excessobs$Filename

tibble::view(excessobs)
print(unique(dates))

# Just two excess observations in 2019 data (collected by Autym Shafer & Laura Hickey with partial field notes)
# remove excess observations
# figure out which rows in dataframe need removal (easiest to just do this by hand at this point)
tibble::view(licordata2019) ## 1EA2_02_20190620 & 4WA2_03_20190702; accidentally logged a sixth time for both

# now, exclude them. 
drop <- c(46, 2042)

licordata2019_cleaned <- licordata2019[-drop, ]

tibble::view(licordata2019_cleaned)  

nrow(licordata2019_cleaned)  

# check to see what overall non-negative sample size is
licordata_2019_means <- licordata2019_cleaned %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo)) %>%
  filter(MeanPhoto >= 0)

nrow(licordata_2019_means)

# join to lookup table to get species, subplot, etc.

licordata2019_cleaned <- licordata2019_cleaned %>%
  mutate(final_ID = as.character(licordata2019_cleaned$Tree_ID))

## make 'Sample' column a numerical vector for joining tables
licordata2019_cleaned$Sample <- as.numeric(licordata2019_cleaned$Sample)

licordata2019_cleaned <- left_join(licordata2019_cleaned, ID_lookup, by = c("final_ID", "Sample"), keep = TRUE) 

# remove unwanted columns
licordata2019_cleaned <- licordata2019_cleaned %>%
  select(-Sample.y, -final_ID.y) %>%
  rename(final_ID = final_ID.x) %>%
  rename(Sample = Sample.x) %>%
  mutate(Year = 2019)

# write.csv(licordata2019_cleaned, "data/cleaned_data_for_analyses/LICOR_canopy_leaf_phys_2019.csv", row.names = FALSE)

check2018 <- read.csv("data/cleaned_data_for_analyses/LICOR_canopy_leaf_phys_2018.csv")
check2019 <- read.csv("data/cleaned_data_for_analyses/LICOR_canopy_leaf_phys_2019.csv")
check2020 <- read.csv("data/cleaned_data_for_analyses/LICOR_canopy_leaf_phys_2020.csv")
check2021 <- read.csv("data/cleaned_data_for_analyses/LICOR_canopy_leaf_phys_2021.csv")
# 
columnssame <- merge(check2018, check2019)
head(columnssame)

#######################################################
### 2020: 
### COMPLETED
#######################################################
# Direct Google Drive link to "FoRTE/data/subcanopy_leaf_physiology/2020"
as_id("https://drive.google.com/drive/u/0/folders/1wrw-yMKN18XrwzscJpsn9KgI-LXHwhtR") %>% 
  drive_ls ->
  gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/LICOR_2020_RAW"
if(!dir.exists(data_dir)) dir.create(data_dir)

# # Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

# Get a (fresh) list of the downloaded data we're working with
# Filenames we want end with four digits and no file extension
files <- list.files(data_dir, pattern = "[0-9]{8}$", full.names = TRUE)
HEADER_PATTERN <- "\"OPEN \\d\\.\\d\\.\\d"
DATA_PATTERN <- "\\$STARTOFDATA\\$"

# Scan through all the data files and read data into list structure
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  first_comment <- text_raw[data_start - 1] # there's always a comment on this line
  
  if(length(data_start)) {
    # What makes this tricky is that there can be additional comments WITHIN the data frame
    # Who on earth thought that was a good idea?!?
    data_raw <- text_raw[data_start+1:length(text_raw)] %>% na.omit
    line_lengths <- lapply(strsplit(data_raw, "\t"), length) %>% unlist
    data_rows <- line_lengths == line_lengths[1]
    comments <- paste(which(!data_rows), data_raw[!data_rows], sep = ". ") %>%
      paste(first_comment, ., sep = "; ") %>% 
      gsub('\"', "", .)
    
    # OK, now read the data into a data frame and add the 'comments'
    con <- textConnection(data_raw[data_rows])
    read.table(con, header = TRUE, stringsAsFactors = FALSE) %>% 
      mutate(Filename = basename(f),
             Timestamp = text_raw[grep(HEADER_PATTERN, text_raw) + 1],
             Comments = paste(comments, collapse = "; ")) ->
      filedata[[f]]
    close(con)
  }
}

# Combine data into a single data frame for analysis
filedata %>% 
  bind_rows %>% 
  as_tibble %>% 
  mutate(Timestamp = mdy_hms(Timestamp)) %>%  # change to a POSIXct object
  separate(Filename, into = c("Tree_ID", "Sample", "Filename_date"), remove = FALSE) ->
  licordata2020 

##############################################################
# ## code to generate the clean 2020 phys data 
excessobs <- licordata2020 %>%
  subset(Obs >= 6)

dates <- excessobs$Filename

tibble::view(excessobs)
print(unique(dates))

# remove excess observations
# figure out which rows in dataframe need removal (easiest to just do this by hand at this point)
tibble::view(licordata2020) ## 3WP2_02_20200726; accidentally logged a sixth time

# now, exclude them. 
drop <- c(746)

licordata2020_cleaned <- licordata2020[-drop, ]

tibble::view(licordata2020_cleaned)  

nrow(licordata2020_cleaned)  

# check to see what overall non-negative sample size is
licordata_2020_means <- licordata2020_cleaned %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo)) %>%
  filter(MeanPhoto >= 0)

nrow(licordata_2020_means)

# join to lookup table to get species, subplot, etc.

licordata2020_cleaned <- licordata2020_cleaned %>%
  mutate(final_ID = as.character(licordata2020_cleaned$Tree_ID))

## make 'Sample' column a numerical vector for joining tables
licordata2020_cleaned$Sample <- as.numeric(licordata2020_cleaned$Sample)

licordata2020_cleaned <- left_join(licordata2020_cleaned, ID_lookup, by = c("final_ID", "Sample"), keep = TRUE) 

# remove unwanted columns
licordata2020_cleaned <- licordata2020_cleaned %>%
  select(-Sample.y, -final_ID.y) %>%
  rename(final_ID = final_ID.x) %>%
  rename(Sample = Sample.x) %>%
  mutate(Year = 2020)
# 
# write.csv(licordata2020_cleaned, "data/cleaned_data_for_analyses/LICOR_canopy_leaf_phys_2020.csv", row.names = FALSE)


##################################################################
### 2021
## COMPLETED
##################################################################
# Direct Google Drive link to "FoRTE/data/subcanopy_leaf_physiology/2021"
as_id("https://drive.google.com/drive/u/0/folders/1abjlpSoz56sEI6jId2I1-SawG1QJce1m") %>% 
  drive_ls ->
  gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/LICOR_2021_RAW"
if(!dir.exists(data_dir)) dir.create(data_dir)

# Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

# Get a (fresh) list of the downloaded data we're working with
# Filenames we want end with four digits and no file extension
files <- list.files(data_dir, pattern = "[0-9]{8}$", full.names = TRUE)
HEADER_PATTERN <- "\"OPEN \\d\\.\\d\\.\\d"
DATA_PATTERN <- "\\$STARTOFDATA\\$"

# Scan through all the data files and read data into list structure
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  first_comment <- text_raw[data_start - 1] # there's always a comment on this line
  
  if(length(data_start)) {
    # What makes this tricky is that there can be additional comments WITHIN the data frame
    # Who on earth thought that was a good idea?!?
    data_raw <- text_raw[data_start+1:length(text_raw)] %>% na.omit
    line_lengths <- lapply(strsplit(data_raw, "\t"), length) %>% unlist
    data_rows <- line_lengths == line_lengths[1]
    comments <- paste(which(!data_rows), data_raw[!data_rows], sep = ". ") %>%
      paste(first_comment, ., sep = "; ") %>% 
      gsub('\"', "", .)
    
    # OK, now read the data into a data frame and add the 'comments'
    con <- textConnection(data_raw[data_rows])
    read.table(con, header = TRUE, stringsAsFactors = FALSE) %>% 
      mutate(Filename = basename(f),
             Timestamp = text_raw[grep(HEADER_PATTERN, text_raw) + 1],
             Comments = paste(comments, collapse = "; ")) ->
      filedata[[f]]
    close(con)
  }
}

# Combine data into a single data frame for analysis
filedata %>% 
  bind_rows %>% 
  as_tibble %>% 
  mutate(Timestamp = mdy_hms(Timestamp)) %>%  # change to a POSIXct object
  separate(Filename, into = c("Tree_ID", "Sample", "Filename_date"), remove = FALSE) ->
  licordata2021 

##############################################################
# ## code to generate the clean 2021 phys data 
excessobs <- licordata2021 %>%
  subset(Obs >= 6)

dates <- excessobs$Filename

tibble::view(excessobs)
print(unique(dates)) # 3WP1_01_20210805 

# remove excess observations
# figure out which rows in dataframe need removal (easiest to just do this by hand at this point)
tibble::view(licordata2021)

# now, exclude them. Only one file had an accidental 6th log in 2021. Cleaned according to field notebook notes from field data collection + above QAQC check
drop <- c(711)

licordata2021_cleaned <- licordata2021[-drop, ]

tibble::view(licordata2021_cleaned)  

nrow(licordata2021_cleaned)  

# check to see what overall non-negative sample size is
licordata_2021_means <- licordata2021_cleaned %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo)) %>%
  filter(MeanPhoto >= 0)

nrow(licordata_2021_means)

# join to lookup table to get species, subplot, etc.

licordata2021_cleaned <- licordata2021_cleaned %>%
  mutate(final_ID = as.character(licordata2021_cleaned$Tree_ID))

## make 'Sample' column a numerical vector for joining tables
licordata2021_cleaned$Sample <- as.numeric(licordata2021_cleaned$Sample)

licordata2021_cleaned <- left_join(licordata2021_cleaned, ID_lookup, by = c("final_ID", "Sample"), keep = TRUE) 

# remove unwanted columns
licordata2021_cleaned <- licordata2021_cleaned %>%
  select(-Sample.y, -final_ID.y) %>%
  rename(final_ID = final_ID.x) %>%
  rename(Sample = Sample.x) %>%
  mutate(Year = 2021)

# write.csv(licordata2021_cleaned, "data/cleaned_data_for_analyses/LICOR_canopy_leaf_phys_2021.csv", row.names = FALSE)

