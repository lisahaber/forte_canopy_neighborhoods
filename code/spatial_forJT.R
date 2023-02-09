######################################
## Spatial Tree Study Sample Data
## For JT
## Lisa Haber 2/6/23
####################################

## load dependencies
library(tidyverse)
library(sf)
library(data.table)
library(sp)
library(rgdal)
library(rgeos)
library(mapview)

## load data -- this is the complete stem map data set for the D rep in FoRTE
all_stems <- read.csv("C:/github/forte_canopy_neighborhoods/data/cleaned_data_for_analyses/trees_spatial.csv")

## make new data frame just for ungirdled (i.e. live) focal trees (70 of an original 72 that were mapped, with 2 accidentally girdled in 2019)
focal_trees <- all_stems %>%
  filter(focal == "Y" & fate == "live")

## graph all stems, plot focal trees in black on top of other stems
all_map <- ggplot(data = all_stems, aes(x = Longitude, y = Latitude, size = (dbh), color = genus, shape = fate)) +
  geom_point(alpha = 1) +
  scale_colour_manual(values=cbbPalette, limits = levels(all_stems$genus))+
  scale_shape_manual(values=c(1, 19))+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  ggtitle("All stems")+
  facet_wrap(~PlotID) +
  theme_classic() +
  geom_point(data = focal_trees, size=1, color = "black") 
all_map

## Now, here's where it gets fun. Take the data frame with all the stems and turn it into a spatial data set
## by assigning a coordinate reference system (CRS). The stem map data contain lat/long but no projected CRS, 
## so we have to assign one in order to do spatial operations like buffering and subsetting in space.

## Let me back up for a second and explain what I'm trying to do here.
## The ultimate outcome I want is to create 70 individual tree "neighborhoods", each of which contains a focal tree "i"
## in the center of the neighborhood, plus all stems >= 8cm DBH (i.e. mapped stems) within 8m radius around the tree "i". 
## I want to use some spatial Bayesian techniques to model the influence of neighborhood attributes on focal tree growth.
## So the final data set I need for each neighborhood would contain focal tree i's outcome variables (physiological measurements + annual growth),
## plus neighborhood attributes including the stems in that neighborhood and their associated features (ID, species, size, distance from focal tree i, 
## live or dead, etc.)

## I want to first visualize the stem neighborhoods by mapping focal trees plus their neighborhoods. Many of these will overlap, so for
## visualization purposes, having circles of 8m radius drawn around the focal trees and superimposed on the complete stem map 
## (i.e. everything depicted in "all_map") would be ideal. 

## Second, I want to run some kind of spatial join operation where I can use the buffer geometries to subset just the stems
## that fall in each focal tree's neighborhood. So I will have a data set with focal tree plus each of its neighborhood trees.


########################################### Ok, back to the code I am trying:

## make the spatial points file using all coordinate data from the "all_stems" data set

crs <- CRS("+init=epsg:32616") ## this is EPSG identifer for WGS 84/UTM zone 16N. UTM should project with distance unit of meters, not angular units.
# crs <- CRS("+init=epsg:3589") # using NAD 83 Michigan North EPSG 3589 (# alternative projection I also tried)
tree_points <- st_as_sf(all_stems,
                        coords = c("Longitude", "Latitude"),
                        crs = crs) 

plot(tree_points) ## hmmm...not sure why ALL the attributes are plotting. 

#### Buffering Attempt 1: use sf::st_buffer
# first, create a layer that just contains focal trees
focal_tree_points <- focal_trees %>%
  select(Tag, Latitude, Longitude)

focal_tree_points <- st_as_sf(focal_tree_points,
                              coords = c("Longitude", "Latitude"),
                              crs = crs)

focal_buff <- st_buffer(focal_tree_points, 8, byid = TRUE)
mapview(focal_buff) ## this makes one big circle that is weirdly located in the Galapagos Islands...


#### Buffering Attempt 2: use rgeos::gBuffer
coords <- data.frame(x = focal_trees$Longitude, y = focal_trees$Latitude)
focal_spdf <- SpatialPointsDataFrame(coords = coords, data=focal_trees, proj4string = CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs"))
settbuff <- gBuffer(focal_spdf, width=8, byid=TRUE)
mapview(settbuff) ## back in the Galapagos with this one; still just one big circle. WTF

