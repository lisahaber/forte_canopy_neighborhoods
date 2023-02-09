#################################
## Adapted from BIOS 549 Project
## Lisa Haber
## original code November 2018
#################################

## Load packages
library(dplyr)
library(R2WinBUGS)
library(RColorBrewer)
library(ggplot2)

## Import data, combine into single data frame, clean up for analysis
asat <- read.table("data/Mean_Photo_Cond_2018_ANALYSIS.txt", header=TRUE)
covariates <- read.csv("data/LMA_reflectance_forte_leaves_NO_PINES_2018.csv")

graph <- ggplot(df, aes(x = MeanPhoto, fill=Species)) + geom_histogram(binwidth = 0.5, show.legend = TRUE) + labs(x = "Light-Saturated Rate of Photosynthesis", y = "Count", fill = "Species") 
graph + theme_classic(base_size = 22) +scale_fill_brewer(palette="Spectral")

palette2 <- brewer.pal(6,"Greens")
graph + theme_classic() +scale_fill_brewer(palette="palette2")

ggplot(data=df, aes(x=NDVI, y=MeanPhoto)) + geom_point() + facet_wrap(~Species_code)

df <- merge(asat, covariates, by="Filename") %>%
  select("Filename", "MeanPhoto", "Subplot_code", "Landform", "Subplot_Lat", "Subplot_Long", "Leaf_lat_rand", "Leaf_long_rand", "Species_code", "Upper_DBH", "LMA_gm.2", "NDVI", "Species")

###############################################
## Model Version 1 (V1)
## Includes species effect as indexed uncorrelated random effect
## Covariates: size, LMA, NDVI, species
##############################################

## Create V1 data file for use in WinBUGS
df$y <- df$MeanPhoto
df$size <- df$Upper_DBH
df$LMA <- df$LMA_gm.2
df$NDVI <- df$NDVI
df$sp <- df$Species_code

getwd()
N <- nrow(df)
write("list(", "Haber_model_V1_data.txt")
write(paste0("N = ", N, ","), "Haber_model_V1_data.txt", append=TRUE)
write("y = c(", "Haber_model_V1_data.txt", append=TRUE)
write(noquote(paste0(as.vector(df$y),",")), "Haber_model_V1_data.txt", ncolumns=1, append=TRUE)
write("),", "Haber_model_V1_data.txt", append=TRUE)
write("size = c(", "Haber_model_V1_data.txt", append=TRUE)
write(noquote(paste0(as.vector(df$size),",")), "Haber_model_V1_data.txt", ncolumns=1, append=TRUE)
write("),", "Haber_model_V1_data.txt", append=TRUE)
write("LMA = c(", "Haber_model_V1_data.txt", append=TRUE)
write(noquote(paste0(as.vector(df$LMA),",")), "Haber_model_V1_data.txt", ncolumns=1, append=TRUE)
write("),", "Haber_model_V1_data.txt", append=TRUE)
write("NDVI = c(", "Haber_model_V1_data.txt", append=TRUE)
write(noquote(paste0(as.vector(df$NDVI),",")), "Haber_model_V1_data.txt", ncolumns=1, append=TRUE)
write("),", "Haber_model_V1_data.txt", append=TRUE)
write("sp = c(", "Haber_model_V1_data.txt", append=TRUE)
write(noquote(paste0(as.vector(df$sp),",")), "Haber_model_V1_data.txt", ncolumns=1, append=TRUE)
write(")", "Haber_model_V1_data.txt", append=TRUE)
write(")", "Haber_model_v1_data.txt", append=TRUE)

# Create an inits file for V1 model, 2 chains
write("list(", "Haber_model_V1_inits.txt")
write("list(", "Haber_model_V1_inits_2chains.txt", append=TRUE)
write(paste0("a0 = 0", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("tau = 1", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("b1 = 0", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("b2 = 0", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("b3 = 0", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("sdb1 = 1", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("sdb2 = 1", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("sdb3 = 1", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("sdusp = 1", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("usp = 0, 0, 0, 0, 0, 0"), "Haber_model_V1_inits.txt", append=TRUE)
write(")", "Haber_model_V1_inits.txt", append=TRUE)
write("list(", "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("a0 = 0.5", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("tau = 1.5", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("b1 = 0", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("b2 = 0", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("b3 = 0", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("sdb1 = 1", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("sdb2 = 1", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("sdb3 = 1", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("sdusp = 1", ","), "Haber_model_V1_inits.txt", append=TRUE)
write(paste0("usp = 0, 0, 0, 0, 0, 0"), "Haber_model_V1_inits.txt", append=TRUE)
write(")", "Haber_model_V1_inits.txt", append=TRUE)
write(")", "Haber_model_V1_inits.txt", append=TRUE)

# Try running V1 model, 2 chains, in R2WinBUGS;
# need to control burn-in period
N <- nrow(df)
d <- list(N=N, y=df$y, size=df$size, sp=df$Species_code,
          LMA=df$LMA, NDVI=df$NDVI)

setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
v1modelfile <- paste(getwd(), "/Haber_project_model_V1.txt", sep="")
v1inits <- paste(getwd(), "/Haber_model_V1_inits.txt", sep="")
wdir <- paste(getwd(), "/V1", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
BugsDir <- "C:/WinBUGS14"

# have to save the output as an object, as below, because WinBUGS will close as soon as it is done evaluating
MCMCresultsV1 <- bugs(data = d, inits = list(list(
  a0 = 0,
  tau = 1,
  b1 = 0,
  b2 = 0,
  b3 = 0,
  sdb1 = 1,
  sdb2 = 1,
  sdb3 = 1,
  sdusp = 1,
  usp = c(0, 0, 0, 0, 0, 0)
),
list(
  a0 = 0.5,
  tau = 1.5,
  b1 = 0,
  b2 = 0,
  b3 = 0,
  sdb1 = 1,
  sdb2 = 1,
  sdb3 = 1,
  sdusp = 1,
  usp = c(0, 0, 0, 0, 0, 0)
)
),
                working.directory = wdir,
                parameters.to.save = c("a0", "tau", "b1", "b2", "b3", "sdb1",
                                       "sdb2", "sdb3", "sdusp", "usp"),
                n.chains = 2, n.iter = 200000, n.burnin = 100000, n.thin = 10,
                model.file = v1modelfile,
                bugs.directory = BugsDir, DIC = TRUE)

MCMCresultsV1

meanuV1<- MCMCresultsV1$mean$usp

v1gelmandiag <- gelman.plot(MCMCresultsV1)

###############################################
## Model Version 2 (V2)
## Includes species effect AND subplot effect as indexed uncorrelated random effects, i.e. contextual effects
## Covariates: size, LMA, NDVI, species, subplot
##############################################

# V2 model, 2 chains, in R2WinBUGS;
N <- nrow(df)
df$sub <- df$Subplot_code
d2 <- list(N=N, y=df$y, size=df$size, LMA=df$LMA, NDVI=df$NDVI, sp=df$sp, sub=df$sub)

setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
v2modelfile <- paste(getwd(), "/Haber_project_model_V2.txt", sep="")
wdir <- paste(getwd(), "/V2", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
BugsDir <- "C:/WinBUGS14"

# Run in WinBUGS
MCMCresultsV2 <- bugs(data = d2, inits = list(list(a0 = 0, tau = 1, b1 = 0, b2 = 0, b3 = 0, sdb1 = 1, sdb2 = 1, sdb3 = 1, sdusp = 1, sdusub = 1, usp = rep(0, 6), usub = rep(0, 32)), list(a0 = 0.5, tau = 1.5, b1 = 0, b2 = 0, b3 = 0, sdb1 = 1, sdb2 = 1, sdb3 = 1, sdusp = 1, sdusub = 1, usp = rep(0, 6), usub = rep(0, 32))),
                      working.directory = wdir,
                      parameters.to.save = c("a0", "tau", "b1", "b2", "b3", "sdb1", 
                                             "sdb2","sdb3", "sdusp", "sdusub", "usp", "usub"),
                      n.chains = 2, n.iter = 200000, n.burnin = 100000, n.thin = 10,
                      model.file = v2modelfile,
                      bugs.directory = BugsDir, DIC = TRUE)

MCMCresultsV2

###############################################
## Model Version 3 (V3)
## Includes landform type (subplot cluster) effect as indexed uncorrelated random effect
## Covariates: size, LMA, NDVI, species, landform type
##############################################

# V3 model, 2 chains, in R2WinBUGS;
N <- nrow(df)
df$land <- df$Landform
d3 <- list(N=N, y=df$y, size=df$size, LMA=df$LMA, NDVI=df$NDVI, sp=df$sp, land=df$land)

setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
v3modelfile <- paste(getwd(), "/Haber_project_model_V3.txt", sep="")
wdir <- paste(getwd(), "/V3", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
BugsDir <- "C:/WinBUGS14"

MCMCresultsV3 <- bugs(data = d3, inits = list(list(a0 = 0, tau = 1, b1 = 0, b2 = 0, b3 = 0, sdb1 = 1, sdb2 = 1, sdb3 = 1, sdusp = 1, sduland = 1, usp = rep(0, 6), uland = rep(0, 4)), list(a0 = 0.5, tau = 1.5, b1 = 0, b2 = 0, b3 = 0, sdb1 = 1, sdb2 = 1, sdb3 = 1, sdusp = 1, sduland = 1, usp = rep(0, 6), uland = rep(0, 4))),
                                       working.directory = wdir,
                                       parameters.to.save = c("a0", "tau", "b1", "b2", "b3", "sdb1", 
                                                              "sdb2","sdb3", "sdusp", "sduland", "usp", "uland"),
                                       n.chains = 2, n.iter = 200000, n.burnin = 100000, n.thin = 10,
                                       model.file = v3modelfile,
                                       bugs.directory = BugsDir, DIC = TRUE)

MCMCresultsV3

v3gelmandiag <- gelman.plot(MCMCresultsV3)

###############################################
## Model Version 4 (V4)
## Includes subplot and species effects as indexed uncorrelated random effect, excludes size
## Covariates: LMA, NDVI, subplot, species
##############################################

# V4 model, 2 chains, in R2WinBUGS;
N <- nrow(df)
df$sub <- df$Subplot_code
d4 <- list(N=N, y=df$y, LMA=df$LMA, NDVI=df$NDVI, sp=df$Species_code, sub=df$sub)

setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
v4modelfile <- paste(getwd(), "/Haber_project_model_V4.txt", sep="")
wdir <- paste(getwd(), "/V4", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
BugsDir <- "C:/WinBUGS14"

# Run in WinBUGS
MCMCresultsV4 <- bugs(data = d4, inits = list(list(a0 = 0, tau = 1, b2 = 0, b3 = 0, sdb2 = 1, sdb3 = 1, sdusp = 1, sdusub = 1, usp = rep(0, 6), usub = rep(0, 32)), list(a0 = 0.5, tau = 1.5, b2 = 0, b3 = 0, sdb2 = 1, sdb3 = 1, sdusp = 1, sdusub = 1, usp = rep(0, 6), usub = rep(0, 32))),
                      working.directory = wdir,
                      parameters.to.save = c("a0", "tau", "b2", "b3", 
                                             "sdb2","sdb3", "sdusp", "sdusub", "usp", "usub"),
                      n.chains = 2, n.iter = 200000, n.burnin = 100000, n.thin = 10,
                      model.file = v4modelfile,
                      bugs.directory = BugsDir, DIC = TRUE)

MCMCresultsV4

###############################################
## Model Version 5 (V5)
## Includes subplot effects as indexed uncorrelated random effect, excludes size and species
## Covariates: LMA, NDVI, subplot
##############################################

# V5 model, 2 chains, in R2WinBUGS;
N <- nrow(df)
df$sub <- df$Subplot_code
d5 <- list(N=N, y=df$y, LMA=df$LMA, NDVI=df$NDVI, sub=df$sub)

setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
v5modelfile <- paste(getwd(), "/Haber_project_model_V5.txt", sep="")
wdir <- paste(getwd(), "/V5", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
BugsDir <- "C:/WinBUGS14"

# Run in WinBUGS
MCMCresultsV5 <- bugs(data = d5, inits = list(list(a0 = 0, tau = 1, b2 = 0, b3 = 0, sdb2 = 1, sdb3 = 1, sdusub = 1, usub = rep(0, 32)), list(a0 = 0.5, tau = 1.5, b2 = 0, b3 = 0, sdb2 = 1, sdb3 = 1, sdusub = 1, usub = rep(0, 32))),
                      working.directory = wdir,
                      parameters.to.save = c("a0", "tau", "b2", "b3", 
                                             "sdb2","sdb3", "sdusub", "usub"),
                      n.chains = 2, n.iter = 200000, n.burnin = 100000, n.thin = 10,
                      model.file = v5modelfile,
                      bugs.directory = BugsDir, DIC = TRUE)

MCMCresultsV5

###############################################
## Model Version 6 (V6)
## Includes jointly specified random effects for subplots and uncorrelated random effects from species, excludes size
## Covariates: LMA, NDVI, species, subplot (as vi)
##############################################

# For subplot location data, calculate distance range, and
# explore prior correlation for MVN spatial random effects

# Spatial coordinates
# xc=df$Leaf_long_rand
# yc=df$Leaf_lat_rand
# summary(xc)
# summary(yc)
# 
# plot(xc, yc, xlab = "Longitude", ylab = "Latitude")
# 
# # Calculate distances between points
# library(fields)
# cords <- cbind(xc, yc)
# summary(cords)
# plot(cords)
# 
# D <- rdist(cords)
# d <- as.vector(D)
# summary(d)  # d is in decimal degrees; 0.01 dd = 787.1 meters @ 45 degrees N latitude (according to Wikipedia)
# d <- d*10000
# summary(d)
# mn <- min(d[d>0])        # Min distance = 0.09477939 meters; scaled to 0.01204
# mx <- max(d)             # Max distance = 2055.111 meters; scaled to 261.0991
# 
# # Check correlation at min and max distances
# 
# phi.l <- 0.03
# phi.u <- 104
# exp(-mn*phi.l)
# exp(-mn*phi.u)
# # prior range for correlation at min distance is 0.9999759 to 0
# exp(-mx*phi.l)
# exp(-mx*phi.u)
# # prior range for correlation at max distance is 0 to 0.4568
# 
# #########################
# # model run
# # V6 model, 1 chain to start/test, in R2WinBUGS;
# N <- nrow(df)
# d6 <- list(N=N, y=df$y, LMA=df$LMA, NDVI=df$NDVI, sp=df$Species_code, xc=xc, yc=yc)
# 
# setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
# v6modelfile <- paste(getwd(), "/Haber_project_model_V6_fixedphi.txt", sep="")
# wdir <- paste(getwd(), "/V6", sep="")
# if(!file.exists(wdir)){dir.create(wdir)}
# BugsDir <- "C:/WinBUGS14"
# 
# # Run in WinBUGS
# MCMCresultsV6 <- bugs(data = d6, inits = list(list(a0 = 0, tau = 1, b2 = 0, b3 = 0, sdb2 = 1, sdb3 = 1, sdusp = 1, usp = rep(0, 6), tauv=1, v=rep(0, N))), 
#                       working.directory = wdir, parameters.to.save = c("a0", "tau", "b2", "b3", "sdb2","sdb3", "sdusp", "usp", "tauv", "v"), 
#                       n.chains = 1, n.iter = 20000, n.burnin = 10000, n.thin = 10,
#                       model.file = v6modelfile, bugs.directory = BugsDir, DIC = TRUE, debug = TRUE)
# 
# MCMCresultsV6
# 

###############################################
## Model Version 7 (V7)
## Includes subplot, landform, AND species effects as indexed uncorrelated random effect, excludes size
## Covariates: LMA, NDVI, subplot, landform, species
##############################################

# V7 model, 2 chains, in R2WinBUGS;
N <- nrow(df)
df$sub <- df$Subplot_code
df$land <- df$Landform
d7 <- list(N=N, y=df$y, LMA=df$LMA, NDVI=df$NDVI, sp=df$Species_code, sub=df$sub, land=df$land)

setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
v7modelfile <- paste(getwd(), "/Haber_project_model_V7.txt", sep="")
wdir <- paste(getwd(), "/V7", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
BugsDir <- "C:/WinBUGS14"

# Run in WinBUGS
MCMCresultsV7 <- bugs(data = d7, inits = list(list(a0 = 0, tau = 1, b2 = 0, b3 = 0, sdb2 = 1, sdb3 = 1, sdusp = 1, sdusub = 1, sduland = 1, usp = rep(0, 6), usub = rep(0, 32), uland = rep(0, 4)), list(a0 = 0.5, tau = 1.5, b2 = 0, b3 = 0, sdb2 = 1.5, sdb3 = 1.5, sdusp = 1.5, sdusub = 1.5, sduland = 1.5, usp = rep(0, 6), usub = rep(0, 32), uland = rep(0, 4))),
                      working.directory = wdir,
                      parameters.to.save = c("a0", "tau", "b2", "b3", 
                                             "sdb2","sdb3", "sdusp", "sdusub", "sduland", "usp", "usub", "uland"),
                      n.chains = 2, n.iter = 200000, n.burnin = 100000, n.thin = 10,
                      model.file = v7modelfile,
                      bugs.directory = BugsDir, DIC = TRUE)

MCMCresultsV7

###############################################################################
## Model V8
## CAR spatial effects; run BYM model for spatial and non-spatial effects
#############################################################################
# Spatial coordinates
xc=df$Leaf_long_rand
yc=df$Leaf_lat_rand
summary(xc)
summary(yc)

## make voronoi polygons
library(dismo)
dat8 <- data.frame(y=df$y, xc=xc, yc=yc)
coordinates(dat8) <- c("xc", "yc")
vor <- voronoi(dat8)
class(vor)
summary(vor)
plot(vor)
points(dat8, col="red")

# Create neighborhoods
library(spdep)
vor.nb <- poly2nb(vor) 

# Convert nb object to WinBUGS format for ICAR prior
vor.WB <- nb2WB(vor.nb)

#class of vor.WB?
class(vor.WB)

# Make list of data inputs and initial values for MCMC
d8 <- list(N=N, y=vor$y, LMA=df$LMA, NDVI=df$NDVI, sp=df$Species_code,
          adj=vor.WB$adj, weights=vor.WB$weights, num=vor.WB$num)

setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
v8modelfile <- paste(getwd(), "/Haber_project_model_V8.txt", sep="")
wdir <- paste(getwd(), "/V8", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
BugsDir <- "C:/WinBUGS14"

# Run in WinBUGS
MCMCresultsV8 <- bugs(data = d8, inits = list(list(a0 = 0, tau = 1, b2 = 0, b3 = 0, sdb2 = 1, sdb3 = 1, sdusp = 1, sdv = 1, usp = rep(0, 6), v = rep(0, N)), list(a0 = 0.5, tau = 1.5, b2 = 0, b3 = 0, sdb2 = 1.5, sdb3 = 1.5, sdusp = 1.5, sdv = 1.5, usp = rep(0, 6), v = rep(0, N))),
                      working.directory = wdir,
                      parameters.to.save = c("a0", "tau", "b2", "b3", 
                                             "sdb2","sdb3", "sdusp", "sdv", "usp", "v"),
                      n.chains = 2, n.iter = 200000, n.burnin = 100000, n.thin = 10,
                      model.file = v8modelfile,
                      bugs.directory = BugsDir, DIC = TRUE)

MCMCresultsV8

v8gelmandiag <- gelman.plot(MCMCresultsV8)

vor$v <- MCMCresultsV8$mean$v

Aplotsvor <-vor[1:95,]
Bplotsvor <- vor[96:191,]
Cplotsvor <- vor[192:276,]
Dplotsvor <- vor[277:339,]

library(RColorBrewer)
cols <- c(rev(brewer.pal(3, "Blues")), brewer.pal(3, "Reds"))
spplot(vor, c("v"), cuts=5, col.regions=cols, axes=TRUE, sp.layout = list("sp.points", dat8, pch=16, cex=1, col="black"))

Aplots <- spplot(Aplotsvor, c("v"), cuts=5, col.regions=cols, axes=TRUE)
Bplots <- spplot(Bplotsvor, c("v"), cuts=5, col.regions=cols, axes=TRUE)
Cplots <- spplot(Cplotsvor, c("v"), cuts=5, col.regions=cols, axes=TRUE)
Dplots <- spplot(Dplotsvor, c("v"), cuts=5, col.regions=cols, axes=TRUE)

Aplots
Bplots
Cplots
Dplots

df$v <- MCMCresultsV8$mean$v

library(rgdal)
subplots.xy <- cbind(xc, yc)
crs1 <- CRS("+proj=longlat +datum=WGS84")
dat.sp <- SpatialPoints(subplots.xy, proj4string = crs1)


library(rgeos)

dat.sp <- gBuffer(dat.sp, byid=TRUE, width = 18, quadsegs = 10)
plot(dat.sp)  

# library(raster)
# x <- crop(polydf2, polydf1)




###############################################################################
## Model V9
## CAR spatial effects; run BYM model for spatial and non-spatial effects
## include nonspatial random effects for subplots in addition to the spatial
#############################################################################

# Make list of data inputs and initial values for MCMC
d9 <- list(N=N, y=vor$y, LMA=df$LMA, NDVI=df$NDVI, sp=df$Species_code, 
           sub=df$Subplot_code,
           adj=vor.WB$adj, weights=vor.WB$weights, num=vor.WB$num)

setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
v9modelfile <- paste(getwd(), "/Haber_project_model_V9.txt", sep="")
wdir <- paste(getwd(), "/V9", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
BugsDir <- "C:/WinBUGS14"

# Run in WinBUGS
MCMCresultsV9 <- bugs(data = d9, inits = list(list(a0 = 0, tau = 1, b2 = 0, b3 = 0, sdb2 = 1, sdb3 = 1, sdusp = 1, sdusub = 1, sdv = 1, usp = rep(0, 6), usub = rep(0, 32), v = rep(0, N)), list(a0 = 0.5, tau = 1.5, b2 = 0, b3 = 0, sdb2 = 1.5, sdb3 = 1.5, sdusp = 1.5, sdusub = 1.5, sdv = 1.5, usp = rep(0, 6), usub = rep(0, 32), v = rep(0, N))),
                      working.directory = wdir,
                      parameters.to.save = c("a0", "tau", "b2", "b3", 
                                             "sdb2","sdb3", "sdusp", "sdusub", "sdv", "usp", "usub", "v"),
                      n.chains = 2, n.iter = 200000, n.burnin = 100000, n.thin = 10,
                      model.file = v9modelfile,
                      bugs.directory = BugsDir, DIC = TRUE)

MCMCresultsV9





###############################################
## Model Version 6 (V6)
## Includes jointly specified spatial random effects for subplots and uncorrelated random effects for species; excludes size
## Covariates: LMA, NDVI, species, subplot (as vi)
##############################################

# For subplot location data, calculate distance range, and
# explore prior correlation for MVN spatial random effects

# Spatial coordinates
xc=df$Subplot_Long
yc=df$Subplot_Lat
summary(xc)
summary(yc)

plot(xc, yc, xlab = "Longitude", ylab = "Latitude")

# Calculate distances between points
library(fields)
cords <- cbind(xc, yc)
summary(cords)
plot(cords)

D <- rdist(cords)
d <- as.vector(D)
summary(d)
mn <- min(d[d>0])        # Min distance = 0.0005083004
mx <- max(d)             # Max distance = 0.02576615

# Check correlation at min and max distances

# My distance units, I believe, are in decimal degrees. I've tried
# playing around with values for phi, and I have noticed little
# variation in the correlation values printed below as a result.
phi.l <- 0.05
phi.u <- 20
exp(-mn*phi.l)
exp(-mn*phi.u)
# prior range for correlation at min distance is 0.989 to 0.999
exp(-mx*phi.l)
exp(-mx*phi.u)
# prior range for correlation at max distance is 0.597 to 0.999

#########################
# model run
# V6 model, 1 chain to start/test, in R2WinBUGS;
N <- nrow(df)
d6 <- list(N=N, y=df$y, LMA=df$LMA, NDVI=df$NDVI, sp=df$Species_code, xc=xc, yc=yc)

setwd("C:/Users/Lisa/Documents/VCU Fall 2018/BIOS 549/BIOS_549_project")
v6modelfile <- paste(getwd(), "/Haber_project_model_V6.txt", sep="")
wdir <- paste(getwd(), "/V6", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
BugsDir <- "C:/WinBUGS14"

# Run in WinBUGS
MCMCresultsV6 <- bugs(data = d6, inits = list(list(a0 = 0, tau = 1, b2 = 0, b3 = 0, sdb2 = 1, sdb3 = 1, sdusp = 1, usp = rep(0, 6), sdv=1, phi=1, kappa=1, v=rep(0, N))),
                      working.directory = wdir,
                      parameters.to.save = c("a0", "tau", "b2", "b3",
                                             "sdb2","sdb3", "sdusp", "usp", "tauv", "phi", "kappa", "v"),
                      n.chains = 1, n.iter = 10000, n.burnin = 5000, n.thin = 10,
                      model.file = v6modelfile,
                      bugs.directory = BugsDir, DIC = TRUE, debug = TRUE)

MCMCresultsV6

