rm(list=ls())

setwd("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments")

library('tidyverse')
library('cmocean')
library('cowplot')
library('car')
library('lme4')
library('MuMIn')
library('dplyr')
library('tidyr')
library('readr')
library('SpatialPack')
library(rnaturalearth)
# library('moranfast')
library('remef')
library(sf)
# library('parallel')
# library('doParallel')
# library('foreach')

source("code/functions/modelFrameFunction.R")
source('code/functions/sizeClassFrameFunction_YZ.R')
source('code/functions/cleanFragFrameFunction_VOD.R')
source('code/functions/fragLM_YZ.R')

frag <- read_csv('data/BigPatchesAllSEE_2010.csv', col_types = cols(.default = 'n'))

# bigPatch_NA1 <- read_csv('data/BigPatches_2010/Polygons_withoutEdge/patchsize_npp_bigPatch_NA1_2010_PG.csv', col_types = cols(.default = 'n'))
bigPatch_NA2 <- read_csv('data/BigPatches_2010/Strict_withoutEdge/patchsize_npp_bigPatch_NA2_2010_PG_EE.csv', col_types = cols(.default = 'n'))


WWF_Biome <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Code/Code_for_Github/Data/WWF_Biome.tif")

# colnames(frag)
# table(frag$ForestContinent)

# frag_NA <- bigPatch_NA2
frag_NA <- frag[frag$ForestContinent==1,]

plot(log10(frag_NA$Area_sumInSqM), frag_NA$VOD_fractionOfValidPixels)


frag_NA$COVARIATE_MODE_Resolve_Biome <- raster::extract(WWF_Biome, frag_NA[,c("Centroid_Lon","Centroid_Lat")])

coords <- frag_NA[,c("Centroid_Lon","Centroid_Lat")]%>%drop_na()

points_sf <- st_as_sf(coords, coords = c("Centroid_Lon", "Centroid_Lat"), crs = 4326)

world <- ne_countries(scale = "medium", returnclass = "sf")

result <- st_join(points_sf, world["admin"])

result <- result %>%
  mutate(Centroid_Lon = st_coordinates(.)[,1],
         Centroid_Lat = st_coordinates(.)[,2]) %>%
  dplyr::select(Centroid_Lon, Centroid_Lat, country = admin)


frag_NA <- left_join(frag_NA, result, by=c("Centroid_Lon", "Centroid_Lat"))

frag_US <- frag_NA[frag_NA$country.y=="United States of America",]
frag <- frag_US[!is.na(frag_US$country.y),]

frag$VOD_mean <- frag$VOD

frag$Centroid_lat <- frag$Centroid_Lat
frag$Centroid_lon <- frag$Centroid_Lon

#get rid of any duplicated rows
# frag <- frag %>% distinct()

#the biome modes need to be rounded to ensure that they are integers
frag$COVARIATE_MODE_Resolve_Biome <- round(frag$COVARIATE_MODE_Resolve_Biome)

sum(is.na(frag$VOD_mean))

frag <- frag[!is.na(frag$VOD_mean),]

# frag <- cleanFragFrame(frag)

range(frag$Area_sumInSqM)
frag <- frag[frag$Area_sumInSqM>=81000000,]
frag$NPP_perArea <- frag$NPP
frag$GPP_perArea <- frag$GPP
# hist(frag$NPP_pixelArea_sumInSqM/frag$Area_sumInSqM, breaks=10)
#figure out which biomes have at least 100000 patches
biomeNumbers <- c(0, (frag %>% group_by(COVARIATE_MODE_Resolve_Biome) %>%
                        summarize(n = n()) %>% filter(n >= 50))$COVARIATE_MODE_Resolve_Biome)

for(i in 1:length(biomeNumbers)){
  fragLM(frag, biomeNumbers[i], '1024Big_SEE')
}


