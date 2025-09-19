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
# library('moranfast')
library("raster")
library('remef')
library(tictoc)
# library('parallel')
# library('doParallel')
# library('foreach')

source("code/functions/modelFrameFunction.R")
source('code/functions/sizeClassFrameFunction_GlobalBigPatches2010_YZ.R')
source('code/functions/cleanFragFrameFunction_GlobalBigPatches2010_YZ.R')
source('code/functions/fragLM_GlobalBigPatches2010_YZ.R')

# frag1 <- read_csv('data/BigPatch_VOD_SIF_2010.csv', col_types = cols(.default = 'n'))
# frag <- read_csv('data/forest_milliSubGlobal_2020.csv', col_types = cols(.default = 'n'))

## 2020 1% global patches 
# frag <- read_csv('data/forest_percentSubGlobalSelect_2020.csv', col_types = cols(.default = 'n'))

## 2010 1% global patches 
frag <- read_csv('data/BigPatchesAllSEE_2010.csv', col_types = cols(.default = 'n'))
# frag$NPP_fractionOfValidPixels <- 1
# forestID_PercentSub_2010 <- read_csv("D:/Global_Functional_Connectivity/Patch/2020/forestID_PercentSub_2010.csv", col_types = cols(.default = 'n'))


# frag <- readRDS("D:/Global_Functional_Connectivity/Patch/2020/patchIDs/df_Globe_2020_Centrality.rds")

# WWF_Biome <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Code/Code_for_Github/Data/WWF_Biome.tif")
# 
# frag$Centroid_Lon <- frag$longitude
# frag$Centroid_Lat <- frag$latitude
# 
# frag$COVARIATE_MODE_Resolve_Biome <- raster::extract(WWF_Biome, frag[,c("Centroid_Lon","Centroid_Lat")])
# 
# frag <- left_join(frag, forestID_PercentSub_2010[,c("ForestID", "SpatialMean_modisNppRed")], by="ForestID")
# frag$NPP <- frag$SpatialMean_modisNppRed


# write.csv(frag, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/forest_percentSubGlobal_2010.csv", row.names=F)

# frag <- frag%>%dplyr::select(ForestPatchSizeInM2, ForestContinent, Centroid_Lat, Centroid_Lon, NPP, NPP_fractionOfValidPixels,
#                              COVARIATE_MODE_Resolve_Biome) 

# table(frag$ForestContinent)
# 
# 
# frag$ForestContinent[frag$ForestContinent==6&frag$Centroid_Lat>=-10] <- 5
# 
# write.csv(frag, file='data/forest_percentSubGlobal_2020.csv', row.names = F)
# write.csv(frag, file='data/forest_percentSubGlobalSelect_2020.csv', row.names = F)

## continents: 1-NA, 2-SA, 3-EU, 4-AF, 5-AS, 6-OC

# ggplot(frag, aes(x=Centroid_Lon, y=Centroid_Lat, color=as.factor(ForestContinent)))+
#   geom_point()+
#   scale_color_manual(values=rainbow(7))



#get rid of any duplicated rows
# frag <- frag %>% distinct()

#the biome modes need to be rounded to ensure that they are integers
frag$COVARIATE_MODE_Resolve_Biome <- round(frag$COVARIATE_MODE_Resolve_Biome)
frag$ForestContinent <- round(frag$ForestContinent)

# AA <- frag[frag$VOD_fractionOfValidPixels>=0.2,]
BB <- frag[!is.na(frag$VOD),]

hist(frag$VOD_fractionOfValidPixels, breaks=100)
hist(frag$NPP_fractionOfValidPixels, breaks=100)
hist(frag$GPP_fractionOfValidPixels, breaks=100)

# frag$GPP <- frag$NPP
# frag <- cleanFragFrame(frag)
frag_VOD <- frag
frag_VOD <- frag_VOD[!is.na(frag_VOD$VOD),]

frag_VOD <- frag_VOD[frag_VOD$VOD_fractionOfValidPixels>0,]
hist(frag_VOD$VOD_fractionOfValidPixels)

# table(frag_VOD$ForestContinent)

hist(log10(frag$Area_sumInSqM), breaks=20)

frag <- frag[!is.na(frag$NPP)&(frag$NPP>0)&(frag$NPP_fractionOfValidPixels>0.5),]
range(frag$Area_sumInSqM)

# frag <- frag[frag$Area_sumInSqM>=81000000,]

frag$VOD[is.na(frag$VOD)] <- 0

sum(is.na(frag$GPP))

table(frag$ForestContinent)
table(frag_VOD$ForestContinent)
# ggplot(frag, aes(x = Centroid_Lon, y = Centroid_Lat)) +
#   geom_point(aes(color = ForestContinent), size = 3) +
#   scale_color_gradientn(colors = rainbow(length(unique(frag$ForestContinent)))) +
#   coord_fixed() +
#   theme_minimal() +
#   labs(
#     title = "Continents as Numeric Values on a Map",
#     x = "Longitude",
#     y = "Latitude",
#     color = "Continent (numeric)"
#   )


# hist(frag$NPP_pixelArea_sumInSqM/frag$Area_sumInSqM, breaks=10)
#figure out which biomes have at least 100000 patches
biomeNumbers <- c(0, (frag %>% group_by(COVARIATE_MODE_Resolve_Biome) %>%
                        summarize(n = n()) %>% filter(n >= 50))$COVARIATE_MODE_Resolve_Biome)
for(i in 1:6){
  for(j in 1:length(biomeNumbers)){
    tic()
    fragLM(frag, biomeNo=biomeNumbers[j], ContinentNo=i, '2010BigPatchesSEE')
    toc()
  }
}

for(i in 1:5){
  for(j in 1:length(biomeNumbers)){
    tic()
    fragLM(frag_VOD, biomeNo=biomeNumbers[j], ContinentNo=i, '2010BigPatchesSEE_VOD')
    toc()
  }
}