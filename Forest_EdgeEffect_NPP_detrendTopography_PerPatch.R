rm(list=ls())

library(raster)
library(tictoc)
library(tidyverse)
library(ranger)
library(ggpubr)
library(jsonlite)
library(patchwork)

## Preprocess the data -----

setwd("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/")

fileLst <- list.files()


fileLst <- fileLst[grep(".csv",fileLst)]


df_list <- lapply(1:length(fileLst), function(x) read.csv(fileLst[x]))


df <- do.call(rbind, df_list)

colnames(df)

df_select <- df%>%dplyr::select(NPP, ForestPatchLabel, CGIAR_Aridity_Index, CHELSA_BIO_Annual_Mean_Temperature, CHELSA_BIO_Annual_Precipitation, DistanceToEdge,
                                CHELSA_BIO_Precipitation_Seasonality, CHELSA_BIO_Temperature_Seasonality, ForestPatchPerimeterAreaInM2, ForestPatchSizeInM2, ForestPatchPerimeterInM, .geo)%>%
  drop_na()

df_select$ID <- 1:nrow(df_select)

df_select$'.geo'[1]

parsed <- fromJSON(df_select$'.geo'[1])

lon <- parsed$coordinates[1]
lat <- parsed$coordinates[2]

tic()
df_select$latitude <- sapply(1:nrow(df_select), function(x) fromJSON(df_select$'.geo'[x])$coordinates[2])
toc()

tic()
df_select$longitude <- sapply(1:nrow(df_select), function(x) fromJSON(df_select$'.geo'[x])$coordinates[1])
toc()

# saveRDS(df_select, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df_2_withCoordinates.rds")



df_select_coordinates <- df_select%>%dplyr::select(ID, longitude, latitude)
write.csv(df_select_coordinates, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df_coordinates.csv", row.names = F)


df_covariates <- read.csv("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/Edge_Covariates_Merge/EdgeEffect_covariates_Merge_1.csv")

colnames(df_covariates)
df_covariates_select <- df_covariates%>%dplyr::select(ID, EarthEnvTopoMed_Elevation, EarthEnvTopoMed_Slope,
                                                      EarthEnvTopoMed_AspectSine, EarthEnvTopoMed_Eastness,
                                                      EarthEnvTopoMed_Northness, EarthEnvTopoMed_AspectCosine)


df_select <- left_join(df_select, df_covariates_select, by="ID")

# saveRDS(df_select, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df_2_withCoordinates.rds")

## Detrend NPP based on topographic information -----

df_select <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df_2_withCoordinates.rds")
df_select <- df_select %>% drop_na()

formula_NPP <- NPP ~ EarthEnvTopoMed_Elevation + EarthEnvTopoMed_Slope + EarthEnvTopoMed_AspectSine + EarthEnvTopoMed_Eastness + EarthEnvTopoMed_Northness

tic()
rf_NPP <- ranger(formula_NPP, data = df_select, importance = "permutation")
toc()

df_select$NPP_DT <- df_select$NPP - rf_NPP$predictions + mean(df_select$NPP)

# saveRDS(df_select, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df_2_withCoordinates.rds")

## Analysis based on the largest patch----


df_select <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df.rds")

AA <- table(df_select$ForestPatchLabel)

df_select_largePatch <- df_select[df_select$ForestPatchLabel==names(AA[AA == max(AA)]),] 

ggplot(df_select_largePatch, aes(x=DistanceToEdge, y=NPP/10000))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab("Distance to Edge (m)")+
  ylab(bquote('NPP ('*kg~C~ m^-2~yr^-1*')'))+
  # scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())


ggplot(df_select_largePatch, aes(x=DistanceToEdge, y=NPP_DT/10000))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab("Distance to Edge (m)")+
  ylab(bquote('Partial NPP ('*kg~C~ m^-2~yr^-1*')'))+
  # scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())



## Analysis based on the 10 largest patches----

df_select <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df.rds")

AA <- table(df_select$ForestPatchLabel)

BB <- sort(AA, decreasing = T)[1:10]

# names(BB) <- 1:10
df_select_largePatches <- NULL

for(i in 1:10){
  df_select_largePatch <- df_select[df_select$ForestPatchLabel==names(BB[i]),] 
  df_select_largePatch$patchID <- i
  
  df_select_largePatches <- rbind(df_select_largePatches, df_select_largePatch)
}

ggplot(df_select_largePatches, aes(x=DistanceToEdge, y=NPP/10000))+
  stat_cor(aes(label = paste(..rr.label..)),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab("Distance to Edge (m)")+
  ylab(bquote('NPP ('*kg~C~ m^-2~yr^-1*')'))+
  # scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())+
        facet_wrap(~patchID, ncol = 4)

ggplot(df_select_largePatches, aes(x=DistanceToEdge, y=NPP_DT/10000))+
  stat_cor(aes(label = paste(..rr.label..)),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab("Distance to Edge (m)")+
  ylab(bquote('Partial NPP ('*kg~C~ m^-2~yr^-1*')'))+
  # scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())+
  facet_wrap(~patchID, ncol = 4)


## Analysis based on all patches with > 20 points----

df_select <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df.rds")

AA <- table(df_select$ForestPatchLabel)

CC <- AA[AA>=20]

# sort(CC, decreasing = T)[1:10]

df_statistics_largePatches <- NULL

# i <- 1

for(i in 1:length(CC)){
  tic()
  df_select_largePatch <- df_select[df_select$ForestPatchLabel==names(CC[i]),] 
  # df_select_largePatch$patchID <- i
  
  # colnames(df_select_largePatch)
  lm_ee_1 <- lm(NPP/10000~log10(DistanceToEdge), data=df_select_largePatch)
  lm_ee_2 <- lm(NPP_DT/10000~log10(DistanceToEdge), data=df_select_largePatch)
  
  summary_1 <- summary(lm_ee_1)
  summary_2 <- summary(lm_ee_2)
  
  df_statistics_largePatch <- as.data.frame(cbind(slope_1=summary_1$coefficients[2,1], R2_1=summary_1$adj.r.squared,
                                                  p_1=summary_1$coefficients[2,4],slope_2=summary_2$coefficients[2,1], R2_2=summary_2$adj.r.squared,
                                                  p_2=summary_2$coefficients[2,4], ForestPatchLabel=df_select_largePatch$ForestPatchLabel[1]))
  
  df_statistics_largePatches <- rbind(df_statistics_largePatches, df_statistics_largePatch)
  
  toc()
}

saveRDS(df_statistics_largePatches, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_statistics_largePatches_Df.rds")


df_statistics_largePatches <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_statistics_largePatches_Df.rds")


df_statistics_largePatches_sub1 <- df_statistics_largePatches[df_statistics_largePatches$p_1<=0.05,]
df_statistics_largePatches_sub2 <- df_statistics_largePatches[df_statistics_largePatches$p_2<=0.05,]


pp1 <- ggplot(df_statistics_largePatches_sub1, aes(x = slope_1)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 0, color="red", lty="dashed")+
  labs(
    # title = "Histogram of YourColumn",
    x = "Slope (NPP ~ log10(Distance to Edge))",
    y = "Count"
  ) +
  theme_minimal()


pp2 <- ggplot(df_statistics_largePatches_sub1, aes(x = R2_1)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  # geom_vline(xintercept = 0, color="red", lty="dashed")+
  labs(
    # title = "Histogram of YourColumn",
    x = "R2 (NPP ~ log10(Distance to Edge))",
    y = "Count"
  ) +
  theme_minimal()

pp3 <- ggplot(df_statistics_largePatches_sub2, aes(x = slope_2)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 0, color="red", lty="dashed")+
  labs(
    # title = "Histogram of YourColumn",
    x = "Slope (Partial NPP ~ log10(Distance to Edge))",
    y = "Count"
  ) +
  theme_minimal()


pp4 <- ggplot(df_statistics_largePatches_sub2, aes(x = R2_2)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  # geom_vline(xintercept = 0, color="red", lty="dashed")+
  labs(
    # title = "Histogram of YourColumn",
    x = "R2 (Partial NPP ~ log10(Distance to Edge))",
    y = "Count"
  ) +
  theme_minimal()


layout <- "
AB
CD
"

MainPlot = pp1 + pp2 + pp3 + pp4 +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
  theme(plot.tag = element_text(face = 'bold'))

MainPlot

ggsave("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/figures/mainTextFigures/Figures_NEE_R2/EdgeEffect_statistics_largePatches.pdf", plot=MainPlot, width=8, height=5)


