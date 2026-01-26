rm(list=ls())

library(raster)
library(tictoc)
library(tidyverse)
library(ranger)
library(ggpubr)



### detrended NPP based on climatic variables----
setwd("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/")

fileLst <- list.files()


df_list <- lapply(1:length(fileLst), function(x) read.csv(fileLst[x]))


df <- do.call(rbind, df_list)
colnames(df)


df_select <- df%>%dplyr::select(NPP, ForestPatchLabel, CGIAR_Aridity_Index, CHELSA_BIO_Annual_Mean_Temperature, CHELSA_BIO_Annual_Precipitation, DistanceToEdge,
                                CHELSA_BIO_Precipitation_Seasonality, CHELSA_BIO_Temperature_Seasonality, ForestPatchPerimeterAreaInM2, ForestPatchSizeInM2, ForestPatchPerimeterInM)%>%
  drop_na()

formula_NPP <- NPP ~ CGIAR_Aridity_Index + CHELSA_BIO_Annual_Mean_Temperature + CHELSA_BIO_Annual_Precipitation + CHELSA_BIO_Precipitation_Seasonality + CHELSA_BIO_Temperature_Seasonality

tic()
rf_NPP <- ranger(formula_NPP, data = df_select, importance = "permutation")
toc()

rf_NPP$variable.importance

formula_NPP_2 <- NPP ~ ForestPatchSizeInM2 + CGIAR_Aridity_Index + CHELSA_BIO_Annual_Mean_Temperature + CHELSA_BIO_Annual_Precipitation + CHELSA_BIO_Precipitation_Seasonality + CHELSA_BIO_Temperature_Seasonality

tic()
rf_NPP_2 <- ranger(formula_NPP_2, data = df_select, importance = "permutation")
toc()

rf_NPP_2$variable.importance



df_select$NPP_DT <- df_select$NPP - rf_NPP$predictions + mean(df_select$NPP)

rf_NPP$r.squared
rf_NPP$predictions

hist(df_select$NPP_DT)

lm1 <- lm(NPP~log(DistanceToEdge), data=df_select)
summary(lm1)

lm2 <- lm(NPP_DT~log(DistanceToEdge), data=df_select)
summary(lm2)


length(unique(df_select$ForestPatchLabel))

max(table(df_select$ForestPatchLabel))
hist(table(df_select$ForestPatchLabel))


saveRDS(df_select, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df.rds")

hist(df_select$DistanceToEdge)


## analysis based on 1% sample----

df_select <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df.rds")

set.seed(20251215)
df_select_sub <- df_select%>%slice_sample(prop=0.1)


ggplot(df_select_sub, aes(x=DistanceToEdge, y=NPP))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab("Distance to Edge (m)")+
  ylab("NPP")+
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

ggplot(df_select_sub, aes(x=DistanceToEdge, y=NPP_DT))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab("Distance to Edge (m)")+
  ylab("NPP (Detrended)")+
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

colnames(df_select_sub)

df_select_sub$EdgeAreaRatio <- df_select_sub$ForestPatchPerimeterAreaInM2/df_select_sub$ForestPatchSizeInM2

ggplot(df_select_sub, aes(x=ForestPatchSizeInM2, y=EdgeAreaRatio))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab("Patch Size (m2)")+
  ylab("Edge to Area Ratio")+
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





## analysis based on largest patches----
AA <- table(df_select$ForestPatchLabel)

df_select <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df.rds")


df_select_largePatch <- df_select[df_select$ForestPatchLabel==names(AA[AA == max(AA)]),] 

formula_NPP <- NPP ~ CHELSA_BIO_Annual_Mean_Temperature + CHELSA_BIO_Annual_Precipitation 
#+ CHELSA_BIO_Precipitation_Seasonality + CHELSA_BIO_Temperature_Seasonality

tic()
rf_NPP <- ranger(formula_NPP, data = df_select_largePatch, importance = "permutation")
toc()

# summary(rf_NPP)
rf_NPP$r.squared

df_select_largePatch$NPP_DT <- df_select_largePatch$NPP - rf_NPP$predictions + mean(df_select_largePatch$NPP)
# df_select_largePatch$ForestPatchSizeInM2[1]

ggplot(df_select_largePatch, aes(x=DistanceToEdge, y=NPP))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab("Distance to Edge (m)")+
  ylab("NPP")+
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


ggplot(df_select_largePatch, aes(x=DistanceToEdge, y=NPP_DT))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab("Distance to Edge (m)")+
  ylab("NPP (Detrended)")+
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

