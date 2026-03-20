rm(list=ls())

library(raster)
library(tictoc)
library(tidyverse)
library(ranger)
library(ggpubr)
library(jsonlite)
library(patchwork)

## Detrend NPP based on topographic information -----

df_select <- readRDS("**/forestFragments/data/Edge_Effect/EdgeEffect_Df_2_withCoordinates.rds")
df_select <- df_select %>% drop_na()

formula_NPP <- NPP ~ EarthEnvTopoMed_Elevation + EarthEnvTopoMed_Slope + EarthEnvTopoMed_AspectSine + EarthEnvTopoMed_Eastness + EarthEnvTopoMed_Northness

tic()
rf_NPP <- ranger(formula_NPP, data = df_select, importance = "permutation")
toc()

df_select$NPP_DT <- df_select$NPP - rf_NPP$predictions + mean(df_select$NPP)


AA <- table(df_select$ForestPatchLabel)

BB <- sort(AA, decreasing = T)[1:10]

# names(BB) <- 1:10
df_select_largePatches <- NULL

for(i in 1:10){
  df_select_largePatch <- df_select[df_select$ForestPatchLabel==names(BB[i]),] 
  df_select_largePatch$patchID <- i
  
  df_select_largePatches <- rbind(df_select_largePatches, df_select_largePatch)
}

# library(writexl)
# write_xlsx(df_select_largePatches, "C:/Zeus/ETHz/ETHz_S7/PatchSize_NPP/NEE/Revision_V3/Source_Data/Zou_SourceData_ExtendedFig2.xlsx")

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


