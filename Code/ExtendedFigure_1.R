rm(list=ls())


library(tictoc)
library(tidyverse)


df_select <- readRDS("**/forestFragments/data/Edge_Effect/EdgeEffect_Df.rds")

set.seed(20251215)
df_select_sub <- df_select%>%slice_sample(prop=0.1)

df_select_sub$EdgeAreaRatio <- df_select_sub$ForestPatchPerimeterAreaInM2/df_select_sub$ForestPatchSizeInM2


# library(writexl)
# write_xlsx(df_select_sub, "C:/Zeus/ETHz/ETHz_S7/PatchSize_NPP/NEE/Revision_V3/Source_Data/Zou_SourceData_ExtendedFig1.xlsx")

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