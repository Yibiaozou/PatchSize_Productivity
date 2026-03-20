rm(list=ls())

library(raster)
library(tictoc)
library(tidyverse)
library(ranger)
library(ggpubr)
library(jsonlite)
library(patchwork)

df_select <- readRDS("**/forestFragments/data/Edge_Effect/EdgeEffect_Df.rds")

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



# library(writexl)
# write_xlsx(df_statistics_largePatches, "C:/Zeus/ETHz/ETHz_S7/PatchSize_NPP/NEE/Revision_V3/Source_Data/Zou_SourceData_ExtendedFig3.xlsx")

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

