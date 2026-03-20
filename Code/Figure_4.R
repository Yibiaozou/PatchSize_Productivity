rm(list=ls())

library(raster)
library(tictoc)
library(tidyverse)
library(ranger)
library(ggpubr)
library(patchwork)

### Point level random forests model------
df_select <- readRDS("**/forestFragments/data/Edge_Effect/EdgeEffect_Df_2_withCoordinates.rds")
df_select <- df_select %>% drop_na()



set.seed(20260108)
df_select <- df_select %>% drop_na()
frag_sub <- df_select

selected_covariates_1 <- c("CHELSA_BIO_Annual_Mean_Temperature", 
                           "CHELSA_BIO_Annual_Precipitation", 
                           "EarthEnvTopoMed_Slope", 
                           "EarthEnvTopoMed_AspectSine",
                           "SG_Soil_pH_H2O_005cm",
                           "CrowtherLab_Soil_CNratio_000_to_200_cm",
                           "patchSize_log"
)

formula_npp_1 <- as.formula(
  paste("NPP ~", paste(selected_covariates_1, collapse = " + "))
)


selected_covariates_2 <- c("CHELSA_BIO_Annual_Mean_Temperature", 
                           "CHELSA_BIO_Annual_Precipitation", 
                           "EarthEnvTopoMed_Slope", 
                           "EarthEnvTopoMed_AspectSine",
                           "SG_Soil_pH_H2O_005cm",
                           "CrowtherLab_Soil_CNratio_000_to_200_cm"
)
formula_npp_2 <- as.formula(
  paste("NPP ~", paste(selected_covariates_2, collapse = " + "))
)

# set.seed(20260106)
# frag_sub <- frag%>%slice_sample(prop=0.01)

tic()
rf_NPP_1 <- ranger(formula_npp_1, data = frag_sub, importance = "permutation")
toc()



tic()
rf_NPP_2 <- ranger(formula_npp_2, data = frag_sub, importance = "permutation")
toc()


rf_df <- as.data.frame(cbind(observation=frag_sub$NPP, prediction1=rf_NPP_1$predictions,
                             prediction2=rf_NPP_2$predictions))

rf_df$observation <- as.numeric(rf_df$observation)
rf_df$prediction1 <- as.numeric(rf_df$prediction1)
rf_df$prediction2 <- as.numeric(rf_df$prediction2)


rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

rmse_1 <- rmse(rf_df$observation, rf_df$prediction1)
rmse_1

rmse_2 <- rmse(rf_df$observation, rf_df$prediction2)
rmse_2

importance_df <- data.frame(
  variable = names(rf_NPP_1$variable.importance),
  importance = rf_NPP_1$variable.importance
)

# Create a column with cleaner short names
importance_df <- importance_df %>%
  mutate(short_name = case_when(
    
    variable == "CHELSA_BIO_Annual_Mean_Temperature" ~ "MAT",
    
    variable == "CHELSA_BIO_Annual_Precipitation" ~ "Precipitation",
    
    variable == "CrowtherLab_Soil_CNratio_000_to_200_cm" ~ "Soil_CNratio",
    
    variable == "EarthEnvTopoMed_Slope" ~ "Slope",
    
    variable == "EarthEnvTopoMed_AspectSine" ~ "Aspect",
    
    variable == "SG_Soil_pH_H2O_005cm" ~ "Soil_pH",
    
    variable == "patchSize_log" ~ "PatchSize_log",
    TRUE ~ variable
  ))

# Optionally sort by importance
importance_df <- importance_df %>%
  arrange(desc(importance))

print(importance_df)
colnames(importance_df)

importance_df$target <- 0
importance_df$target[importance_df$short_name=="PatchSize_log"] <- 1

importance_df <- importance_df %>%
  arrange(importance) %>%                     # sort ascending
  mutate(short_name = factor(short_name, 
                             levels = short_name))




pp1 <- ggplot(rf_df, aes(x=prediction1, y=observation))+
  stat_cor(aes(label = paste(..rr.label..)),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  # scale_x_log10()+
  # ggtitle("Y2020")+
  ylab(bquote('Observed NPP ('*kg~C~ m^-2~yr^-1*')'))+
  xlab(bquote('Predicted NPP with Patch Size ('*kg~C~ m^-2~yr^-1*')'))+
  # scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=12,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=12,family = "sans", angle=0),
        axis.text.y=element_text(size=12,family = "sans"),
        axis.title.x=element_text(size=14,family = "sans"),
        axis.title.y=element_text(size=14,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())

pp2 <- ggplot(rf_df, aes(x=prediction2, y= observation))+
  stat_cor(aes(label = paste(..rr.label..)),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  # scale_x_log10()+
  # ggtitle("Y2020")+
  ylab(bquote('Observed NPP ('*kg~C~ m^-2~yr^-1*')'))+
  xlab(bquote('Predicted NPP without Patch Size ('*kg~C~ m^-2~yr^-1*')'))+
  # scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=12,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=12,family = "sans", angle=0),
        axis.text.y=element_text(size=12,family = "sans"),
        axis.title.x=element_text(size=14,family = "sans"),
        axis.title.y=element_text(size=14,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())


pp3 <- ggplot(importance_df, aes(x=short_name, y=importance, fill=as.factor(target)))+
  geom_bar(stat="identity", position="dodge", width=0.7)+coord_flip()+
  scale_fill_manual(values=c("gray40", "red"))+
  # scale_fill_gradient(low="blue", high="red")+
  # geom_errorbar(aes(ymin=VarImp_Mean-VarImp_SD, ymax=VarImp_Mean+VarImp_SD), width=.12,
  #               position=position_dodge(0.05))+
  xlab("Variables")+
  ylab("Importance")+
  # scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y=element_blank(),
        legend.position="none")

# library(writexl)
# write_xlsx(rf_df, "C:/Zeus/ETHz/ETHz_S7/PatchSize_NPP/NEE/Revision_V3/Source_Data/Zou_SourceData_Fig4ab.xlsx")
# write_xlsx(importance_df, "C:/Zeus/ETHz/ETHz_S7/PatchSize_NPP/NEE/Revision_V3/Source_Data/Zou_SourceData_Fig4c.xlsx")


library(grid)
library(gridExtra)

# Convert each ggplot to a grob with panel label
g1 <- arrangeGrob(
  pp1,
  top = textGrob("a", x = unit(0.02, "npc"),
                 y = unit(0.95, "npc"),
                 just = c("left", "top"),
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

g2 <- arrangeGrob(
  pp2,
  top = textGrob("b", x = unit(0.02, "npc"),
                 y = unit(0.95, "npc"),
                 just = c("left", "top"),
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

g3 <- arrangeGrob(
  pp3,
  top = textGrob("c", x = unit(0.02, "npc"),
                 y = unit(0.95, "npc"),
                 just = c("left", "top"),
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

# First row: A and B side-by-side
top_row <- arrangeGrob(g1, g2, ncol = 2)

# Final figure: top row + C spanning full width
final_plot <- arrangeGrob(top_row, g3, ncol = 1)

# Render
grid.newpage()
grid.draw(final_plot)