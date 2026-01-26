rm(list=ls())

library(raster)
library(tictoc)
library(tidyverse)
library(ranger)
library(ggpubr)
library(patchwork)



### Random forests with PCs -------
fragData <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/fragData_RF.rds")

colnames(fragData)

formula_NPP <- NPP_perArea ~ clim1 + clim2 + clim3 + soil1 + soil2 + soil3 + topo1 + topo2 + topo3 + Area_suminSqM

set.seed(20251215)

fragData_sub <- fragData%>%slice_sample(prop=0.1)


tic()
rf_NPP <- ranger(formula_NPP, data = fragData_sub, importance = "permutation")
toc()

rf_NPP$variable.importance


saveRDS(rf_NPP, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/NPP_RF_3PCs.rds")




fragData_sub$Area_log10 <- log10(fragData_sub$Area_suminSqM)
formula_NPP_2 <- NPP_perArea ~ clim1 + clim2 + clim3 + clim4 + clim5 + soil1 + soil2 + soil3 + topo1 + topo2 + topo3 + Area_log10


tic()
rf_NPP_2 <- ranger(formula_NPP_2, data = fragData_sub, importance = "permutation")
toc()

rf_NPP_2$variable.importance


saveRDS(rf_NPP_2, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/NPP_RF_AllPCs.rds")



### Random forests with selected covariates -------

library('readr')


# frag <- read_csv("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/old/conusFinal_30perc.csv", col_types = cols(.default = "n"))
# 
# colnames(frag)
# 
# saveRDS(frag, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/fragData_RF_AllCovariates.rds")

tic()
frag <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/fragData_RF_AllCovariates.rds")
toc()

# frag <- cleanFragFrame(frag)

frag$patchSize_log <- log10(frag$Area_sumInSqM)

colnames(frag)

selected_covariates <- c("COVARIATE_MEAN_CHELSA_BIO_Annual_Mean_Temperature", "COVARIATE_MEAN_CHELSA_BIO_Annual_Precipitation", 
                         # "COVARIATE_MEAN_CHELSA_BIO_Precipitation_Seasonality", "COVARIATE_MEAN_CHELSA_BIO_Temperature_Seasonality",
                         # "COVARIATE_MEAN_CSP_Global_Human_Modification", 
                         "COVARIATE_MEAN_EarthEnvCloudCover_Mean", 
                         "COVARIATE_MEAN_WorldClim2_WindSpeed_AnnualMean", 
                         "COVARIATE_MEAN_EarthEnvTopoMed_Elevation", 
                         "COVARIATE_MEAN_EarthEnvTopoMed_Slope", 
                         # "COVARIATE_MEAN_WorldClim2_SolarRadiation_AnnualMean",
                         # "COVARIATE_MEAN_EarthEnvTopoMed_Eastness", 
                         # "COVARIATE_MEAN_EarthEnvTopoMed_Northness", 
                         "COVARIATE_MEAN_EarthEnvTopoMed_AspectSine",
                         "COVARIATE_MEAN_SG_Soil_pH_H2O_000cm_to_005cm_mean",
                         # "COVARIATE_MEAN_SG_Clay_Content_000cm_to_005cm_mean", "COVARIATE_MEAN_SG_Silt_Content_000cm_to_005cm_mean",
                         # "COVARIATE_MEAN_SG_Sand_Content_000cm_to_005cm_mean", 
                         # "COVARIATE_MEAN_SG_SOC_Stock_000cm_to_005cm", 
                         # "COVARIATE_MEAN_SG_Coarse_fragments_000cm_to_005cm_mean", 
                         "patchSize_log"
                         )

formula_npp <- as.formula(
  paste("NPP_perArea ~", paste(selected_covariates, collapse = " + "))
)

print(formula_npp)

set.seed(20260106)
frag_sub <- frag%>%slice_sample(prop=0.01)

tic()
rf_NPP <- ranger(formula_npp, data = frag_sub, importance = "permutation")
toc()

rf_NPP$variable.importance


saveRDS(rf_NPP, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/NPP_RF_AllCovariates_1percent_log.rds")


# rf_NPP <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/NPP_RF_AllCovariates_10percent.rds")

rf_NPP$r.squared

library(dplyr)

# Build dataframe from ranger importance
importance_df <- data.frame(
  variable = names(rf_NPP$variable.importance),
  importance = rf_NPP$variable.importance
)

# Create a column with cleaner short names
importance_df <- importance_df %>%
  mutate(short_name = case_when(
    
    variable == "COVARIATE_MEAN_CHELSA_BIO_Annual_Mean_Temperature" ~ "Temp_mean",
    
    variable == "COVARIATE_MEAN_CHELSA_BIO_Annual_Precipitation" ~ "Prec_mean",
    
    variable == "COVARIATE_MEAN_WorldClim2_SolarRadiation_AnnualMean" ~ "SolarRadiation",
    
    variable == "COVARIATE_MEAN_CHELSA_BIO_Precipitation_Seasonality" ~ "Prec_season",
    
    variable == "COVARIATE_MEAN_CHELSA_BIO_Temperature_Seasonality" ~ "Temp_season",
    
    variable == "COVARIATE_MEAN_EarthEnvCloudCover_Mean" ~ "CloudCover",
    
    variable == "COVARIATE_MEAN_WorldClim2_WindSpeed_AnnualMean" ~ "WindSpeed",
    
    variable == "COVARIATE_MEAN_EarthEnvTopoMed_Elevation" ~ "Elevation",
    
    variable == "COVARIATE_MEAN_EarthEnvTopoMed_Slope" ~ "Slope",
    
    variable == "COVARIATE_MEAN_EarthEnvTopoMed_AspectSine" ~ "Aspect_sine",
    
    variable == "COVARIATE_MEAN_SG_Soil_pH_H2O_000cm_to_005cm_mean" ~ "Soil_pH",
    
    variable == "COVARIATE_MEAN_SG_Clay_Content_000cm_to_005cm_mean" ~ "Clay",
    
    variable == "COVARIATE_MEAN_SG_Silt_Content_000cm_to_005cm_mean" ~ "Silt",
    
    variable == "COVARIATE_MEAN_SG_Sand_Content_000cm_to_005cm_mean" ~ "Sand",
    
    variable == "COVARIATE_MEAN_SG_Coarse_fragments_000cm_to_005cm_mean" ~ "Coarse_fragments",
    
    variable == "Area_sumInSqM" ~ "Patch_area",
    
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


ggplot(importance_df, aes(x=short_name, y=importance, fill=as.factor(target)))+
  geom_bar(stat="identity", position="dodge", width=0.7)+coord_flip()+
  scale_fill_manual(values=c("gray40", "red"))+
  # scale_fill_gradient(low="blue", high="red")+
  # geom_errorbar(aes(ymin=VarImp_Mean-VarImp_SD, ymax=VarImp_Mean+VarImp_SD), width=.12,
  #               position=position_dodge(0.05))+
  xlab("Variables")+
  ylab("Importance")+
  # scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14,face="bold"),
        axis.title.y=element_blank(),
        legend.position="none")

### Random forest with and without Patch Size------

tic()
frag <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/fragData_RF_AllCovariates.rds")
toc()

# frag <- cleanFragFrame(frag)

frag$patchSize_log <- log10(frag$Area_sumInSqM)

selected_covariates_1 <- c("COVARIATE_MEAN_CHELSA_BIO_Annual_Mean_Temperature", "COVARIATE_MEAN_CHELSA_BIO_Annual_Precipitation", 
                         "COVARIATE_MEAN_EarthEnvCloudCover_Mean", 
                         "COVARIATE_MEAN_WorldClim2_WindSpeed_AnnualMean", 
                         "COVARIATE_MEAN_EarthEnvTopoMed_Elevation", 
                         "COVARIATE_MEAN_EarthEnvTopoMed_Slope", 
                         "COVARIATE_MEAN_EarthEnvTopoMed_AspectSine",
                         "COVARIATE_MEAN_SG_Soil_pH_H2O_000cm_to_005cm_mean",
                         "patchSize_log"
)

formula_npp_1 <- as.formula(
  paste("NPP_perArea ~", paste(selected_covariates_1, collapse = " + "))
)

# print(formula_npp)

selected_covariates_2 <- c("COVARIATE_MEAN_CHELSA_BIO_Annual_Mean_Temperature", "COVARIATE_MEAN_CHELSA_BIO_Annual_Precipitation", 
                           "COVARIATE_MEAN_EarthEnvCloudCover_Mean", 
                           "COVARIATE_MEAN_WorldClim2_WindSpeed_AnnualMean", 
                           "COVARIATE_MEAN_EarthEnvTopoMed_Elevation", 
                           "COVARIATE_MEAN_EarthEnvTopoMed_Slope", 
                           "COVARIATE_MEAN_EarthEnvTopoMed_AspectSine",
                           "COVARIATE_MEAN_SG_Soil_pH_H2O_000cm_to_005cm_mean")
formula_npp_2 <- as.formula(
  paste("NPP_perArea ~", paste(selected_covariates_2, collapse = " + "))
)

set.seed(20260106)
frag_sub <- frag%>%slice_sample(prop=0.01)

# tic()
# rf_NPP_1 <- ranger(formula_npp, data = frag_sub, importance = "permutation")
# toc()
# rm(rf_NPP)
rf_NPP_1 <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/NPP_RF_AllCovariates_1percent_log.rds")


tic()
rf_NPP_2 <- ranger(formula_npp_2, data = frag_sub, importance = "permutation")
toc()


rf_df <- as.data.frame(cbind(observation=frag_sub$NPP_perArea, prediction1=rf_NPP_1$predictions,
                             prediction2=rf_NPP_2$predictions))

rf_df$observation <- as.numeric(rf_df$observation)
rf_df$prediction1 <- as.numeric(rf_df$prediction1)
rf_df$prediction2 <- as.numeric(rf_df$prediction2)


pp1 <- ggplot(rf_df, aes(x=observation, y=prediction1))+
  stat_cor(aes(label = paste(..rr.label..)),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab(bquote('Observed NPP ('*kg~C~ m^-2~yr^-1*')'))+
  ylab(bquote('Predicted NPP with Patch Size ('*kg~C~ m^-2~yr^-1*')'))+
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

pp2 <- ggplot(rf_df, aes(x=observation, y=prediction2))+
  stat_cor(aes(label = paste(..rr.label..)),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.1)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab(bquote('Observed NPP ('*kg~C~ m^-2~yr^-1*')'))+
  ylab(bquote('Predicted NPP without Patch Size ('*kg~C~ m^-2~yr^-1*')'))+
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
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14,face="bold"),
        axis.title.y=element_blank(),
        legend.position="none")

# layout <- "
# AB
# CC
# "
# 
# MainPlot = pp1 + pp2 + pp3 + 
#   plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
#   theme(plot.tag = element_text(size=14, face = 'bold'))
# 
# MainPlot
library(grid)
library(gridExtra)

# Convert each ggplot to a grob with panel label
g1 <- arrangeGrob(
  pp1,
  top = textGrob("A", x = unit(0.02, "npc"),
                 y = unit(0.95, "npc"),
                 just = c("left", "top"),
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

g2 <- arrangeGrob(
  pp2,
  top = textGrob("B", x = unit(0.02, "npc"),
                 y = unit(0.95, "npc"),
                 just = c("left", "top"),
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

g3 <- arrangeGrob(
  pp3,
  top = textGrob("C", x = unit(0.02, "npc"),
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


## Counter-factual scenarios: most fragmented, realized, least fragmented--------

tic()
frag <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/fragData_RF_AllCovariates.rds")
toc()

NPP_up <- 0.8
NPP_down <- 0.58

# frag <- frag[frag$Area_sumInSqM>=10000,]

mean(frag$NPP_perArea)

range(frag$Area_sumInSqM)


NPP_sum_max <- sum(frag$Area_sumInSqM*0.8)/10^12
NPP_sum_min <- sum(frag$Area_sumInSqM*0.58)/10^12

NPP_sum_real <- sum(frag$Area_sumInSqM*frag$NPP_perArea)/10^12


(NPP_sum_max-NPP_sum_real)/NPP_sum_max
(NPP_sum_max-NPP_sum_min)/NPP_sum_max


CF_df <- as.data.frame(cbind(scenarios=c("Least fragmented", "Realized", "Most fragmented"),
                             total_NPP=c(NPP_sum_max, NPP_sum_real, NPP_sum_min)))


CF_df$total_NPP <- as.numeric(CF_df$total_NPP)

CF_df <- CF_df%>%mutate(
  scenarios=fct_relevel(scenarios,"Least fragmented", "Realized", "Most fragmented")
)

ggplot(CF_df, aes(x=scenarios, y=total_NPP, fill=scenarios))+
  # geom_point(size=4)+
  # geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=0.8)+
  geom_bar(position="dodge", stat = "identity", alpha=0.6, color="black", width=0.5)+
  # geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  scale_y_continuous(expand=c(0,0), limits = c(0,1.2))+
  # ggtitle("Boreal")+
  # scale_x_log10()+
  scale_fill_manual(values=c( "#ca0020","orange","#0571b0"))+
  xlab("Scenarios")+
  ylab("Totoal NPP (Gt C per year)")+
  # ylab(bquote('Total NPP ('*Gt~C~ yr^-1*')'))+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  # geom_hline(yintercept = 50, color="black", linetype="dashed",linewidth=1)+
  theme_classic()+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=18,family = "sans"),
        axis.title.y=element_text(size=18,family = "sans"),
        legend.position = "none")




### Point level random forests model------
df_select <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df_2_withCoordinates.rds")
df_select <- df_select %>% drop_na()


# df_covariates <- read.csv("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/Edge_Covariates_Merge/EdgeEffect_covariates_Merge_2.csv")
# 
# 
# colnames(df_select)
# colnames(df_covariates)
# 
# df_select <- left_join(df_select, df_covariates[,c("CrowtherLab_Soil_N_Content_000_to_200cm", "CrowtherLab_SoilMoisture_Mean_downsampled10km",
#                                                    "CrowtherLab_Soil_CNratio_000_to_200_cm", "ID")], by="ID")
# 
# saveRDS(df_select, file="D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df_2_withCoordinates.rds")

# df_select$patchSize_log <- log10(df_select$ForestPatchSizeInM2)
# df_select$NPP <- df_select$NPP/10000


# full_covariates <- colnames(df_select)[c(3:8, 16:21, 23:30)]
# 
# formula_npp_0 <- as.formula(
#   paste("NPP ~", paste(full_covariates, collapse = " + "))
# )

set.seed(20260108)
df_select <- df_select %>% drop_na()
frag_sub <- df_select
# tic()
# rf_NPP_0 <- ranger(formula_npp_0, data = frag_sub, importance = "permutation")
# toc()

# rf_NPP_0$variable.importance
# rf_NPP_0$r.squared



# selected_covariates_0 <- c("CHELSA_BIO_Annual_Mean_Temperature", 
#                            "CHELSA_BIO_Annual_Precipitation", 
#                            "EarthEnvCloudCover_Mean",
#                            "EarthEnvTopoMed_Eastness",
#                            "WorldClim2_WindSpeed_AnnualMean",
#                            "EarthEnvTopoMed_Elevation",
#                            "EarthEnvTopoMed_Slope", 
#                            "EarthEnvTopoMed_AspectSine",
#                            "SG_Soil_pH_H2O_005cm",
#                            "CrowtherLab_Soil_CNratio_000_to_200_cm",
#                            "CrowtherLab_Soil_N_Content_000_to_200cm",
#                            "patchSize_log"
# )
# 
# 
# ### correlation matrix
# AA <- df_select[,selected_covariates_0]
# colnames(AA) <- c("MAT", "Precipitation", "CloudCover", "Eastness", "WindSpeed",
#                   "Elevation", "Slope", "Aspect", "Soil_pH", "Soil_CNratio", "Soil_N", "PatchSize_log")
# 
# cor_mat <- cor(AA, use = "pairwise.complete.obs")
# 
# library(corrplot)
# 
# corrplot(
#   cor_mat,
#   method = "color",
#   type = "lower",
#   tl.cex = 0.8,
#   tl.col = "black",
#   addCoef.col = NA
# )


selected_covariates_1 <- c("CHELSA_BIO_Annual_Mean_Temperature", 
                           "CHELSA_BIO_Annual_Precipitation", 
                           # "EarthEnvCloudCover_Mean", 
                           # "EarthEnvTopoMed_Eastness",
                           # "WorldClim2_WindSpeed_AnnualMean",
                           # "EarthEnvTopoMed_Elevation", 
                           "EarthEnvTopoMed_Slope", 
                           "EarthEnvTopoMed_AspectSine",
                           "SG_Soil_pH_H2O_005cm",
                           "CrowtherLab_Soil_CNratio_000_to_200_cm",
                           # "CrowtherLab_Soil_N_Content_000_to_200cm",
                           "patchSize_log"
)

formula_npp_1 <- as.formula(
  paste("NPP ~", paste(selected_covariates_1, collapse = " + "))
)

# library(car)

## VIF test
# Fit linear model
# lm_npp_1 <- lm(formula_npp_1, data = frag_sub)
# 
# vif_values <- vif(lm_npp_1)
# print(vif_values)

# print(formula_npp)

selected_covariates_2 <- c("CHELSA_BIO_Annual_Mean_Temperature", 
                           "CHELSA_BIO_Annual_Precipitation", 
                           # "EarthEnvCloudCover_Mean", 
                           # "EarthEnvTopoMed_Eastness",
                           # "WorldClim2_WindSpeed_AnnualMean",
                           # "EarthEnvTopoMed_Elevation", 
                           "EarthEnvTopoMed_Slope", 
                           "EarthEnvTopoMed_AspectSine",
                           "SG_Soil_pH_H2O_005cm",
                           "CrowtherLab_Soil_CNratio_000_to_200_cm"
                           # "CrowtherLab_Soil_N_Content_000_to_200cm"
                           )
formula_npp_2 <- as.formula(
  paste("NPP ~", paste(selected_covariates_2, collapse = " + "))
)

# set.seed(20260106)
# frag_sub <- frag%>%slice_sample(prop=0.01)

tic()
rf_NPP_1 <- ranger(formula_npp_1, data = frag_sub, importance = "permutation")
toc()
# rm(rf_NPP)
# rf_NPP_1 <- readRDS("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/RandomForest/NPP_RF_AllCovariates_1percent_log.rds")


### compute SHAP value
library(fastshap)



# set.seed(123)

# X <- frag_sub[, c(
#   "CHELSA_BIO_Annual_Mean_Temperature",
#   "CHELSA_BIO_Annual_Precipitation",
#   "EarthEnvTopoMed_Slope",
#   "EarthEnvTopoMed_AspectSine",
#   "SG_Soil_pH_H2O_005cm",
#   "CrowtherLab_Soil_CNratio_000_to_200_cm",
#   "patchSize_log"
# )]%>%slice_sample(prop=0.1)
# 
# tic()
# shap_values <- fastshap::explain(
#   object = rf_NPP_1,
#   feature_names = colnames(X),
#   X = X,
#   pred_wrapper = function(object, newdata) {
#     predict(object, newdata)$predictions
#   },
#   nsim = 10
# )
# toc()
# 
# shap_df <- data.frame(
#   patchSize_log = X$patchSize_log,
#   shap_patchSize = shap_values[, "patchSize_log"]
# )
# 
# 
# ggplot(shap_df, aes(x = patchSize_log, y = shap_patchSize)) +
#   geom_point(alpha = 0.15, size = 1) +
#   geom_smooth(method = "loess", color = "red", linewidth = 1.2) +
#   xlab("Log10(Patch size)") +
#   ylab("SHAP value for patch size (effect on NPP)") +
#   theme_classic() +
#   theme(
#     axis.title = element_text(size = 14),
#     axis.text  = element_text(size = 12)
#   )




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
    
    variable == "WorldClim2_SolarRadiation_AnnualMean" ~ "SolarRadiation",
    
    variable == "CHELSA_BIO_Precipitation_Seasonality" ~ "Prec_season",
    
    variable == "CHELSA_BIO_Temperature_Seasonality" ~ "Temp_season",
    
    variable == "EarthEnvCloudCover_Mean" ~ "CloudCover",
    
    variable == "CrowtherLab_Soil_N_Content_000_to_200cm" ~ "Soil_N",
    
    variable == "CrowtherLab_Soil_CNratio_000_to_200_cm" ~ "Soil_CNratio",
    
    variable == "WorldClim2_WindSpeed_AnnualMean" ~ "WindSpeed",
    
    variable == "EarthEnvTopoMed_Eastness" ~ "Eastness",
    
    variable == "EarthEnvTopoMed_Elevation" ~ "Elevation",
    
    variable == "EarthEnvTopoMed_Slope" ~ "Slope",
    
    variable == "EarthEnvTopoMed_AspectSine" ~ "Aspect",
    
    variable == "SG_Soil_pH_H2O_005cm" ~ "Soil_pH",
    
    variable == "SG_Clay_Content_000cm_to_005cm_mean" ~ "Clay",
    
    variable == "SG_Silt_Content_000cm_to_005cm_mean" ~ "Silt",
    
    variable == "SG_Sand_Content_000cm_to_005cm_mean" ~ "Sand",
    
    variable == "SG_Coarse_fragments_000cm_to_005cm_mean" ~ "Coarse_fragments",
    
    variable == "Area_sumInSqM" ~ "Patch_area",
    
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

# layout <- "
# AB
# CC
# "
# 
# MainPlot = pp1 + pp2 + pp3 + 
#   plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
#   theme(plot.tag = element_text(size=14, face = 'bold'))
# 
# MainPlot
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


### Point level sample exported as shapefile------


df_coordinatess <- read.csv("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/EdgeEffect_Df_coordinates.csv")

library(sf)

tic()
# df must contain columns named: longitude, latitude
coords_sf <- st_as_sf(
  df_coordinatess,
  coords = c("longitude", "latitude"),
  crs = 4326,          # WGS84 (lon/lat)
  remove = FALSE       # keep the original lon/lat columns
)

# Write shapefile (creates multiple files with same basename)
st_write(coords_sf, "D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments/data/Edge_Effect/forest_points.shp", delete_layer = TRUE)
toc()


