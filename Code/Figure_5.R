rm(list=ls())

setwd("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments")
library('tidyverse')
library('reshape2')
library('cowplot')
library('directlabels')
library('cmocean')
library(gridExtra)
library(patchwork)
library(ggpubr)


uniqueName='2010BigPatchesSEE'
ContinentName <- c("North America", "South America", "Europe", "Africa", "Asia", "Oceania")

i <- 1

partials_df <- NULL
slope_df <- NULL

for(i in 1:6){
  
  partials <- read_csv(file =  paste0("conus/partialsAndResids/partialsAndResidsGlobal",uniqueName,"_Continent",i,"_Biome0.csv"))
  statistics <- read.csv(file = paste0("conus/modelFrames/modelFrameGlobal",uniqueName,"_Continent",i,"_Biome0.csv"))
  
  
  slope_NPP <- statistics$coefficient[statistics$modelName=="NPP"][2]
  slope_GPP <- statistics$coefficient[statistics$modelName=="GPP"][2]
  
  partials$Continent <- ContinentName[i]
  partials_df <- rbind(partials_df, partials)
  
  slope_df <- rbind(slope_df, as.data.frame(cbind(Continent=ContinentName[i], slope_npp=slope_NPP, slope_gpp=slope_GPP)))
}



slope_df$slope_npp <- as.numeric(slope_df$slope_npp)
slope_df$slope_gpp <- as.numeric(slope_df$slope_gpp)

colnames(partials_df)


colorCT <- c(  "#D55E00",  # blue
               "#56B4E9",  # orange
               "purple",  # green
               "#0072B2",  # vermillion
               "red",  # purple
               "#E69F00")   # sky blue)

# c(  "#0072B2",  # blue
#     "#E69F00",  # orange
#     "purple",  # green
#     "#D55E00",  # vermillion
#     "red",  # purple
#     "#56B4E9")   # sky blue)


gp1 <- ggplot(partials_df, aes(x=ForestPatchSizeInM2, y=partialNpp, group=Continent, color=Continent))+
  stat_cor(aes(label = paste(..rr.label..)),label.x.npc = 0.2, label.y.npc = "top", method = "pearson",label.sep = ",",size = 3)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.2)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab(bquote('Log10(Patch Size)'))+
  ylab(bquote('Partial NPP ('*kg~C~ m^-2~yr^-1*')'))+
  scale_color_manual(values=colorCT)+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=12,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=12,family = "sans", angle=0),
        axis.text.y=element_text(size=12,family = "sans"),
        axis.title.x=element_text(size=14,family = "sans"),
        axis.title.y=element_text(size=14,family = "sans"),
        legend.position = "right",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())


gp2 <- ggplot(slope_df, aes(x=Continent, y=slope_npp , fill=Continent))+
  geom_bar(stat="identity", position="dodge", width=0.5)+coord_flip()+
  scale_y_continuous(expand=c(0,0), limits = c(-0.015,0.12))+
  geom_hline(yintercept = 0, linetype="dashed")+
  scale_fill_manual(values=colorCT)+
  # scale_fill_manual(values=c("gray40", "red"))+
  # scale_fill_gradient(low="blue", high="red")+
  # geom_errorbar(aes(ymin=VarImp_Mean-VarImp_SD, ymax=VarImp_Mean+VarImp_SD), width=.12,
  #               position=position_dodge(0.05))+
  xlab("Continent")+
  ylab("Effect Size (NPP ~ PatchSize_Log)")+
  # scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_blank(),
        legend.position="none")


gp3 <- ggplot(partials_df, aes(x=ForestPatchSizeInM2, y=partialGpp, group=Continent, color=Continent))+
  stat_cor(aes(label = paste(..rr.label..)),label.x.npc = 0.2, label.y.npc = "top", method = "pearson",label.sep = ",",size = 3)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.2)+
  scale_x_log10()+
  # ggtitle("Y2020")+
  xlab(bquote('Log10(Patch Size)'))+
  ylab(bquote('Partial GPP ('*kg~C~ m^-2~yr^-1*')'))+
  scale_color_manual(values=colorCT)+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=12,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=12,family = "sans", angle=0),
        axis.text.y=element_text(size=12,family = "sans"),
        axis.title.x=element_text(size=14,family = "sans"),
        axis.title.y=element_text(size=14,family = "sans"),
        legend.position = "right",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())

gp4 <- ggplot(slope_df, aes(x=Continent, y=slope_gpp , fill=Continent))+
  geom_bar(stat="identity", position="dodge", width=0.5)+coord_flip()+
  scale_y_continuous(expand=c(0,0), limits = c(-0.015,0.12))+
  geom_hline(yintercept = 0, linetype="dashed")+
  scale_fill_manual(values=colorCT)+
  # scale_fill_gradient(low="blue", high="red")+
  # geom_errorbar(aes(ymin=VarImp_Mean-VarImp_SD, ymax=VarImp_Mean+VarImp_SD), width=.12,
  #               position=position_dodge(0.05))+
  xlab("Continent")+
  ylab("Effect Size (GPP ~ PatchSize_Log)")+
  # scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_blank(),
        legend.position="none")


library(writexl)
write_xlsx(partials_df, "C:/Zeus/ETHz/ETHz_S7/PatchSize_NPP/NEE/Revision_V3/Source_Data/Zou_SourceData_Fig5ac.xlsx")
write_xlsx(slope_df, "C:/Zeus/ETHz/ETHz_S7/PatchSize_NPP/NEE/Revision_V3/Source_Data/Zou_SourceData_Fig5bd.xlsx")


layout <- "
AB
CD
"

MainPlot = gp1 + gp2 + gp3 + gp4 +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
  theme(plot.tag = element_text(size=14, face = 'bold'))

MainPlot


