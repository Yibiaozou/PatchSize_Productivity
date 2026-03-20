rm(list=ls())

library(raster)
library(tictoc)
library(tidyverse)

tic()
frag <- readRDS("**/forestFragments/data/RandomForest/fragData_RF_AllCovariates.rds")
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

# library(writexl)
# write_xlsx(CF_df, "C:/Zeus/ETHz/ETHz_S7/PatchSize_NPP/NEE/Revision_V3/Source_Data/Zou_SourceData_Fig3.xlsx")


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