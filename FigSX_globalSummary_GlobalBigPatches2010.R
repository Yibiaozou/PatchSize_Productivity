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
library(tictoc)
# continent <- 1

uniqueName="2010BigPatchesSEE"
ContinentName <- c("North America", "South America", "Europe", "Africa", "Asia", "Oceania")

summaries_list <- list()

for(i in 1:6){
tic()  
continent=i

summaries <- read_csv(file = paste0("conus/biomeSummaries/summaryTableGlobal",uniqueName,"_Continent",continent,"_Biome0.csv"))
summaries$sizeClass <- summaries$sizeClass %>% ordered(levels = c('< 1e4',
                                                                    '< 1e5',
                                                                    '< 1e6',
                                                                    '< 1e7',
                                                                    '< 1e8',
                                                                    '< 1e9',
                                                                    '< 1e10',
                                                                    '< 1e11',
                                                                    '< 1e12'))
summaries$continent <- ContinentName[i]
summaries_list[[i]] <- summaries
# partials <- read_csv(file =  paste0("conus/partialsAndResids/partialsAndResidsGlobal",uniqueName,"_Continent",continent,"_Biome0.csv"))
# statistics <- read.csv(file = paste0("conus/modelFrames/modelFrameGlobal",uniqueName,"_Continent",continent,"_Biome0.csv"))
toc()  
}

summaries_global <- do.call(rbind, summaries_list)

summaries_global <- summaries_global[!is.na(summaries_global$sizeClass),]

summaries_global <- summaries_global[summaries_global$n>=3,]

globalPlotNpp <- ggplot(summaries_global) +
  geom_line(aes(y = nppPartial, x = sizeClass,  group = as.factor(continent), color = as.factor(continent)), linewidth = 1) +
  geom_point(aes(y = nppPartial, x = sizeClass, color = as.factor(continent))) +
  geom_errorbar(aes(ymin = nppPartial-(nppPartialSd/sqrt(n)), ymax = nppPartial+(nppPartialSd/sqrt(n)), color = as.factor(continent), x = sizeClass),
                width = 0, alpha = 0.6) +
  theme_cowplot() +
  scale_color_manual(values = rainbow(6)) +
  ylab(bquote('Partial NPP ('*kg~C~ m^-2~yr^-1*')')) +
  theme(legend.title = element_blank(),
        legend.position = c(0.06,0.87),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))

globalPlotGpp <- ggplot(summaries_global) +
  geom_line(aes(y = gppPartial, x = sizeClass,  group = as.factor(continent), color = as.factor(continent)), linewidth = 1) +
  geom_point(aes(y = gppPartial, x = sizeClass, color = as.factor(continent))) +
  geom_errorbar(aes(ymin = gppPartial-(gppPartialSd/sqrt(n)), ymax = gppPartial+(gppPartialSd/sqrt(n)), color = as.factor(continent), x = sizeClass),
                width = 0, alpha = 0.6) +
  theme_cowplot() +
  scale_color_manual(values = rainbow(6)) +
  ylab(bquote('Partial GPP ('*kg~C~ m^-2~yr^-1*')')) +
  theme(legend.title = element_blank(),
        legend.position = c(0.06,0.87),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))


# globalPlotVod <- ggplot(summaries_global) +
#   geom_line(aes(y = vodPartial, x = sizeClass,  group = as.factor(continent), color = as.factor(continent)), linewidth = 1) +
#   geom_point(aes(y = vodPartial, x = sizeClass, color = as.factor(continent))) +
#   geom_errorbar(aes(ymin = vodPartial-(vodPartialSd/sqrt(n)), ymax = vodPartial+(vodPartialSd/sqrt(n)), color = as.factor(continent), x = sizeClass),
#                 width = 0, alpha = 0.6) +
#   theme_cowplot() +
#   scale_color_manual(values = rainbow(6)) +
#   ylab(bquote('Mean VOD (partial)')) +
#   theme(legend.title = element_blank(),
#         legend.position = c(0.06,0.87),
#         legend.text = element_text(size = 9),
#         legend.key.height = unit(0.2,'cm'),
#         axis.title.x = element_blank(),
#         plot.margin = unit(c(7,7,3,7), 'pt'))


layout <- "
  AB
  "

MainPlot = globalPlotNpp + globalPlotGpp + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
  theme(plot.tag = element_text(size=18,family="sans", face="bold"))

MainPlot


ggsave(paste0('figures/mainTextFigures/figureSX_Global_SEE_Summaries.pdf'), plot=MainPlot, width=10, height=4)



# VOD----

uniqueName="2010BigPatchesSEE_VOD"
ContinentName <- c("North America", "South America", "Europe", "Africa", "Asia", "Oceania")

summaries_list <- list()

for(i in 1:5){
  tic()  
  continent=i
  
  summaries <- read_csv(file = paste0("conus/biomeSummaries/summaryTableGlobal",uniqueName,"_Continent",continent,"_Biome0.csv"))
  summaries$sizeClass <- summaries$sizeClass %>% ordered(levels = c('< 1e4',
                                                                    '< 1e5',
                                                                    '< 1e6',
                                                                    '< 1e7',
                                                                    '< 1e8',
                                                                    '< 1e9',
                                                                    '< 1e10',
                                                                    '< 1e11',
                                                                    '< 1e12'))
  summaries$continent <- ContinentName[i]
  summaries_list[[i]] <- summaries
  # partials <- read_csv(file =  paste0("conus/partialsAndResids/partialsAndResidsGlobal",uniqueName,"_Continent",continent,"_Biome0.csv"))
  # statistics <- read.csv(file = paste0("conus/modelFrames/modelFrameGlobal",uniqueName,"_Continent",continent,"_Biome0.csv"))
  toc()  
}

summaries_global <- do.call(rbind, summaries_list)

summaries_global <- summaries_global[!is.na(summaries_global$sizeClass),]

summaries_global <- summaries_global[summaries_global$n>=3,]

globalPlotVod <- ggplot(summaries_global) +
  geom_line(aes(y = vodPartial, x = sizeClass,  group = as.factor(continent), color = as.factor(continent)), linewidth = 1) +
  geom_point(aes(y = vodPartial, x = sizeClass, color = as.factor(continent))) +
  geom_errorbar(aes(ymin = vodPartial-(vodPartialSd/sqrt(n)), ymax = vodPartial+(vodPartialSd/sqrt(n)), color = as.factor(continent), x = sizeClass),
                width = 0, alpha = 0.6) +
  theme_cowplot() +
  scale_color_manual(values = rainbow(6)[c(1:4,6)]) +
  ylab(bquote('Mean VOD (partial)')) +
  theme(legend.title = element_blank(),
        legend.position = c(0.06,0.87),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))


uniqueName <- "2010BigPatchesSEE_VOD"
partials_Lst <- lapply(1:5, function(x) read_csv(file =  paste0("conus/partialsAndResids/partialsAndResidsGlobal",uniqueName,"_Continent",x,"_Biome0.csv"))) 
partials <- do.call(rbind, partials_Lst)

partialsVodPlot <- partials %>%
  ggplot(aes(x = ForestPatchSizeInM2, y = partialVod)) +
  geom_bin2d(bins = 20) + 
  scale_fill_cmocean(name="rain", trans = "sqrt") +
  labs(fill = 'Patches (n)') +
  # ggtitle(ContinentName[continent])+
  # annotate("text",
  #          x = Inf, y = -Inf,
  #          label = sprintf("Slope = %.2f, p-value < %.3g", slope, 0.001),
  #          hjust = 1.5, vjust = -1.5,
  #          # position="top",
  #          size = 4, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")),label.x.npc = 0.2, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 4)+
  geom_smooth(method = 'lm', color = 'red') +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  ylab("") +
  theme(legend.position = c(0.8,0.3), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.height = unit(0.4,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))


layout2 <- "
  AB
  "

MainPlot2 = globalPlotVod + partialsVodPlot + 
  plot_layout(design = layout2) + plot_annotation(tag_levels = 'a')&
  theme(plot.tag = element_text(size=18,family="sans", face="bold"))

MainPlot2


ggsave(paste0('figures/mainTextFigures/figureSX_Global_VOD_SEE_Summaries.pdf'), plot=MainPlot2, width=10, height=4)
