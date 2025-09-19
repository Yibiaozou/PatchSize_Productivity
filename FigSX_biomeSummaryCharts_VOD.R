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

summaries <- read_csv(file = "conus/biomeSummaries/VOD_summaryTable1024Big_SEEBiome0.csv")

biomes <- c(0,4,5,7,8,12,13)

for(i in 2:length(biomes)) {
  sumPath <- paste('conus/biomeSummaries/VOD_summaryTable1024Big_SEEBiome', biomes[i], '.csv', sep = '')
  if(file.exists(sumPath) == TRUE) {
    summaries <- read_csv(file = sumPath) %>%
      rbind(summaries)
  }
}

summaries$analysis <- '1024Big'

uniqueName <- '1024Big'

#how many patches for each analysis?
filter(summaries, dataset == 0) %>%
  group_by(analysis) %>% summarize(howMany = sum(n))

#filter(summariesNDVI, dataset == 0) %>%
#  group_by(analysis) %>% summarize(howMany = sum(n))

#####create biome dictionary

biomeDict <- c('Full CONUS',
               'Trop/subtr wet broadleaf',
               'Trop/subtr dry broadleaf',
               'Trop/subtr conif',
               'Temp broadleaf/mixed',
               'Temp conif',
               'Boreal',
               'Trop/subtr grass/shrub',
               'Temp grass/shrub',
               'Flooded grass/sav',
               'Montane grass/shrub',
               'Tundra',
               'Medit woodlands/scrub',
               'Deserts/xeric shrub',
               'Mangroves') %>% as.ordered() %>% data.frame()
biomeDict$numb <- rep(0:14)

summaries$biome <- biomeDict$.[match(summaries$dataset, biomeDict$numb)]
summaries$biome <- summaries$biome %>% ordered(levels = c("Full CONUS",
                                                          "Deserts/xeric shrub",
                                                          "Medit woodlands/scrub",
                                                          "Temp broadleaf/mixed",
                                                          "Temp conif",
                                                          "Temp grass/shrub",
                                                          "Trop/subtr grass/shrub",
                                                          "Mangroves",
                                                          "Trop/subtr conif",
                                                          "Flooded grass/sav"))

#summariesNDVI$biome <- biomeDict$.[match(summariesNDVI$dataset, biomeDict$numb)]
#summariesNDVI$biome <- summariesNDVI$biome %>% ordered(levels = c("Full CONUS",
#                                                          "Deserts/xeric shrub",
#                                                          "Medit woodlands/scrub",
#                                                          "Temp broadleaf/mixed",
#                                                          "Temp conif",
#                                                          "Temp grass/shrub",
#                                                          "Trop/subtr grass/shrub",
#                                                          "Mangroves",
#                                                          "Trop/subtr conif",
#                                                          "Flooded grass/sav"))



colPal <- RColorBrewer::brewer.pal(length(biomes)-1, "Paired")

colPal <- c("black", colPal)

summaries$sizeClass <- summaries$sizeClass %>% ordered(levels = c('< 1e4',
                                           '< 1e5',
                                           '< 1e6',
                                           '< 1e7',
                                           '< 1e8',
                                           '< 1e9',
                                           '< 1e10',
                                           '< 1e11',
                                           '< 1e12'))


#summariesNDVI$sizeClass <- summariesNDVI$sizeClass %>% ordered(levels = c('< 1e4',
#                                                                  '< 1e5',
##                                                                  '< 1e6',
#                                                                  '< 1e7',
#                                                                  '< 1e8',
#                                                                  '< 1e9',
#                                                                  '< 1e10',
#                                                                  '< 1e11',
#                                                                  '< 1e12'))#


#now we need the partials

analyses <- c('1024Big', 'SizeCutoff5000')
analysesLowercase <- c('1024Big', 'sizeCutoff5000')
analysesDict <- data.frame('name' = c('',
                                      'patches >= 5,000 sq m'),
                           'analyses' = analyses)

#first we do the main text figure

#this one for the ambizione application
#ambizData <- filter(summaries, n >= 3, analysis == '1024Big')

#numOfBiomes <- ambizData$biome %>% unique() %>%
#  length()

#colPal <- RColorBrewer::brewer.pal(numOfBiomes-1, "Paired")
#colPal <- c("black", colPal)

#switch units to tonnes per hectares
#ambizData$nppPartial_modArea <- ambizData$nppPartial_modArea/0.0001 * 0.001
#ambizData$nppPartialSd_modArea <- ambizData$nppPartialSd_modArea/0.0001 * 0.001

#ambizPlot <- ggplot(ambizData) +
#  geom_line(aes(y = nppPartial_modArea, x = sizeClass,  group = as.factor(biome), color = as.factor(biome)), size = 1) +
#  geom_point(aes(y = nppPartial_modArea, x = sizeClass, color = as.factor(biome))) +
#  geom_errorbar(aes(ymin = nppPartial_modArea-(nppPartialSd_modArea/sqrt(n)), ymax = nppPartial_modArea+(nppPartialSd_modArea/sqrt(n)), color = as.factor(biome), x = sizeClass),
#                width = 0, alpha = 0.6) +
#  theme_cowplot() +
#  scale_color_manual(values = colPal) +
#  ylab(bquote('Partial NPP ('*tonne~C~ ha^-1~yr^-1*')')) +
#  xlab(bquote('Patch area ('*m^2*')')) +
#  theme(legend.title = element_blank(),
#        legend.position = c(0.06,0.87),
#        legend.text = element_text(size = 9),
#        legend.key.height = unit(0.2,'cm'),
#        plot.margin = unit(c(7,7,3,7), 'pt'))

#ggsave(ambizPlot, file = 'figures/ambizFig.png', width = 6, height = 5)

partials <- read_csv(file = 'conus/partialsAndResids/VOD_partialsAndResids1024Big_SEEBiome0.csv')

statistics <- read.csv(file = 'conus/modelFrames/VOD_modelFrame1024Big_SEEBiome0.csv')

slope <- statistics$coefficient[statistics$modelName=="NPP"][2]

partialsNppPlot <- partials %>%
  ggplot(aes(x = Area_sumInSqM, y = partialNpp)) +
  geom_bin2d(bins = 20) + 
  scale_fill_cmocean(name="rain", trans = "sqrt") +
  labs(fill = 'Patches (n)') +
  annotate("text",
           x = Inf, y = -Inf,
           label = sprintf("Slope = %.2f, p-value < %.3g", slope, 0.001),
           hjust = 1.5, vjust = -1.5,
           # position="top",
           size = 4, color = "black") +
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


slope <- statistics$coefficient[statistics$modelName=="GPP"][2]
partialsGppPlot <- partials %>%
  ggplot(aes(x = Area_sumInSqM, y = partialGpp)) +
  geom_bin2d(bins = 20) + 
  scale_fill_cmocean(name="rain", trans = "sqrt") +
  labs(fill = 'Patches (n)') +
  annotate("text",
           x = Inf, y = -Inf,
           label = sprintf("Slope = %.2f, p-value < %.3g", slope, 0.001),
           hjust = 1.5, vjust = -1.5,
           # position="top",
           size = 4, color = "black") +
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



slope <- statistics$coefficient[statistics$modelName=="VOD"][2]


partialsVodPlot <- partials %>%
  ggplot(aes(x = Area_sumInSqM, y = partialVod)) +
  geom_bin2d(bins = 20) + 
  scale_fill_cmocean(name="rain", trans = "sqrt") +
  labs(fill = 'Patches (n)') +
  annotate("text",
           x = Inf, y = -Inf,
           label = sprintf("Slope = %.2f, p-value < %.3g", slope, 0.001),
           hjust = 1.5, vjust = -1.5,
           # position="top",
           size = 4, color = "black") +
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



#make a palette for the quantity of biomes needed

numOfBiomes <- filter(summaries, analysis == '1024Big')$biome %>% unique() %>%
  length()

colPal <- RColorBrewer::brewer.pal(numOfBiomes-1, "Paired")
colPal <- c("black", colPal[1:2])
# colPal <- c("black",colPal[2],"purple")

#we exclude the grassland biomes because pixel level analysis says they don't have a size effect
# biomesToExclude <- c('Trop/subtr grass/shrub',  'Medit woodlands/scrub', 'Deserts/xeric shrub')
biomesToExclude <- NULL
# biomesToExclude <- c('Temp grass/shrub')

biomePlotNpp <- ggplot(filter(summaries, n >= 3, analysis == uniqueName, !(biome %in% biomesToExclude))) +
  geom_line(aes(y = nppPartial, x = sizeClass,  group = as.factor(biome), color = as.factor(biome)), linewidth = 1) +
  geom_point(aes(y = nppPartial, x = sizeClass, color = as.factor(biome))) +
  geom_errorbar(aes(ymin = nppPartial-(nppPartialSd/sqrt(n)), ymax = nppPartial+(nppPartialSd/sqrt(n)), color = as.factor(biome), x = sizeClass),
                width = 0, alpha = 0.6) +
  theme_cowplot() +
  scale_color_manual(values = colPal) +
  ylab(bquote('Partial NPP ('*kg~C~ m^-2~yr^-1*')')) +
  theme(legend.title = element_blank(),
        legend.position = c(0.06,0.87),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))

biomePlotGpp <- ggplot(filter(summaries, n >= 3, analysis == uniqueName, !(biome %in% biomesToExclude))) +
  geom_line(aes(y = gppPartial, x = sizeClass,  group = as.factor(biome), color = as.factor(biome)), linewidth = 1) +
  geom_point(aes(y = gppPartial, x = sizeClass, color = as.factor(biome))) +
  geom_errorbar(aes(ymin = gppPartial-(gppPartialSd/sqrt(n)), ymax = gppPartial+(gppPartialSd/sqrt(n)), color = as.factor(biome), x = sizeClass),
                width = 0, alpha = 0.6) +
  theme_cowplot() +
  scale_color_manual(values = colPal) +
  ylab(bquote('Partial GPP ('*kg~C~ m^-2~yr^-1*')')) +
  theme(legend.title = element_blank(),
        legend.position = c(0.06,0.87),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))


biomePlotVod <- ggplot(filter(summaries, n >= 10, analysis == '1024Big', !(biome %in% biomesToExclude))) +
  geom_line(aes(y = vodPartial, x = sizeClass,  group = as.factor(biome), color = as.factor(biome)), linewidth = 1) +
  geom_point(aes(y = vodPartial, x = sizeClass, color = as.factor(biome))) +
  geom_errorbar(aes(ymin = vodPartial-(vodPartialSd/sqrt(n)), ymax = vodPartial+(vodPartialSd/sqrt(n)), color = as.factor(biome), x = sizeClass),
                width = 0, alpha = 0.6) +
  theme_cowplot() +
  scale_color_manual(values = colPal) +
  ylab(bquote('Mean VOD (partial)')) +
  theme(legend.title = element_blank(),
        legend.position = c(0.06,0.87),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))


# layout <- "
#   AD
#   BE
#   CF
#   "

# MainPlot = biomePlotNpp + biomePlotGpp + biomePlotVod + partialsNppPlot + partialsGppPlot + partialsVodPlot +  
#   plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
#   theme(plot.tag = element_text(size=18,family="sans", face="bold"))

layout <- "
  AB
  "

MainPlot = biomePlotVod + partialsVodPlot +  
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
  theme(plot.tag = element_text(size=18,family="sans", face="bold"))

MainPlot
ggsave(paste0('figures/mainTextFigures/figureSX_US_VOD_allBiomes_SEE.pdf'), plot=MainPlot, width=12, height=5)
# xAxis <- ggdraw() + 
#   draw_label(
#     bquote('Patch area ('*m^2*')'),
#     x = 0.5,
#     hjust = 0.5,
#     size = 14
#   ) +
#   theme(
#     plot.margin = margin(0, 0, 0, 0)
#   )
# 
# plot_grid(biomePlotVod, partialsVodPlot,
#           ncol = 2,
#           labels = c('a','b')) %>%
#   plot_grid(., xAxis, ncol = 1, rel_heights = c(0.95, 0.05)) %>%
#   save_plot(filename = 'figures/mainTextFigures/figure2_SX_VOD_BigPatches.png', ., base_width = 12, base_height = 6)
# 
# 
# summaries %>% group_by(biome) %>%summarise(count=sum(n))
