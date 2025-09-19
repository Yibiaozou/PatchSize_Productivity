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

library(ggpmisc)

summaries <- read_csv(file = "conus/biomeSummaries/summaryTableBaseBiome0.csv")

biomes <- c(0,4,5,7,8,12,13)

for(i in 2:length(biomes)) {
  sumPath <- paste('conus/biomeSummaries/summaryTableBaseBiome', biomes[i], '.csv', sep = '')
  if(file.exists(sumPath) == TRUE) {
    summaries <- read_csv(file = sumPath) %>%
      rbind(summaries)
  }
}

summaries$analysis <- 'base'

#for(i in 1:length(biomes)) {
#  sumPath <- paste('conus/biomeSummaries/summaryTableNoMgmtBiome', biomes[i], '.csv', sep = '')
#  if(file.exists(sumPath) == TRUE) {
#    newSum <- read_csv(file = sumPath)
#    newSum$analysis <- 'noMgmt'
#    summaries <- rbind(summaries, newSum)
#  }
#}

#for(i in 1:length(biomes)) {
#  sumPath <- paste('conus/biomeSummaries/summaryTableSizeCutoff10000Biome', biomes[i], '.csv', sep = '')
#  if(file.exists(sumPath) == TRUE) {
#    newSum <- read_csv(file = sumPath)
#    newSum$analysis <- 'sizeCutoff10000'
#    summaries <- rbind(summaries, newSum)
#  }
#}

#for(i in 1:length(biomes)) {
#  sumPath <- paste('conus/biomeSummaries/summaryTableTreeCutoff60Biome', biomes[i], '.csv', sep = '')
#  if(file.exists(sumPath) == TRUE) {
#    newSum <- read_csv(file = sumPath)
#    newSum$analysis <- 'treeCutoff60'
#    summaries <- rbind(summaries, newSum)
#  }
#}

#for(i in 1:length(biomes)) {
#  sumPath <- paste('conus/biomeSummaries/summaryTableUSFSBiome', biomes[i], '.csv', sep = '')
#  if(file.exists(sumPath) == TRUE) {
#    newSum <- read_csv(file = sumPath)
#    newSum$analysis <- 'usfs'
#    summaries <- rbind(summaries, newSum)
#  }
#}

#ndvi gotta be done separately

#summariesNDVI <- read_csv(file = "conus/biomeSummaries/summaryTableNDVIBiome0.csv")
#summariesNDVI$analysis <- 'NDVI'
#for(i in 2:length(biomes)) {
#  sumPath <- paste('conus/biomeSummaries/summaryTableNDVIBiome', biomes[i], '.csv', sep = '')
#  if(file.exists(sumPath) == TRUE) {
#    newSum <- read_csv(file = sumPath)
#    newSum$analysis <- 'NDVI'
#    summariesNDVI <- rbind(summariesNDVI, newSum)
#  }
#}



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

analyses <- c('Base', 'SizeCutoff5000')
analysesLowercase <- c('base', 'sizeCutoff5000')
analysesDict <- data.frame('name' = c('',
                                      'patches >= 5,000 sq m'),
                           'analyses' = analyses)

#first we do the main text figure

#this one for the ambizione application
#ambizData <- filter(summaries, n >= 3, analysis == 'base')

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

partials <- read_csv(file = 'conus/partialsAndResids/partialsAndResidsBaseBiome0.csv')
statistics <- read.csv(file = 'conus/modelFrames/modelFrameBaseBiome0.csv')


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
  # stat_poly_eq(
  #   aes(label = paste(after_stat(eq.label), after_stat(p.value.label), sep = "~~~")),
  #   formula = y ~ x, 
  #   parse = TRUE,
  #   label.x.npc = "left", label.y.npc = "top",
  #   size = 5
  # ) +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 4)+
  geom_smooth(method = 'lm', color = 'red') +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  ylab("") +
  theme(legend.position = c(0.8,0.8), 
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
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 4)+
  geom_smooth(method = 'lm', color = 'red') +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  ylab("") +
  theme(legend.position = c(0.8,0.8), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.height = unit(0.4,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))


slope <- statistics$coefficient[statistics$modelName=="NDVI"][2]
partialsNdviPlot <- partials %>%
  ggplot(aes(x = Area_sumInSqM, y = partialNdvi)) +
  geom_bin2d(bins = 20) + 
  scale_fill_cmocean(name="rain", trans = "sqrt") +
  labs(fill = 'Patches (n)') +
  annotate("text",
           x = Inf, y = -Inf,
           label = sprintf("Slope = %.2f, p-value < %.3g", slope, 0.001),
           hjust = 1.5, vjust = -1.5,
           # position="top",
           size = 4, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 4)+
  geom_smooth(method = 'lm', color = 'red') +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  ylab("") +
  xlab("'Patch area ('*m^2*')'") +
  theme(legend.position = c(0.8,0.24), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.height = unit(0.4,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))

slope <- statistics$coefficient[statistics$modelName=="GPP"][2]
partialsCoverPlot <- partials %>%
  ggplot(aes(x = Area_sumInSqM, y = partialCover)) +
  geom_bin2d(bins = 20) + 
  scale_fill_cmocean(name="rain", trans = "sqrt") +
  labs(fill = 'Patches (n)') +
  annotate("text",
           x = Inf, y = -Inf,
           label = sprintf("Slope = %.2f, p-value < %.3g", slope, 0.001),
           hjust = 1.5, vjust = -1.5,
           # position="top",
           size = 4, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 4)+
  geom_smooth(method = 'lm', color = 'red') +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  ylab("") +
  ylim(18, 110) +
  theme(legend.position = c(0.8,0.24), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.height = unit(0.4,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))

#make a palette for the quantity of biomes needed

numOfBiomes <- filter(summaries, analysis == 'base')$biome %>% unique() %>%
  length()

colPal <- RColorBrewer::brewer.pal(numOfBiomes-1, "Paired")
colPal <- c("black", colPal)

#we exclude the grassland biomes because pixel level analysis says they don't have a size effect
# biomesToExclude <- c('Trop/subtr grass/shrub', 'Temp grass/shrub')
biomesToExclude <- NULL


biomePlotNpp <- ggplot(filter(summaries, n >= 3, analysis == 'base', !(biome %in% biomesToExclude))) +
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

biomePlotGpp <- ggplot(filter(summaries, n >= 3, analysis == 'base', !(biome %in% biomesToExclude))) +
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

biomePlotCover <- ggplot(filter(summaries, n >= 3, analysis == 'base', !(biome %in% biomesToExclude))) +
  geom_line(aes(y = coverPartial, x = sizeClass,  group = as.factor(biome), color = as.factor(biome)), linewidth = 1) +
  geom_point(aes(y = coverPartial, x = sizeClass, color = as.factor(biome))) +
  geom_errorbar(aes(ymin = coverPartial-(coverPartialSd/sqrt(n)), ymax = coverPartial+(coverPartialSd/sqrt(n)), color = as.factor(biome), x = sizeClass),
                width = 0, alpha = 0.6) +
  theme_cowplot() +
  scale_color_manual(values = colPal) +
  ylab("Mean % cover (partial)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.06,0.87),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))

biomePlotNdvi <- ggplot(filter(summaries, n >= 3, analysis == 'base', !(biome %in% biomesToExclude))) +
  geom_line(aes(y = ndviPartial, x = sizeClass,  group = as.factor(biome), color = as.factor(biome)), linewidth = 1) +
  geom_point(aes(y = ndviPartial, x = sizeClass, color = as.factor(biome))) +
  geom_errorbar(aes(ymin = ndviPartial-(ndviPartialSd/sqrt(n)), ymax = ndviPartial+(ndviPartialSd/sqrt(n)), color = as.factor(biome), x = sizeClass),
                width = 0, alpha = 0.6) +
  theme_cowplot() +
  scale_color_manual(values = colPal) +
  ylab("Mean NDVI (partial)") +
  xlab("'Patch area ('*m^2*')'") +
  theme(legend.title = element_blank(),
        legend.position = c(0.65,0.24),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))

layout <- "
AD
BE
CF
"

MainPlot = biomePlotNpp + biomePlotGpp + biomePlotNdvi + partialsNppPlot + partialsGppPlot + partialsNdviPlot +  
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
  theme(plot.tag = element_text(size=18,family="sans", face="bold"))

MainPlot

ggsave("figures/mainTextFigures/figure2_corrected.pdf", plot=MainPlot, width=12, height=16)


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
# plot_grid(biomePlotNpp, partialsNppPlot,
#           biomePlotGpp, partialsGppPlot,
#           biomePlotNdvi, partialsNdviPlot,
#           biomePlotCover, partialsCoverPlot, 
#           ncol = 2,
#           labels = c('a','e', 'b', 'f', 'c', 'g', 'd', 'h')) %>%
#   plot_grid(., xAxis, ncol = 1, rel_heights = c(0.95, 0.05)) %>%
#   save_plot(filename = 'figures/mainTextFigures/figure2_corrected.png', ., base_width = 12, base_height = 12)

#now we do the rest

## it seems the following code does the same analysis as above but for dataset without small patches below 5000 m2. 
for(i in 2:length(analyses)) {
  
  partialsPath <- paste('conus/partialsAndResids/partialsAndResids', analyses[i], 'biome0.csv', sep = '')
  partialsPlot <- read_csv(file = partialsPath) %>%
    ggplot(aes(x = Area_sumInSqM, y = partialNpp)) +
    geom_bin2d(bins = 20) + 
    scale_fill_cmocean(name="rain", trans = "sqrt") +
    labs(fill = 'Patches (n)') +
    geom_smooth(method = 'lm', color = 'red') +
    cowplot::theme_cowplot() +
    scale_x_log10() +
    ylab("") +
    theme(legend.position = c(0.8,0.8), 
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          legend.key.height = unit(0.4,'cm'),
          axis.title.x = element_blank(),
          plot.margin = unit(c(7,7,3,7), 'pt'))
  
  #make a palette for the quantity of biomes needed
  
  numOfBiomes <- filter(summaries, analysis == analysesLowercase[i], !(biome %in% c(biomesToExclude)))$biome %>% unique() %>%
    length()
  
  colPal <- RColorBrewer::brewer.pal(numOfBiomes-1, "Paired")
  colPal <- c("black", colPal)
  
  biomePlot <- ggplot(filter(summaries, n >= 3, analysis == analysesLowercase[i], !(biome %in% biomesToExclude))) +
    geom_line(aes(y = nppPartial, x = sizeClass,  group = as.factor(biome), color = as.factor(biome)), size = 1) +
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

  theme_cowplot()$plot.margin
  
  plotTitle <- paste('NPP,', analysesDict$name[i])
  
  title <- ggdraw() + 
    draw_label(
      plotTitle,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  
  xAxis <- ggdraw() + 
    draw_label(
      bquote('Patch area ('*m^2*')'),
      x = 0.5,
      hjust = 0.5,
      size = 14
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 0)
    )
  
  plotFilename <- paste('figures/biomeSummaryFigures/npp', analyses[i], '.png', sep = '')
  
  plot_grid(biomePlot, partialsPlot, ncol = 2,
            labels = c('a','b')) %>%
    plot_grid(title, ., xAxis, ncol = 1, rel_heights = c(0.1, 0.85, 0.05)) %>%
    save_plot(filename = plotFilename, ., base_width = 12, base_height = 5)
}

#now we do gpp


for(i in 1:length(analyses)) {
  
  partialsPath <- paste('conus/partialsAndResids/partialsAndResids', analyses[i], 'biome0.csv', sep = '')
  partialsPlot <- read_csv(file = partialsPath) %>%
    ggplot(aes(x = Area_suminSqM, y = partialGpp_modArea)) +
    geom_bin2d(bins = 20) + 
    scale_fill_cmocean(name="rain", trans = "sqrt") +
    labs(fill = 'Patches (n)') +
    geom_smooth(method = 'lm', color = 'red') +
    cowplot::theme_cowplot() +
    scale_x_log10() +
    ylab("") +
    theme(legend.position = c(0.8,0.8), 
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          legend.key.height = unit(0.4,'cm'),
          axis.title.x = element_blank(),
          plot.margin = unit(c(7,7,3,7), 'pt'))
  
  #make a palette for the quantity of biomes needed
  
  numOfBiomes <- filter(summaries, analysis == analysesLowercase[i], !(biome %in% c(biomesToExclude)))$biome %>% unique() %>%
    length()
  
  colPal <- RColorBrewer::brewer.pal(numOfBiomes-1, "Paired")
  colPal <- c("black", colPal)
  
  #the legend position needs to vary
  
  #if(analyses[i] %in% c('Base', 'TreeCutoff40')) {
  #  legendPosit <- c(0.7,0.87)
  #} else {
  #  legendPosit <- c(0.06,0.87)
  #}
  
  biomePlot <- ggplot(filter(summaries, n >= 3, analysis == analysesLowercase[i], !(biome %in% biomesToExclude))) +
    geom_line(aes(y = gppPartial_modArea, x = sizeClass,  group = as.factor(biome), color = as.factor(biome)), size = 1) +
    geom_point(aes(y = gppPartial_modArea, x = sizeClass, color = as.factor(biome))) +
    geom_errorbar(aes(ymin = gppPartial_modArea-(gppPartialSd_modArea/sqrt(n)), ymax = gppPartial_modArea+(gppPartialSd_modArea/sqrt(n)), color = as.factor(biome), x = sizeClass),
                  width = 0, alpha = 0.6) +
    theme_cowplot() +
    scale_color_manual(values = colPal) +
    ylab(bquote('Partial GPP ('*kg~C~ m^-2~yr^-1*')')) +
    theme(legend.title = element_blank(),
          legend.position = c(0.06, 0.87),
          legend.text = element_text(size = 9),
          legend.key.height = unit(0.2,'cm'),
          axis.title.x = element_blank(),
          plot.margin = unit(c(7,7,3,7), 'pt'))
  
  if(analyses[i] == 'Base') {
    plotTitle <- paste('GPP')
  } else {
    plotTitle <- paste('GPP,', analysesDict$name[i])
  }
  
  title <- ggdraw() + 
    draw_label(
      plotTitle,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  
  xAxis <- ggdraw() + 
    draw_label(
      bquote('Patch area ('*m^2*')'),
      x = 0.5,
      hjust = 0.5,
      size = 14
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 0)
    )
  
  plotFilename <- paste('figures/biomeSummaryFigures/gpp', analyses[i], '.png', sep = '')
  
  plot_grid(biomePlot, partialsPlot, ncol = 2,
            labels = c('a','b')) %>%
    plot_grid(title, ., xAxis, ncol = 1, rel_heights = c(0.1, 0.85, 0.05)) %>%
    save_plot(filename = plotFilename, ., base_width = 12, base_height = 5)
}

#now we do ndvi

partialsPlot <- read_csv(file = 'conus/partialsAndResids/partialsAndResidsNDVIbiome0.csv') %>%
  ggplot(aes(x = Area_suminSqM, y = partialNDVI_modArea)) +
  geom_bin2d(bins = 20) + 
  scale_fill_cmocean(name="rain", trans = "sqrt") +
  labs(fill = 'Patches (n)') +
  geom_smooth(method = 'lm', color = 'red') +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  ylab("") +
  theme(legend.position = c(0.8,0.8), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.height = unit(0.4,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))

#make a palette for the quantity of biomes needed

numOfBiomes <- filter(summaries, analysis == 'base', !(biome %in% c(biomesToExclude)))$biome %>% unique() %>%
  length()

colPal <- RColorBrewer::brewer.pal(numOfBiomes-1, "Paired")
colPal <- c("black", colPal)

#the column for the ndvi dataset is called npp, but its actually ndvi

biomePlot <- ggplot(filter(summariesNDVI, n >= 3, !(biome %in% biomesToExclude))) +
  geom_line(aes(y = nppPartial_modArea, x = sizeClass,  group = as.factor(biome), color = as.factor(biome)), size = 1) +
  geom_point(aes(y = nppPartial_modArea, x = sizeClass, color = as.factor(biome))) +
  geom_errorbar(aes(ymin = nppPartial_modArea-(nppPartialSd_modArea/sqrt(n)), ymax = nppPartial_modArea+(nppPartialSd_modArea/sqrt(n)), color = as.factor(biome), x = sizeClass),
                width = 0, alpha = 0.6) +
  theme_cowplot() +
  scale_color_manual(values = colPal) +
  ylab('Partial NDVI') +
  theme(legend.title = element_blank(),
        legend.position = c(0.7,0.2),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        axis.title.x = element_blank(),
        plot.margin = unit(c(7,7,3,7), 'pt'))

xAxis <- ggdraw() + 
  draw_label(
    bquote('Patch area ('*m^2*')'),
    x = 0.5,
    hjust = 0.5,
    size = 14
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

plot_grid(biomePlot, partialsPlot, ncol = 2,
          labels = c('a','b')) %>%
  plot_grid(., xAxis, ncol = 1, rel_heights = c(0.95, 0.05)) %>%
  save_plot(filename = 'figures/biomeSummaryFigures/ndvi.png', ., base_width = 12, base_height = 5)
