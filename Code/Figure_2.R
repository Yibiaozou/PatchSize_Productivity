rm(list=ls())

setwd("**/forestFragments")
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


summaries$sizeClass <- summaries$sizeClass %>% ordered(levels = c('< 1e4',
                                                                  '< 1e5',
                                                                  '< 1e6',
                                                                  '< 1e7',
                                                                  '< 1e8',
                                                                  '< 1e9',
                                                                  '< 1e10',
                                                                  '< 1e11',
                                                                  '< 1e12'))


#now we need the partials

analyses <- c('Base', 'SizeCutoff5000')
analysesLowercase <- c('base', 'sizeCutoff5000')
analysesDict <- data.frame('name' = c('',
                                      'patches >= 5,000 sq m'),
                           'analyses' = analyses)


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
  # xlab("'Patch area ('*m^2*')'") +
  xlab(bquote('Patch area ('*m^2*')'))+
  theme(legend.position = c(0.8,0.24), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.height = unit(0.4,'cm'),
        # axis.title.x = element_blank(),
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

# colPal <- RColorBrewer::brewer.pal(numOfBiomes-1, "Paired")

colPal <- c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4")
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
  xlab(bquote('Patch area ('*m^2*')'))+
  theme(legend.title = element_blank(),
        legend.position = c(0.65,0.24),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.2,'cm'),
        # axis.title.x = element_blank(),
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

ggsave("figures/mainTextFigures/Figures_NEE_R3/Fig_2.pdf", plot=MainPlot, width=12, height=16)


