rm(list=ls())

setwd("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments")

library('tidyverse')
library('reshape2')
library('cowplot')
library('directlabels')

totalPredictions <- read_csv(file = 'conus/totalPredictions/totalPredictionBaseBiome0.csv')

biomes <- c(0,4,5,7,8,12,13)

for(i in 2:length(biomes)) {
  predPath <- paste('conus/totalPredictions/totalPredictionBaseBiome', biomes[i], '.csv', sep = '')
  if(file.exists(predPath) == TRUE) {
    totalPredictions <- read_csv(file = predPath) %>%
      rbind(totalPredictions)
  }
}

totalPredictions$analysis <- 'base'

#for(i in 1:length(biomes)) {
#  predPath <- paste('conus/totalPredictions/totalPredictionNoMgmtBiome', biomes[i], '.csv', sep = '')
#  if(file.exists(predPath) == TRUE) {
#    newPred <- read_csv(file = predPath)
#    newPred$analysis <- 'noMgmt'
#    totalPredictions <- rbind(totalPredictions, newPred)
#  }
#}

for(i in 1:length(biomes)) {
  predPath <- paste('conus/totalPredictions/totalPredictionSizeCutoff5000Biome', biomes[i], '.csv', sep = '')
  if(file.exists(predPath) == TRUE) {
    newPred <- read_csv(file = predPath)
    newPred$analysis <- 'sizeCutoff5000'
    totalPredictions <- rbind(totalPredictions, newPred)
  }
}

#for(i in 1:length(biomes)) {
#  predPath <- paste('conus/totalPredictions/totalPredictionSizeCutoff10000Biome', biomes[i], '.csv', sep = '')
#  if(file.exists(predPath) == TRUE) {
#    newPred <- read_csv(file = predPath)
#    newPred$analysis <- 'sizeCutoff10000'
#    totalPredictions <- rbind(totalPredictions, newPred)
#  }
#}

#for(i in 1:length(biomes)) {
#  predPath <- paste('conus/totalPredictions/totalPredictionTreeCutoff60Biome', biomes[i], '.csv', sep = '')
#  if(file.exists(predPath) == TRUE) {
#    newPred <- read_csv(file = predPath)
#    newPred$analysis <- 'treeCutoff60'
#    totalPredictions <- rbind(totalPredictions, newPred)
#  }
#}

#now we read in the individual patch values

predictionsFullValues <- read_csv(file = 'conus/totalPredictions/totalPredictionFullValuesBase.csv')
predictionsFullValues$analysis <- 'Base'

#which are the different analyses?
analyses <- c('Base' ,'SizeCutoff5000')

for(i in 2:length(analyses)) {
  predPath <- paste('conus/totalPredictions/totalPredictionFullValues', analyses[i], '.csv', sep = '')
  if(file.exists(predPath) == TRUE) {
    newPred <- read_csv(file = predPath)
    newPred$analysis <- analyses[i]
    colnames(predictionsFullValues) <- colnames(newPred)
    predictionsFullValues <- rbind(predictionsFullValues, newPred)
  }
}

rm(newPred)

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

totalPredictions$biome <- biomeDict$.[match(totalPredictions$biome, biomeDict$numb)]

totalPredictions$meanAreaProp <- (totalPredictions$totalNppMeanArea/totalPredictions$totalNpp)*100
totalPredictions$areaProp <- (totalPredictions$totalNpp_mod/totalPredictions$totalNpp)*100

totalPredictions$meanAreaPropGpp <- (totalPredictions$totalGppMeanArea/totalPredictions$totalGpp)*100
totalPredictions$areaPropGpp <- (totalPredictions$totalGpp_mod/totalPredictions$totalGpp)*100

#note these C measurements are for the pathces in this dataset, not necessarily the entire CONUS. thats why we instead multiply the percents by someone else's full CONUS measure

totalPredictions$percIncreaseNpp <- (totalPredictions$totalNpp_mod - totalPredictions$totalNppMeanArea)/totalPredictions$totalNppMeanArea
totalPredictions$mtIncreaseNpp <- totalPredictions$totalNpp_mod - totalPredictions$totalNppMeanArea

totalPredictions$percContributedNpp <- (totalPredictions$totalNpp_mod - totalPredictions$totalNppMeanArea)/totalPredictions$totalNpp_mod

totalPredictions$percIncreaseGpp <- (totalPredictions$totalGpp_mod - totalPredictions$totalGppMeanArea)/totalPredictions$totalGppMeanArea
totalPredictions$mtIncreaseGpp <- totalPredictions$totalGpp_mod - totalPredictions$totalGppMeanArea

totalPredictions$percContributedGpp <- (totalPredictions$totalGpp_mod - totalPredictions$totalGppMeanArea)/totalPredictions$totalGpp_mod

#what was the proportion contributed for the full CONUS?
(totalPredictions %>% filter(biome == 'Full CONUS', analysis == "base"))$percContributedNpp

#order the biomes appropriately
totalPredictions$biome <- totalPredictions$biome %>% ordered(levels = c("Full CONUS",
                                                                        "Deserts/xeric shrub",
                                                                        "Medit woodlands/scrub",
                                                                        "Temp broadleaf/mixed",
                                                                        "Temp conif",
                                                                        "Temp grass/shrub",
                                                                        "Trop/subtr grass/shrub",
                                                                        "Mangroves",
                                                                        "Trop/subtr conif",
                                                                        "Flooded grass/sav")) %>%
  factor(levels = rev(levels(.)))


#a dictionary to give hte analyses the corret names
analysesDict <- data.frame('name' = c('', 'patches >= 5,000 sq m'),
                           'analyses' = analyses)

analysesLowercase <- c('base', 'sizeCutoff5000')

#we exclude these biomes because at pixel scale they have no patch size/npp relationship
# biomesToExclude <- c('Temp grass/shrub', 'Trop/subtr grass/shrub')
biomesToExclude <- NULL

#first we do npp. we do the main text chart first
biomePredictions <- ggplot(filter(totalPredictions, analysis == analysesLowercase[1], !(biome %in% biomesToExclude)) )  +
  geom_segment( aes(x=biome, xend=biome, y=meanAreaProp, yend=areaProp), color="grey") +
  geom_point( aes(x=biome, y=meanAreaProp), shape = 21, fill = "white", size=3 ) +
  geom_point( aes(x=biome, y=areaProp), shape = 16, size=3 ) +
  coord_flip() +
  geom_hline(yintercept = 100, linetype = 2) +
  theme_cowplot() +
  ylab("% of measured total NPP") +
  xlab("") +
  theme(axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12))

predictionsOfInterest <- filter(predictionsFullValues, analysis == analyses[1])
totalNpp <- predictionsOfInterest$NPP_perArea*predictionsOfInterest$Area_sumInSqM

predictionsOfInterest <- dplyr::select(predictionsOfInterest, -NPP_perArea) %>%
  dplyr::select(contains('Npp')) %>%
  melt() %>%
  cbind('total' = rep(totalNpp, length(colnames(.))))

#convert to Mt
predictionsOfInterest$value <- predictionsOfInterest$value/1000000000
predictionsOfInterest$total <- predictionsOfInterest$total/1000000000

#arrange in order to get rid of the top value

#we ignore the first 2 values (because there are 2 variables the top value occurs 2 times)
predictionsOfInterest <- arrange(predictionsOfInterest, desc(total))[3:length(predictionsOfInterest$variable),]

#fix the names of the variables
predictionsOfInterest$variable <- predictionsOfInterest$variable %>% str_replace_all("predictedNpp_meanArea", "No superlinear scaling") %>%
  str_replace_all("predictedNpp", "Superlinear scaling") %>%
  ordered(levels = c('Superlinear scaling', 'No superlinear scaling'))

predictionsOfInterest$shapeV <- '0'
predictionsOfInterest$shapeV[which(predictionsOfInterest$variable == 'Superlinear scaling')] <- '1'

paste("N of patches for ", analyses[1], ": ",
  filter(predictionsOfInterest, variable == 'Superlinear scaling')$value %>% length(),
  sep = "") %>% print()

#this for loop just lists valueable info for writing figure captions
for(i in 1:length(analyses)) {
  paste("N of patches for", analyses[i], ": ",
        length(filter(predictionsFullValues, analysis == analyses[i])$analysis)-1) %>%
    print()
  
  tempPredFull <- filter(predictionsFullValues, analysis == analyses[i])
  
  tempPredFull$npp <- tempPredFull$NPP_perArea*tempPredFull$Area_sumInSqM
  tempPredFull$gpp <- tempPredFull$GPP_perArea*tempPredFull$Area_sumInSqM
  
  paste("Highest NPP value for", analyses[i], ": ",
        max(tempPredFull$npp)/1000000000, sep = '') %>% print()
  
  paste("Highest GPP value for", analyses[i], ": ",
        max(tempPredFull$gpp)/1000000000, sep = '') %>% print()
}

RMSE <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}


rmse_ss <- RMSE(predictionsOfInterest$total[predictionsOfInterest$variable=="Superlinear scaling"], predictionsOfInterest$value[predictionsOfInterest$variable=="Superlinear scaling"])
rmse_nss <- RMSE(predictionsOfInterest$total[predictionsOfInterest$variable=="No superlinear scaling"], predictionsOfInterest$value[predictionsOfInterest$variable=="No superlinear scaling"])

obs <- predictionsOfInterest$total[predictionsOfInterest$variable=="Superlinear scaling"]
predA <- predictionsOfInterest$value[predictionsOfInterest$variable=="Superlinear scaling"]
predB <- predictionsOfInterest$value[predictionsOfInterest$variable=="No superlinear scaling"]

# Compute absolute errors for each prediction
errA <- abs(obs - predA)
errB <- abs(obs - predB)

# Difference in errors (A - B)
diff_err <- errA - errB

t.test(errA, errB, paired = TRUE)

rmse_df <- predictionsOfInterest %>%
  filter(variable %in% c("Superlinear scaling", "No superlinear scaling")) %>%
  group_by(variable) %>%
  summarise(
    RMSE = sqrt(mean((total - value)^2, na.rm = TRUE)),
    .groups = "drop"
  )
rmse_df <- rmse_df %>%
  mutate(
    label = paste0("RMSE = ", round(RMSE, 2)),
    x = 0.05 * max(predictionsOfInterest$total, na.rm = TRUE), # near left edge
    y = 0.95 * max(predictionsOfInterest$total, na.rm = TRUE)  # near top
  )

predObsPlot <- ggplot(filter(predictionsOfInterest, variable %in% c("Superlinear scaling", "No superlinear scaling")),
                             aes(x = value, y = total)) +
  geom_point(aes(shape = shapeV), size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_shape_manual(values = c(1, 16)) +
  cowplot::theme_cowplot() +
  theme(legend.position = 'none') +
  facet_wrap(.~variable) +
  xlim(0, max(predictionsOfInterest$total+1)) + 
  ylim(0, max(predictionsOfInterest$total+1)) +
  xlab(bquote('Predicted total NPP (Mt C '*yr^-1*')')) +
  ylab(bquote('Observed total NPP (Mt C '*yr^-1*')'))+
  geom_text(
    data = rmse_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0
  )

fullPlot <- plot_grid(biomePredictions, predObsPlot,
                      ncol = 1, labels = c('a', 'b'), 
                      rel_heights = c(0.4,0.6))

save_plot(filename = 'figures/mainTextFigures/figure3_6biomes.png', fullPlot, base_height = 6, base_width = 7)

#don't need to do this part

#predictionsOfInterest$variable <- predictionsOfInterest$variable %>% 
#  str_replace_all("No superlinear scaling", "Mean counterfactual") %>%
#  ordered(levels = c('Superlinear scaling', 'Mean counterfactual', '25th percentile' ,'75th percentile'))

#for(i in 1:length(analyses)) {
#  paste("N of patches for ", analyses[i], ": ",
#        filter(predictionsFullValues, analyvariable == 'Superlinear scaling')$value %>% length(),
#        sep = "") %>% print()
#}

#predObsPlot_full <- ggplot(predictionsOfInterest,
#                      aes(x = total, y = value)) +
#  geom_point(aes(shape = shapeV), size = 3) +
#  geom_abline(slope = 1, intercept = 0, linetype = 2) +
#  scale_shape_manual(values = c(1, 16)) +
#  cowplot::theme_cowplot() +
#  theme(legend.position = 'none') +
#  facet_wrap(.~variable) +
#  xlim(0, max(predictionsOfInterest$total+1)) + 
#  ylim(0, max(predictionsOfInterest$total+1)) +
#  xlab(bquote('Predicted total NPP (Mt C '*yr^-1*')')) +
#  ylab(bquote('Observed total NPP (Mt C '*yr^-1*')'))

#ggsave(filename = 'figures/predictionFigures/predObsNppFull.png', predObsPlot_full, width = 7, height = 6)

for(i in 2:length(analyses)) {
  biomePredictions <- ggplot(filter(totalPredictions, analysis == analysesLowercase[i], !(biome %in% biomesToExclude))) +
    geom_segment( aes(x=biome, xend=biome, y=meanAreaProp, yend=areaProp), color="grey") +
    geom_point( aes(x=biome, y=meanAreaProp), shape = 21, fill = "white", size=3 ) +
    geom_point( aes(x=biome, y=areaProp), shape = 16, size=3 ) +
    coord_flip() +
    geom_hline(yintercept = 100, linetype = 2) +
    theme_cowplot() +
    ylab("% of measured total NPP") +
    xlab("") +
    theme(axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 12))
  
  predictionsOfInterest <- filter(predictionsFullValues, analysis == analyses[i])
  totalNpp <- predictionsOfInterest$NPP_perArea*predictionsOfInterest$Area_suminSqM
  
  predictionsOfInterest <- dplyr::select(predictionsOfInterest, -NPP_perArea) %>%
    dplyr::select(contains('Npp')) %>% melt() %>%
    cbind('total' = rep(totalNpp, length(colnames(.))))
  
  #convert to Mt
  predictionsOfInterest$value <- predictionsOfInterest$value/1000000000
  predictionsOfInterest$total <- predictionsOfInterest$total/1000000000
  
  #arrange in order to get rid of the top value
  #we ignore the first 4 values (because there are 4 variables the top value occurs 4 times)
  predictionsOfInterest <- arrange(predictionsOfInterest, desc(total))[5:length(predictionsOfInterest$variable),]
  
  #fix the names of the variables
  predictionsOfInterest$variable <- predictionsOfInterest$variable %>% str_replace_all("predictedNpp_meanArea", "No superlinear scaling") %>%
    str_replace_all("predictedNpp", "Superlinear scaling") %>%
    ordered(levels = c('Superlinear scaling', 'No superlinear scaling'))
  
  predictionsOfInterest$shapeV <- '0'
  predictionsOfInterest$shapeV[which(predictionsOfInterest$variable == 'Superlinear scaling')] <- '1'
  
  predObsPlot <- ggplot(predictionsOfInterest, aes(x = total, y = value)) +
    geom_point(aes(shape = shapeV), size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_shape_manual(values = c(1, 16)) +
    cowplot::theme_cowplot() +
    theme(legend.position = 'none') +
    facet_wrap(.~variable) +
    xlim(0, max(predictionsOfInterest$total+1)) + 
    ylim(0, max(predictionsOfInterest$total+1)) +
    xlab(bquote('Predicted total NPP (Mt C '*yr^-1*')')) +
    ylab(bquote('Observed total NPP (Mt C '*yr^-1*')'))
    
  if(analyses[i] == 'Base') {
    plotTitle <- paste('NPP predictions')
  } else {
    plotTitle <- paste('NPP predictions,', analysesDict$name[i])
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
  
  plotPath <- paste('figures/predictionFigures/npp', analyses[i], '.png', sep = '')
  
  fullPlot <- plot_grid(title, biomePredictions, predObsPlot,
                        ncol = 1, labels = c('', 'a', 'b'), 
                        rel_heights = c(0.1,0.4,0.5))
  
  save_plot(filename = plotPath, fullPlot, base_height = 6, base_width = 7)
}

#now we do gpp

for(i in 1:length(analyses)) {
  
  biomePredictions <- ggplot(filter(totalPredictions, analysis == analysesLowercase[i], !(biome %in% biomesToExclude))) +
    geom_segment( aes(x=biome, xend=biome, y=meanAreaPropGpp, yend=areaPropGpp), color="grey") +
    geom_point( aes(x=biome, y=meanAreaPropGpp), shape = 21, fill = "white", size=3 ) +
    geom_point( aes(x=biome, y=areaPropGpp), shape = 16, size=3 ) +
    coord_flip() +
    geom_hline(yintercept = 100, linetype = 2) +
    theme_cowplot() +
    ylab("% of measured total GPP") +
    xlab("") +
    theme(axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 12))
  
  predictionsOfInterest <- filter(predictionsFullValues, analysis == analyses[i])
  totalGpp <- predictionsOfInterest$GPP_perArea*predictionsOfInterest$Area_suminSqM
  
  predictionsOfInterest <- dplyr::select(predictionsOfInterest, -GPP_perArea) %>%
    dplyr::select(contains('Gpp')) %>% melt() %>%
    cbind('total' = rep(totalGpp, length(colnames(.))))
  
  #convert to Mt
  predictionsOfInterest$value <- predictionsOfInterest$value/1000000000
  predictionsOfInterest$total <- predictionsOfInterest$total/1000000000
  
  #arrange in order to get rid of the top value
  #we ignore the first 2 values (because there are 2 variables the top value occurs 2 times)
  predictionsOfInterest <- arrange(predictionsOfInterest, desc(total))[3:length(predictionsOfInterest$variable),]
  
  #fix the names of the variables
  predictionsOfInterest$variable <- predictionsOfInterest$variable %>% str_replace_all("predictedGpp_meanArea", "No superlinear scaling") %>%
    str_replace_all("predictedGpp", "Superlinear scaling") %>%
    ordered(levels = c('Superlinear scaling', 'No superlinear scaling'))
  
  predictionsOfInterest$shapeV <- '0'
  predictionsOfInterest$shapeV[which(predictionsOfInterest$variable == 'Superlinear scaling')] <- '1'
  
  predObsPlot <- ggplot(predictionsOfInterest, aes(x = total, y = value)) +
    geom_point(aes(shape = shapeV), size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_shape_manual(values = c(1, 16)) +
    cowplot::theme_cowplot() +
    theme(legend.position = 'none') +
    facet_wrap(.~variable) +
    xlim(0, max(predictionsOfInterest$total+1)) + 
    ylim(0, max(predictionsOfInterest$total+1)) +
    xlab(bquote('Predicted total GPP (Mt C '*yr^-1*')')) +
    ylab(bquote('Observed total GPP (Mt C '*yr^-1*')'))
  
  if(analyses[i] == 'Base') {
    plotTitle <- paste('GPP predictions')
  } else {
    plotTitle <- paste('GPP predictions,', analysesDict$name[i])
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
  
  plotPath <- paste('figures/predictionFigures/gpp', analyses[i], '.png', sep = '')
  
  fullPlot <- plot_grid(title, biomePredictions, predObsPlot,
                        ncol = 1, labels = c('', 'a', 'b'), 
                        rel_heights = c(0.1,0.4,0.5))
  
  save_plot(filename = plotPath, fullPlot, base_height = 6, base_width = 7)
}
