# setwd("/Volumes/Scratch2/Gabriel/forestFragments")
setwd("D:/Global_Functional_Connectivity/Gabriel_PatchSize_NPP/forestFragments")
library('dplyr')
library('tidyr')
library('readr')
library('ggplot2')
library('cowplot')
library('stringr')
source('code/functions/cleanFragFrameFunction.R')

gc()

areas <- read_csv("data/hansenConus2Feb2024.csv", col_types = cols(.default = "n"))
largeFrag <- read_csv("data/bigPatches_hansen2010.csv", col_types = cols(.default = "n", label = 'c'))

largeFragVOD <- read_csv("data/global_tree_richness_BigPatchVOD_9km_2010.csv", col_types = cols(.default = "n", label = 'c'))

hist(largeFrag$Centroid_lat)
hist(largeFragVOD$Centroid_lat)

LM1 <- lm(VOD_mean~Area_sumInSqM, data=largeFragVOD)
summary(LM1)


colnames(areas)
colnames(largeFrag)
colnames(largeFragVOD)

#get rid of duplicated rows
areas <- areas %>% distinct()
largeFrag <- largeFrag %>% distinct()

#no labels duplicated between the two
#which(largeFrag$label %in% frag$labels)

#largeFrag <- rename(largeFrag, 'label' = 'labels')

#combine the small and large patches CSV
areas <- bind_rows(select(largeFrag, one_of(colnames(areas))),
                            select(areas, one_of(colnames(largeFrag))))

rm(largeFrag)

#areas <- select(areas, -label)

areas <- cleanFragFrame(areas)

areas <- select(areas, Area_sumInSqM)

areas$sizeClass <- "< 1e12"
areas$sizeClass[which(areas$Area_sumInSqM < 1e11)] <- "< 1e11"
areas$sizeClass[which(areas$Area_sumInSqM < 1e10)] <- "< 1e10"
areas$sizeClass[which(areas$Area_sumInSqM < 1e9)] <- "< 1e9"
areas$sizeClass[which(areas$Area_sumInSqM < 1e8)] <- "< 1e8"
areas$sizeClass[which(areas$Area_sumInSqM < 1e7)] <- "< 1e7"
areas$sizeClass[which(areas$Area_sumInSqM < 1e6)] <- "< 1e6"
areas$sizeClass[which(areas$Area_sumInSqM < 1e5)] <- "< 1e5"
areas$sizeClass[which(areas$Area_sumInSqM < 1e4)] <- "< 1e4"

areas$sizeClass <- factor(areas$sizeClass,
                          levels = c("< 1e4",
                                     "< 1e5",
                                     "< 1e6",
                                     "< 1e7",
                                     "< 1e8",
                                     "< 1e9",
                                     "< 1e10",
                                     "< 1e11",
                                     "< 1e12"))

areaSummary <- areas %>% group_by(sizeClass) %>% 
  summarize(n = n(),
            minSize = min(Area_sumInSqM),
            maxSize = max(Area_sumInSqM),
            totalArea = sum(Area_sumInSqM))

areaSummary$totalArea_prop <- areaSummary$totalArea/sum(areaSummary$totalArea)
areaSummary$n_prop <- areaSummary$n/sum(areaSummary$n)

areaSummary$cumuArea <- (cumsum(areaSummary$totalArea)/sum(areaSummary$totalArea))*max(log10(areaSummary$n))
areaSummary$cumuN <- (cumsum(areaSummary$n)/sum(areaSummary$n))*max(log10(areaSummary$n))

lineFrame <- data.frame(
  proportion = c(areaSummary$cumuArea, areaSummary$cumuN), 
  label = c(rep("Area", 9), rep("Patches", 9)),
  sizeClass = c(areaSummary$sizeClass, areaSummary$sizeClass))

coeff <-(1/max(log10(areaSummary$n)))*100

colPal <- RColorBrewer::brewer.pal(6, "Paired")

colPal <- c(colPal[2], colPal[6])

pareto <- ggplot(areaSummary) +
  geom_bar(aes(y = log10(n+1), x = sizeClass), stat = "identity") +
  geom_line(aes(y= proportion, color = label, x = sizeClass, group = label), size=1, data = lineFrame) +
  xlab(bquote('Patch area ('*m^2*')')) +
  scale_y_continuous(name = expression(paste(log[10],"(number of patches + 1)")), 
                     sec.axis = sec_axis(trans = ~.*coeff, name = "% of total")) +
  theme_cowplot() +
  scale_color_manual(values = colPal) +
  theme(legend.title = element_blank(),
        legend.position = c(0.82, 0.82),
        legend.text = element_text(size = 10),
        legend.key.height = unit(0.2,'cm'))

ggsave("figures/mainTextFigures/pareto.pdf", pareto, width = 7, height = 5)

####now the power analysis

models <- read_csv(file = "conus/modelFrames/modelFrameBaseBiome0.csv")

models <- filter(models, modelName == "NPP",
                 !(rowname %in% c("condR2", "margR2"))) %>%
  select(rowname, coefficient, meanCovariate, biome)

intercepts <- filter(models, rowname != "log10(Area_suminSqM)")
intercepts$meanCovariate[1] <- 1

intercept <- sum(intercepts$coefficient * intercepts$meanCovariate)
coefLog10 <- models$coefficient[which(models$rowname == "log10(Area_suminSqM)")]

#calculate the CDF to get alpha
areas <- arrange(areas, Area_sumInSqM)
areas$i <- seq(1:length(areas$Area_sumInSqM))
areas$cdf <- areas$i/length(areas$i)
areas$tranCdf <- log(1-areas$cdf)

#drop the top observation because it is NA (log of 0)
areasCDF <- areas[-length(areas$tranCdf),]

alpha <- lm(tranCdf ~ log(Area_sumInSqM), data = areasCDF)$coefficients[2]

totalArea <- sum(areaSummary$totalArea)
minSize <- min(areaSummary$minSize)
maxSize <- max(areaSummary$maxSize)

equationTotal <- function(x, log10Area, intercept, minSize, maxSize, total) {
  
  natLogArea <- log10Area/log(10)
  
  return(((intercept - natLogArea/(1-x) + natLogArea * log(minSize) + 
    (natLogArea * log(maxSize/minSize))/(1-(minSize/maxSize)^(1-x)) ) *
    total)/1000000000)
}

equationTotalPercent <- function(x, log10Area, intercept, minSize, maxSize, total) {
  ((equationTotal(x, log10Area, intercept, minSize, maxSize, total) - equationTotal(abs(alpha), log10Area, intercept, minSize, maxSize, total))/
     equationTotal(abs(alpha), log10Area, intercept, minSize, maxSize, total))*100
}

ribbonOne <- data.frame(x = seq(from = abs(alpha)*0.5, abs(alpha), length.out = 100))
ribbonOne$ymax <- equationTotalPercent(ribbonOne$x, coefLog10, intercept, minSize, maxSize, totalArea)
ribbonOne$ymin <- 0

ribbonTwo <- data.frame(x = seq(from = abs(alpha), abs(alpha)*1.5, length.out = 100))
ribbonTwo$ymax <- 0
ribbonTwo$ymin <- equationTotalPercent(ribbonTwo$x, coefLog10, intercept, minSize, maxSize, totalArea)

alphaChart <- ggplot(areaSummary) +
  stat_function(fun = equationTotalPercent,
                args = list(log10Area = coefLog10, 
                            intercept = intercept,
                            minSize = minSize,
                            maxSize = maxSize,
                            total = totalArea)) +
  geom_ribbon(data = ribbonOne,
              aes(ymin = 0, ymax = ymax,
                  x = x, y = 0), fill = "#3E8E6EFF", alpha = 0.5) + 
  geom_ribbon(data = ribbonTwo, 
              aes(ymin = ymin, ymax = 0, xmin = abs(alpha), xmax = abs(alpha)*1.25,
                  x = x, y = 0), fill = "#B5B38AFF", alpha = 0.5) + 
  annotate(geom="point", x = abs(alpha), 
           y=0, size = 3) +
  annotate(geom="point", x = abs(alpha)*0.5, shape = 21, fill = "white", 
           y=equationTotalPercent(abs(alpha)*0.5, coefLog10, intercept, minSize, maxSize, totalArea),
           size = 3) +
  annotate(geom="point", x = abs(alpha)*1.5, shape = 21, fill = "white", 
           y=equationTotalPercent(abs(alpha)*1.5, coefLog10, intercept, minSize, maxSize, totalArea),
           size = 3) +
  xlim(0.3, 1.2) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  xlab(expression(alpha)) +
  ylab("Percent change in total NPP") +
  theme_cowplot()

equationTotalPercent(abs(alpha)*1.5, coefLog10, intercept, minSize, maxSize, totalArea)
equationTotalPercent(abs(alpha)*0.5, coefLog10, intercept, minSize, maxSize, totalArea)

#gotta save it as svg because the pdf version is messed up when opened in illustrator
ggsave("figures/mainTextFigures/alphaChart.svg", alphaChart, width = 6, height = 5)

