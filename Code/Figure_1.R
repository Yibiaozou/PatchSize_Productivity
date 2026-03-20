
setwd("***/forestFragments")
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


hist(largeFrag$Centroid_lat)

colnames(areas)
colnames(largeFrag)


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

