library(ggplot2)



div_201120 <- read.csv("~/Desktop/Aim1/JointEstimation-H3N2-US-seasons/Jointestim-DT-H3N2-US-L2-500/results/run1/2011-2020-jointrates.tsv", sep = '\t', header = TRUE)
ptext <- ggplot(div_2011, aes(x = To, y= From, fill = mean )) +
  geom_tile() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Actual Rates - 2011-2020 - Joint Estimate") +
  scale_fill_distiller(palette = "YlOrRd",trans = 'reverse') +
  xlab("SINK") + ylab("SOURCE")


ptext

div_201120[div_201120 == 0] <- NA
ptext2 <- ggplot(div_201120, aes(x = To, y= From, fill = BAYES_FACTOR )) +
  geom_tile() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Bayes Factor - 2011-2020 - Joint Estimate") +
  scale_fill_distiller(palette = "YlOrRd",trans = 'reverse') +
  xlab("SINK") + ylab("SOURCE")


ptext2