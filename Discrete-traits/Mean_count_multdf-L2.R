library(ggplot2)
library(cowplot)
library(reshape)
library(dplyr)

# Lambodhar Damodaran
# 11-2021
## Summarize the number of times season shows up and calculate average mean rate across seasons
# L2

div_2011 <- read.csv("figure-files/DTBSSVS/L2/2011-L2.tsv", sep = '\t', header = TRUE)
div_2012 <- read.csv("figure-files/DTBSSVS/L2/2012-L2.tsv", sep = '\t', header = TRUE)
div_2013 <- read.csv("figure-files/DTBSSVS/L2/2013-L2.tsv", sep = '\t', header = TRUE)
div_2014 <- read.csv("figure-files/DTBSSVS/L2/2014-L2.tsv", sep = '\t', header = TRUE)
div_2015 <- read.csv("figure-files/DTBSSVS/L2/2015-L2.tsv", sep = '\t', header = TRUE)
div_2016 <- read.csv("figure-files/DTBSSVS/L2/2016-L2.tsv", sep = '\t', header = TRUE)
div_2017 <- read.csv("figure-files/DTBSSVS/L2/2017-L2.tsv", sep = '\t', header = TRUE)
div_2018 <- read.csv("figure-files/DTBSSVS/L2/2018-L2.tsv", sep = '\t', header = TRUE)
div_2019 <- read.csv("figure-files/DTBSSVS/L2/2019-L2.tsv", sep = '\t', header = TRUE)

ptext <- ggplot(div_2012, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,axis.title.x=element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))


mergeCols <- c("FROM", "TO")

divall <- merge(div_2011, div_2012, by = mergeCols, all.x = TRUE, suffixes = c(".2011",".2012"))
divall  <- merge(divall, div_2013, by = mergeCols, all.y = TRUE, suffixes = c(".2012",".2013"))
divall  <- merge(divall, div_2014, by = mergeCols, all.y = TRUE, suffixes = c(".2013",".2014"))
divall  <- merge(divall, div_2015, by = mergeCols, all.y = TRUE, suffixes = c(".2014","2015"))
divall  <- merge(divall, div_2016, by = mergeCols, all.y = TRUE, suffixes = c(".2015",".2016"))
divall  <- merge(divall, div_2017, by = mergeCols, all.y = TRUE, suffixes = c(".2016",".2017"))
divall  <- merge(divall, div_2018, by = mergeCols, all.y = TRUE, suffixes = c(".2017",".2018"))
divall  <- merge(divall, div_2019, by = mergeCols, all.y = TRUE, suffixes = c(".2018",".2019"))

divall <- divall[, -c(3,10,17,24,31,38,45,52,59)]

divall_means <- divall[,c(1,2,5,11,17,23,29,35,41,47,53)]
divall_means$na_count <- apply(divall_means, 1, function(x) sum(is.na(x)))

divall_means <- divall_means %>%
  mutate(season_count = 9 - na_count )


ptext2 <- ggplot(divall_means, aes(x = TO, y= FROM, fill = season_count )) +
  geom_tile() + theme(axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_distiller(palette = "YlOrRd",trans = 'reverse') +
  ggtitle("Seasons with supported transitions", subtitle = " H3N2 - USA - 2011-2020 - Louvain 1" ) +
  xlab("SINK") + ylab("SOURCE")

ptext2