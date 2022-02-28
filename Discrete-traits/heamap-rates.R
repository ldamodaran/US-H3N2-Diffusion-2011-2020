library(ggplot2)
library(cowplot)

div_2011 <- read.csv("figure-files/DTBSSVS/Division/2011-div.tsv", sep = '\t', header = TRUE)
div_2012 <- read.csv("figure-files/DTBSSVS/Division/2012-div.tsv", sep = '\t', header = TRUE)
div_2013 <- read.csv("figure-files/DTBSSVS/Division/2013-div.tsv", sep = '\t', header = TRUE)
div_2014 <- read.csv("figure-files/DTBSSVS/Division/2014-div.tsv", sep = '\t', header = TRUE)
div_2015 <- read.csv("figure-files/DTBSSVS/Division/2015-div.tsv", sep = '\t', header = TRUE)
div_2016 <- read.csv("figure-files/DTBSSVS/Division/2016-div.tsv", sep = '\t', header = TRUE)
div_2017 <- read.csv("figure-files/DTBSSVS/Division/2017-div.tsv", sep = '\t', header = TRUE)
div_2018 <- read.csv("figure-files/DTBSSVS/Division/2018-div.tsv", sep = '\t', header = TRUE)
div_2019 <- read.csv("figure-files/DTBSSVS/Division/2019-div.tsv", sep = '\t', header = TRUE)

ptext <- ggplot(div_2012, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1 <- ggplot(div_2011, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <- ggplot(div_2012, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p3 <- ggplot(div_2013, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                     ,legend.position="none"
                     ,axis.title.x=element_blank()
                     ,axis.text.y = element_blank()
                     ,axis.ticks.y = element_blank()
                     ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p4 <- ggplot(div_2014, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p5 <- ggplot(div_2015, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p6 <- ggplot(div_2016, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p7 <- ggplot(div_2017, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p8 <- ggplot(div_2018, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p9 <- ggplot(div_2019, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 2)


