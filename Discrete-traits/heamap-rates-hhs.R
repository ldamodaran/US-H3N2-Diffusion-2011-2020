library(ggplot2)
library(cowplot)

## L2

div_2011 <- read.csv("figure-files/DTBSSVS/HHS/2011-hhs.tsv", sep = '\t', header = TRUE)
div_2012 <- read.csv("figure-files/DTBSSVS/HHS/2012-hhs.tsv", sep = '\t', header = TRUE)
div_2013 <- read.csv("figure-files/DTBSSVS/HHS/2013-hhs.tsv", sep = '\t', header = TRUE)
div_2014 <- read.csv("figure-files/DTBSSVS/HHS/2014-hhs.tsv", sep = '\t', header = TRUE)
div_2015 <- read.csv("figure-files/DTBSSVS/HHS/2015-hhs.tsv", sep = '\t', header = TRUE)
div_2016 <- read.csv("figure-files/DTBSSVS/HHS/2016-hhs.tsv", sep = '\t', header = TRUE)
div_2017 <- read.csv("figure-files/DTBSSVS/HHS/2017-hhs.tsv", sep = '\t', header = TRUE)
div_2018 <- read.csv("figure-files/DTBSSVS/HHS/2018-hhs.tsv", sep = '\t', header = TRUE)
div_2019 <- read.csv("figure-files/DTBSSVS/HHS/2019-hhs.tsv", sep = '\t', header = TRUE)

ptext <- ggplot(div_2012, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,axis.title.x=element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

p1 <- ggplot(div_2011, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

p2 <- ggplot(div_2012, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

p3 <- ggplot(div_2013, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

p4 <- ggplot(div_2014, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

p5 <- ggplot(div_2015, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

p6 <- ggplot(div_2016, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

p7 <- ggplot(div_2017, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

p8 <- ggplot(div_2018, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

p9 <- ggplot(div_2019, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,legend.position="none"
                      ,axis.title.x=element_blank()
                      ,axis.text.y = element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_continuous(limits=c(0, 9))

cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 2)

ggplot(div_2016, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() +
  scale_fill_continuous(limits=c(0, 9))


---------------------------
# Joint HHS
  
jointhhs <- read.csv("figure-files/DTBSSVS/HHS/jointHHS.tsv", sep = '\t')
ptext <- ggplot(jointhhs, aes(x = TO, y= FROM, fill = mean )) +
  geom_tile() + theme(axis.title.y = element_blank()
                      ,axis.title.x=element_blank()
                      ,axis.ticks.y = element_blank()
                      ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_colour_viridis_d(option = "D")

ptext
