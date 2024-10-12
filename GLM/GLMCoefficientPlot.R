# Name: GLMcoefficientPlot
# Date: 15 June 2019
# Creator: Xueting Qiu
# Purpose: to generate plot from the summary files of GLM
# Input: output from the GLMSummarize.py and format as xlsx



rm(list=ls())
library(readxl)
GLMSummary <- read.csv("Atlantic_380_seqs_Discrete_FULLMODEL_20220713.csv")


library(ggplot2)
library(ggpubr)

GLMSummary$VariableName = with (GLMSummary, reorder(VariableName, Variable, order = is.ordered(VariableName)))

fp <- ggplot(data=GLMSummary, aes(x=VariableName, y=CoefMedian, ymin=CoefLowerHPD, ymax=CoefUpperHPD)) +
  geom_pointrange() + 
  geom_point(color='skyblue', size = 6) +
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=0 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("Conditional effect size") +
  theme_bw()  # use a white background
#print(fp)

GLMSummary$VariableName = with (GLMSummary, reorder(VariableName, Variable, order = is.ordered(VariableName)))

#draw two lines to indicate BF=3 and BF=100, respectively. They are the posterior probability calculated for BF = 3 and BF=100. 
p<-ggplot(data=GLMSummary, aes(x=VariableName, y=pp)) +
  geom_bar(stat="identity", width=0.65,fill="grey")+
  geom_hline(yintercept=0.091465237,lty=2, lwd = 1, color = "steelblue")+
  geom_hline(yintercept=0.770419904, lwd = 1, color = "steelblue")+
  ylab("Posterior Probability") +
  coord_flip()+
  theme_bw()
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank())
#p

plot1 <- ggarrange(fp, p + rremove("y.text"), widths = c(1,0.4))

annotate_figure(plot1, top = text_grob("GLM Joint Estimate 2011-2020 - H3N2 - USA - Division"))

dev.print(pdf, "GLMplot.pdf")