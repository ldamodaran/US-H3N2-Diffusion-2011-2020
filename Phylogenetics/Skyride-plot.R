# Lambodhar Damodaran
# 2021-04-21
# Script to visualize csv output file(export from Tracer )from BEAST Bayesian Skyride coalescent analysis
# Input: csv file (remove first line with	GMRF Skyride: xxxxxxxx.log)
# Output: ggplot of skyride wiht 95% HPD 


library(ggplot2)
library(lubridate)


data1 <- read.csv("skyrideouput.csv", header = TRUE)
data1$date <- as.Date(paste(date_decimal(data1$Time)),"%Y-%m-%d")
p1 <- ggplot(data1, aes(y=Mean, x=date)) + geom_line() + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, alpha = 0.2)) +
  theme(legend.position = "bottom") + labs(alpha="95% hpd") + 
  labs(title = "Skyride coalescent") + 
  scale_y_continuous(name ="Log mean pop (N)", trans = 'log10') 


p1

