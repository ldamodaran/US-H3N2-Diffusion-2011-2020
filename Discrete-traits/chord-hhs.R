#install.packages("circlize")
library(circlize)
library(tidytree)
## read PB2 dataframe as an example
par(mfrow = c(1, 2), cex = 0.7)

data <- read.csv("drafts/figures/results/DT-joint/jointHHS.tsv", header = TRUE, sep = "\t")
df <- data.frame(data)
df$FROM<-gsub("_","",as.character(df$FROM))
df$TO<-gsub("_","",as.character(df$TO))
df$FROM <- paste0("src-", df$FROM)
df$FROM <- gsub('\\s+', '', df$FROM)
df$TO <- paste0("snk-", df$TO)
df$TO <- gsub('\\s+', '', df$TO)
df$BF <- df$BAYES_FACTOR

df$BAYES_FACTOR <- with(df, ifelse(BAYES_FACTOR < 3, "Grey",
                                   ifelse(BAYES_FACTOR < 10, "#ff7b7b",
                                          ifelse(BAYES_FACTOR < 30, "#ff5252",
                                                 ifelse(BAYES_FACTOR < 100, "#ff0000", "#a70000")))
                                   
))

circos.par(start.degree = 90, gap.after = c("src-Region1-Boston" = 0,"src-Region2-New_York" = 0,"src-Region3-Philadelphia" = 0,"src-Region4-Atlanta" = 0,"src-Region5-Chicago" = 0,"src-Region6-Dallas" = 0,"src-Region7-Kansas_City" = 0,"src-Region8-Denver" = 0,"src-Region9-San_Francisco" = 0,"src-Region10-Seattle" = 30,"snk-Region1-Boston" = 0,"snk-Region2-New_York" = 0,"snk-Region3-Philadelphia" = 0,"snk-Region4-Atlanta" = 0,"snk-Region5-Chicago" = 0,"snk-Region6-Dallas" = 0,"snk-Region7-Kansas_City" = 0,"snk-Region8-Denver" = 0,"snk-Region9-San_Francisco" = 0,"snk-Region10-Seattle" = 30))
grid.col = c("src-Region1-Boston" = "#821f07","src-Region2-New_York" = "#821f07","src-Region3-Philadelphia" = "#821f07","src-Region4-Atlanta" = "#821f07","src-Region5-Chicago" = "#821f07","src-Region6-Dallas" = "#821f07","src-Region7-Kansas_City" = "#821f07","src-Region8-Denver" = "#821f07","src-Region9-San_Francisco" = "#821f07","src-Region10-Seattle" = "#821f07","snk-Region1-Boston" = "#821f07","snk-Region2-New_York" = "#821f07","snk-Region3-Philadelphia" = "#821f07","snk-Region4-Atlanta" = "#821f07","snk-Region5-Chicago" = "#821f07","snk-Region6-Dallas" = "#821f07","snk-Region7-Kansas_City" = "#821f07","snk-Region8-Denver" = "#821f07","snk-Region9-San_Francisco" = "#821f07","snk-Region10-Seattle" = "#821f07")


chordDiagram(df[,1:3],grid.col = grid.col,col = df$BAYES_FACTOR,directional = 1,
             direction.type = c("arrows"),link.arr.type = "big.arrow")

title("Jointly estimated BSSVS - HHS regions - 2011-2020")

circos.clear()


