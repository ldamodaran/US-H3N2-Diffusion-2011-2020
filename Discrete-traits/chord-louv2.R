#install.packages("circlize")
library(circlize)
library(tidytree)
## Chord digram louvain 2
par(mfrow = c(1, 2), cex = 0.7)

data <- read.csv("drafts/figures/results/DT-joint/2011-2020-jointrates-l2.tsv", header = TRUE, sep = "\t")
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

circos.par(start.degree = 90, gap.after = c("src-Cluster1" = 0,"src-Cluster2" = 0,"src-Cluster3" = 0,"src-Cluster4" = 0,"src-Cluster5" = 0,"src-Cluster6" = 0,"src-Cluster7" = 15,"snk-Cluster1" = 0,"snk-Cluster2" = 0,"snk-Cluster3" = 0,"snk-Cluster4" = 0,"snk-Cluster5" = 0,"snk-Cluster6" = 0,"snk-Cluster7" = 15))
grid.col = c('src-Cluster1' = "#821f07",'src-Cluster2' = "#821f07",'src-Cluster3' = "#821f07",'src-Cluster4' = "#821f07",'src-Cluster5' = "#821f07",'src-Cluster6' = "#821f07",'src-Cluster7' = "#821f07",'snk-Cluster1' = "#821f07",'snk-Cluster2' = "#821f07",'snk-Cluster3' = "#821f07",'snk-Cluster4' = "#821f07",'snk-Cluster5' = "#821f07",'snk-Cluster6' = "#821f07",'snk-Cluster7' = "#821f07")


chordDiagram(df[,1:3], order= c("src-Cluster1","src-Cluster2","src-Cluster3","src-Cluster4","src-Cluster5","src-Cluster6","src-Cluster7","snk-Cluster1","snk-Cluster2","snk-Cluster3","snk-Cluster4","snk-Cluster5","snk-Cluster6","snk-Cluster7") ,grid.col = grid.col,col = df$BAYES_FACTOR,directional = 1,
             direction.type = c("arrows"),link.arr.type = "big.arrow")

title("Jointly estimated BSSVS - Louvain - 2011-2020")

circos.clear()


