#install.packages("circlize")
library(circlize)
library(tidytree)
## read PB2 dataframe as an example
par(mfrow = c(1, 2), cex = 0.7)

data <- read.csv("drafts/figures/results/DT-joint/divisionjoint.tsv", header = TRUE, sep = "\t")
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

circos.par(start.degree = 90, gap.after = c("src-EastNorthCentral"= 0,"src-EastSouthCentral"= 0,"src-MiddleAtlantic"= 0,"src-Mountain"= 0,"src-NewEngland"= 0,"src-Pacific"= 0,"src-SouthAtlantic"= 0,"src-WestNorthCentral"= 0,"src-WestSouthCentral"= 30,"snk-EastSouthCentral"= 0,"snk-Mountain"= 0,"snk-Pacific"= 0,"snk-WestNorthCentral"= 0,"snk-MiddleAtlantic"= 0,"snk-NewEngland"= 0,"snk-SouthAtlantic"= 0,"snk-WestSouthCentral"= 0,"snk-EastNorthCentral" = 30))
grid.col = c('src-EastNorthCentral' = "#821f07",'src-EastSouthCentral' = "#821f07",'src-MiddleAtlantic' = "#821f07",'src-Mountain' = "#821f07",'src-NewEngland' = "#821f07",'src-Pacific' = "#821f07",'src-SouthAtlantic' = "#821f07",'src-WestNorthCentral' = "#821f07",'src-WestSouthCentral' = "#821f07",'snk-EastSouthCentral' = "#821f07",'snk-Mountain' = "#821f07",'snk-Pacific' = "#821f07",'snk-WestNorthCentral' = "#821f07",'snk-MiddleAtlantic' = "#821f07",'snk-NewEngland' = "#821f07",'snk-SouthAtlantic' = "#821f07",'snk-WestSouthCentral' = "#821f07",'snk-EastNorthCentral' = "#821f07")


chordDiagram(df[,1:3], order= c("src-EastNorthCentral","src-EastSouthCentral","src-MiddleAtlantic","src-Mountain","src-NewEngland","src-Pacific","src-SouthAtlantic","src-WestNorthCentral","src-WestSouthCentral","snk-EastSouthCentral","snk-Mountain","snk-Pacific","snk-WestNorthCentral","snk-MiddleAtlantic","snk-NewEngland","snk-SouthAtlantic","snk-WestSouthCentral","snk-EastNorthCentral") ,grid.col = grid.col,col = df$BAYES_FACTOR,directional = 1,
             direction.type = c("arrows"),link.arr.type = "big.arrow")

title("Jointly estimated BSSVS - U.S. Census Divisions - 2011-2020")

circos.clear()


