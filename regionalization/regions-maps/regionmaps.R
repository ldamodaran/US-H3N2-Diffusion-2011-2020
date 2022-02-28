library(ggplot2)
library(dplyr)
library(cowplot)
library(RColorBrewer)

#Lambodhar Damodaran

# read in state map information
all_states <- map_data("state")
names(all_states)[5] <- "State"
all_states$State = toupper(all_states$State)

#read in region breakdown  (gsub reformts column to match the state column of map data)
HHSreg <- read.csv("HHS-regions.csv", header = TRUE )
HHSreg$State <- toupper(HHSreg$State)
HHSreg$State <- gsub("_"," ", as.character(HHSreg$State))

Censusdiv <- read.csv("UScensusDiv.csv", header = TRUE)
Censusdiv$State <- toupper(Censusdiv$State)
Censusdiv$State <- gsub("_"," ", as.character(Censusdiv$State))

louv2 <- read.csv("Louv2.csv", header = TRUE)
louv2$State <- toupper(louv2$State)
louv2$State <- gsub("_"," ", as.character(louv2$State))

# join datasets
regiondata <- left_join(all_states,HHSreg,by="State")
regiondata <- left_join(regiondata,Censusdiv,by="State")
regiondata <- left_join(regiondata,louv2,by="State")



#plots
hhs <- ggplot() + geom_polygon(data=regiondata, aes(x=long, y=lat, group = group, fill=as.factor(regiondata$HHS.region))
                        ,color="grey50") + coord_map() +
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + scale_fill_brewer(palette = "PRGn")


hhs


Cendiv <- ggplot() + geom_polygon(data=regiondata, aes(x=long, y=lat, group = group, fill=as.factor(regiondata$US_Census_Division))
                                  ,color="grey50") + coord_map() +
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + scale_fill_brewer(palette = "PRGn")
  

Cendiv

louv2map <- ggplot() + geom_polygon(data=regiondata, aes(x=long, y=lat, group = group, fill=as.factor(regiondata$Louvain2))
                                  ,color="grey50") + coord_map() +
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + scale_fill_brewer(palette = "PRGn")


Cendiv


# plot figs together
plot_grid(hhs,Cendiv,louv2map, nrow = 3)
