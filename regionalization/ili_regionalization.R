

# Read data
ili <- read.csv("./data-regionalization/state_ili.csv")


# # Examine structure / format / variables
# str(ili)
# summary(ili)

# Select needed / good variables
ili <- ili[,c("region", "week_start", "year", "week", "ilitotal", "num_of_providers", "total_patients")]


# Cleaning
library(dplyr)
library(magrittr)

## formatting, calculations
ili %<>% 
      mutate_at(1, ~factor(.)) %>% 
      mutate_at(2, ~as.Date(., format = "%Y-%m-%d")) %>% 
      mutate(ili_incidence_p100 = ilitotal / total_patients * 100)

# ## re-examine structure / format / variables
# str(ili)
# summary(ili)


# Evaluate missingness
missingness <- ili %>% 
                group_by(region) %>% 
                  summarise(n.missing = sum(is.na(ili_incidence_p100)), 
                            n = n()) %>% 
                  mutate(p.missing = n.missing / n) %>% 
                ungroup() %>% 
                
                arrange(desc(p.missing))

# View(missingness)

## Florida is completely missing, as is Commonwealth of the Northern Mariana Islands
## everywhere else looks manageable

# What to do with non-mainland / non-contiguous 48?


## new florida data

library(readxl)

ili.fl <- read_xlsx("./data-regionalization/Florida_ILINet Data 2011-2021.xlsx", sheet = "Data")

ili.fl.totili <- ili.fl[2:nrow(ili.fl), c(1, which(ili.fl[1,]=="Total ILI"))] %>% 
                  setNames(., nm = c("week", paste("ilitotal", names(ili.fl)[which(substr(names(ili.fl), 1,1)==2)], sep = "_")))

ili.fl.patients <- ili.fl[2:nrow(ili.fl), c(1, which(ili.fl[1,]=="Total Patients Seen"))] %>%
                    setNames(., nm = c("week", paste("total_patients", names(ili.fl)[which(substr(names(ili.fl), 1,1)==2)], sep = "_")))


library(tidyr)

ili.fl.totili.long <- tidyr::pivot_longer(ili.fl.totili, 2:11, names_to = "year", names_prefix = "ilitotal_", values_to = "ilitotal")
ili.fl.patients.long <- tidyr::pivot_longer(ili.fl.patients, 2:11, names_to = "year", names_prefix = "total_patients_", values_to = "total_patients")


ili.fl <- full_join(ili.fl.totili.long, ili.fl.patients.long, by = c("week", "year")) %>% 
          mutate(year = ifelse(week >= 40, substr(year, 1,4), paste0(20,substr(year, 6,7)))) %>%
          mutate_all(~as.numeric(.)) %>%
          mutate(region = "Florida")


ili <- rbind(ili[which(ili$region!="Florida"),c("region", "year", "week", "week_start", "ilitotal", "total_patients")], 
             full_join(ili[which(ili$region=="Florida"), c("region", "year", "week", "week_start")], ili.fl, by = c("region", "year", "week"))) %>% 
        mutate(ili_incidence_p100 = ilitotal / total_patients * 100)

ili <- ili[which(!is.na(ili$week_start)),]


# Evaluate missingness
missingness <- ili %>% 
  group_by(region) %>% 
  summarise(n.missing = sum(is.na(ili_incidence_p100)), 
            n = n()) %>% 
  mutate(p.missing = n.missing / n) %>% 
  ungroup() %>% 
  
  arrange(desc(p.missing))

# View(missingness)



# need to combine new york and new york city

ili.ny <- ili %>% 
          filter(region %in% c("New York", "New York City")) %>% 
          group_by(week_start, week, year) %>% 
            summarise(ilitotal = sum(ilitotal), total_patients = sum(total_patients), region = "New York") %>% 
          ungroup() %>% 
          mutate(ili_incidence_p100 = ilitotal / total_patients * 100)
          

ili <- ili %>% 
        filter(!region %in% c("New York", "New York City")) %>% 
        rbind(., ili.ny)




# Calculate average incidence for entire nation
ili.avg <- ili %>% 
              filter(!is.na(ilitotal) & !is.na(total_patients)) %>% 
              group_by(week_start) %>% 
                summarise(ilitotal = sum(ilitotal, na.rm = T), total_patients = sum(total_patients, na.rm = T)) %>% 
              ungroup() %>% 
              
              mutate(ili_incidence_p100 = ilitotal / total_patients * 100) %>% 
              arrange(week_start)






# plot incidence curves
## messy messy, too many regions

# layout(matrix(c(1,2), ncol = 1), heights = c(3,1))
# layout.show(2)

# png(filename = "./figures-regionalization/incidence_curve.png", height = 4.5, width = 8, units = "in", res = 300, pointsize = 16, family = "sans")
pdf(file = "./figures-regionalization/incidence_curve.pdf", height = 4.5, width = 8, pointsize = 16, family = "sans")

plot.new()
plot.window(ylim = c(0, max(ili$ili_incidence_p100, na.rm = T)+max(ili$ili_incidence_p100, na.rm = T)*0.1), xlim = c(min(ili$week_start, na.rm = T)-7, max(ili$week_start, na.rm = T)+7))

the.months <- seq(min(ili$week_start, na.rm = T), max(ili$week_start, na.rm = T), by = "months")[seq(1, 128, by = 4)]
the.months.labels <- format(the.months, "%b %Y")

axis(1, at = the.months, labels = NA)
text(x = the.months, y = par('usr')[3], labels = the.months.labels, srt = 45, xpd = T, cex = 0.5, adj = c(1.5,1.5))

axis(2, at = c(0:ceiling(max(ili$ili_incidence_p100, na.rm = T)+max(ili$ili_incidence_p100, na.rm = T)*0.1)), las = 1, cex.axis = 0.5)


the.states <- unique(ili$region)

library(viridis)

the.colors <- viridis(length(the.states))

the.ltys <- rep(1:5, ceiling(length(the.states)/5))

for(i in 1:length(the.states)){
  ili.temp <- ili %>% filter(region %in% the.states[i]) %>% arrange(week_start)
  lines(ili.temp$week_start, ili.temp$ili_incidence_p100, col = the.colors[i], lty = the.ltys[i])
}

lines(ili.avg$week_start, ili.avg$ili_incidence_p100, lwd = 3)

title(ylab = "ILI Incidence per 100 Patients", cex.lab = 0.7)

# .image_scale_factor(the.states, col = the.colors, key.length = 1, key.width = 1, key.pos = c(1), add.axis = F, axes = F, xpd = T)

dev.off()








































































# set up for rates of change calculations

## scaffolding dataset to identify implicitly missing data
scaffold <- expand.grid(region = unique(ili$region), week_start = unique(ili$week_start))


ili.extensive <- left_join(scaffold, ili) %>% arrange(region, week_start)


## pivot wider to have matrix of ili incidence, columns for each region
library(tidyr)
ili.wide <- ili.extensive[,c("region", "week_start", "ili_incidence_p100")] %>% pivot_wider(names_from = region, values_from = ili_incidence_p100)

## convert to matrix for calculations
ili.mat <- as.matrix(ili.wide[,-1])

## create numerator and denominator matrixes
ili.num <- ili.mat[-1,]
ili.den <- ili.mat[-nrow(ili.mat),]

## element wise division for this week divided by last week
ili.roc <- ili.num / ili.den

# calculation pairwise region correlations
roc.cor <- cor(ili.roc, use = "pairwise.complete.obs", method = "spearman")






# visualize correlations

library(sf)

## shapefile from census
us.shape <- st_read("./data-regionalization/cb_2018_us_state_5m.shp")
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

# ## centroids to plot lines
# centroids <- st_centroid(st_geometry(us.shape))

library(geosphere)
centroids <- st_as_sf(as.data.frame(centroid(as_Spatial(us.shape))), coords = c(1,2))



## calculate boundary sharing neighbors
library(spdep)

neighbors.list <- poly2nb(us.shape)

# obviously, these places don't have neighbors
# us.shape$NAME[c(28,40,48,49,50,52,56)]
# remove for now; subset shapefile; recalculate centroids and neighbors
non.contig <- c(28,40,48,49,50,52,56)

contig.us <- us.shape[-non.contig,]
contig.us <- contig.us[order(contig.us$NAME),]
# centroids <- st_centroid(st_geometry(contig.us))
centroids <- st_as_sf(as.data.frame(centroid(as_Spatial(contig.us))), coords = c(1,2))
centroids$NAME <- contig.us$NAME
neighbors.list <- poly2nb(contig.us)

## plot to make sure subsetting worked
plot(contig.us$geometry, border = "grey")
plot(centroids, add=T)
plot(neighbors.list, centroids$geometry, add=T)



## make sure to only have correlations for the contiguous states
roc.cor <- roc.cor[-which(!rownames(roc.cor) %in% contig.us$NAME),-which(!colnames(roc.cor) %in% contig.us$NAME)]

### ensure one to one match
rownames(roc.cor)==contig.us$NAME



## convert neighbors list to matrix for isolating correlations
nb.matrix <- nb2mat(neighbors.list, style = "B")

## neighbors only correlation matrix
roc.cor.mat.nb <- nb.matrix*roc.cor
rownames(roc.cor.mat.nb) <- rownames(roc.cor)
colnames(roc.cor.mat.nb) <- colnames(roc.cor)


## check distribution of correlations for good splitting values
hist(roc.cor[upper.tri(roc.cor)])
quantile(roc.cor[upper.tri(roc.cor)], na.rm = T, probs = seq(0,1, by = 0.05))



## chose 0.3, 0.25, and 0.2 for visualizing strongest correlations
roc.cor.mat3 <- roc.cor
roc.cor.mat3[which(roc.cor.mat3<0.3)] <- 0
roc.cor.mat3[which(is.na(roc.cor.mat3))] <- 0


roc.cor.mat25 <- roc.cor
roc.cor.mat25[which(roc.cor.mat25<0.25)] <- 0
roc.cor.mat25[which(is.na(roc.cor.mat25))] <- 0


roc.cor.mat2 <- roc.cor
roc.cor.mat2[which(roc.cor.mat2<0.2)] <- 0
roc.cor.mat2[which(is.na(roc.cor.mat2))] <- 0


## set up matrix for plotting then convert to listw
nb.matrix.weighted3 <- nb.matrix*roc.cor.mat3
nb.matrix.weighted25 <- nb.matrix*roc.cor.mat25
nb.matrix.weighted2 <- nb.matrix*roc.cor.mat2


nb.weighted3 <- mat2listw(nb.matrix.weighted3)
nb.weighted25 <- mat2listw(nb.matrix.weighted25)
nb.weighted2 <- mat2listw(nb.matrix.weighted2)



## actual correlation plot

# png(filename = "./figures-regionalization/rates_of_change_correlation_neighbors.png", height = 5.63, width = 10, units = "in", res = 300, pointsize = 12, family = "sans")
pdf(file = "./figures-regionalization/rates_of_change_correlation_neighbors.pdf", height = 5.63, width = 10, pointsize = 12, family = "sans")

par(mar = c(0,0,0,0), oma = c(0,0,0,0))

plot(contig.us$geometry, border = "grey")
plot(centroids$geometry, add=T)
# plot(neighbors.list, centroids , add=T)
plot(nb.weighted3, st_coordinates(centroids), lwd = 8, points = F, add = T)
plot(nb.weighted25, st_coordinates(centroids), lwd = 2, points = F, add = T)
plot(nb.weighted2, st_coordinates(centroids), lwd = 1, points = F, add = T)


legend("bottomleft", lwd=c(1,2,8), legend = as.expression(lapply(c(0.2,0.25,0.3), function(x) bquote(rho>.(x)))), cex = 0.6, y.intersp = 2)


dev.off()
















# satscan
non.contig <- c("Hawaii", "United States Virgin Islands", "Guam", "Commonwealth of the Northern Mariana Islands", 
                "American Samoa", "Puerto Rico", "Alaska", "Virgin Islands")

ili <- ili %>% filter(!(region %in% non.contig))

incomplete.weeks <- ili %>% 
                    group_by(week_start) %>% 
                    summarise(nna = sum(is.na(ili_incidence_p100))) %>% 
                    arrange(desc(nna)) %>% 
                    filter(nna>0) %>% 
                    select(week_start) %>% c(.) %>% .[[1]]

ili <- ili %>% filter(!(week_start %in% incomplete.weeks)) %>% mutate(region = gsub(" ", "", region))




coords <- cbind(region = gsub(" ", "", centroids$NAME), st_coordinates(centroids))[,c(1,3,2)]
# write.table(coords, file = "./data-regionalization/satscan/input/coordinates.txt", quote = F, row.names = F)




# 
# 
# write.table(ili[, c("region", "week_start", "ilitotal")], file = "./data-regionalization/satscan/input/ilitotal_2011-2021.txt", quote = F, row.names = F)
# write.table(ili[, c("region", "week_start", "total_patients")], file = "./data-regionalization/satscan/input/total_patients_2011-2021.txt", quote = F, row.names = F)
# 
# 
# 
# # by season
# 
# ends <- seq(as.Date("2010-10-01"), as.Date("2021-10-01"), by = "years")[-1]-1
# starts <- seq(as.Date("2010-10-01"), as.Date("2021-10-01"), by = "years")[1:length(ends)]
# 
# 
# for(j in 1:length(starts)){
#   
#   
#   ili.season <- ili %>% filter(week_start >= starts[j] & week_start <= ends[j])
#   
#   write.table(ili.season[, c("region", "week_start", "ilitotal")], file = paste0("./data-regionalization/satscan/input/ilitotal_", paste0(format(starts[j], "%Y"), "-", format(ends[j], "%Y")), ".txt"), quote = F, row.names = F)
#   write.table(ili.season[, c("region", "week_start", "total_patients")], file = paste0("./data-regionalization/satscan/input/total_patients_", paste0(format(starts[j], "%Y"), "-", format(ends[j], "%Y")), ".txt"), quote = F, row.names = F)
#   
# }
# 



# i = 1
# 
# for(i in 1:length(unique(ili$week_start))){
#   write.table(ili[which(ili$week_start==unique(ili$week_start)[i]), c("region", "week_start", "ilitotal")], file = paste0("./data-regionalization/satscan/input/ilitotal_", unique(ili$week_start)[i], ".txt"), quote = F, row.names = F)
#   write.table(ili[which(ili$week_start==unique(ili$week_start)[i]), c("region", "week_start", "total_patients")], file = paste0("./data-regionalization/satscan/input/total_patients_", unique(ili$week_start)[i], ".txt"), quote = F, row.names = F)
# }
# library(readr)
# # set up parameters file manually first run, now edit within loop
# param <- read_file("./data-regionalization/satscan/input/parameters.prm")
# #save original in case errors
# # write_file(param, file = "./data-regionalization/satscan/input/parameters_original.prm")

# # restore original settings
# param <- read_file("./data-regionalization/satscan/input/parameters_original.prm")
# #save
# write_file(param, file = "./data-regionalization/satscan/input/parameters.prm")



# 
# 
# pb <- txtProgressBar(min = 0, max = length(unique(ili$week_start)), style = 3)
# 
# 
# for(i in 1:length(unique(ili$week_start))){
# 
#   # read previous param file
#   param <- read_file("./data-regionalization/satscan/input/parameters.prm")
# 
#   
#   if(i != 1){
#     
#     # substitute date for next iteration
#     ## this changes input and output files
#     param <- gsub(unique(ili$week_start)[i-1], unique(ili$week_start)[i], param)
#     ## this changes the time interval which is set to a single day, throws error if data don't fall within interval
#     param <- gsub(format(unique(ili$week_start)[i-1], "%Y/%m/%d"), format(unique(ili$week_start)[i], "%Y/%m/%d"), param)
# 
#   }
#   
#   # rewrite the param file to be called by satscan
#   readr::write_file(param, file = "./data-regionalization/satscan/input/parameters.prm")
# 
#   Sys.sleep(2)
#   # run satscan program via CMD
#   # # shell doesnt work but system does
#   # # shell('"C:/Program Files/SaTScan/SaTScanBatch.exe" "C:/Users/daile/OneDrive - University of Georgia/US-H3N2-Diffusion/data-regionalization/satscan/input/parameters.prm"', shell = NULL)
#   system('"C:/Program Files/SaTScan/SaTScanBatch.exe" "C:/Users/daile/OneDrive - University of Georgia/US-H3N2-Diffusion/data-regionalization/satscan/input/parameters.prm"')
# 
#   setTxtProgressBar(pb, i)
#   Sys.sleep(3)
#   
# }
# 














# processing satscan results


satscan.files <- list.files("./data-regionalization/satscan/output", full=T)

satscan.output <- list()
for(i in 1:length(satscan.files)){satscan.output[[i]] <- readLines(satscan.files[i])}

clusters.detected <- list()
for(i in 1:length(satscan.output)){clusters.detected[[i]] <- satscan.output[[i]][which(satscan.output[[i]]%in%c("CLUSTERS DETECTED")):which(satscan.output[[i]]%in%c("PARAMETER SETTINGS"))-1]}



individual.clusters <- list()
for(i in 1:length(clusters.detected)){
  loc.indices <- which(grepl("Location IDs included", clusters.detected[[i]]))
  pval.indices <- which(grepl("P-value", clusters.detected[[i]]))
  each.cluster <- list()
  for(ii in 1:length(loc.indices)){
    each.cluster[[ii]] <- clusters.detected[[i]][loc.indices[ii]:pval.indices[ii]]
    while(substr(each.cluster[[ii]][1], nchar(each.cluster[[ii]][1]), nchar(each.cluster[[ii]][1]))==","){
      each.cluster[[ii]] <- c(paste(each.cluster[[ii]][1], gsub("^[[:space:]]{2,}", "", each.cluster[[ii]][2])), each.cluster[[ii]][3:length(each.cluster[[ii]])])
    }
  }
  individual.clusters[[i]] <- each.cluster
}


weeks <- unique(ili$week_start)

individual.clusters.dfs <- list()
for(i in 1:length(individual.clusters)){
  
  weekly.clusters.dfs <- list()
  
  for(ii in 1:length(individual.clusters[[i]])){
    temp <- unlist(lapply(strsplit(individual.clusters[[i]][[ii]], split="[.]{1,}:[[:space:]]"), function(x){purrr::pluck(x, 2)})) %>% t() %>% data.frame("Cluster Number"=gsub("([0-9]+).*$", "\\1", individual.clusters[[i]][[ii]][1]), .)
    temp2 <- c("Cluster Number", unlist(lapply(strsplit(individual.clusters[[i]][[ii]], split="[.]{1,}:[[:space:]]"), function(x){x%>%purrr::pluck(1)%>%gsub("(^[0-9]+[.])|(^\\s{2,})", "", .)})))
    temp3 <- setNames(temp, temp2)
    
    weekly.clusters.dfs[[ii]] <- temp3
  }
  
  individual.clusters.dfs[[i]] <- bind_rows(weekly.clusters.dfs) %>% mutate(week = weeks[i])
  
}


all.clusters.df <- bind_rows(individual.clusters.dfs) %>% mutate("Cluster Size" = NA) %>% select(week, "Cluster Number", "Cluster Size", everything())
all.clusters.df$`Cluster Size` <- sapply(all.clusters.df$`Location IDs included`, function(x){length(unlist(strsplit(x, split=", ")))})


all.clusters.df <- all.clusters.df %>% filter(`P-value` < 0.05/length(weeks))




# all clusters, all weeks





states <- gsub(" ", "", contig.us$NAME)
cluster.df <- data.frame(state = states)
for(i in 1:length(states)){
  cluster.df[,states[i]] <- NA
  for(ii in 1:length(states)){
    cluster.df[which(cluster.df$state==states[ii]),states[i]] <- length(which(grepl(states[i], all.clusters.df$`Location IDs included`) & grepl(states[ii], all.clusters.df$`Location IDs included`)))
  }
}
# cluster.df[1:5,]
# ncol(cluster.df)



# by season

ends <- seq(as.Date("2010-10-01"), as.Date("2021-10-01"), by = "years")[-1]-1
starts <- seq(as.Date("2010-10-01"), as.Date("2021-10-01"), by = "years")[1:length(ends)]


seasonal.cluster.dfs <- list()


for(j in 1:length(starts)){
  
  seasonal.cluster.dfs[[j]] <- data.frame(state = states)

  acdf.season <- all.clusters.df %>% filter(week >= starts[j] & week <= ends[j])
  
  for(i in 1:length(states)){
    seasonal.cluster.dfs[[j]][,states[i]] <- NA
    for(ii in 1:length(states)){
      seasonal.cluster.dfs[[j]][which(seasonal.cluster.dfs[[j]]$state==states[ii]),states[i]] <- length(which(grepl(states[i], acdf.season$`Location IDs included`) & grepl(states[ii], acdf.season$`Location IDs included`)))
    }
  }
  
  
  
}

















cluster.matrix <- as.matrix(cluster.df[,2:ncol(cluster.df)])
rownames(cluster.matrix) <- cluster.df$state


# hist(diag(cluster.matrix))
# quantile(diag(cluster.matrix))
# 
# hist(cluster.matrix[lower.tri(cluster.matrix)])
# quantile(cluster.matrix[lower.tri(cluster.matrix)])


state.relation <- cbind(state = contig.us$NAME, states)
# state.relation

row.names(cluster.matrix) <- state.relation[,1]

colnames(cluster.matrix) <- state.relation[,1]

cluster.matrix.nd <- cluster.matrix
diag(cluster.matrix.nd) <- 0

sequence <- unique(ceiling(quantile(cluster.matrix.nd[lower.tri(cluster.matrix.nd)], probs = seq(1,0, by=-0.01))))


# png(filename = "./figures-regionalization/satscan/spatial_clustering_%03d.png", height = 5.63, width = 10, units = "in", res = 300, pointsize = 12, family = "sans")
# 
# for(i in 1:length(sequence)){
# 
#   cluster.lw.temp <- cluster.matrix.nd%>%as.data.frame()%>%mutate_all(~ifelse(.<sequence[i], 0, .))%>%as.matrix() %>% mat2listw()
# 
#   par(mar = c(0,0,0.1,0), oma = c(0,0,0,0))
# 
#   plot(contig.us$geometry, border = "grey")
#   plot(centroids, add=T)
# 
#   title(main = paste0("States Appearing Together in Clusters At Least ", sequence[i], " Times"), line = -0.75, cex.main = 1, font.main = 1)
# 
# 
#   plot(cluster.lw.temp, st_coordinates(centroids), lwd = 1, points = F, add=T)
# }
# dev.off()
# 
# 
# 
# setwd("./figures-regionalization/satscan")
# shell("dir")
# 
# 
# shell('"C:/Program Files/ImageMagick-7.0.11-Q16-HDRI/magick.exe" convert -delay 50 *.png spatial_clustering.gif')
# 
# 
# 
# setwd("../..")









# modularity and community detection
## https://www.r-bloggers.com/2020/03/community-detection-with-louvain-and-infomap/

library(igraph)

edges <- pivot_longer(cluster.df, 2:50, names_to = "to", values_to = "weight") %>% 
          select(from = state, to, weight) %>% 
          filter(from != to & weight >0)

for(i in 1:nrow(state.relation)){
  edges$from[which(edges$from==state.relation[i,2])] <- state.relation[i,1]
  edges$to[which(edges$to==state.relation[i,2])] <- state.relation[i,1]
}

g <- graph_from_data_frame(edges, directed = FALSE)






lc <- cluster_louvain(g)
clusters <- communities(lc)



# png(filename = "./figures-regionalization/clusters/louvain.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
pdf(file = "./figures-regionalization/clusters/louvain.pdf", height = 5, width = 7, pointsize = 12, family = "sans")

layout(matrix(c(1,2,3,3), ncol = 2, byrow = T), widths = c(1, lcm(3)), heights = c(1, lcm(1*length(clusters))))
par(mar=c(0,0,0,0))
plot(contig.us$geometry, border = "grey")

for(i in 1:length(clusters)){
  plot(contig.us$geometry[which(contig.us$NAME%in%clusters[[i]])], col = viridis(length(clusters))[i], add = T)
}

par(mar=c(0,0,0,0))
# .image_scale_factor(paste("Cluster", names(clusters)), col = viridis(length(clusters)), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(rep("", length(clusters)), col = viridis(length(clusters))[length(clusters):1], key.length = lcm(0.5*length(clusters)), key.width = lcm(3), key.pos = 4)
text(x=rep(1.5,length(clusters)), y=length(clusters):1, labels = paste("Cluster", names(clusters)), xpd=TRUE, cex=10/12, adj = c(0,0.5))

plot.new()


footnote <- lapply(1:length(clusters), function(x){paste("Cluster", x, ":", paste(clusters[[x]], collapse = ", "))}) %>% unlist()
width.ratio <- strwidth(footnote, units = "inches", cex = 8/12) / par('pin')[1]



for(i in 1:length(footnote)){
  
  if(width.ratio[i]>1){
    
    old <- footnote[i]
    new <- c()
    ii = 1
    
    while(strwidth(old, units = "inches", cex = 8/12)/par('pin')[1]>1){
      spaces <- gregexpr(" ", old) %>% unlist()
      stop.here <- spaces[which(sapply(spaces, function(x){strwidth(substr(old, 1, x), units = "inches", cex = 8/12)})>par('pin')[1])[1]-1]
      
      new[ii] <- substr(old, 1, stop.here-1)
      old <- substr(old, stop.here+1, nchar(old))
      ii = ii + 1
    }
    
    footnote[i] <- paste(c(new, old), collapse = "\n")
    
  }
}

footnote <- paste(footnote, collapse = "\n")




text(par('usr')[1], par('usr')[4], adj = c(0,1), labels = footnote, cex = 8/12)

dev.off()




edges1 <- edges %>% filter(to %in% clusters[[1]] & from %in% clusters[[1]])
lc1 <- cluster_louvain(graph_from_data_frame(edges1, directed = FALSE))
sc1 <- communities(lc1)

edges2 <- edges %>% filter(to %in% clusters[[2]] & from %in% clusters[[2]])
lc2 <- cluster_louvain(graph_from_data_frame(edges2, directed = FALSE))
sc2 <- communities(lc2)

edges3 <- edges %>% filter(to %in% clusters[[3]] & from %in% clusters[[3]])
lc3 <- cluster_louvain(graph_from_data_frame(edges3, directed = FALSE))
sc3 <- communities(lc3)


sc <- c(sc1, sc2, sc3)

names(sc) <- 1:length(sc)




# png(filename = "./figures-regionalization/clusters/louvain_subclusters.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
pdf(file = "./figures-regionalization/clusters/louvain_subclusters.pdf", height = 5, width = 7, pointsize = 12, family = "sans")

layout(matrix(c(1,2,3,3), ncol = 2, byrow = T), widths = c(1, lcm(3)), heights = c(1, lcm(0.5*length(sc))))
par(mar=c(0,0,0,0))
plot(contig.us$geometry, border = "grey")


for(i in 1:length(sc)){
  plot(contig.us$geometry[which(contig.us$NAME%in%sc[[i]])], col = viridis(length(sc))[i], add = T)
}

par(mar=c(0,0,0,0))
# .image_scale_factor(paste("Cluster", names(clusters)), col = viridis(length(clusters)), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(rep("", length(sc)), col = viridis(length(sc))[length(sc):1], key.length = lcm(0.5*length(sc)), key.width = lcm(3), key.pos = 4)
text(x=rep(1.5,length(sc)), y=length(sc):1, labels = paste("Cluster", names(sc)), xpd=TRUE, cex=10/12, adj = c(0,0.5))

plot.new()


footnote <- lapply(1:length(sc), function(x){paste("Cluster", x, ":", paste(sc[[x]], collapse = ", "))}) %>% unlist()
width.ratio <- strwidth(footnote, units = "inches", cex = 8/12) / par('pin')[1]



for(i in 1:length(footnote)){
  
  if(width.ratio[i]>1){
    
    old <- footnote[i]
    new <- c()
    ii = 1
    
    while(strwidth(old, units = "inches", cex = 8/12)/par('pin')[1]>1){
      spaces <- gregexpr(" ", old) %>% unlist()
      stop.here <- spaces[which(sapply(spaces, function(x){strwidth(substr(old, 1, x), units = "inches", cex = 8/12)})>par('pin')[1])[1]-1]
      
      new[ii] <- substr(old, 1, stop.here-1)
      old <- substr(old, stop.here+1, nchar(old))
      ii = ii + 1
    }
    
    footnote[i] <- paste(c(new, old), collapse = "\n")
    
  }
}

footnote <- paste(footnote, collapse = "\n")




text(par('usr')[1], par('usr')[4], adj = c(0,1), labels = footnote, cex = 8/12)

dev.off()













n.clusters.p.state <- diag(as.matrix(cluster.df[,-1]))
my.pallette <- viridis(n = diff(range(n.clusters.p.state))+1)[n.clusters.p.state-min(n.clusters.p.state)+1]


png(filename = "./figures-regionalization/satscan_clusters_overall.png", width = 16, height = 9, units = "in", res = 300, pointsize = 12, family = "sans")

# pdf(file = "./figures-regionalization/satscan_clusters_overall.pdf", width = 16, height = 9, pointsize = 12, family = "sans")

layout(matrix(c(1,2), nrow = 2), heights = c(10,1))
plot(contig.us$geometry, border = "grey", col = my.pallette)
# .image_scale(n.clusters.p.state[order(n.clusters.p.state)], my.pallette[order(n.clusters.p.state)], key.length = 1, key.pos = 1)
.image_scale(seq(min(n.clusters.p.state), max(n.clusters.p.state)), viridis(n = diff(range(n.clusters.p.state))+1), key.length = 1, key.pos = 1)


dev.off()
































####################################
#seasonal


# modularity and community detection
## https://www.r-bloggers.com/2020/03/community-detection-with-louvain-and-infomap/

library(igraph)




for(j in 1:length(starts)){
  
  
  edges.seasonal <- pivot_longer(seasonal.cluster.dfs[[j]], 2:50, names_to = "to", values_to = "weight") %>% 
    select(from = state, to, weight) %>% 
    filter(from != to & weight >0)
  
  for(i in 1:nrow(state.relation)){
    edges.seasonal$from[which(edges.seasonal$from==state.relation[i,2])] <- state.relation[i,1]
    edges.seasonal$to[which(edges.seasonal$to==state.relation[i,2])] <- state.relation[i,1]
  }
  
  g.seasonal <- graph_from_data_frame(edges.seasonal, directed = FALSE)
  
  lc.seasonal <- cluster_louvain(g.seasonal)
  clusters.seasonal <- communities(lc.seasonal)
  
  
  
  
  # png(filename = paste("./figures-regionalization/clusters/seasonal/louvain", format(starts[j], '%Y'), paste0(format(ends[j], '%Y'), ".png"),sep = "_"), height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
  # pdf(file = paste("./figures-regionalization/clusters/seasonal/louvain", format(starts[j], '%Y'), paste0(format(ends[j], '%Y'), ".pdf"),sep = "_"), height = 5, width = 7, pointsize = 12, family = "sans")
  
  layout(matrix(c(1,2,3,3), ncol = 2, byrow = T), widths = c(1, lcm(3)), heights = c(1, lcm(1*length(clusters.seasonal))))
  par(mar=c(0,0,0,0))
  plot(contig.us$geometry, border = "grey")
  
  for(i in 1:length(clusters.seasonal)){
    plot(contig.us$geometry[which(contig.us$NAME%in%clusters.seasonal[[i]])], col = viridis(length(clusters.seasonal))[i], add = T)
  }
  
  par(mar=c(0,0,0,0))
  # .image_scale_factor(paste("Cluster", names(clusters)), col = viridis(length(clusters)), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
  .image_scale_factor(rep("", length(clusters.seasonal)), col = viridis(length(clusters.seasonal))[length(clusters.seasonal):1], key.length = lcm(0.5*length(clusters.seasonal)), key.width = lcm(3), key.pos = 4)
  text(x=rep(1.5,length(clusters.seasonal)), y=length(clusters.seasonal):1, labels = paste("Cluster", names(clusters.seasonal)), xpd=TRUE, cex=10/12, adj = c(0,0.5))
  
  plot.new()
  
  
  footnote <- lapply(1:length(clusters.seasonal), function(x){paste("Cluster", x, ":", paste(clusters.seasonal[[x]], collapse = ", "))}) %>% unlist()
  width.ratio <- strwidth(footnote, units = "inches", cex = 8/12) / par('pin')[1]
  
  
  
  for(i in 1:length(footnote)){
    
    if(width.ratio[i]>1){
      
      old <- footnote[i]
      new <- c()
      ii = 1
      
      while(strwidth(old, units = "inches", cex = 8/12)/par('pin')[1]>1){
        spaces <- gregexpr(" ", old) %>% unlist()
        stop.here <- spaces[which(sapply(spaces, function(x){strwidth(substr(old, 1, x), units = "inches", cex = 8/12)})>par('pin')[1])[1]-1]
        
        new[ii] <- substr(old, 1, stop.here-1)
        old <- substr(old, stop.here+1, nchar(old))
        ii = ii + 1
      }
      
      footnote[i] <- paste(c(new, old), collapse = "\n")
      
    }
  }
  
  footnote <- paste(footnote, collapse = "\n")
  
  
  
  
  text(par('usr')[1], par('usr')[4], adj = c(0,1), labels = footnote, cex = 8/12)
  
  # dev.off()
  













sc.seasonal <- c()

for(k in 1:length(clusters.seasonal)){
  edges.seasonal1 <- edges.seasonal %>% filter(to %in% clusters.seasonal[[k]] & from %in% clusters.seasonal[[k]])
  lc.seasonal1 <- cluster_louvain(graph_from_data_frame(edges.seasonal1, directed = FALSE))
  sc.seasonal <- c(sc.seasonal, communities(lc.seasonal1)) 
}


names(sc.seasonal) <- 1:length(sc.seasonal)


# png(filename = paste("./figures-regionalization/clusters/seasonal/louvain_subclusters", format(starts[j], '%Y'), paste0(format(ends[j], '%Y'), ".png"),sep = "_"), height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
# pdf(file = paste("./figures-regionalization/clusters/seasonal/louvain_subclusters", format(starts[j], '%Y'), paste0(format(ends[j], '%Y'), ".pdf"),sep = "_"), height = 5, width = 7, pointsize = 12, family = "sans")

layout(matrix(c(1,2,3,3), ncol = 2, byrow = T), widths = c(1, lcm(3)), heights = c(1, lcm(0.5*length(sc.seasonal))))
par(mar=c(0,0,0,0))
plot(contig.us$geometry, border = "grey")


for(i in 1:length(sc.seasonal)){
  plot(contig.us$geometry[which(contig.us$NAME%in%sc.seasonal[[i]])], col = viridis(length(sc.seasonal))[i], add = T)
}

par(mar=c(0,0,0,0))
# .image_scale_factor(paste("Cluster", names(clusters)), col = viridis(length(clusters)), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(rep("", length(sc.seasonal)), col = viridis(length(sc.seasonal))[length(sc.seasonal):1], key.length = lcm(0.5*length(sc.seasonal)), key.width = lcm(3), key.pos = 4)
text(x=rep(1.5,length(sc.seasonal)), y=length(sc.seasonal):1, labels = paste("Cluster", names(sc.seasonal)), xpd=TRUE, cex=10/12, adj = c(0,0.5))

plot.new()


footnote <- lapply(1:length(sc.seasonal), function(x){paste("Cluster", x, ":", paste(sc.seasonal[[x]], collapse = ", "))}) %>% unlist()
width.ratio <- strwidth(footnote, units = "inches", cex = 8/12) / par('pin')[1]



for(i in 1:length(footnote)){
  
  if(width.ratio[i]>1){
    
    old <- footnote[i]
    new <- c()
    ii = 1
    
    while(strwidth(old, units = "inches", cex = 8/12)/par('pin')[1]>1){
      spaces <- gregexpr(" ", old) %>% unlist()
      stop.here <- spaces[which(sapply(spaces, function(x){strwidth(substr(old, 1, x), units = "inches", cex = 8/12)})>par('pin')[1])[1]-1]
      
      new[ii] <- substr(old, 1, stop.here-1)
      old <- substr(old, stop.here+1, nchar(old))
      ii = ii + 1
    }
    
    footnote[i] <- paste(c(new, old), collapse = "\n")
    
  }
}

footnote <- paste(footnote, collapse = "\n")




text(par('usr')[1], par('usr')[4], adj = c(0,1), labels = footnote, cex = 8/12)

# dev.off()



}















# epidemic curve GIFs



# plot weekly case counts for each county on map, make animated gif








weeks <- unique(ili$week_start)
weeks <- weeks[order(weeks)]


seasons <- rep(NA, length(weeks))

ili$season <- NA

for(i in 1:length(starts)){
  seasons[which(weeks>=starts[i] & weeks<=ends[i])] <- paste0(format(starts[i], "%Y"), "-", format(ends[i], "%Y"))
  
  ili$season[which(ili$week_start>=starts[i] & ili$week_start<=ends[i])] <- paste0(format(starts[i], "%Y"), "-", format(ends[i], "%Y"))
}



# my.breaks <- unique(ceiling(quantile(ili$ili_incidence_p100*1000, probs = seq(0,1, by=0.025))))
my.breaks <- unique(ceiling(quantile(ili$ilitotal, probs = seq(0,1, by=0.025))))

my.palette <- viridis(length(my.breaks)-1)












# setwd("./figures-regionalization/epidemic_curves/cases")
# 
# png(filename = "epidemic_curve_%03d.png", height = 5.63, width = 10, units = "in", res = 300, pointsize = 12, family = "sans")
for(i in 1:length(weeks)){
  layout(matrix(data=c(1,3,2,3), ncol = 2, byrow = T), widths = c(lcm(5*2.54), lcm(5*2.54)), heights = c(lcm(4.5*2.54),lcm(1.13*2.54)))
  # layout.show(3)
  
  # layout(matrix(1:2, nrow = 2), heights = c(1, lcm(3)))
  # layout.show(2)
  
  temp <- left_join(contig.us, full_join(as.data.frame(state.relation), ili[which(ili$week_start==weeks[i]),], by = c("states"="region")), by = c("NAME"="state")) %>% mutate(ili_incidence_p100 = ili_incidence_p100*1000)
  
  # plot(temp['total.cases'],
  #      pal = my.palette,
  #      breaks = my.breaks,
  #      main = paste0("\n Weekly Total COVID-19 Cases \n ", format(weeks[10], "%d %b")),
  #      family = "sans", ps = 12)
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  
  plot(temp$geometry, axes = F)
  plot(temp['ilitotal'], pal = my.palette, breaks = my.breaks, add = T)
  title(main = paste0(format(weeks[i], "%d %b %Y"), "\nTotal ILI Cases"), line = -1, cex.main = 1, font.main = 1)
  
  # .image_scale_factor(c("MP2", "MP1"), col = viridis(2)[2:1], key.length = lcm(2), key.width = lcm(3), key.pos = 4)
  .image_scale_factor(rep("", length(my.palette)), col = my.palette, key.length = 1, key.width = 1, key.pos = c(1), add.axis = F, axes = F, xpd = T)
  # axis(1, at = seq(1.5, length(my.palette)-0.5, by=1), labels = my.breaks[c(-1, -length(my.breaks))], cex.axis = 6/12, las = 0)
  axis(1, at = seq(1.5, length(my.palette)-0.5, by=2), labels = my.breaks[c(-1, -length(my.breaks))][seq(1,length(my.breaks)-2,by=2)], cex.axis = 10/12, las = 0)
  axis(3, at = seq(2.5, length(my.palette)-0.5, by=2), labels = my.breaks[c(-1, -length(my.breaks))][seq(2,length(my.breaks)-2,by=2)], cex.axis = 10/12, las = 0)
  # axis(3, at = seq(1.5, length(my.palette)-0.5, by=1), labels = rep("", length(my.palette)-1), cex.axis = 10/12, las = 2)
  # text(x=seq(1.5, length(my.palette)-0.5, by=1), y=-2, labels = my.breaks[c(-1, -length(my.breaks))], xpd=TRUE, cex=10/12, srt = 45)
  
  
  
  
  
  
  
  temp <- ili %>% filter(season == seasons[i]) %>% group_by(week_start) %>% summarise(n.cases = sum(ilitotal), n.patients = sum(total_patients)) %>% mutate(ili_per_100_patients = n.cases / n.patients *100)
  
  date.labels <- seq(min(temp$week_start), max(temp$week_start), by = "week")
  
  
  
  # my.axis.dates <- seq(starts[which(weeks[i]>=starts & weeks[i]<=ends)], ends[which(weeks[i]>=starts & weeks[i]<=ends)], by = "months")
  
  at.points <- seq(0.5, length(date.labels), by=1)[which(temp$week_start%in%date.labels)]
  
  
  at.points <- at.points[seq(1, length(date.labels), by = 2)]
  date.labels <- date.labels[seq(1, length(date.labels), by = 2)]
  
  
  par(mar = c(4.1, 4.1, 1.1, 0.1))
  barplot(n.cases~week_start, data=temp, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F)
  axis(1, at=at.points, tick=T, labels = F)
  text(x = at.points, y = par("usr")[3], labels = paste(format(date.labels, "%d %b"),' '), srt = 45, pos = 1, xpd = TRUE,cex=0.7, offset = 1.15)
  axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7)
  title(xlab = "Week", ylab = "ILI Cases", cex.lab = 0.9, main = seasons[i], cex.main = 1, font.main = 1)
 
  barplot(n.cases~week_start, data=temp%>%mutate(n.cases = ifelse(week_start==weeks[i], n.cases, 0)), las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = "#FDE725FF", border = "#440154FF", add = T)
  
  
}
# dev.off()

# shell("dir")
# shell('"C:/Program Files/ImageMagick-7.0.11-Q16-HDRI/magick.exe" convert -delay 25 *.png epidemic_curve_map.gif')
# 
# 
# 
# setwd("../../..")



my.breaks <- unique(ceiling(quantile(ili$ili_incidence_p100*1000, probs = seq(0,1, by=0.025))))

my.palette <- viridis(length(my.breaks)-1)


# setwd("./figures-regionalization/epidemic_curves/rates")
# 
# png(filename = "epidemic_curve_%03d.png", height = 5.63, width = 10, units = "in", res = 300, pointsize = 12, family = "sans")
for(i in 1:length(weeks)){
  layout(matrix(data=c(1,3,2,3), ncol = 2, byrow = T), widths = c(lcm(5*2.54), lcm(5*2.54)), heights = c(lcm(4.5*2.54),lcm(1.13*2.54)))
  # layout.show(3)
  
  temp <- left_join(contig.us, full_join(as.data.frame(state.relation), ili[which(ili$week_start==weeks[i]),], by = c("states"="region")), by = c("NAME"="state")) %>% mutate(ili_incidence_p100 = ili_incidence_p100*1000)
  
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  
  plot(temp$geometry, axes = F)
  plot(temp['ili_incidence_p100'], pal = my.palette, breaks = my.breaks, add = T)
  title(main = paste0(format(weeks[i], "%d %b %Y"), "\nILI Cases per 100k Patients"), line = -1, cex.main = 1, font.main = 1)
  
  .image_scale_factor(rep("", length(my.palette)), col = my.palette, key.length = 1, key.width = 1, key.pos = c(1), add.axis = F, axes = F, xpd = T)
  axis(1, at = seq(1.5, length(my.palette)-0.5, by=2), labels = my.breaks[c(-1, -length(my.breaks))][seq(1,length(my.breaks)-2,by=2)], cex.axis = 10/12, las = 0)
  axis(3, at = seq(2.5, length(my.palette)-0.5, by=2), labels = my.breaks[c(-1, -length(my.breaks))][seq(2,length(my.breaks)-2,by=2)], cex.axis = 10/12, las = 0)
 
  
  
  temp <- ili %>% filter(season == seasons[i]) %>% group_by(week_start) %>% summarise(n.cases = sum(ilitotal), n.patients = sum(total_patients)) %>% mutate(ili_per_100_patients = n.cases / n.patients *100)
  
  date.labels <- seq(min(temp$week_start), max(temp$week_start), by = "week")
  
  at.points <- seq(0.5, length(date.labels), by=1)[which(temp$week_start%in%date.labels)]
  
  
  at.points <- at.points[seq(1, length(date.labels), by = 2)]
  date.labels <- date.labels[seq(1, length(date.labels), by = 2)]
  
  
  par(mar = c(4.1, 4.1, 1.1, 0.1))
  barplot(ili_per_100_patients~week_start, data=temp, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F)
  axis(1, at=at.points, tick=T, labels = F)
  text(x = at.points, y = par("usr")[3], labels = paste(format(date.labels, "%d %b"),' '), srt = 45, pos = 1, xpd = TRUE,cex=0.7, offset = 1.15)
  axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7)
  title(xlab = "Week", ylab = "ILI Cases per 100 Patients", cex.lab = 0.9, main = seasons[i], cex.main = 1, font.main = 1)
  
  barplot(ili_per_100_patients~week_start, data=temp%>%mutate(ili_per_100_patients = ifelse(week_start==weeks[i], ili_per_100_patients, 0)), las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F, col = "#FDE725FF", border = "#440154FF", add = T)
  
  
}
# dev.off()
# 
# shell("dir")
# shell('"C:/Program Files/ImageMagick-7.0.11-Q16-HDRI/magick.exe" convert -delay 25 *.png epidemic_curve_map.gif')
# 
# 
# 
# setwd("../../..")









library(ppcor)

?ppcor::pcor()


ili.1112 <- ili %>% filter(season%in%"2011-2012") %>% arrange(region) %>% dplyr::select(region, week_start, ili_incidence_p100) %>% pivot_wider(names_from = region, values_from = ili_incidence_p100)

View(pcor(ili.1112[,-1])$estimate %>% as.data.frame() %>% setNames(., nm = names(ili.1112)[-1]) %>% cbind(region = names(ili.1112)[-1], .))



# > temp <- pcor(ili.1112[,-1], method = "spearman")$estimate
# > diag(temp) <- NA
# > quantile(temp, na.rm = T)
# 0%         25%         50%         75%        100% 
# -0.85085586 -0.25242257  0.01850849  0.29569687  0.85819829 
# > which(temp==max(temp), arr.ind = T)
# row col
# > which(temp==max(temp), arr.ind = T)
# row col
# > which(temp==max(temp, na.rm=T), arr.ind = T)
# row col
# Nevada  27  26
# > View(pcor(ili.1112[,-1], method = "spearman")$estimate %>% as.data.frame() %>% setNames(., nm = names(ili.1112)[-1]) %>% cbind(region = names(ili.1112)[-1], .))



ccf(ili.1112$Georgia, ili.1112$Florida)
plot(ili.1112$week_start, ili.1112$Georgia, type = 'o')
lines(ili.1112$week_start, ili.1112$Florida, lty=3)
lines(ili.1112$week_start, lead(ili.1112$Florida, 11), lty=5, col="red")
# lines(ili.1112$week_start, lag(ili.1112$Georgia, 11), lty=5, col="red")







oijl








































































roc.cor.df <- cbind(from = rownames(roc.cor), as.data.frame(roc.cor)) %>% 
              pivot_longer(2:50, names_to = "to", values_to = "weight") %>%
              filter(from != to & weight > 0)


g <- graph_from_data_frame(roc.cor.df, directed = FALSE)







lc <- cluster_louvain(g)
clusters <- communities(lc)


# imc <- cluster_infomap(g)
# clusters <- communities(imc)


png(filename = "./figures-regionalization/clusters/roc_louvain.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")

layout(matrix(c(1,2,3,3), ncol = 2, byrow = T), widths = c(1, lcm(3)), heights = c(1, lcm(1*length(clusters))))
par(mar=c(0,0,0,0))
plot(contig.us$geometry, border = "grey")

for(i in 1:length(clusters)){
  plot(contig.us$geometry[which(contig.us$NAME%in%clusters[[i]])], col = viridis(length(clusters))[i], add = T)
}

par(mar=c(0,0,0,0))
# .image_scale_factor(paste("Cluster", names(clusters)), col = viridis(length(clusters)), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(rep("", length(clusters)), col = viridis(length(clusters))[length(clusters):1], key.length = lcm(0.5*length(clusters)), key.width = lcm(3), key.pos = 4)
text(x=rep(1.5,length(clusters)), y=length(clusters):1, labels = paste("Cluster", names(clusters)), xpd=TRUE, cex=10/12, adj = c(0,0.5))

plot.new()


footnote <- lapply(1:length(clusters), function(x){paste("Cluster", x, ":", paste(clusters[[x]], collapse = ", "))}) %>% unlist()
width.ratio <- strwidth(footnote, units = "inches", cex = 8/12) / par('pin')[1]



for(i in 1:length(footnote)){
  
  if(width.ratio[i]>1){
    
    old <- footnote[i]
    new <- c()
    ii = 1
    
    while(strwidth(old, units = "inches", cex = 8/12)/par('pin')[1]>1){
      spaces <- gregexpr(" ", old) %>% unlist()
      stop.here <- spaces[which(sapply(spaces, function(x){strwidth(substr(old, 1, x), units = "inches", cex = 8/12)})>par('pin')[1])[1]-1]
      
      new[ii] <- substr(old, 1, stop.here-1)
      old <- substr(old, stop.here+1, nchar(old))
      ii = ii + 1
    }
    
    footnote[i] <- paste(c(new, old), collapse = "\n")
    
  }
}

footnote <- paste(footnote, collapse = "\n")




text(par('usr')[1], par('usr')[4], adj = c(0,1), labels = footnote, cex = 8/12)

dev.off()
























































# 
# imc <- cluster_infomap(g)
# clusters <- communities(imc)
# 
# 
# png(filename = "./figures-regionalization/clusters/infomap.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
# 
# 
# layout(matrix(c(1,2,3,3), ncol = 2, byrow = T), widths = c(1, lcm(3)), heights = c(1, lcm(min(1*length(clusters), 4))))
# par(mar=c(0,0,0,0))
# plot(contig.us$geometry, border = "grey")
# 
# for(i in 1:length(clusters)){
#   plot(contig.us$geometry[which(contig.us$NAME%in%clusters[[i]])], col = viridis(length(clusters))[i], add = T)
# }
# 
# par(mar=c(0,0,0,0))
# # .image_scale_factor(paste("Cluster", names(clusters)), col = viridis(length(clusters)), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
# .image_scale_factor(rep("", length(clusters)), col = viridis(length(clusters))[length(clusters):1], key.length = lcm(0.5*length(clusters)), key.width = lcm(3), key.pos = 4)
# text(x=rep(1.5,length(clusters)), y=length(clusters):1, labels = paste("Cluster", names(clusters)), xpd=TRUE, cex=10/12, adj = c(0,0.5))
# 
# plot.new()
# 
# 
# footnote <- lapply(1:length(clusters), function(x){paste("Cluster", x, ":", paste(clusters[[x]], collapse = ", "))}) %>% unlist()
# width.ratio <- strwidth(footnote, units = "inches", cex = 8/12) / par('pin')[1]
# 
# 
# 
# for(i in 1:length(footnote)){
#   
#   if(width.ratio[i]>1){
#     
#     old <- footnote[i]
#     new <- c()
#     ii = 1
#     
#     while(strwidth(old, units = "inches", cex = 8/12)/par('pin')[1]>1){
#       spaces <- gregexpr(" ", old) %>% unlist()
#       stop.here <- spaces[which(sapply(spaces, function(x){strwidth(substr(old, 1, x), units = "inches", cex = 8/12)})>par('pin')[1])[1]-1]
#       
#       new[ii] <- substr(old, 1, stop.here-1)
#       old <- substr(old, stop.here+1, nchar(old))
#       ii = ii + 1
#     }
#     
#     footnote[i] <- paste(c(new, old), collapse = "\n")
#     
#   }
# }
# 
# footnote <- paste(footnote, collapse = "\n")
# 
# 
# 
# 
# text(par('usr')[1], par('usr')[4], adj = c(0,1), labels = footnote, cex = 2/12)
# 
# dev.off()










# 
# ebc <- cluster_edge_betweenness(g)
# membership(ebc)
# communities(ebc)
# 



































































library(flextable)
# layout(matrix(c(1,2,3,3), nrow = 2), widths = c(4, 6), heights = c(3, 2.63))
# # layout.show(3)
# 
# library(officer)
# 
# 
# ppt <- read_pptx("./output/covid_incidence/satscan/officer_template.pptx")
# ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
# ppt <- ph_with(ppt, value = adj.mat.example, location = ph_location(left = 0, top = 0, width = 6, height = 5.63))
# ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
# ppt <- ph_with(ppt, value = satscan.example, location = ph_location(left = 0, top = 0, width = 4, height = 2.63))
# 
# print(ppt, target = "./output/covid_incidence/satscan/tables.pptx")
# 

satscan.example <- individual.clusters.dfs[[1]][1,2:13] %>% 
  t() %>% 
  data.frame("Clusters Detected" = sapply(
    names(individual.clusters.dfs[[1]])[2:13], 
    function(x){
      string.temp <- x; 
      if(string.temp=="Location IDs included"){
        string.temp <- paste0("1.", string.temp, ".:")
      }
      else{
        string.temp <- paste0("    ", string.temp, paste0(rep(".", 25-2-1-nchar(string.temp)), collapse = ""), ":")
      };
      return(string.temp)}
  ),
  .) %>% 
  setNames(., nm = c("Clusters Detected", " ")) %>% 
  
  flextable() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 5) %>% 
  
  border_remove() %>%
  
  align(part = "all", align = "left") %>% 
  valign(part = "all", valign = "top") %>%
  
  font(fontname = "Lucida Console", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  
  padding(part = "body", i = 2:12, j = 1, padding.left = 20) %>%
  line_spacing(part = "all", space = 1) 

save_as_image(satscan.example, path = "./figures/satscan_example.png")
# satscan.example



# example.cluster <- c("Grant County", "Douglas County", "Adams County", "Franklin County", "Lincoln County", "Kittitas County", "Benton County", "Chelan County", "Yakima County")

example.cluster <- c("Louisiana", "Mississippi", "Arkansas", "Alabama", "Texas", "Oklahoma", "Tennessee")

my.cluster.example <- individual.clusters.dfs[[1]][1,]

cluster.states <- strsplit(my.cluster.example$`Location IDs included`, split = ", ") %>% unlist()

cluster.coordinates <- my.cluster.example$`Coordinates / radius` %>% gsub("\\((.*)\\)\\s.*", "\\1", .) %>% gsub("[[:space:]][A-Za-z]", "", .) %>% strsplit(split = ", ") %>% unlist() %>% as.numeric()
cluster.radius <- my.cluster.example$`Coordinates / radius` %>% gsub(".*\\s/\\s(.*)", "\\1", .) %>% gsub("[[:space:]][A-Za-z].*", "", .) %>% as.numeric()


# function by Gary Weissman on Stack overflow 
## https://stackoverflow.com/questions/23071026/drawing-a-circle-with-a-radius-of-a-defined-distance-in-a-map
plotCircle <- function(x, y, r) {
  angles <- seq(0,2*pi,length.out=360)
  points(x=x, y=y, pch = 8, cex = 2)
  lines(r*cos(angles)+x,r*sin(angles)+y, lwd = 2, lty = 3)
}


png("./figures-regionalization/example_cluster.png", width = 8, height = 4.5, units = "in", res = 300)
par(mar = c(0,0,0,0))
plot(contig.us$geometry, border = "grey")
# plot(centroids$geometry, add=T)

plot(contig.us$geometry[which(contig.us$NAME%in%example.cluster)], col = rgb(1,0,0,0.8), border = "black", add = T)
plot(contig.us$geometry[which(contig.us$NAME%in%c("Louisiana", "Mississippi"))], border = "black", lwd = 4, add = T)
# text(x = st_coordinates(st_centroid(st_geometry(counties[which(counties$county%in%c("Adams County", "Chelan County")),])))[,1], 
#      y = st_coordinates(st_centroid(st_geometry(counties[which(counties$county%in%c("Adams County", "Chelan County")),])))[,2], 
#      labels = c("Adams", "Chelan"), 
#      adj = 0.5, 
#      cex = 1)
plotCircle(x=-cluster.coordinates[2], y=cluster.coordinates[1], r=cluster.radius/111)


dev.off()






example.adj.mat <- cluster.df[17:23,c(1,18:24)]
names(example.adj.mat)[1] <- " "

adj.mat.example <- flextable(example.adj.mat) %>%
  align(part = "all", align = "center") %>%
  align(j = 1, align = "left") %>% 
  border_remove() %>%
  hline(i=1, j=2:8, part = "header", border = fp_border_default(color = "black", width = 3)) %>%
  vline(i=1:7, j=1, part = "body", border = fp_border_default(color = "black", width = 3)) %>%
  bg(part = "all", j = c(2,8), bg = "whitesmoke") %>%
  bg(part = "body", i = c(1,7), bg = "whitesmoke") %>%
  # bg(part = "header", bg = "gainsboro") %>%
  # bg(part = "body", j=1, bg = "gainsboro")
  bg(part = "body", i = c(1,7), j = c(2,8), bg = "gainsboro") %>%
  fontsize(size = 10, part = "all")


save_as_image(adj.mat.example, path = "./figures-regionalization/adj_mat_example.png")






































# 
# ft.ew <- full_join(
#   full_join(
#     b.ew, 
#     cbind(mp = {wa.ew$mp.ew%>%toupper()}[which(wa.ew$has.seqs==1)], wa.ew.centroids.coords)%>%as.data.frame(), 
#     by = c("From" = "mp")),
#   cbind(mp = {wa.ew$mp.ew%>%toupper()}[which(wa.ew$has.seqs==1)], wa.ew.centroids.coords)%>%as.data.frame(), 
#   by = c("To" = "mp"), 
#   suffix = c(".from", ".to")
# ) %>% 
#   filter(BAYES_FACTOR>3)
# ft.ew$med.lwd <- if(is.na(sd(ft.ew$median))){
#   4
# }else{
#   ((ft.ew$median-mean(ft.ew$median))/sd(ft.ew$median)*2)+abs(min((ft.ew$median-mean(ft.ew$median))/sd(ft.ew$median)*2))+1
# }        
# 
# 
# 
# library(diagram)
# 
# 
# png(filename = "./output/sequence_sampling/metapop_ew_tr2.png", height = 5.63, width = 10, units = "in", res = 300, pointsize = 12, family = "sans")
# 
# layout(matrix(1:2, ncol = 2), widths = c(1, lcm(3)))
# par(mar=c(0,0,0,0))
# plot(wa.ew$geometry, border="grey")
# # plot(wa.ew["mp.ew"], col=viridis(2), add=T, border = "grey", lwd = 2.5)
# plot(wa.ew$geometry[which(wa.ew$mp.ew=="West")], col = viridis(2)[1], add = TRUE, lwd = 2.5, border = "grey")
# plot(wa.ew$geometry[which(wa.ew$mp.ew=="East")], col = viridis(2)[2], add = TRUE, lwd = 2.5, border = "grey")
# plot(metapop.categories$geometry[which(!metapop.categories$county2%in%metadata$county)], col = rgb(220/255, 220/255, 220/255, alpha = 1), add = TRUE, lwd = 2.5, border = "grey")
# 
# plot(wa.ew.centroids, pch=19, col="white", add=T, cex=2)
# 
# # points(wa.ew.centroids.coords[1,1], wa.ew.centroids.coords[1,2], col = "white", pch = 19, cex = 2)
# # points(wa.ew.centroids.coords[1,1], wa.ew.centroids.coords[1,2], col = viridis(2)[2], pch = 19, cex = 1)
# # 
# # points(wa.ew.centroids.coords[2,1], wa.ew.centroids.coords[2,2], col = "white", pch = 19, cex = 2)
# # points(wa.ew.centroids.coords[2,1], wa.ew.centroids.coords[2,2], col = viridis(2)[1], pch = 19, cex = 1)
# 
# curvedarrow(ft.ew[,which(grepl(".from", names(ft.ew)))]%>%as.numeric(), ft.ew[,which(grepl(".to", names(ft.ew)))]%>%as.numeric(), lwd = ft.ew$med.lwd, lty = 1, lcol = "white", curve = 0.1, arr.pos = 0.8, segment = c(0.2, 0.8))
# par(mar=c(0,0,0,2.1))
# .image_scale_factor(c("", "", ""), col = c(viridis(2), rgb(220/255, 220/255, 220/255, alpha = 1)), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
# text(x=c(2,2,2), y=c(1, 2, 3), labels = c("West", "East", "No Seqs"), xpd=TRUE, cex=10/12, adj = 0)
# 
# 
# dev.off()





