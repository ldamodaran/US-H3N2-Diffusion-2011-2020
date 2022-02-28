# Lambodhar Damodaran
# 10-2021
# Script to sumamrize census data averages for different regionalization schema 
# For transportation data

library(dplyr)
library(tidyverse)

airtravel_data <- read.csv("~/Desktop/Aim1/Main-data/Transportation-data/air/Airtravel-passengers.csv", header = TRUE)
amtrak_data <- read.csv("~/Desktop/Aim1/Main-data/Transportation-data/amtrak/amtrak_bordings.csv", header = TRUE)
highway_data_persons_data <- read.csv("~/Desktop/Aim1/Main-data/Transportation-data/highway/highway-persontrips.csv", header = TRUE)
highway_data_vehicmiles_data <- read.csv("~/Desktop/Aim1/Main-data/Transportation-data/highway/highway-vehicmiles.csv", header = TRUE)



##########################
census_regions_NE <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania")
census_regions_MW <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin","Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
census_regions_S <- c("Delaware","District of Columbia","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Alabama","Kentucky","Mississippi","Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
census_regions_W <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming","Alaska","California","Hawaii","Oregon","Washington")


#########
#air
airtravel_data_NE <- airtravel_data[airtravel_data$State %in% census_regions_NE,]

airtravel_data_NE_2019 <- filter(airtravel_data_NE, Year == '2019')
NE_2019_mean <- colMeans(airtravel_data_NE_2019[4])
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- t(NE_2019_mean)
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- tibble::rownames_to_column(NE_2019_mean, "datayear")

airtravel_data_MW <- airtravel_data[airtravel_data$State %in% census_regions_MW,]
airtravel_data_MW_2019 <- filter(airtravel_data_MW, Year == '2019')
MW_2019_mean <- colMeans(airtravel_data_MW_2019[4])
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- t(MW_2019_mean)
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- tibble::rownames_to_column(MW_2019_mean, "datayear")


airtravel_data_S <- airtravel_data[airtravel_data$State %in% census_regions_S,]
airtravel_data_S_2019 <- filter(airtravel_data_S, Year == '2019')
S_2019_mean <- colMeans(airtravel_data_S_2019[4])
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- t(S_2019_mean)
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- tibble::rownames_to_column(S_2019_mean, "datayear")

airtravel_data_W <- airtravel_data[airtravel_data$State %in% census_regions_W,]
airtravel_data_W_2019 <- filter(airtravel_data_W, Year == '2019')
W_2019_mean <- colMeans(airtravel_data_W_2019[4])
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- t(W_2019_mean)
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- tibble::rownames_to_column(W_2019_mean, "datayear")


regions_all_air_2019 <- bind_rows(NE_2019_mean,MW_2019_mean,S_2019_mean,W_2019_mean)
print(regions_all_air_2019)
write.table(regions_all_air_2019, file = "regions_air_2019.tsv", row.names = FALSE, sep = "\t")



#amtrak
amtrak_data_NE <- amtrak_data[amtrak_data$State %in% census_regions_NE,]

amtrak_data_NE_2019 <- filter(amtrak_data_NE, Year == '2019')
NE_2019_mean <- colMeans(amtrak_data_NE_2019[4])
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- t(NE_2019_mean)
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- tibble::rownames_to_column(NE_2019_mean, "datayear")

amtrak_data_MW <- amtrak_data[amtrak_data$State %in% census_regions_MW,]
amtrak_data_MW_2019 <- filter(amtrak_data_MW, Year == '2019')
MW_2019_mean <- colMeans(amtrak_data_MW_2019[4])
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- t(MW_2019_mean)
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- tibble::rownames_to_column(MW_2019_mean, "datayear")


amtrak_data_S <- amtrak_data[amtrak_data$State %in% census_regions_S,]
amtrak_data_S_2019 <- filter(amtrak_data_S, Year == '2019')
S_2019_mean <- colMeans(amtrak_data_S_2019[4])
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- t(S_2019_mean)
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- tibble::rownames_to_column(S_2019_mean, "datayear")

amtrak_data_W <- amtrak_data[amtrak_data$State %in% census_regions_W,]
amtrak_data_W_2019 <- filter(amtrak_data_W, Year == '2019')
W_2019_mean <- colMeans(amtrak_data_W_2019[4])
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- t(W_2019_mean)
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- tibble::rownames_to_column(W_2019_mean, "datayear")


regions_all_amtrak_2019 <- bind_rows(NE_2019_mean,MW_2019_mean,S_2019_mean,W_2019_mean)
print(regions_all_amtrak_2019)
write.table(regions_all_amtrak_2019, file = "regions_amtrak_2019.tsv", row.names = FALSE, sep = "\t")


#highway_data_transitpass
highway_data_transitpass_data_NE <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_regions_NE,]

highway_data_transitpass_data_NE_2019 <- filter(highway_data_transitpass_data_NE, Year == '2019')
NE_2019_mean <- colMeans(highway_data_transitpass_data_NE_2019[4])
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- t(NE_2019_mean)
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- tibble::rownames_to_column(NE_2019_mean, "datayear")

highway_data_transitpass_data_MW <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_regions_MW,]
highway_data_transitpass_data_MW_2019 <- filter(highway_data_transitpass_data_MW, Year == '2019')
MW_2019_mean <- colMeans(highway_data_transitpass_data_MW_2019[4])
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- t(MW_2019_mean)
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- tibble::rownames_to_column(MW_2019_mean, "datayear")


highway_data_transitpass_data_S <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_regions_S,]
highway_data_transitpass_data_S_2019 <- filter(highway_data_transitpass_data_S, Year == '2019')
S_2019_mean <- colMeans(highway_data_transitpass_data_S_2019[4])
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- t(S_2019_mean)
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- tibble::rownames_to_column(S_2019_mean, "datayear")

highway_data_transitpass_data_W <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_regions_W,]
highway_data_transitpass_data_W_2019 <- filter(highway_data_transitpass_data_W, Year == '2019')
W_2019_mean <- colMeans(highway_data_transitpass_data_W_2019[4])
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- t(W_2019_mean)
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- tibble::rownames_to_column(W_2019_mean, "datayear")


regions_all_highway_data_transitpass_2019 <- bind_rows(NE_2019_mean,MW_2019_mean,S_2019_mean,W_2019_mean)
print(regions_all_highway_data_transitpass_2019)
write.table(regions_all_highway_data_transitpass_2019, file = "regions_highway_data_transitpass_2019.tsv", row.names = FALSE, sep = "\t")

#highway_data_vehicmiles
highway_data_vehicmiles_data_NE <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_regions_NE,]

highway_data_vehicmiles_data_NE_2019 <- filter(highway_data_vehicmiles_data_NE, Year == '2019')
NE_2019_mean <- colMeans(highway_data_vehicmiles_data_NE_2019[4])
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- t(NE_2019_mean)
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- tibble::rownames_to_column(NE_2019_mean, "datayear")

highway_data_vehicmiles_data_MW <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_regions_MW,]
highway_data_vehicmiles_data_MW_2019 <- filter(highway_data_vehicmiles_data_MW, Year == '2019')
MW_2019_mean <- colMeans(highway_data_vehicmiles_data_MW_2019[4])
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- t(MW_2019_mean)
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- tibble::rownames_to_column(MW_2019_mean, "datayear")


highway_data_vehicmiles_data_S <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_regions_S,]
highway_data_vehicmiles_data_S_2019 <- filter(highway_data_vehicmiles_data_S, Year == '2019')
S_2019_mean <- colMeans(highway_data_vehicmiles_data_S_2019[4])
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- t(S_2019_mean)
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- tibble::rownames_to_column(S_2019_mean, "datayear")

highway_data_vehicmiles_data_W <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_regions_W,]
highway_data_vehicmiles_data_W_2019 <- filter(highway_data_vehicmiles_data_W, Year == '2019')
W_2019_mean <- colMeans(highway_data_vehicmiles_data_W_2019[4])
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- t(W_2019_mean)
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- tibble::rownames_to_column(W_2019_mean, "datayear")


regions_all_highway_data_vehicmiles_2019 <- bind_rows(NE_2019_mean,MW_2019_mean,S_2019_mean,W_2019_mean)
print(regions_all_highway_data_vehicmiles_2019)
write.table(regions_all_highway_data_vehicmiles_2019, file = "regions_highway_data_vehicmiles_2019.tsv", row.names = FALSE, sep = "\t")






###########################
census_divisions <- c("East_North_Central","East_South_Central","Middle_Atlantic","Mountain","New_England","Pacific","South_Atlantic","West_North_Central","West_South_Central")
census_divisions_ENC <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin")
census_divisions_ESC <- c("Alabama","Kentucky","Mississippi","Tennessee")
census_divisions_MA <- c("New Jersey","New York", "Pennsylvania")
census_divisions_MNT <- c("Arizona","Colorado","Idaho","Montana","New Mexico", "Nevada","Utah","Wyoming")
census_divisions_NEdiv <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")
census_divisions_P <- c("California","Oregon","Washington")
census_divisions_SA <- c("Delaware","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia")
census_divisions_WNC <- c("Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
census_divisions_WSC <- c("Arkansas","Louisiana","Oklahoma","Texas")


# air 
airtravel_data_ENC <- airtravel_data[airtravel_data$State %in% census_divisions_ENC,]
airtravel_data_ENC_2019 <- filter(airtravel_data_ENC, Year == '2019')
ENC_2019_mean <- colMeans(airtravel_data_ENC_2019[4])
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- t(ENC_2019_mean)
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- tibble::rownames_to_column(ENC_2019_mean, "dataYear")


airtravel_data_ESC <- airtravel_data[airtravel_data$State %in% census_divisions_ESC,]
airtravel_data_ESC_2019 <- filter(airtravel_data_ESC, Year == '2019')
ESC_2019_mean <- colMeans(airtravel_data_ESC_2019[4])
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- t(ESC_2019_mean)
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- tibble::rownames_to_column(ESC_2019_mean, "dataYear")

airtravel_data_MA <- airtravel_data[airtravel_data$State %in% census_divisions_MA,]
airtravel_data_MA_2019 <- filter(airtravel_data_MA, Year == '2019')
MA_2019_mean <- colMeans(airtravel_data_MA_2019[4])
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- t(MA_2019_mean)
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- tibble::rownames_to_column(MA_2019_mean, "dataYear")

airtravel_data_MNT <- airtravel_data[airtravel_data$State %in% census_divisions_MNT,]
airtravel_data_MNT_2019 <- filter(airtravel_data_MNT, Year == '2019')
MNT_2019_mean <- colMeans(airtravel_data_MNT_2019[4])
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- t(MNT_2019_mean)
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- tibble::rownames_to_column(MNT_2019_mean, "dataYear")


airtravel_data_NEdiv <- airtravel_data[airtravel_data$State %in% census_divisions_NEdiv,]
airtravel_data_NE_div_2019 <- filter(airtravel_data_NEdiv, Year == '2019')
NE_div_2019_mean <- colMeans(airtravel_data_NE_div_2019[4])
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- t(NE_div_2019_mean)
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- tibble::rownames_to_column(NE_div_2019_mean, "dataYear")


airtravel_data_P <- airtravel_data[airtravel_data$State %in% census_divisions_P,]
airtravel_data_P_2019 <- filter(airtravel_data_P, Year == '2019')
P_2019_mean <- colMeans(airtravel_data_P_2019[4])
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- t(P_2019_mean)
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- tibble::rownames_to_column(P_2019_mean, "dataYear")

airtravel_data_SA <- airtravel_data[airtravel_data$State %in% census_divisions_SA,]
airtravel_data_SA_2019 <- filter(airtravel_data_SA, Year == '2019')
SA_2019_mean <- colMeans(airtravel_data_SA_2019[4])
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- t(SA_2019_mean)
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- tibble::rownames_to_column(SA_2019_mean, "dataYear")


airtravel_data_WNC <- airtravel_data[airtravel_data$State %in% census_divisions_WNC,]
airtravel_data_WNC_2019 <- filter(airtravel_data_WNC, Year == '2019')
WNC_2019_mean <- colMeans(airtravel_data_WNC_2019[4])
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- t(WNC_2019_mean)
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- tibble::rownames_to_column(WNC_2019_mean, "dataYear")


airtravel_data_WSC <- airtravel_data[airtravel_data$State %in% census_divisions_WSC,]
airtravel_data_WSC_2019 <- filter(airtravel_data_WSC, Year == '2019')
WSC_2019_mean <- colMeans(airtravel_data_WSC_2019[4])
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- t(WSC_2019_mean)
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- tibble::rownames_to_column(WSC_2019_mean, "dataYear")


division_all_air_2019 <- bind_rows(ENC_2019_mean,ESC_2019_mean,MA_2019_mean,MNT_2019_mean,NE_div_2019_mean,P_2019_mean,SA_2019_mean,WNC_2019_mean,WSC_2019_mean)
print(division_all_air_2019)
write.table(division_all_air_2019, file = "divisionall_air_2019.tsv", row.names = FALSE, sep = "\t")


# amtrak 
amtrak_data_ENC <- amtrak_data[amtrak_data$State %in% census_divisions_ENC,]
amtrak_data_ENC_2019 <- filter(amtrak_data_ENC, Year == '2019')
ENC_2019_mean <- colMeans(amtrak_data_ENC_2019[4])
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- t(ENC_2019_mean)
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- tibble::rownames_to_column(ENC_2019_mean, "dataYear")


amtrak_data_ESC <- amtrak_data[amtrak_data$State %in% census_divisions_ESC,]
amtrak_data_ESC_2019 <- filter(amtrak_data_ESC, Year == '2019')
ESC_2019_mean <- colMeans(amtrak_data_ESC_2019[4])
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- t(ESC_2019_mean)
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- tibble::rownames_to_column(ESC_2019_mean, "dataYear")

amtrak_data_MA <- amtrak_data[amtrak_data$State %in% census_divisions_MA,]
amtrak_data_MA_2019 <- filter(amtrak_data_MA, Year == '2019')
MA_2019_mean <- colMeans(amtrak_data_MA_2019[4])
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- t(MA_2019_mean)
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- tibble::rownames_to_column(MA_2019_mean, "dataYear")

amtrak_data_MNT <- amtrak_data[amtrak_data$State %in% census_divisions_MNT,]
amtrak_data_MNT_2019 <- filter(amtrak_data_MNT, Year == '2019')
MNT_2019_mean <- colMeans(amtrak_data_MNT_2019[4])
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- t(MNT_2019_mean)
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- tibble::rownames_to_column(MNT_2019_mean, "dataYear")


amtrak_data_NEdiv <- amtrak_data[amtrak_data$State %in% census_divisions_NEdiv,]
amtrak_data_NE_div_2019 <- filter(amtrak_data_NEdiv, Year == '2019')
NE_div_2019_mean <- colMeans(amtrak_data_NE_div_2019[4])
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- t(NE_div_2019_mean)
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- tibble::rownames_to_column(NE_div_2019_mean, "dataYear")


amtrak_data_P <- amtrak_data[amtrak_data$State %in% census_divisions_P,]
amtrak_data_P_2019 <- filter(amtrak_data_P, Year == '2019')
P_2019_mean <- colMeans(amtrak_data_P_2019[4])
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- t(P_2019_mean)
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- tibble::rownames_to_column(P_2019_mean, "dataYear")

amtrak_data_SA <- amtrak_data[amtrak_data$State %in% census_divisions_SA,]
amtrak_data_SA_2019 <- filter(amtrak_data_SA, Year == '2019')
SA_2019_mean <- colMeans(amtrak_data_SA_2019[4])
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- t(SA_2019_mean)
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- tibble::rownames_to_column(SA_2019_mean, "dataYear")


amtrak_data_WNC <- amtrak_data[amtrak_data$State %in% census_divisions_WNC,]
amtrak_data_WNC_2019 <- filter(amtrak_data_WNC, Year == '2019')
WNC_2019_mean <- colMeans(amtrak_data_WNC_2019[4])
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- t(WNC_2019_mean)
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- tibble::rownames_to_column(WNC_2019_mean, "dataYear")


amtrak_data_WSC <- amtrak_data[amtrak_data$State %in% census_divisions_WSC,]
amtrak_data_WSC_2019 <- filter(amtrak_data_WSC, Year == '2019')
WSC_2019_mean <- colMeans(amtrak_data_WSC_2019[4])
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- t(WSC_2019_mean)
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- tibble::rownames_to_column(WSC_2019_mean, "dataYear")


division_all_amtrak_2019 <- bind_rows(ENC_2019_mean,ESC_2019_mean,MA_2019_mean,MNT_2019_mean,NE_div_2019_mean,P_2019_mean,SA_2019_mean,WNC_2019_mean,WSC_2019_mean)
print(division_all_amtrak_2019)
write.table(division_all_amtrak_2019, file = "divisionall_amtrak_2019.tsv", row.names = FALSE, sep = "\t")


# highway_data_transitpass 
highway_data_transitpass_data_ENC <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_divisions_ENC,]
highway_data_transitpass_data_ENC_2019 <- filter(highway_data_transitpass_data_ENC, Year == '2019')
ENC_2019_mean <- colMeans(highway_data_transitpass_data_ENC_2019[4])
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- t(ENC_2019_mean)
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- tibble::rownames_to_column(ENC_2019_mean, "dataYear")


highway_data_transitpass_data_ESC <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_divisions_ESC,]
highway_data_transitpass_data_ESC_2019 <- filter(highway_data_transitpass_data_ESC, Year == '2019')
ESC_2019_mean <- colMeans(highway_data_transitpass_data_ESC_2019[4])
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- t(ESC_2019_mean)
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- tibble::rownames_to_column(ESC_2019_mean, "dataYear")

highway_data_transitpass_data_MA <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_divisions_MA,]
highway_data_transitpass_data_MA_2019 <- filter(highway_data_transitpass_data_MA, Year == '2019')
MA_2019_mean <- colMeans(highway_data_transitpass_data_MA_2019[4])
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- t(MA_2019_mean)
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- tibble::rownames_to_column(MA_2019_mean, "dataYear")

highway_data_transitpass_data_MNT <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_divisions_MNT,]
highway_data_transitpass_data_MNT_2019 <- filter(highway_data_transitpass_data_MNT, Year == '2019')
MNT_2019_mean <- colMeans(highway_data_transitpass_data_MNT_2019[4])
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- t(MNT_2019_mean)
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- tibble::rownames_to_column(MNT_2019_mean, "dataYear")


highway_data_transitpass_data_NEdiv <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_divisions_NEdiv,]
highway_data_transitpass_data_NE_div_2019 <- filter(highway_data_transitpass_data_NEdiv, Year == '2019')
NE_div_2019_mean <- colMeans(highway_data_transitpass_data_NE_div_2019[4])
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- t(NE_div_2019_mean)
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- tibble::rownames_to_column(NE_div_2019_mean, "dataYear")


highway_data_transitpass_data_P <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_divisions_P,]
highway_data_transitpass_data_P_2019 <- filter(highway_data_transitpass_data_P, Year == '2019')
P_2019_mean <- colMeans(highway_data_transitpass_data_P_2019[4])
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- t(P_2019_mean)
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- tibble::rownames_to_column(P_2019_mean, "dataYear")

highway_data_transitpass_data_SA <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_divisions_SA,]
highway_data_transitpass_data_SA_2019 <- filter(highway_data_transitpass_data_SA, Year == '2019')
SA_2019_mean <- colMeans(highway_data_transitpass_data_SA_2019[4])
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- t(SA_2019_mean)
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- tibble::rownames_to_column(SA_2019_mean, "dataYear")


highway_data_transitpass_data_WNC <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_divisions_WNC,]
highway_data_transitpass_data_WNC_2019 <- filter(highway_data_transitpass_data_WNC, Year == '2019')
WNC_2019_mean <- colMeans(highway_data_transitpass_data_WNC_2019[4])
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- t(WNC_2019_mean)
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- tibble::rownames_to_column(WNC_2019_mean, "dataYear")


highway_data_transitpass_data_WSC <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% census_divisions_WSC,]
highway_data_transitpass_data_WSC_2019 <- filter(highway_data_transitpass_data_WSC, Year == '2019')
WSC_2019_mean <- colMeans(highway_data_transitpass_data_WSC_2019[4])
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- t(WSC_2019_mean)
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- tibble::rownames_to_column(WSC_2019_mean, "dataYear")


division_all_highway_data_transitpass_2019 <- bind_rows(ENC_2019_mean,ESC_2019_mean,MA_2019_mean,MNT_2019_mean,NE_div_2019_mean,P_2019_mean,SA_2019_mean,WNC_2019_mean,WSC_2019_mean)
print(division_all_highway_data_transitpass_2019)
write.table(division_all_highway_data_transitpass_2019, file = "divisionall_highway_data_transitpass_2019.tsv", row.names = FALSE, sep = "\t")


# highway_data_vehicmiles 
highway_data_vehicmiles_data_ENC <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_divisions_ENC,]
highway_data_vehicmiles_data_ENC_2019 <- filter(highway_data_vehicmiles_data_ENC, Year == '2019')
ENC_2019_mean <- colMeans(highway_data_vehicmiles_data_ENC_2019[4])
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- t(ENC_2019_mean)
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- tibble::rownames_to_column(ENC_2019_mean, "dataYear")


highway_data_vehicmiles_data_ESC <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_divisions_ESC,]
highway_data_vehicmiles_data_ESC_2019 <- filter(highway_data_vehicmiles_data_ESC, Year == '2019')
ESC_2019_mean <- colMeans(highway_data_vehicmiles_data_ESC_2019[4])
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- t(ESC_2019_mean)
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- tibble::rownames_to_column(ESC_2019_mean, "dataYear")

highway_data_vehicmiles_data_MA <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_divisions_MA,]
highway_data_vehicmiles_data_MA_2019 <- filter(highway_data_vehicmiles_data_MA, Year == '2019')
MA_2019_mean <- colMeans(highway_data_vehicmiles_data_MA_2019[4])
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- t(MA_2019_mean)
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- tibble::rownames_to_column(MA_2019_mean, "dataYear")

highway_data_vehicmiles_data_MNT <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_divisions_MNT,]
highway_data_vehicmiles_data_MNT_2019 <- filter(highway_data_vehicmiles_data_MNT, Year == '2019')
MNT_2019_mean <- colMeans(highway_data_vehicmiles_data_MNT_2019[4])
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- t(MNT_2019_mean)
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- tibble::rownames_to_column(MNT_2019_mean, "dataYear")


highway_data_vehicmiles_data_NEdiv <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_divisions_NEdiv,]
highway_data_vehicmiles_data_NE_div_2019 <- filter(highway_data_vehicmiles_data_NEdiv, Year == '2019')
NE_div_2019_mean <- colMeans(highway_data_vehicmiles_data_NE_div_2019[4])
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- t(NE_div_2019_mean)
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- tibble::rownames_to_column(NE_div_2019_mean, "dataYear")


highway_data_vehicmiles_data_P <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_divisions_P,]
highway_data_vehicmiles_data_P_2019 <- filter(highway_data_vehicmiles_data_P, Year == '2019')
P_2019_mean <- colMeans(highway_data_vehicmiles_data_P_2019[4])
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- t(P_2019_mean)
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- tibble::rownames_to_column(P_2019_mean, "dataYear")

highway_data_vehicmiles_data_SA <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_divisions_SA,]
highway_data_vehicmiles_data_SA_2019 <- filter(highway_data_vehicmiles_data_SA, Year == '2019')
SA_2019_mean <- colMeans(highway_data_vehicmiles_data_SA_2019[4])
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- t(SA_2019_mean)
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- tibble::rownames_to_column(SA_2019_mean, "dataYear")


highway_data_vehicmiles_data_WNC <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_divisions_WNC,]
highway_data_vehicmiles_data_WNC_2019 <- filter(highway_data_vehicmiles_data_WNC, Year == '2019')
WNC_2019_mean <- colMeans(highway_data_vehicmiles_data_WNC_2019[4])
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- t(WNC_2019_mean)
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- tibble::rownames_to_column(WNC_2019_mean, "dataYear")


highway_data_vehicmiles_data_WSC <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% census_divisions_WSC,]
highway_data_vehicmiles_data_WSC_2019 <- filter(highway_data_vehicmiles_data_WSC, Year == '2019')
WSC_2019_mean <- colMeans(highway_data_vehicmiles_data_WSC_2019[4])
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- t(WSC_2019_mean)
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- tibble::rownames_to_column(WSC_2019_mean, "dataYear")


division_all_highway_data_vehicmiles_2019 <- bind_rows(ENC_2019_mean,ESC_2019_mean,MA_2019_mean,MNT_2019_mean,NE_div_2019_mean,P_2019_mean,SA_2019_mean,WNC_2019_mean,WSC_2019_mean)
print(division_all_highway_data_vehicmiles_2019)
write.table(division_all_highway_data_vehicmiles_2019, file = "divisionall_highway_data_vehicmiles_2019.tsv", row.names = FALSE, sep = "\t")


###########################

louv1_c1 <- c("Alabama","Arkansas","Florida","Georgia","Louisiana","Mississippi","Missouri","Oklahoma","Texas")
louv1_c2 <- c("Connecticut","Delaware","District of Columbia","Illinois","Indiana","Kentucky","Maine","Maryland","Massachusetts","Michigan","New Hampshire","New Jersey","New York","North Carolina","Ohio","Pennsylvania","Rhode Island","South Carolina","Tennessee","Vermont","Virginia","West Virginia")
louv1_c3 <- c("Arizona","California","Colorado","Idaho","Iowa","Kansas","Minnesota","Montana","Nebraska","Nevada","New Mexico","North Dakota","Oregon","South Dakota","Utah","Washington","Wisconsin","Wyoming")


# air 
airtravel_data_louv1_c1 <- airtravel_data[airtravel_data$State %in% louv1_c1,]
airtravel_data_louv1_c1_2019 <- filter(airtravel_data_louv1_c1, Year == '2019')
louv1_c1_2019_mean <- colMeans(airtravel_data_louv1_c1_2019[4])
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- t(louv1_c1_2019_mean)
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- tibble::rownames_to_column(louv1_c1_2019_mean, "dataYear")

airtravel_data_louv1_c2 <- airtravel_data[airtravel_data$State %in% louv1_c2,]
airtravel_data_louv1_c2_2019 <- filter(airtravel_data_louv1_c2, Year == '2019')
louv1_c2_2019_mean <- colMeans(airtravel_data_louv1_c2_2019[4])
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- t(louv1_c2_2019_mean)
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- tibble::rownames_to_column(louv1_c2_2019_mean, "dataYear")


airtravel_data_louv1_c3 <- airtravel_data[airtravel_data$State %in% louv1_c3,]
airtravel_data_louv1_c3_2019 <- filter(airtravel_data_louv1_c3, Year == '2019')
louv1_c3_2019_mean <- colMeans(airtravel_data_louv1_c3_2019[4])
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- t(louv1_c3_2019_mean)
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- tibble::rownames_to_column(louv1_c3_2019_mean, "dataYear")

louvain1_all_airtravel_2019 <- bind_rows(louv1_c1_2019_mean,louv1_c2_2019_mean,louv1_c3_2019_mean)
print(louvain1_all_airtravel_2019)
write.table(louvain1_all_airtravel_2019, file = "louv1all_airtravel2019.tsv", row.names = FALSE, sep = "\t")


# amtrak
amtrak_data_louv1_c1 <- amtrak_data[amtrak_data$State %in% louv1_c1,]
amtrak_data_louv1_c1_2019 <- filter(amtrak_data_louv1_c1, Year == '2019')
louv1_c1_2019_mean <- colMeans(amtrak_data_louv1_c1_2019[4])
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- t(louv1_c1_2019_mean)
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- tibble::rownames_to_column(louv1_c1_2019_mean, "dataYear")

amtrak_data_louv1_c2 <- amtrak_data[amtrak_data$State %in% louv1_c2,]
amtrak_data_louv1_c2_2019 <- filter(amtrak_data_louv1_c2, Year == '2019')
louv1_c2_2019_mean <- colMeans(amtrak_data_louv1_c2_2019[4])
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- t(louv1_c2_2019_mean)
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- tibble::rownames_to_column(louv1_c2_2019_mean, "dataYear")


amtrak_data_louv1_c3 <- amtrak_data[amtrak_data$State %in% louv1_c3,]
amtrak_data_louv1_c3_2019 <- filter(amtrak_data_louv1_c3, Year == '2019')
louv1_c3_2019_mean <- colMeans(amtrak_data_louv1_c3_2019[4])
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- t(louv1_c3_2019_mean)
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- tibble::rownames_to_column(louv1_c3_2019_mean, "dataYear")

louvain1_all_amtrak_2019 <- bind_rows(louv1_c1_2019_mean,louv1_c2_2019_mean,louv1_c3_2019_mean)
print(louvain1_all_amtrak_2019)
write.table(louvain1_all_amtrak_2019, file = "louv1all_amtrak2019.tsv", row.names = FALSE, sep = "\t")


# highway_data_transitpass
highway_data_transitpass_data_louv1_c1 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv1_c1,]
highway_data_transitpass_data_louv1_c1_2019 <- filter(highway_data_transitpass_data_louv1_c1, Year == '2019')
louv1_c1_2019_mean <- colMeans(highway_data_transitpass_data_louv1_c1_2019[4])
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- t(louv1_c1_2019_mean)
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- tibble::rownames_to_column(louv1_c1_2019_mean, "dataYear")

highway_data_transitpass_data_louv1_c2 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv1_c2,]
highway_data_transitpass_data_louv1_c2_2019 <- filter(highway_data_transitpass_data_louv1_c2, Year == '2019')
louv1_c2_2019_mean <- colMeans(highway_data_transitpass_data_louv1_c2_2019[4])
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- t(louv1_c2_2019_mean)
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- tibble::rownames_to_column(louv1_c2_2019_mean, "dataYear")


highway_data_transitpass_data_louv1_c3 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv1_c3,]
highway_data_transitpass_data_louv1_c3_2019 <- filter(highway_data_transitpass_data_louv1_c3, Year == '2019')
louv1_c3_2019_mean <- colMeans(highway_data_transitpass_data_louv1_c3_2019[4])
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- t(louv1_c3_2019_mean)
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- tibble::rownames_to_column(louv1_c3_2019_mean, "dataYear")

louvain1_all_highway_data_transitpass_2019 <- bind_rows(louv1_c1_2019_mean,louv1_c2_2019_mean,louv1_c3_2019_mean)
print(louvain1_all_highway_data_transitpass_2019)
write.table(louvain1_all_highway_data_transitpass_2019, file = "louv1all_highway_data_transitpass2019.tsv", row.names = FALSE, sep = "\t")

# highway_data_vehicmiles
highway_data_vehicmiles_data_louv1_c1 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv1_c1,]
highway_data_vehicmiles_data_louv1_c1_2019 <- filter(highway_data_vehicmiles_data_louv1_c1, Year == '2019')
louv1_c1_2019_mean <- colMeans(highway_data_vehicmiles_data_louv1_c1_2019[4])
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- t(louv1_c1_2019_mean)
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- tibble::rownames_to_column(louv1_c1_2019_mean, "dataYear")

highway_data_vehicmiles_data_louv1_c2 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv1_c2,]
highway_data_vehicmiles_data_louv1_c2_2019 <- filter(highway_data_vehicmiles_data_louv1_c2, Year == '2019')
louv1_c2_2019_mean <- colMeans(highway_data_vehicmiles_data_louv1_c2_2019[4])
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- t(louv1_c2_2019_mean)
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- tibble::rownames_to_column(louv1_c2_2019_mean, "dataYear")


highway_data_vehicmiles_data_louv1_c3 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv1_c3,]
highway_data_vehicmiles_data_louv1_c3_2019 <- filter(highway_data_vehicmiles_data_louv1_c3, Year == '2019')
louv1_c3_2019_mean <- colMeans(highway_data_vehicmiles_data_louv1_c3_2019[4])
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- t(louv1_c3_2019_mean)
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- tibble::rownames_to_column(louv1_c3_2019_mean, "dataYear")

louvain1_all_highway_data_vehicmiles_2019 <- bind_rows(louv1_c1_2019_mean,louv1_c2_2019_mean,louv1_c3_2019_mean)
print(louvain1_all_highway_data_vehicmiles_2019)
write.table(louvain1_all_highway_data_vehicmiles_2019, file = "louv1all_highway_data_vehicmiles2019.tsv", row.names = FALSE, sep = "\t")



###########################

louv2_c1 <- c("Alabama","Florida","Georgia","Louisiana","Mississippi")
louv2_c2 <- c("Arkansas","Missouri","Oklahoma","Texas")
louv2_c3 <- c("Illinois","Indiana","Kentucky","Michigan","Ohio","Tennessee","Virginia","West Virginia")
louv2_c4 <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")
louv2_c5 <- c("Delaware","District of Columbia","Maryland","New Jersey","New York","North Carolina","Pennsylvania","South Carolina")
louv2_c6 <- c("Arizona","California","Idaho","Montana","Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")
louv2_c7 <- c("Colorado","Iowa","Kansas","Minnesota","Nebraska","North Dakota","South Dakota","Wisconsin")


#airtravel
airtravel_data_louv2_c1 <- airtravel_data[airtravel_data$State %in% louv2_c1,]
airtravel_data_louv2_c1_2019 <- filter(airtravel_data_louv2_c1, Year == '2019')
louv2_c1_2019_mean <- colMeans(airtravel_data_louv2_c1_2019[4])
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- t(louv2_c1_2019_mean)
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- tibble::rownames_to_column(louv2_c1_2019_mean, "dataYear")


airtravel_data_louv2_c2 <- airtravel_data[airtravel_data$State %in% louv2_c2,]
airtravel_data_louv2_c2_2019 <- filter(airtravel_data_louv2_c2, Year == '2019')
louv2_c2_2019_mean <- colMeans(airtravel_data_louv2_c2_2019[4])
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- t(louv2_c2_2019_mean)
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- tibble::rownames_to_column(louv2_c2_2019_mean, "dataYear")


airtravel_data_louv2_c3 <- airtravel_data[airtravel_data$State %in% louv2_c3,]
airtravel_data_louv2_c3_2019 <- filter(airtravel_data_louv2_c3, Year == '2019')
louv2_c3_2019_mean <- colMeans(airtravel_data_louv2_c3_2019[4])
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- t(louv2_c3_2019_mean)
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- tibble::rownames_to_column(louv2_c3_2019_mean, "dataYear")


airtravel_data_louv2_c4 <- airtravel_data[airtravel_data$State %in% louv2_c4,]
airtravel_data_louv2_c4_2019 <- filter(airtravel_data_louv2_c4, Year == '2019')
louv2_c4_2019_mean <- colMeans(airtravel_data_louv2_c4_2019[4])
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- t(louv2_c4_2019_mean)
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- tibble::rownames_to_column(louv2_c4_2019_mean, "dataYear")


airtravel_data_louv2_c5 <- airtravel_data[airtravel_data$State %in% louv2_c5,]
airtravel_data_louv2_c5_2019 <- filter(airtravel_data_louv2_c5, Year == '2019')
louv2_c5_2019_mean <- colMeans(airtravel_data_louv2_c5_2019[4])
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- t(louv2_c5_2019_mean)
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- tibble::rownames_to_column(louv2_c5_2019_mean, "dataYear")


airtravel_data_louv2_c6 <- airtravel_data[airtravel_data$State %in% louv2_c6,]
airtravel_data_louv2_c6_2019 <- filter(airtravel_data_louv2_c6, Year == '2019')
louv2_c6_2019_mean <- colMeans(airtravel_data_louv2_c6_2019[4])
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- t(louv2_c6_2019_mean)
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- tibble::rownames_to_column(louv2_c6_2019_mean, "dataYear")


airtravel_data_louv2_c7 <- airtravel_data[airtravel_data$State %in% louv2_c7,]
airtravel_data_louv2_c7_2019 <- filter(airtravel_data_louv2_c7, Year == '2019')
louv2_c7_2019_mean <- colMeans(airtravel_data_louv2_c7_2019[4])
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- t(louv2_c7_2019_mean)
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- tibble::rownames_to_column(louv2_c7_2019_mean, "dataYear")

louvain2_all_airtravel_2019 <- bind_rows(louv2_c1_2019_mean,louv2_c2_2019_mean,louv2_c3_2019_mean,louv2_c4_2019_mean,louv2_c5_2019_mean,louv2_c6_2019_mean,louv2_c7_2019_mean)
print(louvain2_all_airtravel_2019)
write.table(louvain2_all_airtravel_2019, file =  "louv2all_airtravel_2019.tsv", row.names = FALSE, sep = "\t")


#amtrak
amtrak_data_louv2_c1 <- amtrak_data[amtrak_data$State %in% louv2_c1,]
amtrak_data_louv2_c1_2019 <- filter(amtrak_data_louv2_c1, Year == '2019')
louv2_c1_2019_mean <- colMeans(amtrak_data_louv2_c1_2019[4])
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- t(louv2_c1_2019_mean)
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- tibble::rownames_to_column(louv2_c1_2019_mean, "dataYear")


amtrak_data_louv2_c2 <- amtrak_data[amtrak_data$State %in% louv2_c2,]
amtrak_data_louv2_c2_2019 <- filter(amtrak_data_louv2_c2, Year == '2019')
louv2_c2_2019_mean <- colMeans(amtrak_data_louv2_c2_2019[4])
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- t(louv2_c2_2019_mean)
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- tibble::rownames_to_column(louv2_c2_2019_mean, "dataYear")


amtrak_data_louv2_c3 <- amtrak_data[amtrak_data$State %in% louv2_c3,]
amtrak_data_louv2_c3_2019 <- filter(amtrak_data_louv2_c3, Year == '2019')
louv2_c3_2019_mean <- colMeans(amtrak_data_louv2_c3_2019[4])
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- t(louv2_c3_2019_mean)
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- tibble::rownames_to_column(louv2_c3_2019_mean, "dataYear")


amtrak_data_louv2_c4 <- amtrak_data[amtrak_data$State %in% louv2_c4,]
amtrak_data_louv2_c4_2019 <- filter(amtrak_data_louv2_c4, Year == '2019')
louv2_c4_2019_mean <- colMeans(amtrak_data_louv2_c4_2019[4])
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- t(louv2_c4_2019_mean)
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- tibble::rownames_to_column(louv2_c4_2019_mean, "dataYear")


amtrak_data_louv2_c5 <- amtrak_data[amtrak_data$State %in% louv2_c5,]
amtrak_data_louv2_c5_2019 <- filter(amtrak_data_louv2_c5, Year == '2019')
louv2_c5_2019_mean <- colMeans(amtrak_data_louv2_c5_2019[4])
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- t(louv2_c5_2019_mean)
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- tibble::rownames_to_column(louv2_c5_2019_mean, "dataYear")


amtrak_data_louv2_c6 <- amtrak_data[amtrak_data$State %in% louv2_c6,]
amtrak_data_louv2_c6_2019 <- filter(amtrak_data_louv2_c6, Year == '2019')
louv2_c6_2019_mean <- colMeans(amtrak_data_louv2_c6_2019[4])
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- t(louv2_c6_2019_mean)
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- tibble::rownames_to_column(louv2_c6_2019_mean, "dataYear")


amtrak_data_louv2_c7 <- amtrak_data[amtrak_data$State %in% louv2_c7,]
amtrak_data_louv2_c7_2019 <- filter(amtrak_data_louv2_c7, Year == '2019')
louv2_c7_2019_mean <- colMeans(amtrak_data_louv2_c7_2019[4])
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- t(louv2_c7_2019_mean)
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- tibble::rownames_to_column(louv2_c7_2019_mean, "dataYear")

louvain2_all_amtrak_2019 <- bind_rows(louv2_c1_2019_mean,louv2_c2_2019_mean,louv2_c3_2019_mean,louv2_c4_2019_mean,louv2_c5_2019_mean,louv2_c6_2019_mean,louv2_c7_2019_mean)
print(louvain2_all_amtrak_2019)
write.table(louvain2_all_amtrak_2019, file =  "louv2all_amtrak_2019.tsv", row.names = FALSE, sep = "\t")


#highway_data_transitpass
highway_data_transitpass_data_louv2_c1 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv2_c1,]
highway_data_transitpass_data_louv2_c1_2019 <- filter(highway_data_transitpass_data_louv2_c1, Year == '2019')
louv2_c1_2019_mean <- colMeans(highway_data_transitpass_data_louv2_c1_2019[4])
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- t(louv2_c1_2019_mean)
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- tibble::rownames_to_column(louv2_c1_2019_mean, "dataYear")


highway_data_transitpass_data_louv2_c2 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv2_c2,]
highway_data_transitpass_data_louv2_c2_2019 <- filter(highway_data_transitpass_data_louv2_c2, Year == '2019')
louv2_c2_2019_mean <- colMeans(highway_data_transitpass_data_louv2_c2_2019[4])
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- t(louv2_c2_2019_mean)
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- tibble::rownames_to_column(louv2_c2_2019_mean, "dataYear")


highway_data_transitpass_data_louv2_c3 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv2_c3,]
highway_data_transitpass_data_louv2_c3_2019 <- filter(highway_data_transitpass_data_louv2_c3, Year == '2019')
louv2_c3_2019_mean <- colMeans(highway_data_transitpass_data_louv2_c3_2019[4])
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- t(louv2_c3_2019_mean)
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- tibble::rownames_to_column(louv2_c3_2019_mean, "dataYear")


highway_data_transitpass_data_louv2_c4 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv2_c4,]
highway_data_transitpass_data_louv2_c4_2019 <- filter(highway_data_transitpass_data_louv2_c4, Year == '2019')
louv2_c4_2019_mean <- colMeans(highway_data_transitpass_data_louv2_c4_2019[4])
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- t(louv2_c4_2019_mean)
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- tibble::rownames_to_column(louv2_c4_2019_mean, "dataYear")


highway_data_transitpass_data_louv2_c5 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv2_c5,]
highway_data_transitpass_data_louv2_c5_2019 <- filter(highway_data_transitpass_data_louv2_c5, Year == '2019')
louv2_c5_2019_mean <- colMeans(highway_data_transitpass_data_louv2_c5_2019[4])
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- t(louv2_c5_2019_mean)
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- tibble::rownames_to_column(louv2_c5_2019_mean, "dataYear")


highway_data_transitpass_data_louv2_c6 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv2_c6,]
highway_data_transitpass_data_louv2_c6_2019 <- filter(highway_data_transitpass_data_louv2_c6, Year == '2019')
louv2_c6_2019_mean <- colMeans(highway_data_transitpass_data_louv2_c6_2019[4])
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- t(louv2_c6_2019_mean)
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- tibble::rownames_to_column(louv2_c6_2019_mean, "dataYear")


highway_data_transitpass_data_louv2_c7 <- highway_data_transitpass_data[highway_data_transitpass_data$State %in% louv2_c7,]
highway_data_transitpass_data_louv2_c7_2019 <- filter(highway_data_transitpass_data_louv2_c7, Year == '2019')
louv2_c7_2019_mean <- colMeans(highway_data_transitpass_data_louv2_c7_2019[4])
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- t(louv2_c7_2019_mean)
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- tibble::rownames_to_column(louv2_c7_2019_mean, "dataYear")

louvain2_all_highway_data_transitpass_2019 <- bind_rows(louv2_c1_2019_mean,louv2_c2_2019_mean,louv2_c3_2019_mean,louv2_c4_2019_mean,louv2_c5_2019_mean,louv2_c6_2019_mean,louv2_c7_2019_mean)
print(louvain2_all_highway_data_transitpass_2019)
write.table(louvain2_all_highway_data_transitpass_2019, file =  "louv2all_highway_data_transitpass_2019.tsv", row.names = FALSE, sep = "\t")


#highway_data_vehicmiles
highway_data_vehicmiles_data_louv2_c1 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv2_c1,]
highway_data_vehicmiles_data_louv2_c1_2019 <- filter(highway_data_vehicmiles_data_louv2_c1, Year == '2019')
louv2_c1_2019_mean <- colMeans(highway_data_vehicmiles_data_louv2_c1_2019[4])
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- t(louv2_c1_2019_mean)
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- tibble::rownames_to_column(louv2_c1_2019_mean, "dataYear")


highway_data_vehicmiles_data_louv2_c2 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv2_c2,]
highway_data_vehicmiles_data_louv2_c2_2019 <- filter(highway_data_vehicmiles_data_louv2_c2, Year == '2019')
louv2_c2_2019_mean <- colMeans(highway_data_vehicmiles_data_louv2_c2_2019[4])
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- t(louv2_c2_2019_mean)
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- tibble::rownames_to_column(louv2_c2_2019_mean, "dataYear")


highway_data_vehicmiles_data_louv2_c3 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv2_c3,]
highway_data_vehicmiles_data_louv2_c3_2019 <- filter(highway_data_vehicmiles_data_louv2_c3, Year == '2019')
louv2_c3_2019_mean <- colMeans(highway_data_vehicmiles_data_louv2_c3_2019[4])
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- t(louv2_c3_2019_mean)
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- tibble::rownames_to_column(louv2_c3_2019_mean, "dataYear")


highway_data_vehicmiles_data_louv2_c4 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv2_c4,]
highway_data_vehicmiles_data_louv2_c4_2019 <- filter(highway_data_vehicmiles_data_louv2_c4, Year == '2019')
louv2_c4_2019_mean <- colMeans(highway_data_vehicmiles_data_louv2_c4_2019[4])
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- t(louv2_c4_2019_mean)
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- tibble::rownames_to_column(louv2_c4_2019_mean, "dataYear")


highway_data_vehicmiles_data_louv2_c5 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv2_c5,]
highway_data_vehicmiles_data_louv2_c5_2019 <- filter(highway_data_vehicmiles_data_louv2_c5, Year == '2019')
louv2_c5_2019_mean <- colMeans(highway_data_vehicmiles_data_louv2_c5_2019[4])
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- t(louv2_c5_2019_mean)
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- tibble::rownames_to_column(louv2_c5_2019_mean, "dataYear")


highway_data_vehicmiles_data_louv2_c6 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv2_c6,]
highway_data_vehicmiles_data_louv2_c6_2019 <- filter(highway_data_vehicmiles_data_louv2_c6, Year == '2019')
louv2_c6_2019_mean <- colMeans(highway_data_vehicmiles_data_louv2_c6_2019[4])
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- t(louv2_c6_2019_mean)
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- tibble::rownames_to_column(louv2_c6_2019_mean, "dataYear")


highway_data_vehicmiles_data_louv2_c7 <- highway_data_vehicmiles_data[highway_data_vehicmiles_data$State %in% louv2_c7,]
highway_data_vehicmiles_data_louv2_c7_2019 <- filter(highway_data_vehicmiles_data_louv2_c7, Year == '2019')
louv2_c7_2019_mean <- colMeans(highway_data_vehicmiles_data_louv2_c7_2019[4])
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- t(louv2_c7_2019_mean)
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- tibble::rownames_to_column(louv2_c7_2019_mean, "dataYear")

louvain2_all_highway_data_vehicmiles_2019 <- bind_rows(louv2_c1_2019_mean,louv2_c2_2019_mean,louv2_c3_2019_mean,louv2_c4_2019_mean,louv2_c5_2019_mean,louv2_c6_2019_mean,louv2_c7_2019_mean)
print(louvain2_all_highway_data_vehicmiles_2019)
write.table(louvain2_all_highway_data_vehicmiles_2019, file =  "louv2all_highway_data_vehicmiles_2019.tsv", row.names = FALSE, sep = "\t")
