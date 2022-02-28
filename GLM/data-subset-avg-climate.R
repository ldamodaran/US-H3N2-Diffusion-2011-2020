# Lambodhar Damodaran 
# 10-2021
# Script to sumamrize census data averages for different regionalization schema 
# For climate data
# Find and replace year e.g. "2012" for desired year and run
# This reads in data already subsetted for flue season and parses by region selections and begging of each section
library(dplyr)
library(tidyverse)
climate_data <- read.csv("~/Desktop/Aim1/Main-data/climate-data/nclimdiv-averages-flu-sep-april.csv", header = TRUE)
stateabv <- read.csv("~/Desktop/Aim1/Main-data/climate-data/US-states-postabv.csv", header = TRUE)


climate_data <- as.data.frame(climate_data)
stateabv <- as.data.frame(stateabv)

climate_data <- merge(climate_data,stateabv, by.x = 'POSTAL.ABBREVIATION')


##########################
census_regions_NE <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania")
census_regions_MW <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin","Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
census_regions_S <- c("Delaware","District of Columbia","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Alabama","Kentucky","Mississippi","Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
census_regions_W <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming","Alaska","California","Hawaii","Oregon","Washington")


climate_data_NE <- climate_data[climate_data$US.STATE %in% census_regions_NE,]

climate_data_NE_2019 <- filter(climate_data_NE, year == '2019')
NE_2019_mean <- colMeans(climate_data_NE_2019[4:10])
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- t(NE_2019_mean)
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- tibble::rownames_to_column(NE_2019_mean, "datayear")

climate_data_MW <- climate_data[climate_data$US.STATE %in% census_regions_MW,]
climate_data_MW_2019 <- filter(climate_data_MW, year == '2019')
MW_2019_mean <- colMeans(climate_data_MW_2019[4:10])
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- t(MW_2019_mean)
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- tibble::rownames_to_column(MW_2019_mean, "datayear")


climate_data_S <- climate_data[climate_data$US.STATE %in% census_regions_S,]
climate_data_S_2019 <- filter(climate_data_S, year == '2019')
S_2019_mean <- colMeans(climate_data_S_2019[4:10])
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- t(S_2019_mean)
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- tibble::rownames_to_column(S_2019_mean, "datayear")


climate_data_W <- climate_data[climate_data$US.STATE %in% census_regions_W,]
climate_data_W_2019 <- filter(climate_data_W, year == '2019')
W_2019_mean <- colMeans(climate_data_W_2019[4:10])
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- t(W_2019_mean)
W_2019_mean <- as.data.frame(W_2019_mean)
W_2019_mean <- tibble::rownames_to_column(W_2019_mean, "datayear")


regions_all_2019 <- bind_rows(NE_2019_mean,MW_2019_mean,S_2019_mean,W_2019_mean)
print(regions_all_2019)
write.table(regions_all_2019, file = "regions_2019.tsv", row.names = FALSE, sep = "\t")



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


climate_data_ENC <- climate_data[climate_data$US.STATE %in% census_divisions_ENC,]
climate_data_ENC_2019 <- filter(climate_data_ENC, year == '2019')
ENC_2019_mean <- colMeans(climate_data_ENC_2019[4:10])
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- t(ENC_2019_mean)
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- tibble::rownames_to_column(ENC_2019_mean, "datayear")


climate_data_ESC <- climate_data[climate_data$US.STATE %in% census_divisions_ESC,]
climate_data_ESC_2019 <- filter(climate_data_ESC, year == '2019')
ESC_2019_mean <- colMeans(climate_data_ESC_2019[4:10])
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- t(ESC_2019_mean)
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- tibble::rownames_to_column(ESC_2019_mean, "datayear")

climate_data_MA <- climate_data[climate_data$US.STATE %in% census_divisions_MA,]
climate_data_MA_2019 <- filter(climate_data_MA, year == '2019')
MA_2019_mean <- colMeans(climate_data_MA_2019[4:10])
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- t(MA_2019_mean)
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- tibble::rownames_to_column(MA_2019_mean, "datayear")

climate_data_MNT <- climate_data[climate_data$US.STATE %in% census_divisions_MNT,]
climate_data_MNT_2019 <- filter(climate_data_MNT, year == '2019')
MNT_2019_mean <- colMeans(climate_data_MNT_2019[4:10])
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- t(MNT_2019_mean)
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- tibble::rownames_to_column(MNT_2019_mean, "datayear")


climate_data_NEdiv <- climate_data[climate_data$US.STATE %in% census_divisions_NEdiv,]
climate_data_NE_div_2019 <- filter(climate_data_NEdiv, year == '2019')
NE_div_2019_mean <- colMeans(climate_data_NE_div_2019[4:10])
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- t(NE_div_2019_mean)
NE_div_2019_mean <- as.data.frame(NE_div_2019_mean)
NE_div_2019_mean <- tibble::rownames_to_column(NE_div_2019_mean, "datayear")


climate_data_P <- climate_data[climate_data$US.STATE %in% census_divisions_P,]
climate_data_P_2019 <- filter(climate_data_P, year == '2019')
P_2019_mean <- colMeans(climate_data_P_2019[4:10])
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- t(P_2019_mean)
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- tibble::rownames_to_column(P_2019_mean, "datayear")

climate_data_SA <- climate_data[climate_data$US.STATE %in% census_divisions_SA,]
climate_data_SA_2019 <- filter(climate_data_SA, year == '2019')
SA_2019_mean <- colMeans(climate_data_SA_2019[4:10])
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- t(SA_2019_mean)
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- tibble::rownames_to_column(SA_2019_mean, "datayear")


climate_data_WNC <- climate_data[climate_data$US.STATE %in% census_divisions_WNC,]
climate_data_WNC_2019 <- filter(climate_data_WNC, year == '2019')
WNC_2019_mean <- colMeans(climate_data_WNC_2019[4:10])
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- t(WNC_2019_mean)
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- tibble::rownames_to_column(WNC_2019_mean, "datayear")


climate_data_WSC <- climate_data[climate_data$US.STATE %in% census_divisions_WSC,]
climate_data_WSC_2019 <- filter(climate_data_WSC, year == '2019')
WSC_2019_mean <- colMeans(climate_data_WSC_2019[4:10])
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- t(WSC_2019_mean)
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- tibble::rownames_to_column(WSC_2019_mean, "datayear")


division_all_2019 <- bind_rows(ENC_2019_mean,ESC_2019_mean,MA_2019_mean,MNT_2019_mean,NE_div_2019_mean,P_2019_mean,SA_2019_mean,WNC_2019_mean,WSC_2019_mean)
print(division_all_2019)
write.table(division_all_2019, file = "divisionall_2019.tsv", row.names = FALSE, sep = "\t")


###########################

louv1_c1 <- c("Alabama","Arkansas","Florida","Georgia","Louisiana","Mississippi","Missouri","Oklahoma","Texas")
louv1_c2 <- c("Connecticut","Delaware","District of Columbia","Illinois","Indiana","Kentucky","Maine","Maryland","Massachusetts","Michigan","New Hampshire","New Jersey","New York","North Carolina","Ohio","Pennsylvania","Rhode Island","South Carolina","Tennessee","Vermont","Virginia","West Virginia")
louv1_c3 <- c("Arizona","California","Colorado","Idaho","Iowa","Kansas","Minnesota","Montana","Nebraska","Nevada","New Mexico","North Dakota","Oregon","South Dakota","Utah","Washington","Wisconsin","Wyoming")

climate_data_louv1_c1 <- climate_data[climate_data$US.STATE %in% louv1_c1,]
climate_data_louv1_c1_2019 <- filter(climate_data_louv1_c1, year == '2019')
louv1_c1_2019_mean <- colMeans(climate_data_louv1_c1_2019[4:10])
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- t(louv1_c1_2019_mean)
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- tibble::rownames_to_column(louv1_c1_2019_mean, "datayear")

climate_data_louv1_c2 <- climate_data[climate_data$US.STATE %in% louv1_c2,]
climate_data_louv1_c2_2019 <- filter(climate_data_louv1_c2, year == '2019')
louv1_c2_2019_mean <- colMeans(climate_data_louv1_c2_2019[4:10])
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- t(louv1_c2_2019_mean)
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- tibble::rownames_to_column(louv1_c2_2019_mean, "datayear")


climate_data_louv1_c3 <- climate_data[climate_data$US.STATE %in% louv1_c3,]
climate_data_louv1_c3_2019 <- filter(climate_data_louv1_c3, year == '2019')
louv1_c3_2019_mean <- colMeans(climate_data_louv1_c3_2019[4:10])
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- t(louv1_c3_2019_mean)
louv1_c3_2019_mean <- as.data.frame(louv1_c3_2019_mean)
louv1_c3_2019_mean <- tibble::rownames_to_column(louv1_c3_2019_mean, "datayear")

louvain1_all_2019 <- bind_rows(louv1_c1_2019_mean,louv1_c2_2019_mean,louv1_c3_2019_mean)
print(louvain1_all_2019)
write.table(louvain1_all_2019, file = "louv1all_2019.tsv", row.names = FALSE, sep = "\t")

###########################

louv2_c1 <- c("Alabama","Florida","Georgia","Louisiana","Mississippi")
louv2_c2 <- c("Arkansas","Missouri","Oklahoma","Texas")
louv2_c3 <- c("Illinois","Indiana","Kentucky","Michigan","Ohio","Tennessee","Virginia","West Virginia")
louv2_c4 <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")
louv2_c5 <- c("Delaware","District of Columbia","Maryland","New Jersey","New York","North Carolina","Pennsylvania","South Carolina")
louv2_c6 <- c("Arizona","California","Idaho","Montana","Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")
louv2_c7 <- c("Colorado","Iowa","Kansas","Minnesota","Nebraska","North Dakota","South Dakota","Wisconsin")


climate_data_louv2_c1 <- climate_data[climate_data$US.STATE %in% louv2_c1,]
climate_data_louv2_c1_2019 <- filter(climate_data_louv2_c1, year == '2019')
louv2_c1_2019_mean <- colMeans(climate_data_louv2_c1_2019[4:10])
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- t(louv2_c1_2019_mean)
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- tibble::rownames_to_column(louv2_c1_2019_mean, "datayear")


climate_data_louv2_c2 <- climate_data[climate_data$US.STATE %in% louv2_c2,]
climate_data_louv2_c2_2019 <- filter(climate_data_louv2_c2, year == '2019')
louv2_c2_2019_mean <- colMeans(climate_data_louv2_c2_2019[4:10])
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- t(louv2_c2_2019_mean)
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- tibble::rownames_to_column(louv2_c2_2019_mean, "datayear")


climate_data_louv2_c3 <- climate_data[climate_data$US.STATE %in% louv2_c3,]
climate_data_louv2_c3_2019 <- filter(climate_data_louv2_c3, year == '2019')
louv2_c3_2019_mean <- colMeans(climate_data_louv2_c3_2019[4:10])
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- t(louv2_c3_2019_mean)
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- tibble::rownames_to_column(louv2_c3_2019_mean, "datayear")


climate_data_louv2_c4 <- climate_data[climate_data$US.STATE %in% louv2_c4,]
climate_data_louv2_c4_2019 <- filter(climate_data_louv2_c4, year == '2019')
louv2_c4_2019_mean <- colMeans(climate_data_louv2_c4_2019[4:10])
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- t(louv2_c4_2019_mean)
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- tibble::rownames_to_column(louv2_c4_2019_mean, "datayear")


climate_data_louv2_c5 <- climate_data[climate_data$US.STATE %in% louv2_c5,]
climate_data_louv2_c5_2019 <- filter(climate_data_louv2_c5, year == '2019')
louv2_c5_2019_mean <- colMeans(climate_data_louv2_c5_2019[4:10])
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- t(louv2_c5_2019_mean)
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- tibble::rownames_to_column(louv2_c5_2019_mean, "datayear")


climate_data_louv2_c6 <- climate_data[climate_data$US.STATE %in% louv2_c6,]
climate_data_louv2_c6_2019 <- filter(climate_data_louv2_c6, year == '2019')
louv2_c6_2019_mean <- colMeans(climate_data_louv2_c6_2019[4:10])
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- t(louv2_c6_2019_mean)
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- tibble::rownames_to_column(louv2_c6_2019_mean, "datayear")


climate_data_louv2_c7 <- climate_data[climate_data$US.STATE %in% louv2_c7,]
climate_data_louv2_c7_2019 <- filter(climate_data_louv2_c7, year == '2019')
louv2_c7_2019_mean <- colMeans(climate_data_louv2_c7_2019[4:10])
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- t(louv2_c7_2019_mean)
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- tibble::rownames_to_column(louv2_c7_2019_mean, "datayear")

louvain2_all_2019 <- bind_rows(louv2_c1_2019_mean,louv2_c2_2019_mean,louv2_c3_2019_mean,louv2_c4_2019_mean,louv2_c5_2019_mean,louv2_c6_2019_mean,louv2_c7_2019_mean)
print(louvain2_all_2019)
write.table(louvain2_all_2019, file =  "louv2all_2019.tsv", row.names = FALSE, sep = "\t")

