# Lambodhar Damodaran
# 10-2021
# Script to sumamrize census data averages for different regionalization schema 
# For epi data
# Use values from September to April end (example: 2019.66,	2020.33)

library(lubridate)
library(dplyr)
library(tidyverse)

ili_data <- read.csv("~/Desktop/Aim1/Main-data/Epi-data/state_ili-wFL.csv", header = TRUE)
ili_data <- as.data.frame(ili_data)
ili_data$datedec <- decimal_date(as.Date(ili_data$week_start))

##########################
census_regions_NE <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania")
census_regions_MW <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin","Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
census_regions_S <- c("Delaware","District of Columbia","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Alabama","Kentucky","Mississippi","Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
census_regions_W <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming","Alaska","California","Hawaii","Oregon","Washington")


ili_data_NE <- ili_data[ili_data$region.stateili %in% census_regions_NE,]
ili_data_NE_2019 <- ili_data_NE %>% filter(between(datedec,2019.66,	2020.33))
NE_2019_mean <- colMeans(ili_data_NE_2019[7])
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- t(NE_2019_mean)
NE_2019_mean <- as.data.frame(NE_2019_mean)
NE_2019_mean <- tibble::rownames_to_column(NE_2019_mean, "datayear")

ili_data_MW <- ili_data[ili_data$region.stateili %in% census_regions_MW,]
ili_data_MW_2019 <- ili_data_MW %>% filter(between(datedec,2019.66,	2020.33))
MW_2019_mean <- colMeans(ili_data_MW_2019[7])
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- t(MW_2019_mean)
MW_2019_mean <- as.data.frame(MW_2019_mean)
MW_2019_mean <- tibble::rownames_to_column(MW_2019_mean, "datayear")


ili_data_S <- ili_data[ili_data$region.stateili %in% census_regions_S,]
ili_data_S_2019 <- ili_data_S %>% filter(between(datedec,2019.66,	2020.33))
S_2019_mean <- colMeans(ili_data_S_2019[7])
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- t(S_2019_mean)
S_2019_mean <- as.data.frame(S_2019_mean)
S_2019_mean <- tibble::rownames_to_column(S_2019_mean, "datayear")


ili_data_W <- ili_data[ili_data$region.stateili %in% census_regions_W,]
ili_data_W_2019 <- ili_data_W %>% filter(between(datedec,2019.66,	2020.33))
W_2019_mean <- colMeans(ili_data_W_2019[7])
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


ili_data_ENC <- ili_data[ili_data$region.stateili %in% census_divisions_ENC,]
ili_data_ENC_2019 <- ili_data_ENC %>% filter(between(datedec,2019.66,	2020.33))
ENC_2019_mean <- colMeans(ili_data_ENC_2019[7])
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- t(ENC_2019_mean)
ENC_2019_mean <- as.data.frame(ENC_2019_mean)
ENC_2019_mean <- tibble::rownames_to_column(ENC_2019_mean, "datayear")


ili_data_ESC <- ili_data[ili_data$region.stateili %in% census_divisions_ESC,]
ili_data_ESC_2019 <- ili_data_ESC %>% filter(between(datedec,2019.66,	2020.33))
ESC_2019_mean <- colMeans(ili_data_ESC_2019[7])
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- t(ESC_2019_mean)
ESC_2019_mean <- as.data.frame(ESC_2019_mean)
ESC_2019_mean <- tibble::rownames_to_column(ESC_2019_mean, "datayear")


ili_data_MA <- ili_data[ili_data$region.stateili %in% census_divisions_MA,]
ili_data_MA_2019 <- ili_data_MA %>% filter(between(datedec,2019.66,	2020.33))
MA_2019_mean <- colMeans(ili_data_MA_2019[7])
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- t(MA_2019_mean)
MA_2019_mean <- as.data.frame(MA_2019_mean)
MA_2019_mean <- tibble::rownames_to_column(MA_2019_mean, "datayear")


ili_data_MNT <- ili_data[ili_data$region.stateili %in% census_divisions_MNT,]
ili_data_MNT_2019 <- ili_data_MNT %>% filter(between(datedec,2019.66,	2020.33))
MNT_2019_mean <- colMeans(ili_data_MNT_2019[7])
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- t(MNT_2019_mean)
MNT_2019_mean <- as.data.frame(MNT_2019_mean)
MNT_2019_mean <- tibble::rownames_to_column(MNT_2019_mean, "datayear")


ili_data_NEdiv <- ili_data[ili_data$region.stateili %in% census_divisions_NEdiv,]
ili_data_NEdiv_2019 <- ili_data_NEdiv %>% filter(between(datedec,2019.66,	2020.33))
NEdiv_2019_mean <- colMeans(ili_data_NEdiv_2019[7])
NEdiv_2019_mean <- as.data.frame(NEdiv_2019_mean)
NEdiv_2019_mean <- t(NEdiv_2019_mean)
NEdiv_2019_mean <- as.data.frame(NEdiv_2019_mean)
NEdiv_2019_mean <- tibble::rownames_to_column(NEdiv_2019_mean, "datayear")


ili_data_P <- ili_data[ili_data$region.stateili %in% census_divisions_P,]
ili_data_P_2019 <- ili_data_P %>% filter(between(datedec,2019.66,	2020.33))
P_2019_mean <- colMeans(ili_data_P_2019[7])
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- t(P_2019_mean)
P_2019_mean <- as.data.frame(P_2019_mean)
P_2019_mean <- tibble::rownames_to_column(P_2019_mean, "datayear")


ili_data_SA <- ili_data[ili_data$region.stateili %in% census_divisions_SA,]
ili_data_SA_2019 <- ili_data_SA %>% filter(between(datedec,2019.66,	2020.33))
SA_2019_mean <- colMeans(ili_data_SA_2019[7])
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- t(SA_2019_mean)
SA_2019_mean <- as.data.frame(SA_2019_mean)
SA_2019_mean <- tibble::rownames_to_column(SA_2019_mean, "datayear")


ili_data_WNC <- ili_data[ili_data$region.stateili %in% census_divisions_WNC,]
ili_data_WNC_2019 <- ili_data_WNC %>% filter(between(datedec,2019.66,	2020.33))
WNC_2019_mean <- colMeans(ili_data_WNC_2019[7])
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- t(WNC_2019_mean)
WNC_2019_mean <- as.data.frame(WNC_2019_mean)
WNC_2019_mean <- tibble::rownames_to_column(WNC_2019_mean, "datayear")


ili_data_WSC <- ili_data[ili_data$region.stateili %in% census_divisions_WSC,]
ili_data_WSC_2019 <- ili_data_WSC %>% filter(between(datedec,2019.66,	2020.33))
WSC_2019_mean <- colMeans(ili_data_WSC_2019[7])
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- t(WSC_2019_mean)
WSC_2019_mean <- as.data.frame(WSC_2019_mean)
WSC_2019_mean <- tibble::rownames_to_column(WSC_2019_mean, "datayear")

division_all_2019 <- bind_rows(ENC_2019_mean,ESC_2019_mean,MA_2019_mean,MNT_2019_mean,NEdiv_2019_mean,P_2019_mean,SA_2019_mean,WNC_2019_mean,WSC_2019_mean)
print(division_all_2019)
write.table(division_all_2019, file = "divisionall_2019.tsv", row.names = FALSE, sep = "\t")

###########################

louv1_c1 <- c("Alabama","Arkansas","Florida","Georgia","Louisiana","Mississippi","Missouri","Oklahoma","Texas")
louv1_c2 <- c("Connecticut","Delaware","District of Columbia","Illinois","Indiana","Kentucky","Maine","Maryland","Massachusetts","Michigan","New Hampshire","New Jersey","New York","North Carolina","Ohio","Pennsylvania","Rhode Island","South Carolina","Tennessee","Vermont","Virginia","West Virginia")
louv1_c3 <- c("Arizona","California","Colorado","Idaho","Iowa","Kansas","Minnesota","Montana","Nebraska","Nevada","New Mexico","North Dakota","Oregon","South Dakota","Utah","Washington","Wisconsin","Wyoming")


ili_data_louv1_c1 <- ili_data[ili_data$region.stateili %in% louv1_c1,]
ili_data_louv1_c1_2019 <- ili_data_louv1_c1 %>% filter(between(datedec,2019.66,	2020.33))
louv1_c1_2019_mean <- colMeans(ili_data_louv1_c1_2019[7])
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- t(louv1_c1_2019_mean)
louv1_c1_2019_mean <- as.data.frame(louv1_c1_2019_mean)
louv1_c1_2019_mean <- tibble::rownames_to_column(louv1_c1_2019_mean, "datayear")

ili_data_louv1_c2 <- ili_data[ili_data$region.stateili %in% louv1_c2,]
ili_data_louv1_c2_2019 <- ili_data_louv1_c2 %>% filter(between(datedec,2019.66,	2020.33))
louv1_c2_2019_mean <- colMeans(ili_data_louv1_c2_2019[7])
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- t(louv1_c2_2019_mean)
louv1_c2_2019_mean <- as.data.frame(louv1_c2_2019_mean)
louv1_c2_2019_mean <- tibble::rownames_to_column(louv1_c2_2019_mean, "datayear")


ili_data_louv1_c3 <- ili_data[ili_data$region.stateili %in% louv1_c3,]
ili_data_louv1_c3_2019 <- ili_data_louv1_c3 %>% filter(between(datedec,2019.66,	2020.33))
louv1_c3_2019_mean <- colMeans(ili_data_louv1_c3_2019[7])
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


ili_data_louv2_c1 <- ili_data[ili_data$region.stateili %in% louv2_c1,]
ili_data_louv2_c1_2019 <- ili_data_louv2_c1 %>% filter(between(datedec,2019.66,	2020.33))
louv2_c1_2019_mean <- colMeans(ili_data_louv2_c1_2019[7])
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- t(louv2_c1_2019_mean)
louv2_c1_2019_mean <- as.data.frame(louv2_c1_2019_mean)
louv2_c1_2019_mean <- tibble::rownames_to_column(louv2_c1_2019_mean, "datayear")


ili_data_louv2_c2 <- ili_data[ili_data$region.stateili %in% louv2_c2,]
ili_data_louv2_c2_2019 <- ili_data_louv2_c2 %>% filter(between(datedec,2019.66,	2020.33))
louv2_c2_2019_mean <- colMeans(ili_data_louv2_c2_2019[7])
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- t(louv2_c2_2019_mean)
louv2_c2_2019_mean <- as.data.frame(louv2_c2_2019_mean)
louv2_c2_2019_mean <- tibble::rownames_to_column(louv2_c2_2019_mean, "datayear")


ili_data_louv2_c3 <- ili_data[ili_data$region.stateili %in% louv2_c3,]
ili_data_louv2_c3_2019 <- ili_data_louv2_c3 %>% filter(between(datedec,2019.66,	2020.33))
louv2_c3_2019_mean <- colMeans(ili_data_louv2_c3_2019[7])
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- t(louv2_c3_2019_mean)
louv2_c3_2019_mean <- as.data.frame(louv2_c3_2019_mean)
louv2_c3_2019_mean <- tibble::rownames_to_column(louv2_c3_2019_mean, "datayear")


ili_data_louv2_c4 <- ili_data[ili_data$region.stateili %in% louv2_c4,]
ili_data_louv2_c4_2019 <- ili_data_louv2_c4 %>% filter(between(datedec,2019.66,	2020.33))
louv2_c4_2019_mean <- colMeans(ili_data_louv2_c4_2019[7])
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- t(louv2_c4_2019_mean)
louv2_c4_2019_mean <- as.data.frame(louv2_c4_2019_mean)
louv2_c4_2019_mean <- tibble::rownames_to_column(louv2_c4_2019_mean, "datayear")


ili_data_louv2_c5 <- ili_data[ili_data$region.stateili %in% louv2_c5,]
ili_data_louv2_c5_2019 <- ili_data_louv2_c5 %>% filter(between(datedec,2019.66,	2020.33))
louv2_c5_2019_mean <- colMeans(ili_data_louv2_c5_2019[7])
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- t(louv2_c5_2019_mean)
louv2_c5_2019_mean <- as.data.frame(louv2_c5_2019_mean)
louv2_c5_2019_mean <- tibble::rownames_to_column(louv2_c5_2019_mean, "datayear")


ili_data_louv2_c6 <- ili_data[ili_data$region.stateili %in% louv2_c6,]
ili_data_louv2_c6_2019 <- ili_data_louv2_c6 %>% filter(between(datedec,2019.66,	2020.33))
louv2_c6_2019_mean <- colMeans(ili_data_louv2_c6_2019[7])
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- t(louv2_c6_2019_mean)
louv2_c6_2019_mean <- as.data.frame(louv2_c6_2019_mean)
louv2_c6_2019_mean <- tibble::rownames_to_column(louv2_c6_2019_mean, "datayear")


ili_data_louv2_c7 <- ili_data[ili_data$region.stateili %in% louv2_c7,]
ili_data_louv2_c7_2019 <- ili_data_louv2_c7 %>% filter(between(datedec,2019.66,	2020.33))
louv2_c7_2019_mean <- colMeans(ili_data_louv2_c7_2019[7])
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- t(louv2_c7_2019_mean)
louv2_c7_2019_mean <- as.data.frame(louv2_c7_2019_mean)
louv2_c7_2019_mean <- tibble::rownames_to_column(louv2_c7_2019_mean, "datayear")


louvain2_all_2019 <- bind_rows(louv2_c1_2019_mean,louv2_c2_2019_mean,louv2_c3_2019_mean,louv2_c4_2019_mean,louv2_c5_2019_mean,louv2_c6_2019_mean,louv2_c7_2019_mean)
print(louvain2_all_2019)
write.table(louvain2_all_2019, file =  "louv2all_2019.tsv", row.names = FALSE, sep = "\t")




