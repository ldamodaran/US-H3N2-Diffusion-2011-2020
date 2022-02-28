
# Script to sumamrize census data averages for different regionalization schema 
# For census data 

census_data <- read.csv("~/Desktop/Aim1/Main-data/Demographic-data/nst-est2019-alldata.csv", header = TRUE)


##########################
census_regions_NE <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania")
census_regions_MW <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin","Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
census_regions_S <- c("Delaware","District of Columbia","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Alabama","Kentucky","Mississippi","Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
census_regions_W <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming","Alaska","California","Hawaii","Oregon","Washington")

census_data_NE <- census_data[census_data$US.STATE %in% census_regions_NE,]
na.omit(census_data_NE)
NE_mean <- colMeans(census_data_NE[6:151])
NE_mean <- as.data.frame(NE_mean)
NE_mean <- t(NE_mean)
NE_mean <- as.data.frame(NE_mean)
NE_mean <- tibble::rownames_to_column(NE_mean, "datayear")

census_data_MW <- census_data[census_data$US.STATE %in% census_regions_MW,]
na.omit(census_data_MW)
MW_mean <- colMeans(census_data_MW[6:151])
MW_mean <- as.data.frame(MW_mean)
MW_mean <- t(MW_mean)
MW_mean <- as.data.frame(MW_mean)
MW_mean <- tibble::rownames_to_column(MW_mean, "datayear")


census_data_S <- census_data[census_data$US.STATE %in% census_regions_S,]
na.omit(census_data_S)
S_mean <- colMeans(census_data_S[6:151])
S_mean <- as.data.frame(S_mean)
S_mean <- t(S_mean)
S_mean <- as.data.frame(S_mean)
S_mean <- tibble::rownames_to_column(S_mean, "datayear")


census_data_W <- census_data[census_data$US.STATE %in% census_regions_W,]
na.omit(census_data_W)
W_mean <- colMeans(census_data_W[6:151])
W_mean <- as.data.frame(W_mean)
W_mean <- t(W_mean)
W_mean <- as.data.frame(W_mean)
W_mean <- tibble::rownames_to_column(W_mean, "datayear")

regions_all <- bind_rows(NE_mean,MW_mean,S_mean,W_mean)
print(regions_all)
write.table(regions_all, file = "regions.tsv", row.names = FALSE, sep = "\t")


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
  
census_data_ENC <- census_data[census_data$US.STATE %in% census_divisions_ENC,]
na.omit(census_data_ENC)
ENC_mean <- colMeans(census_data_ENC[6:151])
ENC_mean <- as.data.frame(ENC_mean)
ENC_mean <- t(ENC_mean)
ENC_mean <- as.data.frame(ENC_mean)
ENC_mean <- tibble::rownames_to_column(ENC_mean, "datayear")


census_data_ESC <- census_data[census_data$US.STATE %in% census_divisions_ESC,]
na.omit(census_data_ESC)
ESC_mean <- colMeans(census_data_ESC[6:151])
ESC_mean <- as.data.frame(ESC_mean)
ESC_mean <- t(ESC_mean)
ESC_mean <- as.data.frame(ESC_mean)
ESC_mean <- tibble::rownames_to_column(ESC_mean, "datayear")

census_data_MA <- census_data[census_data$US.STATE %in% census_divisions_MA,]
na.omit(census_data_MA)
MA_mean <- colMeans(census_data_MA[6:151])
MA_mean <- as.data.frame(MA_mean)
MA_mean <- t(MA_mean)
MA_mean <- as.data.frame(MA_mean)
MA_mean <- tibble::rownames_to_column(MA_mean, "datayear")

census_data_MNT <- census_data[census_data$US.STATE %in% census_divisions_MNT,]
na.omit(census_data_MNT)
MNT_mean <- colMeans(census_data_MNT[6:151])
MNT_mean <- as.data.frame(MNT_mean)
MNT_mean <- t(MNT_mean)
MNT_mean <- as.data.frame(MNT_mean)
MNT_mean <- tibble::rownames_to_column(MNT_mean, "datayear")


census_data_NEdiv <- census_data[census_data$US.STATE %in% census_divisions_NEdiv,]
na.omit(census_data_NEdiv)
NE_div_mean <- colMeans(census_data_NEdiv[6:151])
NE_div_mean <- as.data.frame(NE_div_mean)
NE_div_mean <- t(NE_div_mean)
NE_div_mean <- as.data.frame(NE_div_mean)
NE_div_mean <- tibble::rownames_to_column(NE_div_mean, "datayear")


census_data_P <- census_data[census_data$US.STATE %in% census_divisions_P,]
na.omit(census_data_P)
P_mean <- colMeans(census_data_P[6:151])
P_mean <- as.data.frame(P_mean)
P_mean <- t(P_mean)
P_mean <- as.data.frame(P_mean)
P_mean <- tibble::rownames_to_column(P_mean, "datayear")

census_data_SA <- census_data[census_data$US.STATE %in% census_divisions_SA,]
na.omit(census_data_SA)
SA_mean <- colMeans(census_data_SA[6:151])
SA_mean <- as.data.frame(SA_mean)
SA_mean <- t(SA_mean)
SA_mean <- as.data.frame(SA_mean)
SA_mean <- tibble::rownames_to_column(SA_mean, "datayear")


census_data_WNC <- census_data[census_data$US.STATE %in% census_divisions_WNC,]
na.omit(census_data_WNC)
WNC_mean <- colMeans(census_data_WNC[6:151])
WNC_mean <- as.data.frame(WNC_mean)
WNC_mean <- t(WNC_mean)
WNC_mean <- as.data.frame(WNC_mean)
WNC_mean <- tibble::rownames_to_column(WNC_mean, "datayear")


census_data_WSC <- census_data[census_data$US.STATE %in% census_divisions_WSC,]
na.omit(census_data_WSC)
WSC_mean <- colMeans(census_data_WSC[6:151])
WSC_mean <- as.data.frame(WSC_mean)
WSC_mean <- t(WSC_mean)
WSC_mean <- as.data.frame(WSC_mean)
WSC_mean <- tibble::rownames_to_column(WSC_mean, "datayear")


division_all <- bind_rows(ENC_mean,ESC_mean,MA_mean,MNT_mean,NE_div_mean,P_mean,SA_mean,WNC_mean,WSC_mean)
print(division_all)
write.table(division_all, file = "divisionall.tsv", row.names = FALSE, sep = "\t")
  
  
###########################

louv1_c1 <- c("Alabama","Arkansas","Florida","Georgia","Louisiana","Mississippi","Missouri","Oklahoma","Texas")
louv1_c2 <- c("Connecticut","Delaware","District of Columbia","Illinois","Indiana","Kentucky","Maine","Maryland","Massachusetts","Michigan","New Hampshire","New Jersey","New York","North Carolina","Ohio","Pennsylvania","Rhode Island","South Carolina","Tennessee","Vermont","Virginia","West Virginia")
louv1_c3 <- c("Arizona","California","Colorado","Idaho","Iowa","Kansas","Minnesota","Montana","Nebraska","Nevada","New Mexico","North Dakota","Oregon","South Dakota","Utah","Washington","Wisconsin","Wyoming")

census_data_louv1_c1 <- census_data[census_data$US.STATE %in% louv1_c1,]
na.omit(census_data_louv1_c1)
louv1_c1_mean <- colMeans(census_data_louv1_c1[6:151])
louv1_c1_mean <- as.data.frame(louv1_c1_mean)
louv1_c1_mean <- t(louv1_c1_mean)
louv1_c1_mean <- as.data.frame(louv1_c1_mean)
louv1_c1_mean <- tibble::rownames_to_column(louv1_c1_mean, "datayear")

census_data_louv1_c2 <- census_data[census_data$US.STATE %in% louv1_c2,]
na.omit(census_data_louv1_c2)
louv1_c2_mean <- colMeans(census_data_louv1_c2[6:151])
louv1_c2_mean <- as.data.frame(louv1_c2_mean)
louv1_c2_mean <- t(louv1_c2_mean)
louv1_c2_mean <- as.data.frame(louv1_c2_mean)
louv1_c2_mean <- tibble::rownames_to_column(louv1_c2_mean, "datayear")


census_data_louv1_c3 <- census_data[census_data$US.STATE %in% louv1_c3,]
na.omit(census_data_louv1_c3)
louv1_c3_mean <- colMeans(census_data_louv1_c3[6:151])
louv1_c3_mean <- as.data.frame(louv1_c3_mean)
louv1_c3_mean <- t(louv1_c3_mean)
louv1_c3_mean <- as.data.frame(louv1_c3_mean)
louv1_c3_mean <- tibble::rownames_to_column(louv1_c3_mean, "datayear")

louvain1_all <- bind_rows(louv1_c1_mean,louv1_c2_mean,louv1_c3_mean)
print(louvain1_all)
write.table(louvain1_all, file = "louv1all.tsv", row.names = FALSE, sep = "\t")


###########################

louv2_c1 <- c("Alabama","Florida","Georgia","Louisiana","Mississippi")
louv2_c2 <- c("Arkansas","Missouri","Oklahoma","Texas")
louv2_c3 <- c("Illinois","Indiana","Kentucky","Michigan","Ohio","Tennessee","Virginia","West Virginia")
louv2_c4 <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")
louv2_c5 <- c("Delaware","District of Columbia","Maryland","New Jersey","New York","North Carolina","Pennsylvania","South Carolina")
louv2_c6 <- c("Arizona","California","Idaho","Montana","Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")
louv2_c7 <- c("Colorado","Iowa","Kansas","Minnesota","Nebraska","North Dakota","South Dakota","Wisconsin")



census_data_louv2_c1 <- census_data[census_data$US.STATE %in% louv2_c1,]
na.omit(census_data_louv2_c1)
louv2_c1_mean <- colMeans(census_data_louv2_c1[6:151])
louv2_c1_mean <- as.data.frame(louv2_c1_mean)
louv2_c1_mean <- t(louv2_c1_mean)
louv2_c1_mean <- as.data.frame(louv2_c1_mean)
louv2_c1_mean <- tibble::rownames_to_column(louv2_c1_mean, "datayear")


census_data_louv2_c2 <- census_data[census_data$US.STATE %in% louv2_c2,]
na.omit(census_data_louv2_c2)
louv2_c2_mean <- colMeans(census_data_louv2_c2[6:151])
louv2_c2_mean <- as.data.frame(louv2_c2_mean)
louv2_c2_mean <- t(louv2_c2_mean)
louv2_c2_mean <- as.data.frame(louv2_c2_mean)
louv2_c2_mean <- tibble::rownames_to_column(louv2_c2_mean, "datayear")


census_data_louv2_c3 <- census_data[census_data$US.STATE %in% louv2_c3,]
na.omit(census_data_louv2_c3)
louv2_c3_mean <- colMeans(census_data_louv2_c3[6:151])
louv2_c3_mean <- as.data.frame(louv2_c3_mean)
louv2_c3_mean <- t(louv2_c3_mean)
louv2_c3_mean <- as.data.frame(louv2_c3_mean)
louv2_c3_mean <- tibble::rownames_to_column(louv2_c3_mean, "datayear")


census_data_louv2_c4 <- census_data[census_data$US.STATE %in% louv2_c4,]
na.omit(census_data_louv2_c4)
louv2_c4_mean <- colMeans(census_data_louv2_c4[6:151])
louv2_c4_mean <- as.data.frame(louv2_c4_mean)
louv2_c4_mean <- t(louv2_c4_mean)
louv2_c4_mean <- as.data.frame(louv2_c4_mean)
louv2_c4_mean <- tibble::rownames_to_column(louv2_c4_mean, "datayear")


census_data_louv2_c5 <- census_data[census_data$US.STATE %in% louv2_c5,]
na.omit(census_data_louv2_c5)
louv2_c5_mean <- colMeans(census_data_louv2_c5[6:151])
louv2_c5_mean <- as.data.frame(louv2_c5_mean)
louv2_c5_mean <- t(louv2_c5_mean)
louv2_c5_mean <- as.data.frame(louv2_c5_mean)
louv2_c5_mean <- tibble::rownames_to_column(louv2_c5_mean, "datayear")


census_data_louv2_c6 <- census_data[census_data$US.STATE %in% louv2_c6,]
na.omit(census_data_louv2_c6)
louv2_c6_mean <- colMeans(census_data_louv2_c6[6:151])
louv2_c6_mean <- as.data.frame(louv2_c6_mean)
louv2_c6_mean <- t(louv2_c6_mean)
louv2_c6_mean <- as.data.frame(louv2_c6_mean)
louv2_c6_mean <- tibble::rownames_to_column(louv2_c6_mean, "datayear")


census_data_louv2_c7 <- census_data[census_data$US.STATE %in% louv2_c7,]
na.omit(census_data_louv2_c7)
louv2_c7_mean <- colMeans(census_data_louv2_c7[6:151])
louv2_c7_mean <- as.data.frame(louv2_c7_mean)
louv2_c7_mean <- t(louv2_c7_mean)
louv2_c7_mean <- as.data.frame(louv2_c7_mean)
louv2_c7_mean <- tibble::rownames_to_column(louv2_c7_mean, "datayear")

louvain2_all <- bind_rows(louv2_c1_mean,louv2_c2_mean,louv2_c3_mean,louv2_c4_mean,louv2_c5_mean,louv2_c6_mean,louv2_c7_mean)
print(louvain2_all)
write.table(louvain2_all, file =  "louv2all.tsv", row.names = FALSE, sep = "\t")


