install.packages("dplyr")
install.packages("ggplot2")
install.packages("viridis")
install.packages("ggthemes")
install.packages("XML")
install.packages("tm")
install.packages("rJava")
install.packages("wordcloud")
install.packages("plotly")
install.packages("readxl")
install.packages('gcookbook')


rm(list = ls())
library(plotly)
library(dplyr) #資料處理package
library(ggplot2) #視覺化package
library(viridis) #主題package
library(ggthemes) #配色package
library(tm)
#library(rJava)
#library(wordcloud)
library(XML)
#library(methods)
library(readxl) #讀取xlsx檔案
library(gcookbook)



AC_2395 <- read.csv("taiwan50/AC_2395.csv")
ACC_1102 <- read.csv("taiwan50/ACC_1102.csv")
AOC_2409 <- read.csv("taiwan50/AOC_2409.csv")
ASE_2311 <- read.csv("taiwan50/ASE_2311.csv")
ASUS_2357 <- read.csv("taiwan50/ASUS_2357.csv")
CDFHC_2883 <- read.csv("taiwan50/CDFHC_2883.csv")
CFHC_2882 <- read.csv("taiwan50/CFHC_2882.csv")
CHCB_2801 <- read.csv("taiwan50/CHCB_2801.csv")
CSC_2202 <- read.csv("taiwan50/CSC_2202.csv")
CSR_2105 <- read.csv("taiwan50/CSR_2105.csv")
CTBC_2891 <- read.csv("taiwan50/CTBC_2891.csv")
CTC_2412 <- read.csv("taiwan50/CTC_2412.csv")
CTC_2474 <- read.csv("taiwan50/CTC_2474.csv")
DE_2308 <- read.csv("taiwan50/DE_2308.csv")
EFHC_2884 <- read.csv("taiwan50/EFHC_2884.csv")
ETC_1476 <- read.csv("taiwan50/ETC_1476.csv")
FCFC_1326 <- read.csv("taiwan50/FCFC_1326.csv")
FENC_1402 <- read.csv("taiwan50/FENC_1402.csv")
FETT_4904 <- read.csv("taiwan50/FETT_4904.csv")
FFH_2892 <- read.csv("taiwan50/FFH_2892.csv")
FFHC_2881 <- read.csv("taiwan50/FFHC_2881.csv")
FPC_1301 <- read.csv("taiwan50/FPC_1301.csv")
FPC_6505 <- read.csv("taiwan50/FPC_6505.csv")
FTC_2354 <- read.csv("taiwan50/FTC_2354.csv")
HHPI <- read.csv("taiwan50/HHPI_2317.csv")
HMC_2207 <- read.csv("taiwan50/HMC_2207.csv")
HNFHC_2280 <- read.csv("taiwan50/HNFHC_2880.csv")
IM_3474 <- read.csv("taiwan50/IM_3474.csv")
LC_3481 <- read.csv("taiwan50/LC_3481.csv")
LOTC_2301 <- read.csv("taiwan50/LOTC_2301.csv")
LP_3008 <- read.csv("taiwan50/LP_3008.csv")
MFHC_2886 <- read.csv("taiwan50/MFHC_2886.csv")
MT_2454 <- read.csv("taiwan50/MT_2454.csv")
NTC_2408 <- read.csv("taiwan50/NTC_2408.csv")
NYPC_1303 <- read.csv("taiwan50/NYPC_1303.csv")
PC_4938 <- read.csv("taiwan50/PC_4938.csv")
PCC_9904 <- read.csv("taiwan50/PCC_9904.csv")
PCSC_2912 <- read.csv("taiwan50/PCSC_2912.csv")
QC_2382 <- read.csv("taiwan50/QC_2382.csv")
SFHC_2890 <- read.csv("taiwan50/SFHC_2890.csv")
SPIC_2325 <- read.csv("taiwan50/SPIC_2325.csv")
TCC_1101 <- read.csv("taiwan50/TCC_1101.csv")
TCF_5880 <- read.csv("taiwan50/TCF_5880.csv")
TFHC_2887 <- read.csv("taiwan50/TFHC_2887.csv")
TMC_3045 <- read.csv("taiwan50/TMC_3045.csv")
TSM_2330 <- read.csv("taiwan50/TSM_2330.csv")
UMC_2303 <- read.csv("taiwan50/UMC_2303.csv")
UPE_1216 <- read.csv("taiwan50/UPE_1216.csv")
YFHC_2885 <- read.csv("taiwan50/YFHC_2885.csv")
YNMC_2227 <- read.csv("taiwan50/YNMC_2227.csv")
time <- AC_2395$date
value <- AC_2395$closing_price
#qplot(AC_2395$date, AC_2395$closing_price, xlab = "date", ylab = "closing_price", geom = "line")

p <- ggplot(data.frame(time,value),aes(time,value))
p + geom_line()


















