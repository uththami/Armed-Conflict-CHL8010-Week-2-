library(here)
here()
finaldata<-read.csv(here("finaldata.csv"),header=TRUE)

data2017infmor <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(infmor)) 
infmor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(infmor) & armconf1 == 1) |>
  dplyr::select(ISO, infmor)
infmor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(infmor) & armconf1 == 0) |>
  dplyr::select(ISO, infmor)
set.seed(2024)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- infmor.arm1[sample(nrow(infmor.arm1), size = nrow(infmor.arm1), replace = TRUE),]
  resamp.arm0 <- infmor.arm0[sample(nrow(infmor.arm0), size = nrow(infmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$infmor) - median(resamp.arm0$infmor)
}
library(boot)
getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$infmor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}
bootout.infmor <- boot(data2017infmor, statistic = getmeddiff, strata = data2017infmor$armconf1, R = 1000)

data2017un5mor <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(un5mor)) 
un5mor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(un5mor) & armconf1 == 1) |>
  dplyr::select(ISO, un5mor)
un5mor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(un5mor) & armconf1 == 0) |>
  dplyr::select(ISO, un5mor)
set.seed(2024)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- un5mor.arm1[sample(nrow(un5mor.arm1), size = nrow(un5mor.arm1), replace = TRUE),]
  resamp.arm0 <- un5mor.arm0[sample(nrow(un5mor.arm0), size = nrow(un5mor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$un5mor) - median(resamp.arm0$un5mor)
}
library(boot)
getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$un5mor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}
bootout.un5mor <- boot(data2017un5mor, statistic = getmeddiff, strata = data2017un5mor$armconf1, R = 1000)

data2017neomor <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(neomor)) 
neomor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(neomor) & armconf1 == 1) |>
  dplyr::select(ISO, neomor)
neomor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(neomor) & armconf1 == 0) |>
  dplyr::select(ISO, neomor)
set.seed(2024)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- neomor.arm1[sample(nrow(neomor.arm1), size = nrow(neomor.arm1), replace = TRUE),]
  resamp.arm0 <- neomor.arm0[sample(nrow(neomor.arm0), size = nrow(neomor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$neomor) - median(resamp.arm0$neomor)
}
library(boot)
getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$neomor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}
bootout.neomor <- boot(data2017neomor, statistic = getmeddiff, strata = data2017neomor$armconf1, R = 1000)


boot.ci(boot.out = bootout.infmor, conf = 0.95, type = c("basic", "perc", "bca"))
#Interpretation: The true median value for infant mortality will fall between [11.20,32.50] 95% of the time.
boot.ci(boot.out = bootout.un5mor, conf = 0.95, type = c("basic", "perc", "bca"))
#Interpretation: The true median value for under 5 mortality will fall between [15.20,49.20] 95% of the time.
boot.ci(boot.out = bootout.neomor, conf = 0.95, type = c("basic", "perc", "bca"))
#Interpretation: The true median value for neonatal mortality will fall between [6.7,19.0] 95% of the time.

