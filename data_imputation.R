library(readr)
library(dplyr)
library(zoo)

momentum = 4


vital_trajectory <- read_csv("C:/Users/zlr5321/Desktop/HSIPCourse/Quarter2/HSIP 442 Informatics II/vitals_trajectory_cleaned.csv")


allVitals = unique(vital_trajectory$event_name)
vitalGrid = expand.grid(unique(vital_trajectory$EncID), 0:12)
names(vitalGrid) = c("EncID","event_hour")

for (thisVital in allVitals)
{
  print(thisVital)
  vital1 = vital_trajectory[vital_trajectory$event_name == thisVital,c("EncID","event_hour","died_hour_round","event_value")]
  vital1 = vital1[!is.na(vital1$event_value),]
  vital1$imputed = 0
  # vitalImputeLeft = vital1[order(vital1$EncID, vital1$event_hour),]
  # n = dim(vitalImputeLeft)[1]
  # ind = c(1,1+which(vitalImputeLeft[1:(n-1),]$EncID != vitalImputeLeft[2:n,]$EncID))
  # vitalImputeLeft = vitalImputeLeft[ind,]
  # vitalImputeLeft$imputed = 1
  
  vitalImputeRight = vital1[order(vital1$EncID, -vital1$event_hour),]
  n = dim(vitalImputeRight)[1]
  ind = c(1,1+which(vitalImputeRight[1:(n-1),]$EncID != vitalImputeRight[2:n,]$EncID))
  vitalImputeRight = vitalImputeRight[ind,]
  vitalImputeRight$imputed = 1
  
  for (i in (1:momentum))
  {
    # vitalImputeLefti = vitalImputeLeft
    # vitalImputeLefti$event_hour = vitalImputeLefti$event_hour - i
    # vitalImputeLefti = vitalImputeLefti[vitalImputeLefti$event_hour >= 0,]
    # vital1 = rbind(vital1, vitalImputeLefti)
    
    vitalImputeRighti = vitalImputeRight
    vitalImputeRighti$event_hour = vitalImputeRighti$event_hour + i
    vitalImputeRighti = vitalImputeRighti[vitalImputeRighti$event_hour <= 12,]
    vital1 = rbind(vital1, vitalImputeRighti)
  }
  
  
  
  vitalExpanded = expand.grid(unique(vital1$EncID), 0:12)
  names(vitalExpanded) = c("EncID","event_hour")
  vitalExpanded = merge(vitalExpanded, vital1, by = c("EncID","event_hour"), all.x = T)
  vitalExpanded = vitalExpanded[order(vitalExpanded$EncID, vitalExpanded$event_hour),]
  
  ind = which(!is.na(vitalExpanded$imputed) & vitalExpanded$imputed == 0)
  vitalExpanded$imputeEx = 0
  
  for (i in 1:momentum)
  {
    # vitalExpanded[ind[floor((ind-1-i)/13) == floor((ind-1)/13)]-i,]$imputeEx = 1
    vitalExpanded[ind[floor((ind-1+i)/13) == floor((ind-1)/13)]+i,]$imputeEx = 1
  }
  vitalExpanded[is.na(vitalExpanded$imputed),]$imputed = vitalExpanded[is.na(vitalExpanded$imputed),]$imputeEx
  vitalExpanded = vitalExpanded %>% mutate(event_value_Interp = na.approx(event_value, na.rm = FALSE))
  
  if (thisVital == "FIO2")
    vitalExpanded[is.na(vitalExpanded$imputed) | vitalExpanded$imputed == 1, ]$event_value = 0.21
  
  vitalExpanded$died_hour_round = ave(vitalExpanded$died_hour_round, vitalExpanded$EncID, FUN = function(x) mean(x, na.rm = TRUE))
  treatasNA = is.na(vitalExpanded$imputed) | (!is.na(vitalExpanded$died_hour_round) & vitalExpanded$event_hour > vitalExpanded$died_hour_round)
  if (any(treatasNA))
    vitalExpanded[treatasNA,]$event_value_Interp = NA
  
  names(vitalExpanded)[names(vitalExpanded) == "event_value_Interp"] = paste0(thisVital, "Imputed")
  
  vitalGrid = merge(vitalGrid, vitalExpanded[,c("EncID","event_hour", paste0(thisVital, "Imputed"))], by = c("EncID","event_hour"), all.x = T)
}

vitalGrid = merge(vitalGrid, unique(vital_trajectory[,c("EncID","hospital_id","age_group","mods_day7","died_in_hosp","died_hour_round")]), by = "EncID")

write.csv(vitalGrid, file = "C:/Users/zlr5321/Desktop/HSIPCourse/Quarter2/HSIP 442 Informatics II/vitals_trajectory_imputed.csv")






