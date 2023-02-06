library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(reshape2)

momentum = 4


vital_trajectory <- read_csv("C:/Users/zlr5321/Desktop/HSIPCourse/Quarter2/HSIP 442 Informatics II/vitals_trajectory_cleaned.csv")


allVitals = unique(vital_trajectory$event_name)
vitalGrid = expand.grid(unique(vital_trajectory$EncID), 0:12)
names(vitalGrid) = c("EncID","event_hour")
vitalGrid = merge(vitalGrid, unique(vital_trajectory[,c("EncID","hospital_id","age_group","mods_day7","died_in_hosp","died_hour_round")]), by = "EncID")

for (thisVital in allVitals)
{
  print(thisVital)
  vital1 = vital_trajectory[vital_trajectory$event_name == thisVital,c("EncID","event_hour","died_hour_round","event_value")]
  vital1 = vital1[!is.na(vital1$event_value),]
  vital1$imputed = 0

  vitalImputeRight = vital1[order(vital1$EncID, -vital1$event_hour),]
  n = dim(vitalImputeRight)[1]
  ind = c(1,1+which(vitalImputeRight[1:(n-1),]$EncID != vitalImputeRight[2:n,]$EncID))
  vitalImputeRight = vitalImputeRight[ind,]
  vitalImputeRight$imputed = 1
  
  for (i in (1:momentum))
  {
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
  vitalExpanded$imputeEx = NA
  
  for (i in 1:momentum)
    vitalExpanded[ind[floor((ind-1+i)/13) == floor((ind-1)/13)]+i,]$imputeEx = 1

  vitalExpanded[is.na(vitalExpanded$imputed),]$imputed = vitalExpanded[is.na(vitalExpanded$imputed),]$imputeEx
  vitalExpanded = vitalExpanded %>% mutate(event_value_Interp = na.approx(event_value, na.rm = FALSE))
  
  vitalExpanded$died_hour_round = ave(vitalExpanded$died_hour_round, vitalExpanded$EncID, FUN = function(x) mean(x, na.rm = TRUE))
  treatasNA = is.na(vitalExpanded$imputed) | (!is.na(vitalExpanded$died_hour_round) & vitalExpanded$event_hour > vitalExpanded$died_hour_round)
  if (any(treatasNA))
    vitalExpanded[treatasNA,]$event_value_Interp = NA
  
  if (thisVital == "SF_RATIO")
  {
    vitalGridR = vitalGrid
    vitalGridR$SF_R = vitalGridR$PULSE_OXImputed/vitalGridR$FIO2Imputed
    vitalExpanded = merge(vitalExpanded, vitalGridR[!is.na(vitalGridR$SF_R),c("EncID","event_hour","SF_R")], by = c("EncID","event_hour"), all.x = T, all.y = T)
    vitalExpanded[is.na(vitalExpanded$imputed) | vitalExpanded$imputed != 0,]$event_value_Interp = vitalExpanded[is.na(vitalExpanded$imputed) | vitalExpanded$imputed != 0,]$SF_R
  }
  names(vitalExpanded)[names(vitalExpanded) == "event_value_Interp"] = paste0(thisVital, "Imputed")
  
  vitalGrid = merge(vitalGrid, vitalExpanded[,c("EncID","event_hour", paste0(thisVital, "Imputed"))], by = c("EncID","event_hour"), all.x = T)
  
  if (thisVital == "FIO2")  
    vitalGrid[is.na(vitalGrid$FIO2Imputed) & (is.na(vitalGrid$died_hour_round) | vitalGrid$event_hour <= vitalGrid$died_hour_round), ]$FIO2Imputed = 0.21
  
}

for (thisVital in allVitals)
  vitalGrid[,paste0(thisVital, "Imputed")] = (vitalGrid[,paste0(thisVital, "Imputed")] - mean(vitalGrid[,paste0(thisVital, "Imputed")], na.rm = T))/sd(vitalGrid[,paste0(thisVital, "Imputed")],na.rm = T)


write.csv(vitalGrid, file = "C:/Users/zlr5321/Desktop/HSIPCourse/Quarter2/HSIP 442 Informatics II/vitals_trajectory_imputed.csv")


# Graphs and stats
 
vitalGrid = read_csv("C:/Users/zlr5321/Desktop/HSIPCourse/Quarter2/HSIP 442 Informatics II/vitals_trajectory_imputed.csv")

vitalGrid$predead = 1
vitalGrid[!is.na(vitalGrid$died_hour_round) & vitalGrid$died_hour_round < vitalGrid$event_hour,]$predead = 0
vitalGridNM = vitalGrid[vitalGrid$predead == 1,]

allVitals = c("FIO2Imputed","MAPImputed","PULSEImputed","PULSE_OXImputed","RESP_RATEImputed","SBPImputed","SF_RATIOImputed","TEMPImputed")
dataStats = data.frame(num.Complete = rep(NA, 7), num.missing = rep(NA, 7), miss.percent.among.missing = rep(NA, 7))
plotData = data.frame(matrix(nrow = 12,ncol = 8))
names(plotData) = allVitals

k = 1
for (thisVital in allVitals)
{
  thisVitalPercent = aggregate(vitalGridNM[,thisVital], list(vitalGridNM$EncID), FUN = function(x) sum(is.na(x))/length(x))
  thisVitalPercent = thisVitalPercent[,2]
  dataStats[k,]$num.Complete = sum(thisVitalPercent == 0)
  dataStats[k,]$num.missing = sum(thisVitalPercent > 0)
  dataStats[k,]$miss.percent.among.missing = mean(thisVitalPercent[thisVitalPercent > 0])
  for (i in 1:12)
    plotData[i,thisVital] = mean(unlist(vitalGridNM[vitalGridNM$event_hour == i,thisVital]), na.rm = T)
  k = k + 1
}

thisVitalPercent = aggregate(rowSums(vitalGridNM[,allVitals[c(2,3,5:8)]]), list(vitalGridNM$EncID), FUN = function(x) sum(is.na(x))/length(x))
thisVitalPercent = thisVitalPercent[,2]
dataStats[k,]$num.Complete = sum(thisVitalPercent == 0)
dataStats[k,]$num.missing = sum(thisVitalPercent > 0)
dataStats[k,]$miss.percent.among.missing = mean(thisVitalPercent[thisVitalPercent > 0])

plotData$time = 1:12
plotData = melt(plotData, id.vars = "time", variable.name = "series")
ggplot(plotData, aes(time, value)) + geom_line(aes(colour = series))

write.csv(dataStats, file = "C:/Users/zlr5321/Desktop/HSIPCourse/Quarter2/HSIP 442 Informatics II/data.csv")


vitalGridNM$na.percent = ave(rowSums(vitalGridNM[,allVitals[c(2,3,5:8)]]), vitalGridNM$EncID, FUN = function(x) sum(is.na(x))/length(x))
vitalGridnoNA = vitalGrid[is.element(vitalGrid$EncID, vitalGridNM[vitalGridNM$na.percent == 0,]$EncID),]
write.csv(vitalGridnoNA, file = "C:/Users/zlr5321/Desktop/HSIPCourse/Quarter2/HSIP 442 Informatics II/vitals_trajectory_imputed_noNA.csv")

