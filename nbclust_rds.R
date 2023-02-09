library(NbClust)

df = readRDS('/home/ydn4687/sepsis/dtw_out.Rds')

res<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=4, 
            method = "ward.D", index = "all")

print(res)

