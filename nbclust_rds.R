library(NbClust)

df = read.csv('/home/ydn4687/sepsis/dtw_out.csv')

#res<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=4, 
            #method = "ward.D", index = "all")

list.methods = c( "kl", "ch", "hartigan", "ccc","scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")

#tabla = as.data.frame(matrix(ncol = 2, nrow = length(lista.methods)))
#names(tabla) = "euclidean"

for(i in 1:length(list.methods)){
    nb = NbClust(df, distance = "euclidean",
                 min.nc = 2, max.nc = 6, 
                 method = "ward.D", index = list.methods[i])
    write.csv(nb$Best.nc[1], paste('/home/ydn4687/sepsis/nbclust',list.methods[i], 'csv', sep = '.'))
}



