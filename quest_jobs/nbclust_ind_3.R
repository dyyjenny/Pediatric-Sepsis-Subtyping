library(NbClust)

# change directory
df = read.csv('/home/ydn4687/sepsis/dtw_out.csv')

list.methods = c( "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")

             
for(i in 1:length(list.methods)){
  nb = try(NbClust(df, distance = "euclidean",
                min.nc = 2, max.nc = 6,
                method = "ward.D", index = list.methods[i]))
  if(inherits(nb, "try-error"))
    {
      next
    }
  cat(nb$Best.nc[1], list.methods[i],'\n')
  # change directory
  testcon <- file(description = "/home/ydn4687/sepsis/nbclust_ind_out.txt", open = "a")
  cat(paste0(nb$Best.nc[1], ' ', list.methods[i]), file=testcon, append=TRUE,sep="\n")
  close(testcon)}

