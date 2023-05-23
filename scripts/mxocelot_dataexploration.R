#load libraries 
library(oSCR)
#i think you need these 
library(data.table)
library(reshape2)

###load the workspaces here 

setwd("G:/MT FWP Hard Drive Backup/MXOcelot/DesktopCopy_ACTIVE/MX_Ocelot/Routputs")
load("2024_0412_mxocelot_modelbuildingblocks_2sessLRB_singles_LRsep.rda")

plot(sf.tam_2013.R)


###Make inputs,
caphist <- sf.tam_2013.R[["caphist"]][[1]]
traps <- sf.tam_2013.R[["traps"]][[1]]


######Getting distances by CAT ID 

#flatten the caphist across occasions so any recaptures are in 1 dimension  
caphist.2d <- rowSums(caphist, dims = 2)
#then flatten further, see who has spatial recaptures 
caphist.1d <- rowSums(caphist.2d, dims = 1)
#now any cat > 1 is a spatially recapture cat (these would be at multiple traps or same trap)

#then make this a truefalse 
caphist.TF <- caphist.1d > 1
#sum it, how many bears spatcapped? 
sum(caphist.TF)
 
##which rows? this gets you the row ID of any spatially recaptured cat
caphist.TF.rows <- which(caphist.1d >1 )
#print and look at them, can refer to caphist to check make sure, looks good  
caphist.TF.rows
#then subset the orig matrix, get only the recapped cats
caphist.2d.recaps <- caphist.2d[caphist.TF.rows,]

###then prepare for a heinous for loop bc I can never figure out apply funcs :\
distlist <- list() #make empty list to get all distances 


for (i in 1:length(caphist.TF.rows)) { #loop through the list of spatcapped bears 
  that <- which(caphist.2d.recaps[i,]>0) #for each bear, get vector of trap #s where that bear was caught 
  dists <- reshape2::melt(as.matrix(dist(traps[that, #then get dist matrix for that vector of traps 
                                               c("X", "Y")])))
  dists <- dists[dists$Var1 > dists$Var2,] #trim the melted dist matrix so it isn't repetitive 
  distlist[[i]] <- data.frame(dists$value) #append it to a list, length of list should be length of spatcapped bears 
  #it has to be a df to append right
}


names(distlist) <- caphist.TF.rows #name the list elements so they reflect their caphist bear ID 

recaps.df <- rbindlist(distlist, idcol = TRUE) #then unlist the list (seriously i am sure there is a better way)

#rename the columns to be helpful 
names(recaps.df)<-c("CaphistCatID","SpatCapDist")


###then you can look at the distances, filter distances as needed and know what bear they belong to 

hist(recaps.df$SpatCapDist)


####REPS with ocelot SCRframes
###Make inputs,
caphist <- sf.tam_2013.R[["caphist"]][[1]]
traps <- sf.tam_2013.R[["traps"]][[1]]
######Getting distances by CAT ID 
#flatten the caphist across occasions so any recaptures are in 1 dimension  
caphist.2d <- rowSums(caphist, dims = 2)
#then flatten further, see who has spatial recaptures 
caphist.1d <- rowSums(caphist.2d, dims = 1)
#now any cat > 1 is a spatially recapture cat (these would be at multiple traps or same trap)
#then make this a truefalse 
caphist.TF <- caphist.1d > 1
#sum it, how many bears spatcapped? 
sum(caphist.TF)
##which rows? this gets you the row ID of any spatially recaptured cat
caphist.TF.rows <- which(caphist.1d >1 )
#print and look at them, can refer to caphist to check make sure, looks good  
caphist.TF.rows
#then subset the orig matrix, get only the recapped cats
caphist.2d.recaps <- caphist.2d[caphist.TF.rows,]
max(rowSums(caphist.2d.recaps))
min(rowSums(caphist.2d.recaps))
sum(caphist.2d.recaps>0)
apply(caphist.2d.recaps, 1, function(row) sum(row != 0))
max(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))
min(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))


###
caphist <- sf.tam_2017.R[["caphist"]][[1]]
traps <- sf.tam_2017.R[["traps"]][[1]]
######Getting distances by CAT ID 
#flatten the caphist across occasions so any recaptures are in 1 dimension  
caphist.2d <- rowSums(caphist, dims = 2)
#then flatten further, see who has spatial recaptures 
caphist.1d <- rowSums(caphist.2d, dims = 1)
#now any cat > 1 is a spatially recapture cat (these would be at multiple traps or same trap)
#then make this a truefalse 
caphist.TF <- caphist.1d > 1
#sum it, how many bears spatcapped? 
sum(caphist.TF)
##which rows? this gets you the row ID of any spatially recaptured cat
caphist.TF.rows <- which(caphist.1d >1 )
#print and look at them, can refer to caphist to check make sure, looks good  
caphist.TF.rows
#then subset the orig matrix, get only the recapped cats
caphist.2d.recaps <- caphist.2d[caphist.TF.rows,]
max(rowSums(caphist.2d.recaps))
min(rowSums(caphist.2d.recaps))
sum(caphist.2d.recaps)
187/27
apply(caphist.2d.recaps, 1, function(row) sum(row != 0))
max(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))
min(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))
sum(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))/27


###
caphist <- sf.tam_2013.L[["caphist"]][[1]]
traps <- sf.tam_2013.L[["traps"]][[1]]
######Getting distances by CAT ID 
#flatten the caphist across occasions so any recaptures are in 1 dimension  
caphist.2d <- rowSums(caphist, dims = 2)
#then flatten further, see who has spatial recaptures 
caphist.1d <- rowSums(caphist.2d, dims = 1)
#now any cat > 1 is a spatially recapture cat (these would be at multiple traps or same trap)
#then make this a truefalse 
caphist.TF <- caphist.1d > 1
#sum it, how many bears spatcapped? 
sum(caphist.TF)
##which rows? this gets you the row ID of any spatially recaptured cat
caphist.TF.rows <- which(caphist.1d >1 )
#print and look at them, can refer to caphist to check make sure, looks good  
caphist.TF.rows
#then subset the orig matrix, get only the recapped cats
caphist.2d.recaps <- caphist.2d[caphist.TF.rows,]
max(rowSums(caphist.2d.recaps))
min(rowSums(caphist.2d.recaps))
sum(caphist.2d.recaps)
88/9
apply(caphist.2d.recaps, 1, function(row) sum(row != 0))
max(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))
min(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))
sum(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))/9

###
caphist <- sf.tam_2017.L[["caphist"]][[1]]
traps <- sf.tam_2017.L[["traps"]][[1]]
######Getting distances by CAT ID 
#flatten the caphist across occasions so any recaptures are in 1 dimension  
caphist.2d <- rowSums(caphist, dims = 2)
#then flatten further, see who has spatial recaptures 
caphist.1d <- rowSums(caphist.2d, dims = 1)
#now any cat > 1 is a spatially recapture cat (these would be at multiple traps or same trap)
#then make this a truefalse 
caphist.TF <- caphist.1d > 1
#sum it, how many bears spatcapped? 
sum(caphist.TF)
##which rows? this gets you the row ID of any spatially recaptured cat
caphist.TF.rows <- which(caphist.1d >1 )
#print and look at them, can refer to caphist to check make sure, looks good  
caphist.TF.rows
#then subset the orig matrix, get only the recapped cats
caphist.2d.recaps <- caphist.2d[caphist.TF.rows,]
max(rowSums(caphist.2d.recaps))
min(rowSums(caphist.2d.recaps))
sum(caphist.2d.recaps)
185/26
apply(caphist.2d.recaps, 1, function(row) sum(row != 0))
max(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))
min(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))
sum(apply(caphist.2d.recaps, 1, function(row) sum(row != 0)))/26
