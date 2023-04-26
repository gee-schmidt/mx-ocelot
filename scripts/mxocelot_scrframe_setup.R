rm(list=ls())

setwd("G:/MT FWP Hard Drive Backup/MXOcelot/DesktopCopy_ACTIVE/MX_Ocelot/InputData_Edited")
# rm(list=ls())
library(dplyr)
library(car)
library(oSCR)
library(reshape2)
library(raster)
library(rgdal)
library(maptools)
library(ggplot2)
library(sp)
library(rgeos)
library(tibble)
library(lubridate)
library(BiocManager)
library(fuzzyjoin)
library(spatialEco)
library(sf)

####2017 OCELOT DATA FORMATTING 
#load the captures
# ocelot.2017<-read.csv("Capturas_GMS_2017_expanded_dates.csv")
ocelot.2017<-read.csv("Capturas_2017__16Aug2021.csv")

table(ocelot.2017$SIDE)

#load the tdf
tdf.2017 <- read.csv("Trampas_GMS_2017_21SO.csv")
#convert cap date to lubridate
# ocelot.2017$capdate<-ymd(ocelot.2017$Fecha)

###want biweekly sampling occs to match biweekly 2013 data 
# break.df<-data.frame(breaks=seq(ymd("2017-01-01"), ymd("2017-10-31"), by="2 weeks"))
# break.df$week.int<-interval(break.df$breaks, break.df$breaks+days(13))
# break.df$occasion <- 1:22

# getwd()
# write.csv(break.df, file = "2017_sampling_occasions_21.csv", row.names = FALSE)

##make merge-able date columns 
# break.df$start<-int_start(break.df$week.int)
# break.df$end<-int_end(break.df$week.int)
# break.df$start<-ymd(break.df$start)
# break.df$end<-ymd(break.df$end)

##do same w/ capture dataset
# ocelot.2017$start<-ocelot.2017$capdate
# ocelot.2017$end<-ocelot.2017$capdate

#confirm that the date classes match before merging 
# dput(ocelot.2017$start[1])
# dput(ocelot.2017$end[1])
# dput(break.df$start[1])
# dput(break.df$end[1])


###this should get it to the correct occasions for each detection 
# ocelot.occ.2017<-interval_inner_join(ocelot.2017,break.df,by=c("start","end"))
# ocelot.occ.2017[216,11]<-21
# 

##want to merge age and sex info 
# ltest<-subset(agesex.2017, SIDE == "B"| SIDE == "L")
# rtest<-subset(agesex.2017, SIDE == "B"| SIDE == "R")
# 
# 
# ocelot.occ.2017$SEX<-agesex.2017$SEX[match(ocelot.occ.2017$ANIMAL_ID, agesex.2017$ANIMAL_ID)]
# ocelot.occ.2017$AGE<-agesex.2017$AGE[match(ocelot.occ.2017$ANIMAL_ID, agesex.2017$ANIMAL_ID)]
# ocelot.occ.2017$SIDE<-agesex.2017$SIDE[match(ocelot.occ.2017$ANIMAL_ID, agesex.2017$ANIMAL_ID)]
# 

ocelot.2017$session<-2

edf.2017<-ocelot.2017[c(2:8)]
# names(edf.2017)[3]<-"SO"

# edf.2017.L
# edf.2017.R

tdf.2017<-tdf.2017[c(2:25)]

###2013 DATA 
ocelot.2013<-read.csv("Captures_AllSides_2013_13Oct2021.csv")
ocelot.2013$session<-1

table(ocelot.2013$SIDE)

edf.2013<-ocelot.2013
edf.2013$ANIMAL_ID<-edf.2013$ANIMAL_ID+100
tdf.2013<-read.csv("Trampas_2013.csv")

###NEED TO SPLIT 2017 LEFT AND RIGHTS HERE 

edf.2017.L<- subset(edf.2017, SIDE == "L" | SIDE == "B")
edf.2017.R <- subset(edf.2017, SIDE == "R" | SIDE == "B")
edf.2017.B <- subset(edf.2017, SIDE == "B")

edf.2017.L<-edf.2017.L[-c(5,6)]
edf.2017.R<-edf.2017.R[-c(5,6)]
edf.2017.B<-edf.2017.B[-c(5,6)]

unique(edf.2017$SIDE)


edf.2013.L<- subset(edf.2013, SIDE == "L" | SIDE == "B")
edf.2013.R <- subset(edf.2013, SIDE == "R" | SIDE == "B")
edf.2013.B <- subset(edf.2013, SIDE == "B")

edf.2013.L<-edf.2013.L[-c(1,6)]
edf.2013.R<-edf.2013.R[-c(1,6)]
edf.2013.B<-edf.2013.B[-c(1,6)]

unique(edf.2013$SIDE)

###COMBINE THE EDFS

ocelot.2013.2017.L<-rbind(edf.2013.L,edf.2017.L)
ocelot.2013.2017.R<-rbind(edf.2013.R,edf.2017.R)

ocelot.2013.2017.B <- rbind(edf.2013.B,edf.2017.B)

##REDUCE THE TDFS
tdf.2013[,c(2:3)]<-tdf.2013[,c(2:3)]/1000
tdf.2017[,c(2:3)]<-tdf.2017[,c(2:3)]/1000

# unique(ocelot.2013.2017$SEX)

tdf.2013["sep"]<-"/"
tdf.2017["sep"]<-"/"

covs.2013<-read.csv("trapcovs_2013_add.csv")
covs.2013<-covs.2013[-c(2,3)]
covs.2017<-read.csv("trapcovs_2017_add.csv")
covs.2017<-covs.2017[-c(1,3,4)]

tdf.2013.covs<-merge(tdf.2013,covs.2013,by="LOC_ID")
tdf.2017.covs<-merge(tdf.2017,covs.2017,by="LOC_ID")



###ADDING 30m TRAP COVARIATES 

###TESTING MULTISESSION
ocelot.2sess.L<-data2oscr(ocelot.2013.2017.L,tdf=list(tdf.2013.covs,tdf.2017.covs),
                    sess.col = 5,
                    id.col = 2,
                    occ.col = 3,
                    trap.col = 1,
                    sex.col = 4,
                    sex.nacode = "U",
                    tdf.sep = "/",
                    trapcov.names = c("coverutm2","heightutm2"),
                    K= c(9,21),
                    ntraps = c(32,17))

ocelot.2sess.R<-data2oscr(ocelot.2013.2017.R,tdf=list(tdf.2013.covs,tdf.2017.covs),
                        sess.col = 5,
                        id.col = 2,
                        occ.col = 3,
                        trap.col = 1,
                        sex.col = 4,
                        sex.nacode = "U",
                        tdf.sep = "/",
                        trapcov.names = c("coverutm2","heightutm2"),
                        K= c(9,21),
                        ntraps = c(32,17))

plot(ocelot.2sess.L[["scrFrame"]])
plot(ocelot.2sess.R[["scrFrame"]])

ocelot.2sess.B<-data2oscr(ocelot.2013.2017.B,tdf=list(tdf.2013.covs,tdf.2017.covs),
                          sess.col = 5,
                          id.col = 2,
                          occ.col = 3,
                          trap.col = 1,
                          sex.col = 4,
                          sex.nacode = "U",
                          tdf.sep = "/",
                          trapcov.names = c("coverutm2","heightutm2"),
                          K= c(9,21),
                          ntraps = c(32,17))


##Single sessions 
edf.2013.L$session<-1
oscrdat.2013.L<-data2oscr(edf.2013.L,tdf=list(tdf.2013.covs),
                        sess.col = 5,
                        id.col = 2,
                        occ.col = 3,
                        trap.col = 1,
                        sex.col = 4,
                        sex.nacode = "U",
                        tdf.sep = "/",
                        trapcov.names = c("coverutm2","heightutm2"),
                        K= c(9),
                        ntraps = c(32))
edf.2013.R$session<-1
oscrdat.2013.R<-data2oscr(edf.2013.R,tdf=list(tdf.2013.covs),
                          sess.col = 5,
                          id.col = 2,
                          occ.col = 3,
                          trap.col = 1,
                          sex.col = 4,
                          sex.nacode = "U",
                          tdf.sep = "/",
                          trapcov.names = c("coverutm2","heightutm2"),
                          K= c(9),
                          ntraps = c(32))



edf.2017.L$session<-1
oscrdat.2017.L<-data2oscr(edf.2017.L,tdf=list(tdf.2017.covs),
                        sess.col = 5,
                        id.col = 2,
                        occ.col = 3,
                        trap.col = 1,
                        sex.col = 4,
                        sex.nacode = "U",
                        K= c(21),
                        tdf.sep = "/",
                        trapcov.names = c("coverutm2","heightutm2"),
                        ntraps = c(17))

edf.2017.R$session<-1
oscrdat.2017.R<-data2oscr(edf.2017.R,tdf=list(tdf.2017.covs),
                        sess.col = 5,
                        id.col = 2,
                        occ.col = 3,
                        trap.col = 1,
                        sex.col = 4,
                        sex.nacode = "U",
                        K= c(21),
                        tdf.sep = "/",
                        trapcov.names = c("coverutm2","heightutm2"),
                        ntraps = c(17))
# 
# getwd()
# write.csv(edf.2013,"capturas_2013.csv")
# write.csv(tdf.2013,"trampas_2013.csv")
# write.csv(edf.2017,"capturas_2017.csv")
# write.csv(tdf.2017,"trampas_2017.csv")

ocelot.2sess.L$scrFrame


tam_2sess.ssDF<-make.ssDF(ocelot.2sess.L[["scrFrame"]], buffer = 5, res = .5)

tam_2013.ssDF<-make.ssDF(oscrdat.2013.L$scrFrame, buffer = 5, res = .5)

tam_2017.ssDF<-make.ssDF(oscrdat.2017.L$scrFrame, buffer = 5, res = .5)

##isolate scrFrames for all
sf.tam_2sess.L<-ocelot.2sess.L$scrFrame
sf.tam_2sess.R<-ocelot.2sess.R$scrFrame
sf.tam_2sess.B<-ocelot.2sess.B$scrFrame

sf.tam_2013.L<-oscrdat.2013.L$scrFrame
sf.tam_2013.R<-oscrdat.2013.R$scrFrame

sf.tam_2017.L<-oscrdat.2017.L$scrFrame
sf.tam_2017.R<-oscrdat.2017.R$scrFrame


plot.ssDF(tam_2sess.ssDF,sf.tam_2sess.L)
plot.ssDF(tam_2013.ssDF)
plot.ssDF(tam_2017.ssDF)



#MASK SINGLE 2017 
laguna <- readOGR(dsn = "G:/MT FWP Hard Drive Backup/MXOcelot/DesktopCopy_ACTIVE/MX_Ocelot/State-space_14April2021", layer = "2017OcelotArea_Laguna")
# 
plot(laguna)

laguna1 <- st_as_sf(laguna)

ssdf_tomask<- tam_2017.ssDF[[1]]
ssdf_tomask<-ssdf_tomask[,c(1,2)]*1000

spdf <- SpatialPoints(coords = ssdf_tomask,
                               proj4string = CRS("+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
points(spdf)

spdf1 <- st_as_sf(spdf)
masked <- erase.point(spdf1,laguna1)
plot(masked)
# 
masked_df<- data.frame(st_coordinates(masked[,1]))
masked_df_reduced<-masked_df[,c(1:2)]/1000
tam_2017.ssDF[[1]]<-masked_df_reduced
plot(tam_2017.ssDF)

####MASK 2 SESS 2017 ssdf 
tam_2sess.ssDF[[2]]<-masked_df_reduced

plot(tam_2sess.ssDF)



#####CURRENTLY NOT DOING HAB COVS ANYWAY ON SSDF, not bothering w/ this rn####

# 
###Extract rasters here for TWO SESSION AND SINGLE SESSION 
# load("E:/MXOcelot/Spatial/2021_0816_ocelotmx_ssDFcovrasters.rda")
# .rs.unloadPackage("tidyr")
# tam_2017.ssDF.cover<-extract.rast(tam_2017.ssDF, cover_500.scale,  mult = 1000,cov.name = "cover")
# tam_2017.ssDF.height<-extract.rast(tam_2017.ssDF.cover, height_500.scale, mult = 1000, cov.name = "height")
# 
# tam_2013.ssDF.cover<-extract.rast(tam_2013.ssDF, cover_500.scale,  mult = 1000,cov.name = "cover")
# tam_2013.ssDF.height<-extract.rast(tam_2013.ssDF.cover, height_500.scale, mult = 1000, cov.name = "height")
# 
# tam_2sess.ssDF.cover<-extract.rast(tam_2sess.ssDF, cover_500.scale,  mult = 1000,cov.name = "cover")
# tam_2sess.ssDF.height<-extract.rast(tam_2sess.ssDF.cover, height_500.scale, mult = 1000, cov.name = "height")

# ###Getting the habitat covs on the traps.. prob not most efficient way 
# caphist <- sf.tam_2013$caphist
# traps <- sf.tam_2013$traps
# trapCovs <- sf.tam_2013$trapCovs
# sf <- make.scrFrame(caphist = caphist, traps = traps, trapCovs = trapCovs, rsfDF =tam_2013.ssDF.height, type = "scr")
# sf.tam_2013$trapCovs<-sf[["trapCovs"]]
# sf.tam_2sess.L[["trapCovs"]][[1]]<-sf[["trapCovs"]][[1]]
# sf.tam_2sess.R[["trapCovs"]][[1]]<-sf[["trapCovs"]][[1]]
# 
# trapCovs.2013<-trapCovs
# #same for 2017
# caphist <- sf.tam_2017.L$caphist
# traps <- sf.tam_2017.L$traps
# trapCovs <- sf.tam_2017.L$trapCovs
# sf <- make.scrFrame(caphist = caphist, traps = traps, trapCovs = trapCovs, rsfDF =tam_2017.ssDF.height, type = "scr")
# sf.tam_2017.L$trapCovs<-sf[["trapCovs"]]
# sf.tam_2017.R$trapCovs<-sf[["trapCovs"]]
# trapCovs.2017<-trapCovs
# sf.tam_2sess.L[["trapCovs"]][[2]]<-sf[["trapCovs"]][[1]]
# sf.tam_2sess.R[["trapCovs"]][[2]]<-sf[["trapCovs"]][[1]]

# 
# ###same for 2 session version 
# 

# sf.tam_2sess$trapCovs<-list(trapCovs.2013,trapCovs.2017)
# rightframe<-print.scrFrame.new(sf.tam_2017.R)
# leftframe<-print.scrFrame.new(sf.tam_2017.L)

####AND... i think that is it for the building blocks?!
##then save it all 
getwd()
setwd("G:/MT FWP Hard Drive Backup/MXOcelot/DesktopCopy_ACTIVE/MX_Ocelot/Routputs")
save(tam_2013.ssDF,tam_2017.ssDF,tam_2sess.ssDF,
     sf.tam_2013.L,sf.tam_2013.R,
     sf.tam_2017.L,sf.tam_2017.R,
     sf.tam_2sess.L,sf.tam_2sess.R,sf.tam_2sess.B,
     file="2024_0412_mxocelot_modelbuildingblocks_2sessLRB_singles_LRsep.rda")

