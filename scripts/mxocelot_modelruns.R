rm(list=ls())

# remotes::install_github("jaroyle/oSCR", force = TRUE)
library(oSCR)
setwd("G:/MT FWP Hard Drive Backup/MXOcelot/DesktopCopy_ACTIVE/MX_Ocelot/Routputs")
load("2024_0412_mxocelot_modelbuildingblocks_2sessLRB_singles_LRsep.rda")


###make suitable prevcap objects for the multi-session models
do.prevcap <- function(scrFrame){
  prevcap <- list()
  for (s in 1:length(scrFrame$caphist)) {
    Ys <- scrFrame$caphist[[s]]
    prevcap[[s]] <- array(0, dim = c(dim(Ys)[1], dim(Ys)[2], dim(Ys)[3]))
    first <- matrix(0, dim(Ys)[1], dim(Ys)[2])
    for (i in 1:dim(Ys)[1]) {
      for (j in 1:dim(Ys)[2]) {
        if (sum(Ys[i, j, ]) > 0) {
          first[i, j] <- min((1:(dim(Ys)[3]))[Ys[i, j, ] > 0])
          prevcap[[s]][i, j, 1:first[i, j]] <- 0
          if (first[i, j] < dim(Ys)[3])
            prevcap[[s]][i, j, (first[i, j] + 1):(dim(Ys)[3])] <- 1
        }
      }  
    }
    zeros <- array(0, c(1, dim(prevcap[[s]])[2], dim(prevcap[[s]])[3]))
    prevcap[[s]] <- abind(prevcap[[s]], zeros, along = 1)
  }
  return(prevcap)
}


prevcap <- do.prevcap(sf.tam_2sess.L)
prevcap[[2]] <- array(0,dim=c(38,17,21))

prevcap.L <- prevcap


prevcap <- do.prevcap(sf.tam_2sess.R)
prevcap[[2]] <- array(0,dim=c(41,17,21))

prevcap.R <- prevcap


###########################################
###########Single year mods################
##########################################

##2013 Right

tam_2013.R<-oSCR.fit(model=list(D~1,p0~1,sig~sex),scrFrame = sf.tam_2013.R,ssDF = tam_2013.ssDF)

tam_2013.R.B<-oSCR.fit(model=list(D~1,p0~b,sig~sex),scrFrame = sf.tam_2013.R,ssDF = tam_2013.ssDF)

###2013 Models LEFT

tam_2013.L<-oSCR.fit(model=list(D~1,p0~1,sig~sex),scrFrame = sf.tam_2013.L,ssDF = tam_2013.ssDF)

tam_2013.L.B<-oSCR.fit(model=list(D~1,p0~b,sig~sex),scrFrame = sf.tam_2013.L,ssDF = tam_2013.ssDF)

##2017 Right

tam_2017.R<-oSCR.fit(model=list(D~1,p0~1,sig~sex),scrFrame = sf.tam_2017.R,ssDF = tam_2017.ssDF)

tam_2017.R.B<-oSCR.fit(model=list(D~1,p0~b,sig~sex),scrFrame = sf.tam_2017.R,ssDF = tam_2017.ssDF)

###2017 Models LEFT

tam_2017.L<-oSCR.fit(model=list(D~1,p0~1,sig~sex),scrFrame = sf.tam_2017.L,ssDF = tam_2017.ssDF)

tam_2017.L.B<-oSCR.fit(model=list(D~1,p0~b,sig~sex),scrFrame = sf.tam_2017.L,ssDF = tam_2017.ssDF)


###########################################
###########Multi-session Mods################
##########################################

####RIGHT
tam_2sess.R <- oSCR.fit(model=list(D~session,p0~session,sig~sex),scrFrame = sf.tam_2sess.R,ssDF = tam_2sess.ssDF)

tam_2sess.R.B <- oSCR.fit.addzeroes(model=list(D~session,p0~session + b,sig~sex),
                                    scrFrame = sf.tam_2sess.R,ssDF = tam_2sess.ssDF,
                                    prevcap = prevcap.R)

####LEFT
tam_2sess.L <- oSCR.fit.addzeroes(model=list(D~session,p0~session,sig~sex),scrFrame = sf.tam_2sess.L,ssDF = tam_2sess.ssDF)

tam_2sess.L.fittest <- oSCR.fit(model=list(D~session,p0~session,sig~sex),scrFrame = sf.tam_2sess.L,ssDF = tam_2sess.ssDF)

tam_2sess.L.B <- oSCR.fit.addzeroes(model=list(D~session,p0~session + b,sig~sex),
                                  scrFrame = sf.tam_2sess.L,ssDF = tam_2sess.ssDF,
                                  prevcap = prevcap)





save(tam_2013.R,tam_2013.R.B,
     tam_2013.L,tam_2013.L.B,
     tam_2017.R,tam_2017.R.B,
     tam_2017.L,tam_2017.L.B,
     tam_2sess.R,tam_2sess.R.B,
     tam_2sess.L,tam_2sess.L.B,
     file = "2023_0418_modelruns.RDA")




