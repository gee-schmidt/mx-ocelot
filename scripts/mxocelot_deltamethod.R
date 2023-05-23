library(car)

ocelot <- read.csv("C:/Users/greta/Downloads/mxocelot_deltamethodresults_Mitch_Input_Vers2.csv")


obj <- c("n" = ocelot[1,4])
sigma <- matrix(c(ocelot[,5]),1,1)
ocelot[1,8]
g = paste("n/",ocelot[1,8],sep="")
# ocelot[12:15]<-deltaMethod(object = obj, g="n/108.98", vcov = sigma)
deltaMethod(object = obj, g="n/59.97", vcov = sigma)



# obj <- c("X" = 3.1, "W" = 3.4)
# sigma <- matrix(c(0.8^2, 0.55, 0.55, 0.31^2), 2, 2, byrow = T)
# deltaMethod(object = obj, g="X * W", vcov = sigma)


###trying to streamline this w function I can apply to do it all 

dm_func <- function(input) {
  obj <- c("n" = as.numeric(input[1]))
  sigma <- matrix(c(as.numeric(input[2])),1,1)
  g = paste("n/",input[3],sep="")
  return(deltaMethod(object=obj,g=g,vcov=sigma))
}

###getting the outputs for mmdm and 1/2mmdm 
ocelot[12:15]<-do.call(rbind.data.frame,apply(ocelot[,c(4,5,8)],1,dm_func))
ocelot[16:19]<-do.call(rbind.data.frame,apply(ocelot[,c(4,5,9)],1,dm_func))

##putting it into 100km2 
ocelot[12:19] <- ocelot[12:19]*100


###naming it right 
names(ocelot[12:19])<-c("estimate_MMDM","SE_MMDM","lwr_2point5_MMDM","upr_97point5_MMDM",
                        "estimate_halfMMDM","SE_halfMMDM","lwr_2point5_halfMMDM","upr_97point5_halfMMDM")

##above doesn't seem to work for whatever reason


##testing the function
# dm_func(ocelot[1,c(4,5,8)])
# input <- ocelot[1,c(4,5,8)]
# dm_func(input)
# obj <- as.numeric(c("n" = input[1]))
# sigma <- matrix(c(as.numeric(input[2])),1,1)
# g = paste("n/",input[3],sep="")
# input[1]

write.csv(ocelot, "C:/Users/greta/OneDrive/mxocelot_deltamethodresults.csv",row.names = FALSE)