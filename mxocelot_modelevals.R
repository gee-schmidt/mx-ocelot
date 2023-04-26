# rm(list=ls())

# remotes::install_github("jaroyle/oSCR", force = TRUE)
library(oSCR)
library(car)
setwd("G:/MT FWP Hard Drive Backup/MXOcelot/DesktopCopy_ACTIVE/MX_Ocelot/Routputs")

load("2023_0418_modelruns.RDA")

exp(-0.626)
inv.logit
# exp(-.727)
x = -0.626
exp(x)/(1+exp(x))

log(0.348/(1-0.348))

0.348/(1-0.348)

pp <- tam_2sess.R$outStats$mle
names(pp) <- paste(tam_2sess.R$outStats$parameters)
pp
vcv <- solve(tam_2sess.R$rawOutput$hessian)

pp

deltaMethod(object=pp, vcov.=vcv, g="(exp(psi.constant)/(1+exp(psi.constant)))")



pp <- tam_2sess.L$outStats$mle
names(pp) <- paste(tam_2sess.L$outStats$parameters)
pp
vcv <- solve(tam_2sess.L$rawOutput$hessian)

deltaMethod(object=pp, vcov.=vcv, g="(exp(psi.constant)/(1+exp(psi.constant)))") 

######
# write.csv(ms.L[["aic.tab"]], file = "2022_0207_leftocelotmodsel.csv")
# 
# write.csv(tam_2sess.L.9[["outStats"]], file = "2022_0210_leftocelotmod1.csv")
# # write.csv(tam_2sess.L.12[["outStats"]], file = "2022_0210_leftocelotmod2.csv")
# write.csv(tam_2sess.L.7[["outStats"]], file = "2022_0210_leftocelotmod3.csv")
# 
# write.csv(tam_2sess.R.9[["outStats"]], file = "2022_0210_rightocelotmod1.csv")


###New dataframes for get.real()
nd.1 <- data.frame(session = factor(1))
nd.1.sex <- data.frame(session = factor(1), 
                       sex = factor(c(0,1)))
nd.2 <- data.frame(session = factor(c(1,2)))
nd.2.sex <- data.frame(session = factor(c(1,2)),
                       sex = factor(c(0,1)))

nd.b <- data.frame(b=c(0,1))
nd.b.sex <- data.frame(b=c(0,1),
                       sex = factor(c(0,1)))
nd.2.b <- data.frame(session = factor(c(1,2,1,2)), b=c(0,0,1,0))
nd.2.b.sex <- data.frame(session = factor(c(1,2,1,2)), b=c(0,0,1,0),
                         sex = factor(c(0,1)))



###FITLISTS

fl.2013.L <- fitList.oSCR(list(tam_2013.L,tam_2013.L.B), rename = TRUE) 

ms.2013.L <- modSel.oSCR(fl.2013.L)

ms.2013.L

fl.2013.R <- fitList.oSCR(list(tam_2013.R,tam_2013.R.B), rename = TRUE) 

ms.2013.R <- modSel.oSCR(fl.2013.R)

ms.2013.R

fl.2017.L <- fitList.oSCR(list(tam_2017.L,tam_2017.L.B), rename = TRUE) 

ms.2017.L <- modSel.oSCR(fl.2017.L)

ms.2017.L

fl.2017.R <- fitList.oSCR(list(tam_2017.R,tam_2017.R.B), rename = TRUE) 

ms.2017.R <- modSel.oSCR(fl.2017.R)

ms.2017.R

fl.2sess.L <- fitList.oSCR(list(tam_2sess.L,tam_2sess.L.B), rename = TRUE)

ms.2sess.L <- modSel.oSCR(fl.2sess.L)

ms.2sess.L

fl.2sess.R <- fitList.oSCR(list(tam_2sess.R,tam_2sess.R.B), rename = TRUE)

ms.2sess.R <- modSel.oSCR(fl.2sess.R)

ms.2sess.R




modsels <- as.data.frame(rbind(ms.2013.R$aic.tab,ms.2017.R$aic.tab,ms.2sess.R$aic.tab,
                                ms.2013.L$aic.tab,ms.2017.L$aic.tab,ms.2sess.L$aic.tab))

modsels$Side <- c("Right","Right","Right","Right","Right","Right",
                  "Left","Left","Left","Left","Left","Left")
modsels$Site <- c("Inland","Inland","Coastal","Coastal","Multi-session","Multi-session",
                  "Inland","Inland","Coastal","Coastal","Multi-session","Multi-session")
getwd()
write.csv(modsels,file = "2023_0419_behaviormodsel.csv")


#####FOCUSING ON NO BEHAVIOR

###Right

#2013

dens.2013.R <- get.real(tam_2013.R, type = "dens", d.factor = 400,N_sex = F, newdata = nd.1)
dens.2013.R.sex <- get.real(tam_2013.R, type = "dens", d.factor = 400,N_sex = T, newdata = nd.1)
det.2013.R <- get.real(tam_2013.R, type = "det", newdata = nd.1)
sig.2013.R <- get.real(tam_2013.R, type = "sig", newdata = nd.1.sex)

#2017

dens.2017.R <- get.real(tam_2017.R, type = "dens", d.factor = 400,N_sex = F, newdata = nd.1)
dens.2017.R.sex <- get.real(tam_2017.R, type = "dens", d.factor = 400,N_sex = T, newdata = nd.1)
det.2017.R <- get.real(tam_2017.R, type = "det", newdata = nd.1)
sig.2017.R <- get.real(tam_2017.R, type = "sig", newdata = nd.1.sex)

#Both

dens.2sess.R <- get.real(tam_2sess.R, type = "dens", d.factor = 400,N_sex = F, newdata = nd.2)
dens.2sess.R.sex <- get.real(tam_2sess.R, type = "dens", d.factor = 400,N_sex = T, newdata = nd.2)
det.2sess.R <- get.real(tam_2sess.R, type = "det", newdata = nd.2)
sig.2sess.R <- get.real(tam_2sess.R, type = "sig", newdata = nd.2.sex)


###Left

#2013

dens.2013.L <- get.real(tam_2013.L, type = "dens", d.factor = 400,N_sex = F, newdata = nd.1)
dens.2013.L.sex <- get.real(tam_2013.L, type = "dens", d.factor = 400,N_sex = T, newdata = nd.1)
det.2013.L <- get.real(tam_2013.L, type = "det", newdata = nd.1)
sig.2013.L <- get.real(tam_2013.L, type = "sig", newdata = nd.1.sex)

#2017

dens.2017.L <- get.real(tam_2017.L, type = "dens", d.factor = 400,N_sex = F, newdata = nd.1)
dens.2017.L.sex <- get.real(tam_2017.L, type = "dens", d.factor = 400,N_sex = T, newdata = nd.1)
det.2017.L <- get.real(tam_2017.L, type = "det", newdata = nd.1)
sig.2017.L <- get.real(tam_2017.L, type = "sig", newdata = nd.1.sex)

#Both

dens.2sess.L <- get.real(tam_2sess.L, type = "dens", d.factor = 400,N_sex = F, newdata = nd.2)
dens.2sess.L.sex <- get.real(tam_2sess.L, type = "dens", d.factor = 400,N_sex = T, newdata = nd.2)
det.2sess.L <- get.real(tam_2sess.L, type = "det", newdata = nd.2)
sig.2sess.L <- get.real(tam_2sess.L, type = "sig", newdata = nd.2.sex)





#####SINGLE SESSION OUTSTATS 

single.outstats <- rbind(tam_2013.R[["outStats"]][c(4,1,2,3,5),c(1,2,3)],
                         tam_2013.L[["outStats"]][c(4,1,2,3,5),c(1,2,3)],
                         tam_2017.R[["outStats"]][c(4,1,2,3,5),c(1,2,3)],
                         tam_2017.L[["outStats"]][c(4,1,2,3,5),c(1,2,3)])

single.outstats$site <- rep(c("Inland","Coastal"), each = 10)

single.outstats$side <- rep(c("Right","Left"), each = 5, times = 2)



pp <- tam_2013.R$outStats$mle
names(pp) <- paste(tam_2013.R$outStats$parameters)
vcv <- solve(tam_2013.R$rawOutput$hessian)
psi.2013.R <- deltaMethod(object=pp, vcov.=vcv, g="(exp(psi.constant)/(1+exp(psi.constant)))")
names(psi.2013.R)<-c("estimate","se","lwr","upr")
rownames(psi.2013.R) <- 1


pp <- tam_2013.L$outStats$mle
names(pp) <- paste(tam_2013.L$outStats$parameters)
vcv <- solve(tam_2013.L$rawOutput$hessian)
psi.2013.L <- deltaMethod(object=pp, vcov.=vcv, g="(exp(psi.constant)/(1+exp(psi.constant)))")
names(psi.2013.L)<-c("estimate","se","lwr","upr")
rownames(psi.2013.L) <- 1

pp <- tam_2017.R$outStats$mle
names(pp) <- paste(tam_2017.R$outStats$parameters)
vcv <- solve(tam_2017.R$rawOutput$hessian)
psi.2017.R <- deltaMethod(object=pp, vcov.=vcv, g="(exp(psi.constant)/(1+exp(psi.constant)))")
names(psi.2017.R)<-c("estimate","se","lwr","upr")
rownames(psi.2017.R) <- 1

pp <- tam_2017.L$outStats$mle
names(pp) <- paste(tam_2017.L$outStats$parameters)
vcv <- solve(tam_2017.L$rawOutput$hessian)
psi.2017.L <- deltaMethod(object=pp, vcov.=vcv, g="(exp(psi.constant)/(1+exp(psi.constant)))")
names(psi.2017.L)<-c("estimate","se","lwr","upr")
rownames(psi.2017.L) <- 1


single.trans <- rbind(dens.2013.R[,c(1:4)],
det.2013.R[,c(2:5)],
sig.2013.R[,c(1:4)],
psi.2013.R[,c(1:4)],

dens.2013.L[,c(1:4)],
det.2013.L[,c(2:5)],
sig.2013.L[,c(1:4)],
psi.2013.L[,c(1:4)],

dens.2017.R[,c(1:4)],
det.2017.R[,c(2:5)],
sig.2017.R[,c(1:4)],
psi.2017.R[,c(1:4)],

dens.2017.L[,c(1:4)],
det.2017.L[,c(2:5)],
sig.2017.L[,c(1:4)],
psi.2017.L[,c(1:4)]
)

single.outstats <- cbind(single.outstats,single.trans)


write.csv(single.outstats,file = "2023_0423_singlesession_estimates.csv")



#####FOCUSING ON YES BEHAVIOR 


###Right

#2013

dens.2013.R.B <- get.real(tam_2013.R.B, type = "dens", d.factor = 400,N_sex = F, newdata = nd.b)
dens.2013.R.sex.B <- get.real(tam_2013.R.B, type = "dens", d.factor = 400,N_sex = T, newdata = nd.b)
det.2013.R.B <- get.real(tam_2013.R.B, type = "det", newdata = nd.b)
sig.2013.R.B <- get.real(tam_2013.R.B, type = "sig", newdata = nd.b.sex)

#2017

dens.2017.R.B <- get.real(tam_2017.R.B, type = "dens", d.factor = 400,N_sex = F, newdata = nd.b)
dens.2017.R.sex.B <- get.real(tam_2017.R.B, type = "dens", d.factor = 400,N_sex = T, newdata = nd.b)
det.2017.R.B <- get.real(tam_2017.R.B, type = "det", newdata = nd.b)
sig.2017.R.B <- get.real(tam_2017.R.B, type = "sig", newdata = nd.b.sex)

dens.2sess.R.B <- get.real(tam_2sess.R.B, type = "dens", d.factor = 400,N_sex = F, newdata = nd.2.b)
dens.2sess.R.sex.B <- get.real(tam_2sess.R.B, type = "dens", d.factor = 400,N_sex = T, newdata = nd.2.b)
det.2sess.R.B <- get.real(tam_2sess.R.B, type = "det", newdata = nd.2.b)
sig.2sess.R.B <- get.real(tam_2sess.R.B, type = "sig", newdata = nd.2.b.sex)


###Left

#2013

dens.2013.L.B <- get.real(tam_2013.L.B, type = "dens", d.factor = 400,N_sex = F, newdata = nd.b)
dens.2013.L.sex.B <- get.real(tam_2013.L.B, type = "dens", d.factor = 400,N_sex = T, newdata = nd.b)
det.2013.L.B <- get.real(tam_2013.L.B, type = "det", newdata = nd.b)
sig.2013.L.B <- get.real(tam_2013.L.B, type = "sig", newdata = nd.b.sex)

#2017

dens.2017.L.B <- get.real(tam_2017.L.B, type = "dens", d.factor = 400,N_sex = F, newdata = nd.b)
dens.2017.L.sex.B <- get.real(tam_2017.L.B, type = "dens", d.factor = 400,N_sex = T, newdata = nd.b)
det.2017.L.B <- get.real(tam_2017.L.B, type = "det", newdata = nd.b)
sig.2017.L.B <- get.real(tam_2017.L.B, type = "sig", newdata = nd.b.sex)

#Both

dens.2sess.L.B <- get.real(tam_2sess.L.B, type = "dens", d.factor = 400,N_sex = F, newdata = nd.2.b)
dens.2sess.L.sex.B <- get.real(tam_2sess.L.B, type = "dens", d.factor = 400,N_sex = T, newdata = nd.2.b)
det.2sess.L.B <- get.real(tam_2sess.L.B, type = "det", newdata = nd.2.b)
sig.2sess.L.B <- get.real(tam_2sess.L.B, type = "sig", newdata = nd.2.b.sex)


###NOW take these outputs and turn them into tables and plots as needed for the behavior section 


####behav table 
tam_2sess.R.B

multisess.b.outstats <- rbind(tam_2sess.R.B[["outStats"]][c(6,7,1,2,3,4,5,8),c(1,2,3)])

pp <- tam_2sess.R.B$outStats$mle
names(pp) <- paste(tam_2sess.R.B$outStats$parameters)
vcv <- solve(tam_2sess.R.B$rawOutput$hessian)
psi.2sess.R.B <- deltaMethod(object=pp, vcov.=vcv, g="(exp(psi.constant)/(1+exp(psi.constant)))")
names(psi.2sess.R.B)<-c("estimate","se","lwr","upr")
rownames(psi.2sess.R.B) <- 1


multisess.b.outstats <- cbind(multisess.b.outstats, rbind(dens.2sess.R.B[c(1:2),c(1:4)],
det.2sess.R.B[c(1:3),c(3:6)],
sig.2sess.R.B[c(1:2),c(1:4)],
psi.2sess.R.B[,c(1:4)]))


write.csv(multisess.b.outstats, file = "2023_0424_multisess_w_behavior_ests.csv")

det.2013.R.B$session <- c("single",'single')
det.2sess.R.B <- det.2sess.R.B[c(1,3),c(2:6)]
det.2sess.R.B$session <- c("multi","multi")

det.R <- rbind(det.2013.R.B,det.2sess.R.B)

det.R$side <- "R"


det.2013.L.B$session <- c("single",'single')
det.2sess.L.B <- det.2sess.L.B[c(1,3),c(2:6)]
det.2sess.L.B$session <- c("multi","multi")

det.L <- rbind(det.2013.L.B,det.2sess.L.B)

det.L$side <- "L"

det.B <- rbind(det.R, det.L)

det.B$behav <- ifelse(det.B$b == 0, "First Capture", "> First Capture")
det.B$behav <- factor(det.B$behav, levels = c("First Capture","> First Capture"))
det.B$side <- factor(det.B$side, levels = c("R","L"), labels = c("Right","Left"))
det.B$session <- factor(det.B$session, levels = c("single","multi"),labels = c("Single-session","Multi-session"))


library(ggplot2)


det.plot <- ggplot(det.B, aes(x = behav, y = estimate, ymin = lwr, ymax = upr)) +
  geom_pointrange(size = 0.8)  + xlab("Behavioral Effect") + ylab ("Detection Probability")+
  theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = "grey70",linetype = 'dotted'),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2, size = 16),
        axis.title.x = element_text(vjust = -0.2, size = 16),
        axis.text.x =  element_text(size = 12, face = "italic"), 
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(),
        axis.text = element_text(size = 10),
        legend.key = element_rect(colour = NA),
        legend.title = element_text(face="bold",size=12),
        legend.box.background = element_rect(color = "black", linewidth = 1.4),
        legend.background = element_rect(color = NA),
        legend.position = c(.93,.82),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold",size=20))+ facet_wrap(~side+session)

det.plot
ggsave("G:/MT FWP Hard Drive Backup/MXOcelot/DesktopCopy_ACTIVE/MX_Ocelot/Routputs/supp_behav_2023_0424.png",plot = det.plot, width = 10,height = 8, dpi = 800)

#####BEHAVIOR EVALUATION 
tam_2sess.L.9.behav
dens.L.9 <- get.real(tam_2sess.L.9.behav, type = "dens", d.factor = 400,N_sex = F, newdata = new.data)

det.test.behav.2sess <- get.real(tam_2sess.L.9.behav, type = "det", newdata = new.data.5)

library(ggplot2)

det.test.behav.2sess <- det.test.behav.2sess[c(1,2,4),]

det.test.behav.2sess$detection_probability <- c("coastal","inland first capture","inland subsequent captures")

ggplot(det.test.behav.2sess, aes(x = detection_probability, y = estimate, ymin = lwr, ymax = upr, shape = session)) +
  geom_pointrange(size = 0.8)



det.test.behav.2013L <- get.real(tam_2013.L.sigsex.behav, type = "det", newdata = new.data.4)

ggplot(det.test.behav.2013L, aes(x = eval, y = estimate, ymin = lwr, ymax = upr)) +
  geom_pointrange(size = 0.8) 



