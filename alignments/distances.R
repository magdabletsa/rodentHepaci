library(adephylo);library(phytools);library(PhylogeneticEM);library(adegenet);library(vioplot);library(ggtree)
library(ggplot2);library(tidyverse)

#Set working directory
setwd("~/Dropbox/Rprojects/diversity.plot/")

#Read the tree file and start linking the evolutionary traits 
hepaci <- read.nexus("../complete.hepaci.phylogeny/all.hosts.hepaci.aa.mafft.bmge.phy.BOOT.tree")
hepaci 
ggtree(hepaci) + #gives you the tree as stored in the nwk file 
  geom_text(aes(label=node)) +
  geom_tiplab(hjust = -.2, lwd=3) 

all.rodent.taxa <- read.delim("./drop.Tip.list.rodents.txt", header = F)
all.rodent.taxa$V1 = as.character(all.rodent.taxa$V1) 

#Summary plots manuscript (combination of genus/species levels previously)

c1 <- c(D.rod.lop.sta)
c2 <- c(D.rod.myo)
c3 <- c(D.rod.lop.dud)
c4 <- c(D.rod.lop.mac)
c5 <- c(D.rod.rat)
c6 <- c(D.hum.hom)
c7 <- c(D.rod.lop.lat)
c8 <- c(D.pri.pro)
c9 <- c(D.rod.rhi)
c10 <- c(D.pri.col)
c11 <- c(D.bat.oto)
c12 <- c(D.rod.pra)

maxDist <- c(max(D.rod.lop.sta), 
             max(D.rod.myo), 
             max(D.rod.lop.dud), 
             max(D.rod.lop.mac), 
             max(D.rod.rat), 
             max(D.hum.hom), 
             max(D.rod.lop.lat), 
             max(D.pri.pro), 
             max(D.rod.rhi), 
             max(D.pri.col), 
             max(D.bat.oto), 
             max(D.rod.pra))
maxDist

meanDist <- c(mean(D.hum.hom), 
              mean(D.pri.pro), 
              mean(D.pri.col), 
              mean(D.rod.lop.sta), 
              mean(D.rod.myo), 
              mean(D.rod.lop.dud), 
              mean(D.rod.lop.mac), 
              mean(D.rod.rat), 
              mean(D.rod.lop.lat), 
              mean(D.rod.rhi), 
              mean(D.rod.pra), 
              mean(D.bat.oto))
meanDist


boxplot(c1, c2, c3, c4, c5,c6,c7, c8, c9, c10, c11, c12, range=2, xlab="hosts", ylab=" pairwise divergence", 
        names=c("L. stanleyi", "M. glareolus", "L. dudui", "L. machangui", "R. norvegicus", "H. sapiens", 
                "L. laticeps", "Pr. diadema", "R. pruinosus", "C. guereza", "O. martiensseni", "P. jacksoni")) 
points(meanDist, col="red", pch=19)
#lines(maxDist,col="darkgrey",lwd=2)

#arrange by host orders in the previous plot 
boxplot(c6, c8, c10, c1, c2,c3,c4, c5, c7, c9, c12, c11, range=2, xlab="hosts", ylab=" pairwise divergence", 
        names=c("H. sapiens", "Pr. diadema", "C. guereza", "L. stanleyi", "M. glareolus", "L. dudui", 
                "L. machangui", "R. norvegicus", "L. laticeps", "R. pruinosus", "P. jacksoni", "O. martiensseni")) 
points(meanDist, col="red", pch=19)
#lines(maxDist,col="darkgrey",lwd=2)







