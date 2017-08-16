library(shiny)
library(ggplot2)
library(car)
library(multcomp)
library(RColorBrewer)
library(doBy)
library(plotrix)
library(car)
library(gplots)
library(ggrepel)
library(colorRamps)
library(plotly)
library(shinythemes)

pimp <- read.csv("Mitch_TPA_flm.csv")
pimp$Accession <- as.character(pimp$Accession)

PimPsum <- summaryBy(NapDW + Na_mM + KpDW + K_mM + NapK ~ Accession + Treatment, data = pimp, FUN = function(x) { c(m = mean(x), s = sd(x), se = std.error(x)) })
head(PimPsum)
PIMPavg <- PimPsum[,c(1:3,6,9,12,15)]
PIMPavg_0 <- subset(PIMPavg, PIMPavg$Treatment == "C")
PIMPavg_S <- subset(PIMPavg, PIMPavg$Treatment == "S")

for(i in 3:ncol(PIMPavg_0)){
  names(PIMPavg_0)[i] <- paste(names(PIMPavg_0[i]),"0", sep="_")}

PIMPavg_0 <- PIMPavg_0[,c(1,3:ncol(PIMPavg_0))]

for(i in 3:ncol(PIMPavg_S)){
  names(PIMPavg_S)[i] <- paste(names(PIMPavg_S[i]),"75", sep="_")}

PIMPavg_S <- PIMPavg_S[,c(1,3:ncol(PIMPavg_S))]

PIMPavg_all <- merge(PIMPavg_0, PIMPavg_S, by = "Accession", all = T)

