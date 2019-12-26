library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

## see calculations in "Gas Transfer Velocity and Flux Calculations.xlsx" file.

streamFlux <- 
  read_csv("~/Dropbox/Stream biogeochemistry research/R Scripts/stream flux averages.csv")
streamFluxreps <-
  read_csv("~/Dropbox/Stream biogeochemistry research/R Scripts/stream flux replicates.csv")

ggplot(streamFlux, aes(x=location, y=CH4.eq3, fill=stream)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=CH4.eq3-sdCH4.eq3, ymax = CH4.eq3+sdCH4.eq3), 
                position = "dodge") +
  scale_fill_discrete(label = c("Heath Creek", "Rice Creek"), name = "", l=40) +
  ylab(expression(paste("Methane Flux (umol C-CH"["4"], " day"^"-1", ")"))) +
  xlab("Sampling Location") +
  ggtitle("Methane Flux Estimate using Eq.3") 

anova(aov(CH4.eq3~location, data=streamFluxreps))
TukeyHSD(aov(CH4.eq3~location, data=streamFluxreps))

ggplot(streamFlux, aes(x=location, y=CH4.eq5, fill=stream)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=CH4.eq5-sdCH4.eq5, ymax = CH4.eq5+sdCH4.eq5), 
                position = "dodge") +
  scale_fill_discrete(label = c("Heath Creek", "Rice Creek"), name = "", l=40) +
  ylab(expression(paste("Methane Flux (umol C-CH"["4"], " day"^"-1", ")"))) +
  ggtitle("Methane Flux Estimate using Eq.5") +
  xlab("Sampling Location")

anova(aov(CH4.eq5~location, data=streamFluxreps))
TukeyHSD(aov(CH4.eq5~location, data=streamFluxreps))

ggplot(streamFlux, aes(x=location, y=N2O.eq3, fill=stream)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=N2O.eq3-sdN2O.eq3, ymax = N2O.eq3+sdN2O.eq3), 
                position = "dodge") +
  scale_fill_discrete(label = c("Heath Creek", "Rice Creek"), name = "", l=40) +
  ylab(expression(paste("Nitrous Oxide Flux (umol N-N"["2"], "O day"^"-1", ")"))) +
  ggtitle("Nitrous Oxide Flux Estimate using Eq.3") +
  xlab("Sampling Location")

ggplot(streamFlux, aes(x=location, y=N2O.eq5, fill=stream)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=N2O.eq5-sdN2O.eq5, ymax = N2O.eq5+sdN2O.eq5), 
                position = "dodge") +
  scale_fill_discrete(label = c("Heath Creek", "Rice Creek"), name = "", l=40) +
  ylab(expression(paste("Nitrous Oxide Flux (umol N-N"["2"], "O day"^"-1", ")"))) +
  ggtitle("Nitrous Oxide Flux Estimate") +
  xlab("Sampling Location")
