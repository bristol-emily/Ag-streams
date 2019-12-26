library(ggplot2)

water <- read.csv("Water-chemistry.csv")
water$date <- as.Date(water$date, format = "%m/%d/%y")

#Correcting for dilution
water$CH4 <- 2 * water$CH4
water$N2O <- 2 * water$N2O
water$SE.methane <- 2 * water$SE.methane
water$SE.N2O <- 2 * water$SE.N2O

## Streams N2O time series
ggplot(water[which (water$location != "RP"), ], aes(x=date, y=N2O)) + 
  geom_point(aes(color = location, group = location)) +
  geom_errorbar(aes(ymin=N2O-SE.N2O, ymax=N2O+SE.N2O, color = location, group = location), width=.05) +
  theme_classic() + 
  xlab(lab = "") +
  ylab("Nitrous Oxide Concentration (mg/L)") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  geom_line(aes(color = location, group = location)) +
  scale_color_manual(values = c("#FF872D", "#FFB884", "#9BCBB5", "#285842"),
                     name = "", labels = c("Heath Creek - Albers", "Heath Creek - Cannon",
                                           "Rice Creek - Cannon", "Rice Creek - Decker")) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  theme(legend.justification = c(1,.85), legend.position = c(0.23,1), 
        legend.text = element_text(size = 6))


## Streams + pipe N2O time series
ggplot(water, aes(x=date, y=N2O, color = location)) + 
  geom_point(shape=20) +
  geom_errorbar(aes(ymin=N2O-SE.N2O, ymax=N2O+SE.N2O), width=.1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme_bw() + 
  ylab("Nitrous Oxide Concentration (ppm)") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  ggtitle("Dissolved Nitrous Oxide Concentrations") +geom_line()

##################################################################################################

## Nitrate Concentrations
ggplot(water[which (water$location != "RP"), ], aes(x=date, y=nitrate)) + 
  geom_point(aes(color = location, group = location), shape=20) +
  geom_errorbar(aes(ymin=nitrate-SE.nitrate, ymax=nitrate+SE.nitrate, color = location), width=.1) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Nitrate Concentration (mg/L)") + 
  xlab("") +
  geom_line(aes(color = location, group = location)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  scale_color_discrete(name="Sampling Location",
                       labels = c("Heath-Albers", "Heath-Cannon", "Rice-Cannon", "Rice-Decker", 
                                  "Rice-Pipe Drainage")) +
  geom_line(aes(color = location, group = location)) +
  scale_color_manual(values = c("#FF872D", "#FFB884", "#C0D1C2", "#7CA080"),
                     name = "", labels = c("Heath Creek - Albers", "Heath Creek - Cannon",
                                                   "Rice Creek - Cannon", "Rice Creek - Decker")) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  theme(legend.justification = c(1,.85), legend.position = c(1,1))

## DOC Concentrations
ggplot(water[which (water$location != "RP"), ], aes(x=date, y=DOC)) + 
  geom_point(aes(color = location, group = location), shape=20) +
  geom_errorbar(aes(ymin=DOC-SE.DOC, ymax=DOC+SE.DOC, color = location), width=.1) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("") +
  ylab("DOC Concentration (mg/L)") + 
  geom_line(aes(color = location, group = location)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  scale_color_discrete(name="Sampling Location",
                       labels = c("Heath-Albers", "Heath-Cannon", "Rice-Cannon", "Rice-Decker", 
                                  "Rice-Pipe Drainage")) +
  geom_line(aes(color = location, group = location)) +
  scale_color_manual(values = c("#FF872D", "#FFB884", "#C0D1C2", "#7CA080"),
                     name = "", labels = c("Heath Creek - Albers", "Heath Creek - Cannon",
                                                   "Rice Creek - Cannon", "Rice Creek - Decker")) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  theme(legend.justification = c(1,.85), legend.position = c(1,1))


## Methane
ggplot(water[which (water$location != "RP"), ], aes(x=date, y=CH4)) + 
  geom_point(aes(color = location, group = location), shape=20) +
  geom_errorbar(aes(ymin=CH4-SE.methane, ymax=CH4+SE.methane, color = location, group = location),
                width=.01) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme_classic() + 
  xlab("") +
  ylab("Methane Concentration (mg/L)") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  geom_line(aes(color = location)) +
  scale_color_manual(values = c("#FF872D", "#FFB884", "#C0D1C2", "#7CA080"),
                     name = "", labels = c("Heath Creek - Albers", "Heath Creek - Cannon",
                                                   "Rice Creek - Cannon", "Rice Creek - Decker")) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  theme(legend.justification = c(1,.85), legend.position = c(1,1))


##################################################################################################
  
## n20 vs no3-
RC <- water[which (water$location == "RC"), ]
RD <- water[which (water$location == "RD"), ]
HA <- water[which (water$location == "HA"), ]
HC <- water[which (water$location == "HC"), ]
  
ggplot(RP, aes(x=temp, y=N2O, col = temp)) + 
  geom_point(shape=20, size = 3) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Nitrate Concentration (ppm)") + ylab("Nitrous Oxide Concentration (ppm)") +
  scale_colour_gradient2(low = "blue", high = "red", midpoint = 10)
  
ggplot(RD, aes(x=nitrate, y=N2O, col = temp)) + 
    geom_point(shape=20, size = 3) +
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    xlab("Nitrate Concentration (ppm)") + ylab("Nitrous Oxide Concentration (ppm)") +
  scale_colour_gradient2(low = "blue", high = "red", midpoint = 10)


summary(lm(CH4~nitrate, data = heath))
    
ggplot(HA, aes(x=nitrate, y=N2O, col = temp)) + 
      geom_point(shape=20, size = 3) +
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black")) +
      xlab("Nitrate Concentration (ppm)") + ylab("Nitrous Oxide Concentration (ppm)") +
  scale_colour_gradient2(low = "blue", high = "red", midpoint = 10)

ggplot(HC, aes(x=nitrate, y=N2O, col = temp)) + 
  geom_point(shape=20, size = 3) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Nitrate Concentration (ppm)") + ylab("Nitrous Oxide Concentration (ppm)") +
  scale_colour_gradient2(low = "blue", high = "red", midpoint = 10)

rice = water[which (water$location == "RC" | water$location == "RD"), ]
heath = water[which (water$location == "HC" | water$location == "HA"), ]

ggplot(water[which (water$location != "RP"), ], aes(x=nitrate, y=N2O, col = location)) + 
  geom_point(shape=20, size = 3) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Nitrate Concentration (ppm)") + ylab("Nitrous Oxide Concentration (ppm)") +
  scale_colour_manual(values = c("#FF872D", "#FFB884", "#9BCBB5", "#285842"),
                      name = "Location", labels = c("Heath Creek - Albers", "Heath Creek - Cannon",
                                                    "Rice Creek - Cannon", "Rice Creek - Decker"))

summary(lm(N2O~temp+nitrate+nitrate:temp, data = RD))


plot(CH4~nitrate, data=HA)

plot(N2O~temp, data = RD)

##################################################################################################

## Methane

ggplot(RD, aes(x=nitrate, y=CH4)) +
  geom_point(shape=20, size = 3) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme_classic() + 
  ylab("Methane Concentration (mg/L)") + 
  xlab("Nitrate Concentration (mg/L)") +
  ggtitle("Rice Creek at Decker Ave") +
  geom_abline(intercept = 24.9523, slope = -0.8742)

ggplot(HA, aes(x=DOC, y=CH4, col=temp)) +
  geom_point(shape=20, size = 3) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme_bw() + 
  ylab("Methane Concentration (ppm)") + 
  xlab("DOC Concentration (ppm)") +
  scale_colour_gradient2(low = "blue", high = "red", midpoint= 10)

##################################################################################################

## Subsurface tiling drainage

RP = water[which (water$location == "RP"), ]
RD = water[which (water$location =="RD"),]

ggplot(RD, aes(x = date)) + 
  geom_point(aes(y=nitrate), col = "#5F738C", shape = 20) +
  geom_line(aes(y=nitrate, linetype = stream), col = "#5F738C") +
  geom_point(aes(y=N2O), col = "#5F738C", shape = 17) +
  geom_line(aes(y=N2O, linetype = location), col = "#5F738C") +
  geom_errorbar(aes(ymin=N2O-SE.N2O, ymax=N2O+SE.N2O), col = "#5F738C", width=.05) +
  geom_errorbar(aes(ymin=nitrate-SE.nitrate, ymax=nitrate+SE.nitrate),col = "#5F738C", width=.05) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme_classic() + 
  xlab("") +
  ylab("Concentration (mg/L)") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  scale_linetype_manual(values = c(1,2),
                          name = "", labels = c("Nitrate", "Nitrous Oxide")) 
  

ggplot(RD, aes(x=nitrate, y=N2O)) +
  geom_point(shape=20, size = 3) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme_bw() + 
  ylab("Nitrous Oxide Concentration (ppm)") + 
  xlab("Nitrate Concentration (ppm)") +
  scale_colour_gradient2(low = "blue", high = "red", midpoint= 10)
  
  
mean(RD$temp, na.rm = TRUE)
mean(HA$temp, na.rm = TRUE)

ggplot(water[which (water$location == "RD" | water$location == "RP"), ], aes(x=date, y=nitrate, col=location)) +
  geom_point() +
  geom_line


