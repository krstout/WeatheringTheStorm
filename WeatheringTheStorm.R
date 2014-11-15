### CODE FOR WEATHERING THE STORM

## >>>>> NOTE: Better to script line by line instead of all at once <<<<< 
## >>>>> NOTE: Some lines will take a few minutes to run <<<<<

rm(list = ls()) # This clears the R environment


setwd("C:\\Users\\Sheryl\\Documents\\Deshaies, Stout, and Tillman") 

govparty <- read.csv("Data/GovParty.csv")
presparty <- read.csv("Data/PresParty.csv")

govdata <- read.csv("Data/problematicgovdata.csv") # Reads in original data
govdata[8563, "inc.twpct"] <- 72.71 # Fixes wrong observation

#install.packages("arm")
#install.packages("compactr")
#install.packages("MASS")
#install.packages("car")
#install.packages("robust")
#install.packages("texreg")

library(MASS)
library(arm)
library(compactr)
library(car)
library(robust)
library(plyr)
library(texreg)
library(ggplot2)

party <- join(presparty, govparty)

party$same_as_pres <- ifelse(party$pres_party == party$govparty_b, 1, 0)

# Creating dummies for each state
govdata$state[govdata$countyfips > 1000 & govdata$countyfips < 2000] <- "Alabama"
govdata$state[govdata$countyfips > 2000 & govdata$countyfips < 3000] <- "Alaska"
govdata$state[govdata$countyfips > 4000 & govdata$countyfips < 5000] <- "Arizona" 
govdata$state[govdata$countyfips > 5000 & govdata$countyfips < 6000] <- "Arkansas"
govdata$state[govdata$countyfips > 6000 & govdata$countyfips < 7000] <- "California"
govdata$state[govdata$countyfips > 8000 & govdata$countyfips < 9000] <- "Colorado"
govdata$state[govdata$countyfips > 9000 & govdata$countyfips < 10000] <- "Connecticut"
govdata$state[govdata$countyfips > 10000 & govdata$countyfips < 11000] <- "Delaware"
govdata$state[govdata$countyfips > 12000 & govdata$countyfips < 13000] <- "Florida"
govdata$state[govdata$countyfips > 13000 & govdata$countyfips < 14000] <- "Georgia"
govdata$state[govdata$countyfips > 15000 & govdata$countyfips < 16000] <- "Hawaii"
govdata$state[govdata$countyfips > 16000 & govdata$countyfips < 17000] <- "Idaho"
govdata$state[govdata$countyfips > 17000 & govdata$countyfips < 18000] <- "Illinois"
govdata$state[govdata$countyfips > 18000 & govdata$countyfips < 19000] <- "Indiana"
govdata$state[govdata$countyfips > 19000 & govdata$countyfips < 20000] <- "Iowa"
govdata$state[govdata$countyfips > 20000 & govdata$countyfips < 21000] <- "Kansas"
govdata$state[govdata$countyfips > 21000 & govdata$countyfips < 22000] <- "Kentucky"
govdata$state[govdata$countyfips > 22000 & govdata$countyfips < 23000] <- "Louisiana"
govdata$state[govdata$countyfips > 23000 & govdata$countyfips < 24000] <- "Maine"
govdata$state[govdata$countyfips > 24000 & govdata$countyfips < 25000] <- "Maryland"
govdata$state[govdata$countyfips > 25000 & govdata$countyfips < 26000] <- "Massachusetts"
govdata$state[govdata$countyfips > 26000 & govdata$countyfips < 27000] <- "Michigan"
govdata$state[govdata$countyfips > 27000 & govdata$countyfips < 28000] <- "Minnesota"
govdata$state[govdata$countyfips > 28000 & govdata$countyfips < 29000] <- "Mississippi"
govdata$state[govdata$countyfips > 29000 & govdata$countyfips < 30000] <- "Missouri"
govdata$state[govdata$countyfips > 30000 & govdata$countyfips < 31000] <- "Montana"
govdata$state[govdata$countyfips > 31000 & govdata$countyfips < 32000] <- "Nebraska"
govdata$state[govdata$countyfips > 32000 & govdata$countyfips < 33000] <- "Nevada"
govdata$state[govdata$countyfips > 33000 & govdata$countyfips < 34000] <- "New Hampshire"
govdata$state[govdata$countyfips > 34000 & govdata$countyfips < 35000] <- "New Jersey"
govdata$state[govdata$countyfips > 35000 & govdata$countyfips < 36000] <- "New Mexico"
govdata$state[govdata$countyfips > 36000 & govdata$countyfips < 37000] <- "New York"
govdata$state[govdata$countyfips > 37000 & govdata$countyfips < 38000] <- "North Carolina"
govdata$state[govdata$countyfips > 38000 & govdata$countyfips < 39000] <- "North Dakota"
govdata$state[govdata$countyfips > 39000 & govdata$countyfips < 40000] <- "Ohio"
govdata$state[govdata$countyfips > 40000 & govdata$countyfips < 41000] <- "Oklahoma"
govdata$state[govdata$countyfips > 41000 & govdata$countyfips < 42000] <- "Oregon"
govdata$state[govdata$countyfips > 42000 & govdata$countyfips < 43000] <- "Pennsylvania"
govdata$state[govdata$countyfips > 44000 & govdata$countyfips < 45000] <- "Rhode Island"
govdata$state[govdata$countyfips > 45000 & govdata$countyfips < 46000] <- "South Carolina"
govdata$state[govdata$countyfips > 46000 & govdata$countyfips < 47000] <- "South Dakota"
govdata$state[govdata$countyfips > 47000 & govdata$countyfips < 48000] <- "Tennessee"
govdata$state[govdata$countyfips > 48000 & govdata$countyfips < 49000] <- "Texas"
govdata$state[govdata$countyfips > 49000 & govdata$countyfips < 50000] <- "Utah"
govdata$state[govdata$countyfips > 50000 & govdata$countyfips < 51000] <- "Vermont"
govdata$state[govdata$countyfips > 51000 & govdata$countyfips < 52000] <- "Virginia"
govdata$state[govdata$countyfips > 53000 & govdata$countyfips < 54000] <- "Washington"
govdata$state[govdata$countyfips > 54000 & govdata$countyfips < 55000] <- "West Virginia"
govdata$state[govdata$countyfips > 55000 & govdata$countyfips < 56000] <- "Wisconsin"
govdata$state[govdata$countyfips > 56000 & govdata$countyfips < 57000] <- "Wyoming"

govdata <- join(govdata, party)

# Turning declarations into a dichotomous variable

govdata$dideclarations <- ifelse(govdata$disdecs.all.6mo > 0,
                                 c(1), c(0))


### REPLICATING TABLES AND FIGURES


## Model for governor vote % as dependent variable

#>>>>> Model takes a few minutes to run <<<<<

gov.decs <- lm(inc.twpct ~ presgov.twpct + govvote.lag + propertypercapl.6mo + dideclarations + 
                 medincK + propertypercapl.6mo*dideclarations + same_as_pres + as.factor(countyfips) + 
                 as.factor(year), data = govdata)

htmlreg(gov.decs, file = "Table1.doc", omit.coef = ("as.factor"))



## Process for generating Marginal Effect Figures

# This section contains needed backgound processes

V.gov.decs <- vcov(gov.decs) # Takes a minute to run

est.gov.decs <- coef(gov.decs)

z0.damage.gov.decs <- seq(min(govdata["propertypercapl.6mo"]), max(govdata["propertypercapl.6mo"]))
z0.declaration.gov.decs <- seq(min(govdata["dideclarations"]), max(govdata["dideclarations"]))

govvote.dy.damage.dx.gov.decs <- est.gov.decs["propertypercapl.6mo"] + est.gov.decs["propertypercapl.6mo:dideclarations"]*z0.declaration.gov.decs
govvote.dy.declaration.dx.gov.decs <- est.gov.decs["dideclarations"] + est.gov.decs["propertypercapl.6mo:dideclarations"]*z0.damage.gov.decs

inter.se.damage.gov.decs.vitals <- sqrt(V.gov.decs["propertypercapl.6mo", "propertypercapl.6mo"] + z0.declaration.gov.decs^2*V.gov.decs["propertypercapl.6mo:dideclarations", "propertypercapl.6mo:dideclarations"] + 2*z0.declaration.gov.decs*V.gov.decs["propertypercapl.6mo", "propertypercapl.6mo:dideclarations"])
inter.se.declaration.gov.decs.vitals <- sqrt(V.gov.decs["dideclarations", "dideclarations"] + z0.damage.gov.decs^2*V.gov.decs["propertypercapl.6mo:dideclarations", "propertypercapl.6mo:dideclarations"] + 2*z0.damage.gov.decs*V.gov.decs["dideclarations", "propertypercapl.6mo:dideclarations"])

upper.bound.damage.gov.decs <- govvote.dy.damage.dx.gov.decs + 1.96*inter.se.damage.gov.decs.vitals
lower.bound.damage.gov.decs <- govvote.dy.damage.dx.gov.decs - 1.96*inter.se.damage.gov.decs.vitals

upper.bound.declaration.gov.decs <- govvote.dy.declaration.dx.gov.decs + 1.96*inter.se.declaration.gov.decs.vitals
lower.bound.declaration.gov.decs <- govvote.dy.declaration.dx.gov.decs - 1.96*inter.se.declaration.gov.decs.vitals


# Figure 1

figure1 <- data.frame(Declaration = c("No", "Yes"), 
                      Effect = c(govvote.dy.damage.dx.gov.decs), 
                      lower = c(lower.bound.damage.gov.decs),
                      upper = c(upper.bound.damage.gov.decs))

g <- ggplot(figure1, aes(Declaration, Effect))
plot1 <- g + geom_point(size = 4, color = "Black") + 
     geom_errorbar(data = figure1, aes(Declaration, ymin = upper, ymax = lower),
     width = 0, size = 1, color = "Black") + ylim(-0.4, 0.4) +
     geom_hline(yintercept = 0, slope = 1, size = 1) +
     xlab("Disaster Declaration") + ylab("Marginal Effect") +
     ggtitle("Figure 1: Marginal Effect of Weather Damage") +
     theme_classic(base_size = 16) + 
     theme(axis.title.x = element_text(vjust = 0),
           axis.title.y = element_text(vjust = 1.5), plot.title = element_text(vjust = 2))
plot1

pdf(file = "Figure 1.pdf")
plot1
dev.off()

# Figure 2

figure2 <- data.frame(WeatherDamage = z0.damage.gov.decs,
                      DecEffect = govvote.dy.declaration.dx.gov.decs,
                      Declower = lower.bound.declaration.gov.decs,
                      Decupper = upper.bound.declaration.gov.decs)
                      

g2 <- ggplot(figure2, aes(WeatherDamage, DecEffect))
plot2 <- g2 + #geom_hline(yintercept = 0, slope = 1, size = .5, lty = "dotted") +
  geom_line(data = figure2, aes(WeatherDamage), size = 1, color = "Black") + 
  geom_line(data = figure2, aes(WeatherDamage),
                width = 0, size = 1, color = "Black") +
  geom_line(data = figure2, aes(WeatherDamage, Declower), 
            width = 0, size = .6, color = "Black", lty = "longdash") +
  geom_line(data = figure2, aes(WeatherDamage, Decupper), 
            width = 0, size = .6, color = "Black", lty = "longdash") + 
  scale_y_continuous(limits = c(-2, 8), breaks = c(-2, 0, 2, 4, 6, 8))  +
  xlab("Weather Damage in Logged Dollars") + ylab("Marginal Effect") +
  ggtitle("Figure 1: Marginal Effect of Disaster Declaration") +
  theme_classic(base_size = 16) + 
  theme(axis.title.x = element_text(vjust = 0),
        axis.title.y = element_text(vjust = 1.5), plot.title = element_text(vjust = 2))

plot2

pdf(file = "Figure 2.pdf")
plot2
dev.off()

## APPENDIX B

# Plot of Marginal Effect of Disaster Declaration

eplot(xlim = mm(z0.damage.gov.decs), ylim = mm(c(upper.bound.declaration.gov.decs, lower.bound.declaration.gov.decs)), 
      xlab = "Weather Damage in Logged Dollars", 
      ylab = "Marginal Effect of Declaration",
      main = "Figure 2: Marginal Effect of Disaster Declaration")
lines(z0.damage.gov.decs, govvote.dy.declaration.dx.gov.decs, lwd = 3)
lines(z0.damage.gov.decs, lower.bound.declaration.gov.decs, lty = 3)
lines(z0.damage.gov.decs, upper.bound.declaration.gov.decs, lty = 3)

