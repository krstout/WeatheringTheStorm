### CODE FOR WEATHERING THE STORM ###

rm(list = ls())

setwd("C:\\Users\\Kevin\\Dropbox\\Weathering the Storm") 

library(texreg)
library(ggplot2)

govdata <- read.csv("WtS 1970-2016 Data.csv")
presdata <- read.csv("presidentialdata.csv")

sameparty <- govdata[govdata$sameparty == 1,]
diffparty <- govdata[govdata$sameparty == 0,]

d7006 <- govdata[govdata$year < 2007,]
d0716 <- govdata[govdata$year > 2006,]

# Turn declarations into dichotomous variable

presdata$dideclaration <- ifelse(presdata$disdecs.all.6mo > 0, 1, 0)

presdata$turndown <- ifelse(presdata$turndowns.6mo > 0, 1, 0)

## Main Text Tables and Figures

# Table 1

gr.model <- lm(inc.gov.twpct ~ gr.damage + presvote + govvote.lag + median.inc.k + declarations +
                turndown + as.factor(fips) + as.factor(year), data = d7006)

gov.interact <- lm(inc.gov.twpct ~ gr.damage + dideclaration + gr.damage*dideclaration + 
                     govvote.lag + presvote + median.inc.k + as.factor(fips) + 
                     as.factor(year), data = d7006)

htmlreg(l = list(gr.model, gov.interact), file = "Table1.doc", omit.coef = ("as.factor"), digits = 3, ci.force = F, 
        single.row = F, reorder.coef = c(1, 2, 8, 9, 3, 4, 5, 6, 7))

# Figure 1

vcov.govint <- vcov(gov.interact)
coef.govint <- coef(gov.interact)

z0.damage.govint <- seq(min(d7006["gr.damage"]), max(d7006["gr.damage"]))
z0.declaration.govint <- seq(min(d7006["dideclaration"]), max(d7006["dideclaration"]))

govvote.dy.damage.dx.govint <- coef.govint["gr.damage"] + coef.govint["gr.damage:dideclaration"]*z0.declaration.govint
govvote.dy.declaration.dx.govint <- coef.govint["dideclaration"] + coef.govint["gr.damage:dideclaration"]*z0.damage.govint

inter.se.damage.govint <- sqrt(vcov.govint["gr.damage", "gr.damage"] + z0.declaration.govint^2*vcov.govint["gr.damage:dideclaration", "gr.damage:dideclaration"] + 2*z0.declaration.govint*vcov.govint["gr.damage", "gr.damage:dideclaration"])
inter.se.declaration.govint <- sqrt(vcov.govint["dideclaration", "dideclaration"] + z0.damage.govint^2*vcov.govint["gr.damage:dideclaration", "gr.damage:dideclaration"] + 2*z0.damage.govint*vcov.govint["dideclaration", "gr.damage:dideclaration"])

upper.bound.damage.govint <- govvote.dy.damage.dx.govint + 1.96*inter.se.damage.govint
lower.bound.damage.govint <- govvote.dy.damage.dx.govint - 1.96*inter.se.damage.govint

upper.bound.declaration.govint <- govvote.dy.declaration.dx.govint + 1.96*inter.se.declaration.govint
lower.bound.declaration.govint <- govvote.dy.declaration.dx.govint - 1.96*inter.se.declaration.govint

no.dec <- coef.govint["gr.damage"]*z0.damage.govint
no.dec.upper <- no.dec + 1.96*coef(summary(gov.interact))[2, 2]
no.dec.lower <- no.dec - 1.96*coef(summary(gov.interact))[2, 2]

f1 <- data.frame(WeatherDamage = z0.damage.govint,
                 DecEffect = govvote.dy.declaration.dx.govint,
                 Declower = lower.bound.declaration.govint,
                 Decupper = upper.bound.declaration.govint,
                 NoInt = no.dec,
                 NoIntUpper = no.dec.upper,
                 NoIntLower = no.dec.lower)

p1 <- ggplot(f1, aes(WeatherDamage, DecEffect))
figure1 <- p1 + #geom_hline(yintercept = 0, slope = 1, size = .5, lty = "dotted") +
  geom_line(data = f1, aes(WeatherDamage), size = 1, color = "Red", lty = "longdash") + 
  geom_ribbon(data = f1, aes(ymin = Declower, ymax = Decupper), alpha = 0.3) +
  geom_line(data = f1, aes(WeatherDamage, NoInt), size = 1) +
  geom_ribbon(data = f1, aes(ymin = no.dec.lower, ymax = no.dec.upper), alpha = 0.3) +
  geom_hline(yintercept = 0, size = .5, lty = "dotted") +
  scale_y_continuous(limits = c(-4, 8), breaks = c(-4, -2, 0, 2, 4, 6, 8))  +
  xlab("Weather Damage in Logged Dollars") + ylab("Marginal Effect") +
  ggtitle("Figure 1: Effects of Declaration and No Declaration") +
  theme_classic(base_size = 16) + 
  theme(axis.title.x = element_text(vjust = 0),
        axis.title.y = element_text(vjust = 1.5), plot.title = element_text(vjust = 2))

figure1

# Table 2

gov.sameparty <- lm(inc.gov.twpct ~ gr.damage + dideclaration + gr.damage*dideclaration + 
                      govvote.lag + presvote + median.inc.k + as.factor(fips) + 
                      as.factor(year), data = sameparty[sameparty$year < 2007,])

gov.diffparty <- lm(inc.gov.twpct ~ gr.damage + dideclaration + gr.damage*dideclaration + 
                      govvote.lag + presvote + median.inc.k + as.factor(fips) + 
                      as.factor(year), data = diffparty[diffparty$year < 2007,])

htmlreg(l = list(gov.sameparty, gov.diffparty), file = "Table2.doc", omit.coef = ("as.factor"), 
        digits = 3, ci.force = F, single.row = F,
        custom.model.names = c("Same Party", "Different Party"),
        reorder.coef = c(1, 2, 3, 7, 4, 5, 6))

## Appendix B

# Figure B1

b1 <- data.frame(Declaration = c("No", "Yes"), 
                 Effect = c(govvote.dy.damage.dx.govint), 
                 lower = c(lower.bound.damage.govint),
                 upper = c(upper.bound.damage.govint))

pb1 <- ggplot(b1, aes(Declaration, Effect))
figureb1 <- pb1 + geom_point(size = 4, color = "Black") + 
  geom_errorbar(data = b1, aes(Declaration, ymin = upper, ymax = lower),
                width = 0, size = 1, color = "Black") + ylim(-0.4, 0.4) +
  geom_hline(yintercept = 0, size = 1) +
  xlab("Disaster Declaration") + ylab("Marginal Effect") +
  ggtitle("Figure B1: Marginal Effect of Weather Damage") +
  theme_classic(base_size = 16) + 
  theme(axis.title.x = element_text(vjust = 0),
        axis.title.y = element_text(vjust = 1.5), plot.title = element_text(vjust = 2))

figureb1

# Table B1

unsmooth.replicate <- lm(inc.gov.twpct ~ log.damage + declarations + turndown + presvote +
                           govvote.lag + median.inc.k + as.factor(fips) + as.factor(year), data = d7006)

with.turn <- lm(inc.gov.twpct ~ gr.damage + dideclaration + gr.damage*dideclaration + 
                  turndown + govvote.lag + presvote + median.inc.k + as.factor(fips) + 
                  as.factor(year), data = d7006)

gov.decs.govparty <- lm(inc.gov.twpct ~ gr.damage + dideclaration + gr.damage*dideclaration + 
                          dem + govvote.lag + presvote + median.inc.k + as.factor(fips) + 
                          as.factor(year), data = d7006)

no.turn <- d7006[d7006$turndown == 0,]

gov.decs.noturn <- lm(inc.gov.twpct ~ gr.damage + dideclaration + gr.damage*dideclaration + 
                        dem + govvote.lag + presvote + median.inc.k + as.factor(fips) + 
                        as.factor(year), data = no.turn)

htmlreg(l = list(unsmooth.replicate, with.turn, gov.decs.govparty, gov.decs.noturn), file = "TableB1.doc", 
        omit.coef = ("as.factor"), digits = 3, ci.force = F, single.row = F, 
        reorder.coef = c(1, 2, 8, 3, 9, 10, 4, 5, 6, 7, 11))

# Table B2 

triple.interaction <- lm(inc.gov.twpct ~ gr.damage + dideclaration + 
                           gr.damage*dideclaration*sameparty + govvote.lag + presvote + 
                           median.inc.k + as.factor(fips) + as.factor(year), data = d7006)

htmlreg(triple.interaction, file = "TableB2.doc", omit.coef = ("as.factor"), digits = 3, ci.force = F, 
        single.row = T)

# Table B3 

pres.decs <- lm(inc.twpct ~ propertypercapl.6mo + dideclaration + propertypercapl.6mo*dideclaration +
                  turndown + turndown*propertypercapl.6mo + pres.vote.lag1 + pres.vote.lag2 + 
                  medincK + as.factor(countyfips) + as.factor(year), data = presdata)

htmlreg(pres.decs, file = "TableB3.doc", omit.coef = ("as.factor"), digits = 3, ci.force = F, single.row = T)

# Figure B2

v.pres <- vcov(pres.decs)
coefs.pres.decs <- coef(pres.decs)

z0.damage.pres.decs <- seq(min(presdata["propertypercapl.6mo"]), max(presdata["propertypercapl.6mo"]))
z0.turndown.pres.decs <- seq(min(presdata["turndown"]), max(presdata["turndown"]))
z0.declaration.pres.decs <- seq(min(presdata["dideclaration"]), max(presdata["dideclaration"]))

presvote.dy.declaration.dx.pres.decs <- coefs.pres.decs["dideclaration"] + coefs.pres.decs["propertypercapl.6mo:dideclaration"]*z0.damage.pres.decs
presvote.dy.turndown.dx.pres.decs <- coefs.pres.decs["turndown"] + coefs.pres.decs["propertypercapl.6mo:turndown"]*z0.damage.pres.decs

inter.se.declaration.pres.decs.vitals <- sqrt(v.pres["dideclaration", "dideclaration"] + z0.damage.pres.decs^2*v.pres["propertypercapl.6mo:dideclaration", "propertypercapl.6mo:dideclaration"] + 2*z0.damage.pres.decs*v.pres["dideclaration", "propertypercapl.6mo:dideclaration"])
inter.se.turndown.pres.decs.vitals <- sqrt(v.pres["turndown", "turndown"] + z0.damage.pres.decs^2*v.pres["propertypercapl.6mo:turndown", "propertypercapl.6mo:turndown"] + 2*z0.damage.pres.decs*v.pres["turndown", "propertypercapl.6mo:turndown"])

upper.bound.declaration.pres.decs <- presvote.dy.declaration.dx.pres.decs + 1.96*inter.se.declaration.pres.decs.vitals
lower.bound.declaration.pres.decs <- presvote.dy.declaration.dx.pres.decs - 1.96*inter.se.declaration.pres.decs.vitals

upper.bound.turndown.pres.decs <- presvote.dy.turndown.dx.pres.decs + 1.96*inter.se.turndown.pres.decs.vitals
lower.bound.turndown.pres.decs <- presvote.dy.turndown.dx.pres.decs - 1.96*inter.se.turndown.pres.decs.vitals


b2 <- data.frame(WeatherDamage.p = z0.damage.pres.decs,
                 DecEffect.p = presvote.dy.declaration.dx.pres.decs,
                 Declower.p = lower.bound.declaration.pres.decs,
                 Decupper.p = upper.bound.declaration.pres.decs,
                 TurnEffect.p = presvote.dy.turndown.dx.pres.decs,
                 Turnlower.p = lower.bound.turndown.pres.decs,
                 Turnupper.p = upper.bound.turndown.pres.decs)


pb2 <- ggplot(b2, aes(WeatherDamage.p, DecEffect.p))
figureb2 <- pb2 + #geom_hline(yintercept = 0, slope = 1, size = .5, lty = "dotted") +
  geom_line(data = b2, aes(WeatherDamage.p), size = 1, color = "Red", lty = "longdash") + 
  geom_ribbon(data = b2, aes(ymin = Declower.p, ymax = Decupper.p), alpha = 0.3) +
  geom_line(data = b2, aes(WeatherDamage.p, TurnEffect.p), size = 1) +
  geom_ribbon(data = b2, aes(ymin = Turnlower.p, ymax = Turnupper.p), alpha = 0.3) +
  geom_hline(yintercept = 0, size = .5, lty = "dotted") +
  scale_y_continuous(limits = c(-4, 8), breaks = c(-4, -2, 0, 2, 4, 6, 8))  +
  xlab("Weather Damage in Logged Dollars") + ylab("Marginal Effect") +
  ggtitle("Figure B2: Marginal Effect of Disaster Declaration \n and Turndown on Presidential Vote") +
  theme_classic(base_size = 16) + 
  theme(axis.title.x = element_text(vjust = 0),
        axis.title.y = element_text(vjust = 1.5), plot.title = element_text(vjust = 2))

figureb2

# Table B4

unsmooth.7006 <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration +
                      govvote.lag + presvote + median.inc.k + as.factor(fips) +
                      as.factor(year), data = d7006)

unsmooth.7006party <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration +
                           govvote.lag + presvote + median.inc.k + dem + as.factor(fips) +
                           as.factor(year), data = d7006)

unsmooth.0716 <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration + 
                  govvote.lag + presvote + median.inc.k + as.factor(fips) + 
                  as.factor(year), data = d0716)

unsmooth.0716party <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration + 
                         govvote.lag + presvote + median.inc.k + dem + as.factor(fips) + 
                         as.factor(year), data = d0716)

unsmooth.all <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration +
                     govvote.lag + presvote + median.inc.k + as.factor(fips) +
                     as.factor(year), data = govdata)

unsmooth.allparty <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration +
                        govvote.lag + presvote + median.inc.k + dem + as.factor(fips) +
                        as.factor(year), data = govdata)

htmlreg(l = list(unsmooth.7006, unsmooth.7006party, unsmooth.0716, unsmooth.0716party, 
                 unsmooth.all, unsmooth.allparty), "TableB4.doc", omit.coef = "as.factor", 
                 digits = 3, ci.force = F, single.row = F, reorder.coef = c(1, 2, 3, 7, 4, 5, 6, 8),
        custom.model.names = c("1970-2006 Unsmoothed Damage", "1970-2006 Unsmoothed with Party", 
                               "2007-2016 Unsmoothed Damage", "2007-2016 Unsmoothed with Party",
                               "1970-2016 Unsmoothed Damage", "1970-2016 Unsmoothed with Party"))

# Figure B3

varcov0716 <- vcov(unsmooth.0716)
coefs0716 <- coef(unsmooth.0716)

z0.damage <- seq(min(d0716["log.damage"]), max(d0716["log.damage"]))
z0.declaration <- seq(min(d0716["dideclaration"]), max(d0716["dideclaration"]))

govvote.dy.damage.dx.0716 <- coefs0716["log.damage"] + coefs0716["log.damage:dideclaration"]*z0.declaration
govvote.dy.declaration.dx.0716 <- coefs0716["dideclaration"] + coefs0716["log.damage:dideclaration"]*z0.damage

damage.se.0716 <- sqrt(varcov0716["log.damage", "log.damage"] + z0.declaration^2*varcov0716["log.damage:dideclaration", "log.damage:dideclaration"] + 2*z0.declaration*varcov0716["log.damage", "log.damage:dideclaration"])
declaration.se.0716 <- sqrt(varcov0716["dideclaration", "dideclaration"] + z0.damage^2*varcov0716["log.damage:dideclaration", "log.damage:dideclaration"] + 2*z0.damage*varcov0716["dideclaration", "log.damage:dideclaration"])

upper.bound.damage.0716 <- govvote.dy.damage.dx.0716 + 1.96*damage.se.0716
lower.bound.damage.0716 <- govvote.dy.damage.dx.0716 - 1.96*damage.se.0716

upper.bound.declaration.0716 <- govvote.dy.declaration.dx.0716 + 1.96*declaration.se.0716
lower.bound.declaration.0716 <- govvote.dy.declaration.dx.0716 - 1.96*declaration.se.0716


no.dec <- coefs0716["log.damage"]*z0.damage
no.dec.upper <- no.dec + 1.96*coefs0716["log.damage"]
no.dec.lower <- no.dec - 1.96*coefs0716["log.damage"]

b3 <- data.frame(WeatherDamage.d = z0.damage,
                 DecEffect.d = govvote.dy.declaration.dx.0716,
                 Declower.d = lower.bound.declaration.0716,
                 Decupper.d = upper.bound.declaration.0716,
                 NoInt.d = no.dec,
                 NoIntUpper.d = no.dec.upper,
                 NoIntLower.d = no.dec.lower)


pb3 <- ggplot(b3, aes(WeatherDamage.d, DecEffect.d))
figureb3 <- pb3 + 
  geom_hline(yintercept = 0, size = .5, lty = "dotted") +
  geom_line(data = b3, aes(WeatherDamage.d), size = 1, color = "Red", lty = "longdash") + 
  geom_ribbon(data = b3, aes(ymin = Declower.d, ymax = Decupper.d), alpha = 0.3) +
  geom_line(data = b3, aes(WeatherDamage.d, NoInt.d), size = 1) +
  geom_ribbon(data = b3, aes(ymin = NoIntUpper.d, ymax = NoIntLower.d), alpha = 0.3) +
  xlab("Weather Damage in Logged Dollars") + ylab("Marginal Effect") +
  ggtitle("Figure B3: Effects of Declaration and \n No Declaration for 2007 to 2016 Data") +
  theme_classic(base_size = 16) + 
  theme(axis.title.x = element_text(vjust = 0),
        axis.title.y = element_text(vjust = 1.5), plot.title = element_text(vjust = 2))

figureb3

# Figure B4

varcov.all <- vcov(unsmooth.all)
coefs.all <- coef(unsmooth.all)

z0.damage <- seq(min(govdata["log.damage"]), max(govdata["log.damage"]))
z0.declaration <- seq(min(govdata["dideclaration"]), max(govdata["dideclaration"]))

govvote.dy.damage.dx.all <- coefs.all["log.damage"] + coefs.all["log.damage:dideclaration"]*z0.declaration
govvote.dy.declaration.dx.all <- coefs.all["dideclaration"] + coefs.all["log.damage:dideclaration"]*z0.damage

damage.se.all <- sqrt(varcov.all["log.damage", "log.damage"] + z0.declaration^2*varcov.all["log.damage:dideclaration", "log.damage:dideclaration"] + 2*z0.declaration*varcov.all["log.damage", "log.damage:dideclaration"])
declaration.se.all <- sqrt(varcov.all["dideclaration", "dideclaration"] + z0.damage^2*varcov.all["log.damage:dideclaration", "log.damage:dideclaration"] + 2*z0.damage*varcov.all["dideclaration", "log.damage:dideclaration"])

upper.bound.damage.all <- govvote.dy.damage.dx.all + 1.96*damage.se.all
lower.bound.damage.all <- govvote.dy.damage.dx.all - 1.96*damage.se.all

upper.bound.declaration.all <- govvote.dy.declaration.dx.all + 1.96*declaration.se.all
lower.bound.declaration.all <- govvote.dy.declaration.dx.all - 1.96*declaration.se.all

no.dec <- coefs.all["log.damage"]*z0.damage
no.dec.upper <- no.dec + 1.96*coefs.all["log.damage"]
no.dec.lower <- no.dec - 1.96*coefs.all["log.damage"]

b4 <- data.frame(WeatherDamage.d = z0.damage,
                 DecEffect.d = govvote.dy.declaration.dx.all,
                 Declower.d = lower.bound.declaration.all,
                 Decupper.d = upper.bound.declaration.all,
                 NoInt.d = no.dec,
                 NoIntUpper.d = no.dec.upper,
                 NoIntLower.d = no.dec.lower)


pb4 <- ggplot(b4, aes(WeatherDamage.d, DecEffect.d))
figureb4 <- pb4 + 
  geom_hline(yintercept = 0, size = .5, lty = "dotted") +
  geom_line(data = b4, aes(WeatherDamage.d), size = 1, color = "Red", lty = "longdash") + 
  geom_ribbon(data = b4, aes(ymin = Declower.d, ymax = Decupper.d), alpha = 0.3) +
  geom_line(data = b4, aes(WeatherDamage.d, NoInt.d), size = 1) +
  geom_ribbon(data = b4, aes(ymin = NoIntUpper.d, ymax = NoIntLower.d), alpha = 0.3) +
  xlab("Weather Damage in Logged Dollars") + ylab("Marginal Effect") +
  ggtitle("Figure B4: Effects of Declaration and \n No Declaration for 1970 to 2016 Data") +
  theme_classic(base_size = 16) + 
  theme(axis.title.x = element_text(vjust = 0),
        axis.title.y = element_text(vjust = 1.5), plot.title = element_text(vjust = 2))

figureb4

# Table B5

sameparty <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration +
                  govvote.lag + presvote + median.inc.k + as.factor(fips) + as.factor(year),
                data = d0716[d0716$sameparty == 1,])

diffparty <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration +
                  govvote.lag + presvote + median.inc.k + as.factor(fips) + as.factor(year),
                data = d0716[d0716$sameparty == 0,])

same.full <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration +
                  govvote.lag + presvote + median.inc.k + as.factor(fips) + as.factor(year),
                data = govdata[govdata$sameparty == 1,])

diff.full <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration +
                  govvote.lag + presvote + median.inc.k + as.factor(fips) + as.factor(year),
                data = govdata[govdata$sameparty == 0,])

htmlreg(l = list(sameparty, diffparty, same.full, diff.full), file = "TableB5.doc", omit.coef = "as.factor", 
        digits = 3, ci.force = F, single.row = F,
        custom.model.names = c("Same Party 2007-2016", "Same Party 1970-2016", 
                               "Different Party 2007-2016", "Different Party 1970-2016"))

# Table B6

threeway.inter <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration +
                       govvote.lag + presvote + median.inc.k + sameparty + sameparty*dideclaration + 
                       sameparty*log.damage + log.damage*dideclaration*sameparty +
                       as.factor(fips) + as.factor(year),
                     data = govdata)

htmlreg(l = list(threeway.inter), 
        file = "TableB6.doc", omit.coef = "as.factor", digits = 3, ci.force = F, 
        single.row = T)

# Table B7

govdata$diturndown <- ifelse(govdata$turndown > 0, 1, 0)

with.party.turn <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration + dem +
                        diturndown + govvote.lag + presvote + median.inc.k + as.factor(fips) + 
                        as.factor(year), data = govdata)

no.turn.all <- lm(inc.gov.twpct ~ log.damage + dideclaration + log.damage*dideclaration + dem +
                govvote.lag + presvote + median.inc.k + as.factor(fips) + 
                as.factor(year), data = govdata[govdata$turndown == 0,])

htmlreg(l = list(with.party.turn, no.turn.all), file = "TableB7.doc", omit.coef = "as.factor", digits = 3,
        ci.force = F, single.row = F)
