# DATA AND ANALYSES FROM:
# Mori, E., Ancillotto, L., Lovari, S., Russo, D., Nerva, L., Mohamed, W.F., Motro, Y., Di Bari, P. and Plebani, M. (2019), Skull shape and Bergmann's rule in mammals: hints from Old World porcupines. J Zool, 308: 47-55. https://doi.org/10.1111/jzo.12651

# Analyses by Marco Plebani - marcoplebani85@gmail.com
# Code last checked on 27 May 2021

##########################
# Skull size vs Body size
##########################

rm(list=ls())

# Load packages, Import and tidy up data
source('Hystrix_skull_analyses_intro.R')

# contains the lines:
# aa1 <- subset(aa, aa$Verydubious=="no")
# aa1 <- subset(aa1, aa1$locality_country=="locality")
                            
names(ll)
library(data.table)
ll <- data.table(ll)
ll[, .(count=.N), by = species]
ll <- as.data.frame(ll)
lm1 <- lm(condylobasal.length ~ Length.of.the.spine, data=ll)
summary(lm1)$adj.r.squared
AIC(lm1)

# PLOT DIAGNOSTICS
par(mfrow=c(3,2)); plot(lm1); hist(lm1$resid)

###########################

# Test correlation taking species id into account in a MEM
# Given the small sample size, fitting a logistic model (which has three parameters) would be a long shot. Let's start with fitting a straight line.
library(lme4)
library(lmerTest) # a pimped-up version of lme4 which also provides pseudo-p-values.
mem.skull.vs.body <- glmer(condylobasal.length ~ Length.of.the.spine + (1 | species), 
							data=ll,
							family="gaussian")
summary(mem.skull.vs.body)
library(MuMIn) # gives pseudo-R-squared via r.squaredGLMM()
r.squaredGLMM(mem.skull.vs.body)
AIC(mem.skull.vs.body)

# PLOT DIAGNOSTICS

## Do they show any trend (evidence of nonlinearity)?
## plot residuals against fitted values
quartz(); par(mfrow=c(2,2))

resids <- resid(mem.skull.vs.body, type='pearson')
plot(resids~fitted(mem.skull.vs.body))
lines(lowess(resids~fitted(mem.skull.vs.body)), col='red')

## Are the residuals normally distributed?
qqnorm(resids)
qqline(resids, col='red')

## Are the residuals homoscedastic
## plot the sqrt of the absolute residuals against fitted values
plot(sqrt(abs(resids))~fitted(mem.skull.vs.body))
lines(lowess(sqrt(abs(resids))~fitted(mem.skull.vs.body)), col='red')

hist(resids)
###########################

# PLOT

quartz(); par(mfrow=c(1,1)); plot(ll[,c(3,2)])#, xlim=c(50,75), ylim=c(9,25))

# fake.spine.vals <- data.frame(Length.of.the.spine = seq(0, 100, 0.1))
fake.spine.vals <- seq(50, 72, 0.1)
# predCL <- predict(mem.skull.vs.body, data.frame(x= fake.spine.vals), type="response", se=T)
predCL <- fixef(mem.skull.vs.body)[1] + (fixef(mem.skull.vs.body)[2]*fake.spine.vals)
lines(fake.spine.vals, predCL, lty=1)


pdf(file="~/Desktop/skull_vs_body.pdf", family="Times", width=5, height=5)
par(mfcol=c(1,1), mar=c(5,5,1,1),cex.lab=1.1)
plot(NA, xlim=c(50,75), ylim=c(9,17),
		xlab="Spine length [cm]", ylab="Condylobasal length [cm]", 
		pch=NA
		)

# points:		
points(condylobasal.length ~ Length.of.the.spine, 
	data=ll[ll$species=="africaeaustralis",],
	pch=3, col="red" # col="salmon"
	)
points(condylobasal.length ~ Length.of.the.spine, 
	data=ll[ll$species=="brachyura",],
	pch=5, col= "black"#"yellow4"
	)
points(condylobasal.length ~ Length.of.the.spine, 
	data=ll[ll$species=="cristata",],
	pch=1, col="springgreen3"
	)
points(condylobasal.length ~ Length.of.the.spine, 
	data=ll[ll$species=="indica",],
	pch=2, col="blue" #"deepskyblue"
	)
points(condylobasal.length ~ Length.of.the.spine, 
	data=ll[ll$species=="javanica",],
	pch=4, col="darkviolet" #"deepskyblue"
	)		

legend("bottomright", bty="n", cex=0.9,
	legend=c(expression(italic("Hystrix africaeaustralis")),
			expression(italic("H. brachyura")),
			expression(italic("H. cristata")),
			expression(italic("H. indica")),
			expression(italic("H. javanica"))
			),
	pch=c(3,5,1,2,4),
	col=c("red", "black", "springgreen3", "blue","darkviolet")
	#col=c("salmon", "yellow4", "springgreen3", "blue","darkviolet")
)

#text(52, 15.2, expression(paste("slope=0.22304, p<0.0001, ",R^2,"=0.68")), adj=0)
#abline(lm(condylobasal.length~Length.of.the.spine, data=ll), lty=1)
lines(fake.spine.vals, predCL, lty=1)
dev.off()


####################
# POWER ANALYSIS
####################

# with package pwr
library(pwr)
cohen.ES(test = "r", size = "small") # "medium", "large"
p.out <- pwr.r.test(r = sqrt(summary(lm1)$adj.r.squared), sig.level = 0.05, power = 0.8, alternative = "greater")
p.out
quartz(); plot(p.out)

# with package simr

# # summary(mem.skull.vs.body)
# fixef(mem.skull.vs.body)["Length.of.the.spine"] <- 0.1 # run summary(mem.skull.vs.body) again and see the difference
# ps1 <- simr::powerSim(mem.skull.vs.body, nsim=10)
# pc1 <- simr::powerCurve(mem.skull.vs.body, nsim= 10)
# model2 <- simr::extend(mem.skull.vs.body, along="Length.of.the.spine", n= 10)
# ps2 <- simr::powerSim(model2, nsim= 10) # same as before
# pc2 <- simr::powerCurve(model2, nsim= 10)

# fixef(mem.skull.vs.body)["Length.of.the.spine"] <- 0.2 # run summary(mem.skull.vs.body) again and see the difference
# ps3 <- simr::powerSim(mem.skull.vs.body, nsim=10)
# pc3 <- simr::powerCurve(mem.skull.vs.body, nsim= 10)
# model4 <- simr::extend(mem.skull.vs.body, along="Length.of.the.spine", n= 10)
# ps4 <- simr::powerSim(model2, nsim= 10) # same as before
# pc4 <- simr::powerCurve(model2, nsim= 10)
# quartz(); par(mfrow=c(2,2)); plot(pc1);plot(pc2);plot(pc3);plot(pc4)
