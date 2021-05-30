# DATA AND ANALYSES FROM:
# Mori, E., Ancillotto, L., Lovari, S., Russo, D., Nerva, L., Mohamed, W.F., Motro, Y., Di Bari, P. and Plebani, M. (2019), Skull shape and Bergmann's rule in mammals: hints from Old World porcupines. J Zool, 308: 47-55. https://doi.org/10.1111/jzo.12651

# Analyses by Marco Plebani - marcoplebani85@gmail.com
# Code last checked on 27 May 2021

################################
# Correlation between skull volume and condylobasal length
# (to choose which to use as a proxy of skull size)
################################

rm(list=ls())

# Load packages, Import and tidy up data
source('Hystrix_skull_analyses_intro.R')

################################
# Are skull volume and condylobasal length correlated?
# YES.
################################

summary(lm(skullvolume ~ condylobasal_length, data=aa1))

# Let's colour different species differently:
pdf(file="~/Desktop/histrix_skullvolume_vs_condylobasal_length.pdf", family="Times", width=7, height=7)
par(mfcol=c(1,1), mar=c(5,5,1,1),cex.lab=1.1)
plot(c(0,3), # just an empty plot, with the right layout, to be filled later
	ylim=c(80,530),#ylim=c(4.3,8.2), 
	xlim=c(10,16.5),
	xlab="Condylobasal length [cm]",
	ylab=expression(paste("Cranial volume  [", cm^{3},"]")),
	main="")

# yaxt="n",
# xlab=expression(paste(Delta, "T (°C)")),
# tck = 0.02,ylim=c(0,1)
# )
# axis(2, at=c(0,0.25,0.5,0.75,1),labels=c(0,0.25,0.5,0.75,1),las=2,tck = 0.02)

lmafricae <- lm(skullvolume ~ condylobasal_length, 
	data= aa1[aa1$species=="africaeaustralis",])
lmcristata <- lm(skullvolume ~ condylobasal_length, 
	data=aa1[aa1$species=="cristata",])
lmindica <- lm(skullvolume ~ condylobasal_length, 
	data=aa1[aa1$species=="indica",])
lmbrachyura <- lm(skullvolume ~ condylobasal_length, 
	data=aa1[aa1$species=="brachyura",])
# check that conditions for a linear model are met:
# par(mfrow=c(2,2))
# plot(lmafricae)
# plot(lmcristata)
# plot(lmindica)
# Mmmmh, so-so, but let's not get picky.
summary(lmafricae) 
summary(lmcristata) 
summary(lmindica)
summary(lmbrachyura)

# Make some new x data at which predictions are made
newXa<-data.frame(
	condylobasal_length =seq(
		14, #min(aa1[aa1$species=="africaeaustralis",]$condylobasal_length, na.rm=T),
		16 #max(aa1[aa1$species=="africaeaustralis",]$condylobasal_length, na.rm=T)
		))
newXc<-data.frame(
	condylobasal_length =seq(
		min(aa1[aa1$species=="cristata",]$condylobasal_length, na.rm=T),
		max(aa1[aa1$species=="cristata",]$condylobasal_length, na.rm=T)
		))
newXi<-data.frame(
	condylobasal_length =seq(
		min(aa1[aa1$species=="indica",]$condylobasal_length, na.rm=T),
		16#max(aa1[aa1$species=="indica",]$condylobasal_length, na.rm=T)
		))
newXb<-data.frame(
	condylobasal_length =seq(
		11, # min(aa1[aa1$species=="brachyura",]$condylobasal_length, na.rm=T),
		14 # max(aa1[aa1$species=="brachyura",]$condylobasal_length, na.rm=T)
		))		
# generate predictions using predict
newYa <- predict(lmafricae,newdata=newXa,type="response",se.fit=TRUE)
newYc <- predict(lmcristata,newdata=newXc,type="response",se.fit=TRUE)
newYi <- predict(lmindica,newdata=newXi,type="response",se.fit=TRUE)		
newYb <- predict(lmbrachyura,newdata=newXb,type="response",se.fit=TRUE)		

newXa$skullvolume <- newYa$fit
newXa$se <- newYa$se.fit
newXc$skullvolume <- newYc$fit
newXc$se <- newYc$se.fit
newXi$skullvolume <- newYi$fit
newXi$se <- newYi$se.fit
newXb$skullvolume <- newYb$fit
newXb$se <- newYb$se.fit

# Shortcuts for drawing confidence intervals:
yya<- newXa$condylobasal_length
yyc<- newXc$condylobasal_length
yyi<- newXi$condylobasal_length
yyb<- newXb$condylobasal_length
aasep<- newXa$skullvolume + 1.96 * newXa$se
aasem<- newXa$skullvolume - 1.96 * newXa$se
ccsep<- newXc$skullvolume + 1.96 * newXc$se
ccsem<- newXc$skullvolume - 1.96 * newXc$se
iisep<- newXi$skullvolume + 1.96 * newXi$se
iisem<- newXi$skullvolume - 1.96 * newXi$se
bbsep<- newXb$skullvolume + 1.96 * newXb$se
bbsem<- newXb$skullvolume - 1.96 * newXb$se

# Confidence interval as a shaded area:
polygon(c(yya,rev(yya)),c(aasep,rev(aasem)),col=grey(0.6, 0.5), border=NA)
polygon(c(yyc,rev(yyc)),c(ccsep,rev(ccsem)),col=grey(0.6, 0.5), border=NA)
polygon(c(yyi,rev(yyi)),c(iisep,rev(iisem)),col=grey(0.6, 0.5), border=NA)
polygon(c(yyb,rev(yyb)),c(bbsep,rev(bbsem)),col=grey(0.6, 0.5), border=NA)

# regression lines:
lines(skullvolume ~ condylobasal_length,data= newXa,col="red",lwd=1.5)
lines(skullvolume ~ condylobasal_length,data= newXc,col="springgreen3",lwd=1.5)
lines(skullvolume ~ condylobasal_length,data= newXi,col="blue",lwd=1.5)
lines(skullvolume ~ condylobasal_length,data= newXb,col="black",lwd=1.5)

# points:		
points(skullvolume ~ condylobasal_length, 
	data=aa1[aa1$species=="africaeaustralis",],
	pch=3, col="red"
	)
points(skullvolume ~ condylobasal_length, 
	data=aa1[aa1$species=="brachyura",],
	pch=5, col="black"
	)
points(skullvolume ~ condylobasal_length, 
	data=aa1[aa1$species=="cristata",],
	pch=1, col="springgreen3"
	)
points(skullvolume ~ condylobasal_length, 
	data=aa1[aa1$species=="indica",],
	pch=2, col="blue" #"deepskyblue"
	)
points(skullvolume ~ condylobasal_length, 
	data=aa1[aa1$species=="javanica",],
	pch=4, col="darkviolet" #"deepskyblue"
	)		

legend("bottomright", bty="n", cex=1.1,
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

dev.off()

# sample sizes
sum(!is.na(aa1[aa1$species=="africaeaustralis",]$skullvolume))
sum(!is.na(aa1[aa1$species=="cristata",]$skullvolume))
sum(!is.na(aa1[aa1$species=="indica",]$skullvolume))
sum(!is.na(aa1[aa1$species=="brachyura",]$skullvolume))
sum(!is.na(aa1[aa1$species=="javanica",]$skullvolume))

sum(!is.na(aa1$skullvolume))
sum(!is.na(aa1$condylobasal_length))