# DATA AND ANALYSES FROM:
# Mori, E., Ancillotto, L., Lovari, S., Russo, D., Nerva, L., Mohamed, W.F., Motro, Y., Di Bari, P. and Plebani, M. (2019), Skull shape and Bergmann's rule in mammals: hints from Old World porcupines. J Zool, 308: 47-55. https://doi.org/10.1111/jzo.12651

# Analyses by Marco Plebani - marcoplebani85@gmail.com
# Code last checked on 27 May 2021

###########################################################
########### HYSTRIX SKULL SIZE VS LATITUDE ################
###########################################################

# Only on H. cristata and H. indica. Hystrix africaeaustralis's sample is too small.
# Specimens from islands dropped

rm(list=ls())

# Load packages, Import and tidy up data
source('Hystrix_skull_analyses_intro.R')

# contains the lines:
# aa1 <- subset(aa, aa$Verydubious=="no")
# aa1 <- subset(aa1, aa1$locality_country=="locality")

length(aa1[,1])
length(aa2[,1])
aaX <- subset(aa, aa$Verydubious=="yes")
################################
################################
# Are skulls found near the equator smaller than those found far from it?
################################
################################

# Use aa2 (known localities)
names(aa2)
aa2 <- subset(aa2, aa2$island=="Continents")
str(aa2)
# sample sizes
sum(!is.na(aa2[aa2$species=="africaeaustralis",]$condylobasal_length))
sum(!is.na(aa2[aa2$species=="cristata",]$condylobasal_length)) # N=99
sum(!is.na(aa2[aa2$species=="indica",]$condylobasal_length)) # N=41
sum(!is.na(aa2[aa2$species=="brachyura",]$condylobasal_length))
sum(!is.na(aa2[aa2$species=="javanica",]$condylobasal_length))

# lmafricae.cb <- lm(condylobasal_length ~ absolute_latitude, 
	# data=aa2[aa2$species=="africaeaustralis",])
lmcristata.cb <- lm(condylobasal_length ~ absolute_latitude, 
	data=aa2[aa2$species=="cristata",])
lmindica.cb <- lm(condylobasal_length ~ absolute_latitude, 
	data=aa2[aa2$species=="indica",])
# par(mfrow=c(2,2))
# plot(lmafricae.cb)
# plot(lmcristata.cb)
# plot(lmindica.cb)
# Mmmmh, so-so, but let's not get picky.
# summary(lmafricae.cb) # NS
summary(lmcristata.cb) # negative correlation. 
summary(lmindica.cb) # NS
# Larger sample sizes, all trends are confirmed.

# POWER ANALYSIS
pacman::p_load(pwr)
p.out.crist <- pwr.r.test(r = sqrt(summary(lmcristata.cb)$adj.r.squared), n=99,
					sig.level = 0.05, power = NULL, alternative = "greater")
p.out.indic <- pwr.r.test(r = 0, n=41,
					sig.level = 0.05, power = NULL, alternative = "greater")




# Make some new x data at which predictions are made
# newXa<-data.frame(
	# absolute_latitude =seq(
		# min(aa2[aa2$species=="africaeaustralis",]$absolute_latitude, na.rm=T),
		# 31 # max(aa2[aa2$species=="africaeaustralis",]$absolute_latitude, na.rm=T)
		# ))

newXc<-data.frame(
	absolute_latitude =seq(
		min(aa2[aa2$species=="cristata",]$absolute_latitude, na.rm=T),
		max(aa2[aa2$species=="cristata",]$absolute_latitude, na.rm=T)
		))
newXi<-data.frame(
	absolute_latitude =seq(
		min(aa2[aa2$species=="indica",]$absolute_latitude, na.rm=T),
		max(aa2[aa2$species=="indica",]$absolute_latitude, na.rm=T)
		))
		
# generate predictions using predict
# newYa <- predict(lmafricae.cb,newdata=newXa,type="response",se.fit=TRUE)
newYc <- predict(lmcristata.cb,newdata=newXc,type="response",se.fit=TRUE)
newYi <- predict(lmindica.cb,newdata=newXi,type="response",se.fit=TRUE)		

# newXa$condylobasal_length <- newYa$fit
# newXa$se <- newYa$se.fit
newXc$condylobasal_length <- newYc$fit
newXc$se <- newYc$se.fit
newXi$condylobasal_length <- newYi$fit
newXi$se <- newYi$se.fit

# Shortcuts for drawing confidence intervals:
# yya<- newXa$absolute_latitude
yyc<- newXc$absolute_latitude
yyi<- newXi$absolute_latitude
# aasep<- newXa$condylobasal_length + 1.96 * newXa$se
# aasem<- newXa$condylobasal_length - 1.96 * newXa$se
ccsep<- newXc$condylobasal_length + 1.96 * newXc$se
ccsem<- newXc$condylobasal_length - 1.96 * newXc$se
iisep<- newXi$condylobasal_length + 1.96 * newXi$se
iisem<- newXi$condylobasal_length - 1.96 * newXi$se

# quartz()
# Let's colour different species differently:
pdf(file="~/Desktop/histrix_skullsize_vs_latitude.pdf", family="Times", width=5, height=5)
par(mfcol=c(1,1), mar=c(5,5,1,1),cex.lab=1.1)
plot(c(0,3), # just an empty plot, with the right layout, to be filled later
	ylim=c(11,17), xlim=c(0,47),
	xlab="Absolute latitude [degrees]",
	ylab="Condylobasal length [cm]",
	main="")

# yaxt="n",
# xlab=expression(paste(Delta, "T (°C)")),
# tck = 0.02,ylim=c(0,1)
# )
# axis(2, at=c(0,0.25,0.5,0.75,1),labels=c(0,0.25,0.5,0.75,1),las=2,tck = 0.02)

# Confidence interval as a shaded area:
#polygon(c(yya,rev(yya)),c(aasep,rev(aasem)),col=grey(0.6, 0.5), border=NA)
polygon(c(yyc,rev(yyc)),c(ccsep,rev(ccsem)),col=grey(0.6, 0.5), border=NA)
#polygon(c(yyi,rev(yyi)),c(iisep,rev(iisem)),col=grey(0.6, 0.5), border=NA)

# regression lines:
#lines(condylobasal_length ~ absolute_latitude,data= newXa,col="salmon",lwd=1.5)
lines(condylobasal_length ~ absolute_latitude,data= newXc,col="springgreen3",lwd=1.5)
#lines(condylobasal_length ~ absolute_latitude,data= newXi,col="blue",lwd=1.5)

# points:		
# points(condylobasal_length ~ absolute_latitude, 
	# data=aa2[aa2$species=="africaeaustralis",],
	# pch=3, col="red" # col="salmon"
	# )
# points(condylobasal_length ~ absolute_latitude, 
	# data=aa2[aa2$species=="brachyura",],
	# pch=5, col= "black"#"yellow4"
	# )
points(condylobasal_length ~ absolute_latitude, 
	data=aa2[aa2$species=="cristata",],
	pch=1, col="springgreen3"
	)
points(condylobasal_length ~ absolute_latitude, 
	data=aa2[aa2$species=="indica",],
	pch=2, col="blue" #"deepskyblue"
	)
# points(condylobasal_length ~ absolute_latitude, 
	# data=aa2[aa2$species=="javanica",],
	# pch=4, col="darkviolet" #"deepskyblue"
	# )		

legend("topright", bty="n", cex=0.9,
	legend=c(#expression(italic("Hystrix africaeaustralis")),
			#expression(italic("H. brachyura")),
			expression(italic("H. cristata")),
			expression(italic("H. indica"))#,
			#expression(italic("H. javanica"))
			),
		pch=c(1,2),
		col=c("springgreen3", "blue")
	# pch=c(3,5,1,2,4),
	# col=c("red", "black", "springgreen3", "blue","darkviolet")
	#col=c("salmon", "yellow4", "springgreen3", "blue","darkviolet")
)

dev.off()
