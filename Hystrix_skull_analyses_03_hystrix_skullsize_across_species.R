########################################################
########################################################
########### HYSTRIX SKULL MORPHOMETRICS ################
########################################################
########################################################
# Code by Marco Plebani

##########################
# Import and tidy up data
##########################

rm(list=ls())

# Load packages, Import and tidy up data
source('Hystrix_skull_analyses_intro.R')

# contains the lines:
# aa1 <- subset(aa, aa$Verydubious=="no")
# aa1 <- subset(aa1, aa1$locality_country=="locality")

################################
################################
# Let's look at differences in skull volume across all species at once 
################################
################################

sum(!is.na(aa1[aa1$cranial_height<60 & aa1$clusters =="cristata_mediter_clade_Italy",]$condylobasal_length)) # 74, not 75
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$clusters =="cristata_mediter_clade_Africa",]$condylobasal_length)) # 10, not 07
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$clusters =="cristata_subsaharian_clade",]$condylobasal_length)) # 38		
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$clusters =="indica",]$condylobasal_length)) # 61
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$clusters =="africaeaustralis",]$condylobasal_length)) # 14								
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$clusters =="javanica",]$condylobasal_length)) # 06								
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$clusters =="brachyura",]$condylobasal_length)) # 11

pdf(file="~/Desktop/skullsize_byspecies.pdf", family="Times", width=15, height=7.5)	
par(mfrow=c(1,2), mar=c(6,5,1.5,1), cex=1.2)#, cex.lab=1.2)

boxplot(condylobasal_length ~ species, data= aa1[aa1$cranial_height<60,], ylim=c(9.5,17),
	xaxt = "n", xlab="", ylab="Condylobasal length [cm]",#ylab=expression(paste(sqrt("Cranial volume", 3),"  [", cm^{1/3},"]"))
	main=""
		)
mtext(expression(paste("(",bold(a),")")), side=3, line=0, adj=0, cex=1.2)
mtext("Species comparison", side=3, line=0.1, adj=0.5, cex=1.4)
labs3 <- c(expression(italic("H. africaeaustralis")), 
			expression(italic("H. brachyura")),
			expression(italic("H. cristata")), 
			expression(italic("H. indica")), 
			expression(italic("H. javanica"))
			)
axis(at=c(1,2,3,4,5), side=1, labels=NA)
text(c(1,2,3,4,5), par("usr")[3]-0.3, 
     srt = 45, adj= 1, xpd = TRUE,
     labels = labs3)
text(x=c(1,2,3,4,5), y=c(9.5,9.5,9.5,9.5,9.5), c("N=14","N=11","N=122","N=61", "N=10"))


anova(
	lm(condylobasal_length ~ species, data= aa1[aa1$cranial_height<60,]),
	lm(condylobasal_length ~ 1, data= aa1[aa1$cranial_height<60,])
	)
anova(
	lm(condylobasal_length ~ species, data= aa1[aa1$cranial_height<60,])
	)
pairwise.t.test(x = aa1[aa1$cranial_height<60,]$condylobasal_length, 
				g = aa1[aa1$cranial_height<60,]$species,
				p.adj = "holm"
				)
pwr.anova.test(k = 5, n = 7, f = 29.758, sig.level = 0.05, power = NULL)			

	
unique(aa1$species) ; aa1$species <- as.factor(aa1$species)
summary(lm(condylobasal_length ~ species, data= aa1))
aa1$species <- relevel(aa1$species, "cristata")
summary(lm(condylobasal_length ~ species, data= aa1))
aa1$species <- relevel(aa1$species, "indica")
summary(lm(condylobasal_length ~ species, data= aa1))
aa1$species <- relevel(aa1$species, "brachyura")
summary(lm(condylobasal_length ~ species, data= aa1))
aa1$species <- relevel(aa1$species, "javanica")
summary(lm(condylobasal_length ~ species, data= aa1))
	
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$species =="africaeaustralis",]$condylobasal_length)) # 	
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$species =="brachyura",]$condylobasal_length)) # 
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$species =="cristata",]$condylobasal_length)) # 
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$species =="indica",]$condylobasal_length)) # 						
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$species =="javanica",]$condylobasal_length)) # 

########################################
########################################
# Do clades of H. cristata differ in size?
########################################
########################################

# Recent literature (Trucchi and Sbordoni 2009) suggests two clades: Mediterranean and Sub-saharian. I'm ditching the subspecies and sticking to the clades. 

# check sample sizes
	
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$subspecies.updated2=="mediter_clade_Italy",]$condylobasal_length)) # 60, not 61
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$subspecies.updated2=="mediter_clade_Sicily",]$condylobasal_length)) # 14
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$subspecies.updated2=="mediter_clade_Africa",]$condylobasal_length)) # 10, not 07
sum(!is.na(aa1[aa1$cranial_height<60 & aa1$subspecies.updated2=="subsaharian_clade",]$condylobasal_length))	# 38


aa.cristata <- subset(aa1, aa1$species=="cristata")
aa.cristata$clades <- ifelse(aa.cristata$subspecies.updated=="subsaharian_clade", "subsaharian_clade", "mediterranean_clade")

sum(!is.na(aa.cristata[aa.cristata$clades =="mediterranean_clade",]$condylobasal_length)) # 85, not 58
sum(!is.na(aa.cristata[aa.cristata$clades =="subsaharian_clade",]$condylobasal_length))	# 45, not 28		

boxplot(condylobasal_length ~ subspecies.updated2, data=aa.cristata,  ylim=c(9.5,17), main="",
		xaxt = "n", xlab=""#, ylab="Condylobasal length [cm]"
		)
mtext(expression(paste("(",bold(b),")")), side=3, line=0, adj=0, cex=1.2)
mtext(expression(paste(italic("H. cristata"), " clades")), side=3, line=0.1, adj=0.5, cex=1.4)
labs <- c("Mediterranean clade \n (North Africa)","Mediterranean clade \n (Continental Italy)",
	"Mediterranean clade \n (Sicily)", "Subsaharian clade")
axis(at=c(1,2,3,4), side=1, labels=NA)
text(c(1,2,3,4), par("usr")[3]-0.2, 
     srt = 45, adj= 1, xpd = TRUE,
     labels = labs)
text(x=c(1,2,3,4), y=c(9.5,9.5,9.5,9.5), c("N=10","N=60","N=14","N=38"))

dev.off() 

summary(lm(condylobasal_length ~ clades, data= aa.cristata))
t.test(condylobasal_length ~ clades, data= aa.cristata)
library(effsize)
cohen.d(d= aa.cristata$condylobasal_length,
		f= aa.cristata$clades, 
		pooled=T, paired=F, na.rm=T)
pwr.t.test(n=45, d=0.2, power=NULL)

aa.cristmed <- aa.cristata[aa.cristata$clades =="mediterranean_clade",]
unique(aa.cristmed$subspecies.updated2)
summary(lm(condylobasal_length ~ subspecies.updated2, data= aa.cristmed))
aa.cristmed$subspecies.updated2 <- as.factor(aa.cristmed$subspecies.updated2)
aa.cristmed$subspecies.updated2 <- relevel(aa.cristmed$subspecies.updated2, "mediter_clade_Sicily")
summary(lm(condylobasal_length ~ subspecies.updated2, data= aa.cristmed))
aa.cristmed$subspecies.updated2 <- relevel(aa.cristmed$subspecies.updated2, "mediter_clade_Italy")
summary(lm(condylobasal_length ~ subspecies.updated2, data= aa.cristmed))

anova(
	lm(condylobasal_length ~ subspecies.updated2, data= aa.cristmed),
	lm(condylobasal_length ~ 1, data= aa.cristmed)
	)
anova(
	lm(condylobasal_length ~ subspecies.updated2, data= aa.cristmed)
	)
pairwise.t.test(x=aa.cristmed$condylobasal_length, 
				g=aa.cristmed$subspecies.updated2,
				p.adj = "holm"
				)
pwr.anova.test(k = 3, n = 11, f = 13.523, sig.level = 0.05, power = NULL)								

