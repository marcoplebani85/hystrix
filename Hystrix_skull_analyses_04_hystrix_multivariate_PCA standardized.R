# DATA AND ANALYSES FROM:
# Mori, E., Ancillotto, L., Lovari, S., Russo, D., Nerva, L., Mohamed, W.F., Motro, Y., Di Bari, P. and Plebani, M. (2019), Skull shape and Bergmann's rule in mammals: hints from Old World porcupines. J Zool, 308: 47-55. https://doi.org/10.1111/jzo.12651

# Analyses by Marco Plebani - marcoplebani85@gmail.com
# Code last checked on 27 May 2021

######################################################
# Multivariate analyses: Principal Component Analysis
######################################################

rm(list=ls())

# Load packages, Import and tidy up data
source('Hystrix_skull_analyses_intro.R')

# includes the lines:
# aa1 <- subset(aa, aa$Verydubious=="no")
# aa1 <- subset(aa1, aa1$locality_country=="locality")
# aa1clean0 <- aa1[aa1$cranial_height<60,] # remove outlier

################################
################################		
# Multivariate analyses - PCA
################################
################################

# we need to build a distance matrix based on skull measurements.
# Use aa1
## first isolate the dimensions data matrix
names(aa1); length(aa1[,1])
aa1 <- aa1[aa1$cranial_height<60,]
aa1$pch <- ifelse(aa1$species=="cristata", 21,
				ifelse(aa1$species=="indica", 2,
					ifelse(aa1$species=="africaeaustralis", 3,
						ifelse(aa1$species=="javanica", 4, 5 #"brachyura"
						))))
aa1$col <- 	ifelse(aa1$species=="cristata", "black", #"springgreen3",
				ifelse(aa1$species=="indica", "blue",
					ifelse(aa1$species=="africaeaustralis", "red",
						ifelse(aa1$species=="javanica", "darkviolet", "black" #"brachyura"
						))))
aa1$bg <- 	ifelse(aa1$subspecies.updated =="subsaharian_clade", grey(1, alpha=0.5), # white
					ifelse(aa1$subspecies.updated =="mediter_clade_Africa", grey(0.05, alpha=0.5),
						ifelse(aa1$subspecies.updated =="mediter_clade_Italy", alpha("springgreen3", 0.5), NA
						)))											
# drop unnecessary columns:
aa1_b <- aa1[,c(1:3,5:23,26:28,32:38)] # col. 29 is skullvolume
						
# IMPORTANT: WE CANNOT HAVE NAs HERE
# this means dropping either some of the skulls (those with NA) or those skull measures that we could not gather from ALL skulls.
# see which columns have the least usable values:
sum(!is.na(aa1$nasal_length))
sum(!is.na(aa1$frontal_length))
sum(!is.na(aa1$parietal_length))
sum(!is.na(aa1$interorbital_maximum_width)) 		# drop this -- aa1_b[8]
sum(!is.na(aa1$zygomatic_arch_width)) 				# drop this -- aa1_b[9]
sum(!is.na(aa1$condylobasal_length))
sum(!is.na(aa1$cranial_height))
sum(!is.na(aa1$foramen_occipitalis_width)) 			# drop this -- aa1_b[12]
sum(!is.na(aa1$ex_occipitalis_height))				# drop this -- aa1_b[13]
sum(!is.na(aa1$maximum_length_of_upper_left_alveolar_line))
sum(!is.na(aa1$palatal_length))
sum(!is.na(aa1$central_incisor_foramina_length))
sum(!is.na(aa1$bullae_tympanicae_width)) 			# drop this -- aa1_b[17]
sum(!is.na(aa1$basioccipitalis_width))				# drop this -- aa1_b[18]
sum(!is.na(aa1$mandibular_length))
sum(!is.na(aa1$maximum_length_of_lower_left_alveolar_line))
sum(!is.na(aa1$mandibular_maximum_height))
sum(!is.na(aa1$distance_length_of_condylar_process_to_dental_foramen))

# drop columns 8,9,12,13,17,18:
aa1_bb <- aa1_b[,-c(8,9,12,13,17,18)]

# Remove the rest of NAs:

aa1_bfull <- aa1_bb[complete.cases(aa1_bb[,c(5:16)]),]
str(aa1_bfull) # 12 variables for 201 skulls
yy <- aa1_bfull[, c(5:16)]
rownames(yy) <- aa1_bfull[,19]
str(yy)

# STANDARDIZE/NORMALIZE?
# standardize yy by row so that all the values are bounded between 0 and 1
# yy <- as.data.frame(t(apply(yy,1,function(x){x/max(x)})))
# or use scale() to have each row with mean 0 and sd 1
yy <- scale(yy)
# or:
# z-transformation, where you subtract the mean and divide by the standard deviation of your variable. The result will have mean=0 and sd=1.
### Z-TRANSFORMED DATA PRODUCE WEIRD DOUGHNUT-SHAPED ORDINATIONS! SCALE() APPLIED TO THE TRANSPOSED MATRIX DOES THE SAME. AVOID. 

pca0<-prcomp(yy, scale.=F, center=F)
summary(pca0)
# the summary indicates that four PCs where created: the number 
# of possible PCs always equals the number of original variables.

# PC1 and PC2 explain respectively ~73% and ~23% of the data's 
# total variability, summing up to a more-than-respectable 96% of
# the total variability. There is no fixed rule about this, but 
# this already tells us that all the other PCs can be ignored as 
# they explain only crumbs of the total data variability.

# plot(pca0,type="lines")
# a "scree plot" allows a graphical assessment of the relative 
# contribution of the PCs in explaining the variability of the data.

pca0[2] 
# the "Rotation" matrix contains the "loadings" of each 
# of the original variables on the newly created PCs.
# The concept of eigenvalue would require to be introduced for
# understanding how the loadings are estimated, and in general
# for a quantitative understanding of how the principal
# components are calculated. See this excellent page: http://setosa.io/ev/principal-component-analysis/

# biplot(pca0)
# the arrows provide a graphical rendition of the 
# loadings of each of the original variables on the used PCs.

# Package GGPLOT2 and its derivative GGBIPLOT have a somehow esoteric
# syntax, but they produce much fancier plots:

# Variances of principal components
variances <- data.frame(variances= pca0$sdev**2, pcomp=1:length(pca0$sdev))
# **2 means ^2

#Plot of variances
varPlot <- ggplot(variances, aes(pcomp, variances)) + geom_bar(stat="identity", fill="gray") + geom_line()
#varPlot

Taxa <- aa1_bfull$clusters

pdf(file="~/Desktop/histrix_PCA_skullvolumeNscaled.pdf", family="Times", width=12, height=6)

par(mfcol=c(1,2), mar=c(5,5,1,1),cex.lab=1.1)
plot(pca0$x[,c(1,2)], #display="sites",
	type="n",
	xlab ="Principal Coordinate 1", ylab="Principal Coordinate 2",
	xlim = c(-8, 8), 
	#ylim = c(-50, 20),
	main = ""
	)
points(pca0$x[,c(1,2)], #display="sites", 
	pch = aa1_bfull$pch, cex = 2,
	col = aa1_bfull$col, bg=aa1_bfull$bg
	)
arrows(rep(0, length(pca0$rotation[,1])),
		rep(0, length(pca0$rotation[,1])),
		pca0$rotation[,c(1)]*15,
		pca0$rotation[,c(2)]*5,
		lwd=1.5,
		length=0.1,
		col="black"
		)
for(i in 1:12){
	text(x = pca0$rotation[i,1]*15.1, 
	y = pca0$rotation[i,2]*5.1,
	i, #rownames(pca0$rotation)[i],
	adj=1, cex=1.2
	)
}

plot(NULL)
legend("topleft", cex=1.5,
	#title=expression(bold("PCA based on 12 variables. N=201 (data scaled and centered)")),
	legend=c(expression(italic("Hystrix africaeaustralis")),
			expression(italic("H. brachyura")),
			substitute(paste(italic('H. cristata'), " (sub-Saharan clade)")),
			substitute(paste(italic(" ''       ''  "), " (Mediterranean clade - Africa)")),
			substitute(paste(italic(" ''       ''  "), " (Mediterranean clade - Italy)")),
			expression(italic("H. indica")),
			expression(italic("H. javanica"))
			),
	bty="n",
	pch=c(3,5,21,21,21,2,4), 
	col=c("red","black","black", "black", "black", "blue", "darkviolet"),
	pt.bg = c(NA, NA, NA, grey(0.05, alpha=0.5), alpha("springgreen3", 0.5), NA, NA)
)
# legend("bottomleft", cex=1,
	# #title=expression(bold("Leverage of the original variables")),
	# legend=paste(1:12, " - ", rownames(pca0$rotation)[1:12]),
	# bty="n"#,
	# #pch=as.character(1:12)
# )
dev.off()

pdf(file="~/Desktop/histrix_PCA_skullvolumeNscaled2.pdf", family="Times", width=6, height=12)

par(mfcol=c(2,1), mar=c(5,5,1,1),cex.lab=1.1)
plot(pca0$x[,c(1,2)], #display="sites",
	type="n",
	xlab ="Principal Coordinate 1", ylab="Principal Coordinate 2",
	xlim = c(-8, 8), 
	#ylim = c(-50, 20),
	main = ""
	)
points(pca0$x[,c(1,2)], #display="sites", 
	pch = aa1_bfull$pch, cex = 2,
	col = aa1_bfull$col, bg=aa1_bfull$bg
	)
arrows(rep(0, length(pca0$rotation[,1])),
		rep(0, length(pca0$rotation[,1])),
		pca0$rotation[,c(1)]*15,
		pca0$rotation[,c(2)]*5,
		lwd=1.5,
		length=0.1,
		col="black"
		)
for(i in 1:12){
	text(x = pca0$rotation[i,1]*15.1, 
	y = pca0$rotation[i,2]*5.1,
	i, #rownames(pca0$rotation)[i],
	adj=1, cex=1.2
	)
}

plot(NULL)
legend("topleft", cex=1.5,
	#title=expression(bold("PCA based on 12 variables. N=201 (data scaled and centered)")),
	legend=c(expression(italic("Hystrix africaeaustralis")),
			expression(italic("H. brachyura")),
			substitute(paste(italic('H. cristata'), " (sub-Saharan clade)")),
			substitute(paste(italic(" ''       ''  "), " (Mediterranean clade - Africa)")),
			substitute(paste(italic(" ''       ''  "), " (Mediterranean clade - Italy)")),
			expression(italic("H. indica")),
			expression(italic("H. javanica"))
			),
	bty="n",
	pch=c(3,5,21,21,21,2,4), 
	col=c("red","black","black", "black", "black", "blue", "darkviolet"),
	pt.bg = c(NA, NA, NA, grey(0.05, alpha=0.5), alpha("springgreen3", 0.5), NA, NA)
)
# legend("bottomleft", cex=1,
	# #title=expression(bold("Leverage of the original variables")),
	# legend=paste(1:12, " - ", rownames(pca0$rotation)[1:12]),
	# bty="n"#,
	# #pch=as.character(1:12)
# )
dev.off()
# The Eigenvalues tell the amount of variance that each variable contributes to each component. If you sum the Eigenvalues you get the total variance in the data.

#pca0$sdev^2 / sum(pca0$sdev^2)

# Correlation coef between PC1 and frontal length 0.996, all other coefs have absolute values <= 0.084
# Correlation coef between PC2 and central_incisor_foramina_length 0.993, all other coefs have absolute values <= 0.083



