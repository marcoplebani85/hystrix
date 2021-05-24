########################################################
########################################################
########### HYSTRIX SKULL MORPHOMETRICS ################
########################################################
########################################################

# Code by Marco Plebani

rm(list=ls())

# Load packages, Import and tidy up data
source('Hystrix_skull_analyses_intro.R')

# drop unnecessary columns:
aa1_b <- aa1[,c(1:3,5:23,26:28,32:34)] # col. 29 is skullvolume		
		
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

# STANDARDIZE/NORMALIZE?
# standardize yy by row so that all the values are bounded between 0 and 1
# yy <- as.data.frame(t(apply(yy,1,function(x){x/max(x)})))
# or:
# z-transformation, where you subtract the mean and divide by the standard deviation of your variable. The result will have mean=0 and sd=1. ### DON'T! ### z-transformed data produce weird doughnut-shaped ordinations
# or: scale()
# t(scale(t(dataset, center=T, scale=T))) ### also performs a z-transformation!!!
# but scale() on the untransposed matrix operates across columns, not rows:
yy <- scale(yy)
# calculate dissimilarities
dd2 <- vegdist(yy, method="euclidean")



############
# perMANOVA
############

names(aa1_bfull)
unique(aa1_bfull$subspecies.sensu.strictu)
unique(aa1_bfull$subspecies.updated)
unique(aa1_bfull$subspecies.updated2)
unique(aa1_bfull$clusters)

# ?adonis
# dataframe of response variables:
hystrix.adonis.response <- aa1_bfull[,c(5:16)]
# dataframe of explanatory variables:
hystrix.adonis.explanatory <- data.frame(Species = aa1_bfull[,2]#,
												#Location = aa1_bfull[,45]
												)
## default test by terms
hystrix.adonis <- adonis(hystrix.adonis.response ~ Species, 
		data = hystrix.adonis.explanatory,
		method = "euclidean"
		)
hystrix.adonis

# Post-hoc pairwise comparisons:
library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
#?pairwise.adonis

hystrix.adonis.pw <- pairwise.adonis(x = hystrix.adonis.response,
					factors= hystrix.adonis.explanatory$Species,
					sim.method='euclidean',
					p.adjust.m='holm')

# Do it for H. cristata alone

aa1_bfull_cristata <- subset(aa1_bfull, aa1_bfull$species=="cristata")
aa1_bfull_cristata$subspecies.sensu.strictu <- ifelse(aa1_bfull_cristata$subspecies.updated2=="subsaharian_clade", "subsaharian_clade", "mediterranean_clade")

# dataframe of response variables:
cristata.adonis.response <- aa1_bfull_cristata[,c(5:16)]
# dataframe of explanatory variables:
cristata.adonis.explanatory <- data.frame(Subspecies = aa1_bfull_cristata$subspecies.sensu.strictu#,
												#Location = aa1_bfull[,45]
												)
## default test by terms
cristata.adonis <- adonis(cristata.adonis.response ~ Subspecies, 
		data = cristata.adonis.explanatory,
		method = "euclidean"
		)
cristata.adonis

# Post-hoc pairwise comparisons:
cristata.adonis.pw <- pairwise.adonis(x = cristata.adonis.response,
					factors=cristata.adonis.explanatory$Subspecies,
					sim.method='euclidean',
					p.adjust.m='holm')

# dataframe of response variables:
cristata.adonis.response2 <- aa1_bfull_cristata[,c(5:16)]
# dataframe of explanatory variables:
cristata.adonis.explanatory2 <- data.frame(Clades = aa1_bfull_cristata$subspecies.updated2)
## default test by terms
cristata.adonis2 <- adonis(cristata.adonis.response2 ~ Clades, 
		data = cristata.adonis.explanatory2,
		method = "euclidean"
		)
cristata.adonis2

# Post-hoc pairwise comparisons:
cristata.adonis.pw2 <- pairwise.adonis(x = cristata.adonis.response2,
					factors=cristata.adonis.explanatory2$Clades,
					sim.method='euclidean',
					p.adjust.m='holm')
					
cristata.adonis.pw2 <- as.data.frame(cristata.adonis.pw2)
cristata.adonis.pw2$F.Model <- round(cristata.adonis.pw2$F.Model, 2)
cristata.adonis.pw2$R2 <- round(cristata.adonis.pw2$R2, 2)
hystrix.adonis.pw <- as.data.frame(hystrix.adonis.pw)
hystrix.adonis.pw$F.Model <- round(hystrix.adonis.pw$F.Model, 2)
hystrix.adonis.pw$R2 <- round(hystrix.adonis.pw$R2, 2)
write.csv(cristata.adonis.pw2[,c(1:6)], "~/Desktop/cristata_adonis_pw.csv")
write.csv(hystrix.adonis.pw[,c(1:6)], "~/Desktop/hystrix_adonis_pw.csv")