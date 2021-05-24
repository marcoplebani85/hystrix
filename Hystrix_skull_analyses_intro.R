########################################################
########################################################
########### HYSTRIX SKULL MORPHOMETRICS ################
########################################################
########################################################

##########################
# Import and tidy up data
##########################

# rm(list=ls())

# install/update/load useful packages:
if (!require(devtools)) install.packages('devtools'); library(devtools)
# or use pacman::p_load:
if (!require(pacman)) install.packages('pacman'); library(pacman)
pacman::p_load(vegan) # for multivariate stats
pacman::p_load(ecodist) # distance matrices
pacman::p_load(geosphere) # distance matrices
pacman::p_load(mclust) # clustering
# graphics
pacman::p_load(ggplot2)
pacman::p_load(RColorBrewer)
pacman::p_load(scales)
# utilities
pacman::p_load(plotrix)
if (!require(stringi)) install_github("gagolews/stringi"); library(stringi)
pacman::p_load(ade4) # mixed effect models


# Load data

load('Hystrix.RData')

