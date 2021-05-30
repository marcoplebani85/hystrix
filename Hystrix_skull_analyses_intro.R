# DATA AND ANALYSES FROM:
# Mori, E., Ancillotto, L., Lovari, S., Russo, D., Nerva, L., Mohamed, W.F., Motro, Y., Di Bari, P. and Plebani, M. (2019), Skull shape and Bergmann's rule in mammals: hints from Old World porcupines. J Zool, 308: 47-55. https://doi.org/10.1111/jzo.12651

# Analyses by Marco Plebani - marcoplebani85@gmail.com
# Code last checked on 27 May 2021

############################################
# Import and tidy up data; load packages
############################################

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
# a copy of the datasets stored at DOI: XXX
# see metadata provided at OI: XXX for details
