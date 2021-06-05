# DATA AND ANALYSES FROM:
# Mori, E., Ancillotto, L., Lovari, S., Russo, D., Nerva, L., Mohamed, W.F., Motro, Y., Di Bari, P. and Plebani, M. (2019), Skull shape and Bergmann's rule in mammals: hints from Old World porcupines. J Zool, 308: 47-55. https://doi.org/10.1111/jzo.12651

# TO CITE:
# Plebani, Marco; Ancillotto, Leonardo; Lovari, Sandro; Russo, Danilo; Nerva, Luca; Mohamed, Walid Fathy; Motro, Yoav; Di Bari, Pietro; Mori, Emiliano. (2021, June 5). Reproducible statistical analyses from: “Skull shape and Bergmann’s rule in mammals: hints from Old World porcupines”. Zenodo. http://doi.org/10.5281/zenodo.4903838

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
# a copy of the datasets stored at DOI: 10.5281/zenodo.4903784
# see "Hystrix_metadata.xlsx" for details

# In short:

# hhh: Dataset with species, origin, and morphometric information for each specimen.# aa: Same as hhh, plus reconstructed latitude/longitude of origin and cranial volume estimates.# aa1: A subset of aa. It excludes an outlier and specimens with overly vague/dubious geographic origin.# aa2: A subset of aa. It includes only specimens with precise geographic origin, namely at the level of locality and not only at the level of country/nation.

