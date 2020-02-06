# ******************************************************************************
# ******************************************************************************

# ===================================================
# Load and download (if necessary) required packages
# ===================================================

# use (and install if necessary) pacman package 
if (!require("pacman")) install.packages("pacman")
library(pacman)

# load and install (if necessary) required packages for this course
p_load(TraMineR, TraMineRextras, tidyverse, haven, Hmisc, gganimate,
       RColorBrewer, colorspace, knitr, kableExtra, reshape2, summarytools, 
       ecodisc, vegan, MCMCpack, corrplot, ade4)




# ******************************************************************************
# ******************************************************************************