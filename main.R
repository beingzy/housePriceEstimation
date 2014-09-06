# ############################################################ #
# House Price Esitmation Model (Zillow.com interview exercise) #
#                                                              #
# Author: Yi Zhang                                             #
# Email: uc.yizhang@gmail.com (beingzy@gmail.com)              #
# Create Date: SEP/22/2014                                     #
# ############################################################ #
# Install dependent packages
temp.pkgs <- c("reshape2", "plyr", "timeDate", "ggplot2", "nnet", 
               "randomForest", "e1071")
install.packages(pkgs=temp.pkgs)
rm(list = ls())
# ---- toolbox ------ #
#library(rattle)      # toolkit box
library(reshape2)     # Data Preparating
library(plyr)         # Data Wrangling
library(timeDate)     # Date time manipulating
# ----- visual ------ #
library(ggplot2)      # Data Visualization
#library(rCharts)     # Javascript dataVisual
#library(gridExtra)   # Multiple sub-plots
# ----- modeling ---- #
library(nnet)         # Nueral Network
library(randomForest) # Utitlize na.roughFix() to impute missing data
library(e1071)        # SVM

# ############################### #
# SETTING ENVIRONMENT -------------
# ############################### #
dir     <- list() # Directory
temp    <- list() # Store temporary data to keep working space clean
config  <- list() # global setting
summary <- list() # Store aggregate information
img     <- list() # Store images objects

dir$root   <- paste(getwd(), "/", sep="")
dir$data   <- paste(dir$root, "/data/", sep = "")
dir$output <- paste(dir$root, "/output/", sep = "")
dir$doc    <- paste(dir$root, "/doc/", sep = "")

# develop code data
config$data_util_rate <- 0.2 # < 1 for developing purpose, =1 for training model

# ############################### #
# FUNCTION DEFINITION --------------
# ############################### #
getDataPath <- function(filename, dir){
  # ************************************ #
  # Return file path given dir&filename  #
  # input:                               #
  #  filename: (string), file name       #
  #  dir: (string), diretory path        #
  # return:                              #
  #  res: (string)                       #
  # ************************************ #
  res <- paste(filename, dir, sep="")
  return(res)
}


# ############################### #
# LOAD DATA ----------------------
# ############################### #





# ############################### #
# DATA PROCESSING ------------------
# ############################### #

# ############################### #
# EXPLORATERY STUDY ----------------
# ############################### #

# ############################### #
# MODEL TRAINING ------------------
# ############################### #

# ############################### #
# MODEL SELECTION -----------------
# ############################### #

# ############################### #
# PERFORMANCE REPORTING ----------
# ############################### #


