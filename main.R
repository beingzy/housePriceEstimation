# ############################################################ #
# House Price Esitmation Model (Zillow.com interview exercise) #
#                                                              #
# Author: Yi Zhang                                             #
# Email: uc.yizhang@gmail.com (beingzy@gmail.com)              #
# Create Date: SEP/22/2014                                     #
# ############################################################ #
# Install dependent packages
# temp.pkgs <- c("reshape2", "plyr", "timeDate", "ggplot2", "nnet", 
#               "randomForest", "e1071")
# install.packages(pkgs=temp.pkgs)
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

dir$root   <- paste(getwd(),  "/",       sep = "")
dir$data   <- paste(dir$root, "data/",   sep = "")
dir$output <- paste(dir$root, "output/", sep = "")
dir$doc    <- paste(dir$root, "doc/",    sep = "")
dir$img    <- paste(dir$root, "images",  sep = "")

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
  res <- paste(dir, filename, sep="")
  return(res)
}

dataSummarization <- function(db){
  # ****************************************** #
  # Summarize basic information of data sets   #
  # nobs, var's range, missing rate            #
  # > Input:                                   #
  #   db: (matrix-like), data set              #
  # > return:                                  #
  #   res:(list), varname, datatype...         #
  # ****************************************** #
  nobs         <- nrow(db)
  varnames     <- colnames(db)
  datatypes    <- apply(db, MARGIN=2, typeof)
  missing_cnt  <- apply(db, MARGIN=2, FUN=function(x) sum(is.na(x)) )
  missing_pct  <- round(missing_cnt / nobs * 100) / 100 
  val_range    <- apply(db, MARGIN=2, FUN=function(x){ ifelse(typeof(x) != "character", paste(range(x, na.rm=T), sep=","), x[!is.na(x)][1]) })
  
  res                    <- list()
  res$nobs               <- nrow(db)
  res$varnames           <- varnames
  res$var_info           <- as.data.frame(rbind(datatypes, missing_cnt, missing_pct, val_range))
  colnames(res$var_info) <- varnames
  return(res)
}


# ############################### #
# LOAD DATA ----------------------
# ############################### #
db <- list()

db$TRAIN_ZILLOW <- read.csv(file=getDataPath(filename="training_ZILLOW_CONFIDENTIAL.csv",   dir=dir$data), header = TRUE, sep = ",", stringsAsFactor = FALSE)
db$VAL_ZILLOW   <- read.csv(file=getDataPath(filename="validation_ZILLOW_CONFIDENTIAL.csv", dir=dir$data), header = TRUE, sep = ",", stringsAsFactor = FALSE)

# ############################### #
# DATA PROCESSING ------------------
# ############################### #

# ############################### #
# EXPLORATERY STUDY ----------------
# ############################### #
summary$train_data      <- list() # Summarize train data
summary$validation_data <- list() # Summarize validation data

summary$train_data$overview     <- str(db$TRAIN_ZILLOW)
summary$train_data$missing_rate <- apply(db$TRAIN_ZILLOW, MARGIN=2, 
                                         FUN=function(x){
                                           missing_cnt <- sum(is.na(x))
                                           res         <- c()
                                         } res <- c("rate" = sum(is.na(x))/length(x))

# ############################### #
# MODEL TRAINING ------------------
# ############################### #

# ############################### #
# MODEL SELECTION -----------------
# ############################### #

# ############################### #
# PERFORMANCE REPORTING ----------
# ############################### #


