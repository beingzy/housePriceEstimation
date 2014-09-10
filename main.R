# ############################################################ #
# House Price Esitmation Model (Zillow.com interview exercise) #
#                                                              #
# Author: Yi Zhang                                             #
# Email: uc.yizhang@gmail.com (beingzy@gmail.com)              #
# Create Date: SEP/22/2014                                     #
# ############################################################ #
# Install dependent packages
# temp.pkgs <- c("reshape2", "plyr", "timeDate", "ggplot2", "ggmap", 
#                "nnet", "randomForest", "e1071", "RCurl")
# install.packages(pkgs=temp.pkgs)
rm(list = ls())

# ---- toolbox ------ #
library(reshape2)     # Data Preparating
library(plyr)         # Data Wrangling
library(timeDate)     # Date time manipulating
# ---- Web Request--- #
library(RCurl)        #
library(XML)          # XML IO
library(RJSONIO)      # JSON IO
# ----- visual ------ #
library(ggplot2)      # Data Visualization
library(ggmap)        # Map data visualization
# ----- modeling ---- #
library(nnet)         # Nueral Network
library(randomForest) # Utitlize na.roughFix() to impute missing data
library(e1071)        # SVM
# ----- data -------- #
library(zipcode)
library(maps)

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
config$data_util_rate <- .2  # < 1 for developing purpose, =1 for training model
config$zillow_api_key <- "X1-ZWz1dwiguygqob_agge7"
config$census_api_key <- "bca6ac619083f4619242e65caa01ebb1f765c107"

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
  missing_cnt  <- apply(db, MARGIN=2, function(x) sum(is.na(x)) )
  missing_pct  <- round(missing_cnt / nobs * 100) / 100 
  val_range    <- apply(db, MARGIN=2, function(x){ ifelse(typeof(x) != "character", paste(range(x, na.rm=T), sep=","), x[!is.na(x)][1]) })
  
  res                    <- list()
  res$nobs               <- nrow(db)
  res$varnames           <- varnames
  res$var_info           <- as.data.frame(rbind(datatypes, missing_cnt, missing_pct, val_range))
  colnames(res$var_info) <- varnames
  return(res)
}

num2char <- function(x, numchar = 5, appendix=""){
  # ******************************************* #
  # convert number into characters of specific  #
  # length with leading 0                       #
  # input:                                      #
  #   x: (numeric),                             #
  #  numchar: (numeric), length of characters   #
  # return:                                     #
  #  res: (character), numchar(res) == ncahr    #
  # ******************************************* #
  x_char <- as.character(x)
  if(nchar(x_char) <= numchar){
    zeros <- paste(rep(0, times = numchar - nchar(x_char)), collapse="")
    res   <- paste(zeros, x_char, sep="")
  }else{
    res <- x_char[1:numchar]
  }
  res <- paste(appendix, res, sep="")
  res <- ifelse(is.na(x), NA, res)
  return(res)
}

getZillowZipPrice <- function(zip, APIkey){
  # ********************************************* #
  # Query zillow.com to get median list price     #
  # and price per square feet median of an area   #
  # specified by zip                              #
  # input:                                        #
  #   zip: (numeric), 5-digit zip code            #
  #  zillow_id: (string), zillow id               #
  # return:                                       #
  #   res: (vector), median list price and value  #
  #        per square feet                        #
  # ********************************************* #
  print(paste("Querying Zillow.com about ZIP:", zip, "...", sep = ""))
  url          <- paste("http://www.zillow.com/webservice/GetDemographics.htm?zws-id=", APIkey,"&zip=", zip,sep="")
  res_xml      <- xmlInternalTreeParse(dev$url)
  price_median <- xpathApply(dev$res_xml, "//table[name = 'Affordability Data']/data/attribute[name = 'Median List Price']/values/zip/value", xmlValue)[[1]][1]
  sqft_median  <- xpathApply(dev$res_xml, "//table[name = 'Affordability Data']/data/attribute[name = 'Median Value Per Sq Ft']/values/zip/value", xmlValue)[[1]][1]
  price_median <- as.numeric(price_median)
  sqft_median  <- as.numeric(sqft_median)
  res <- c("median_list_price"= price_median, "median_value_per_sqft" = sqft_median)
  return(res)
}

getCensusData <- function(state, fieldnm, APIkey, year = 2010){
  # ******************************************* #
  # Query census API                            #
  # input:                                      #
  #   state: (numeric), 2-digit state code      #
  #   filednm: (string), fild name              #
  #   APIKey: (string), your census.gov API key #
  # return:                                     #
  #   res: (matrix-like) zip, field_val         #
  # ******************************************* #
  resURL=paste("http://api.census.gov/data/", year, "/sf1?get=",fieldnm,
               "&for=zip+code+tabulation+area:*&in=state:",state,"&key=",
               APIkey,sep="")
  dfJSON=fromJSON(resURL)
  dfJSON=dfJSON[2:length(dfJSON)]
  dfJSON_zip=sapply(dfJSON,function(x) x[3])
  dfJSON_val=sapply(dfJSON,function(x) x[1])
  df=data.frame(dfJSON_zip,as.numeric(dfJSON_val))
  names(df)=c("zip","val")
  return(df)
}


# ############################### #
# LOAD DATA ----------------------
# ############################### #
db  <- list()
sup <- list()

db$TRAIN_ZILLOW <- read.csv(file=getDataPath(filename="training_ZILLOW_CONFIDENTIAL.csv",   dir=dir$data), header = TRUE, sep = ",", stringsAsFactor = FALSE)
db$VAL_ZILLOW   <- read.csv(file=getDataPath(filename="validation_ZILLOW_CONFIDENTIAL.csv", dir=dir$data), header = TRUE, sep = ",", stringsAsFactor = FALSE)
# load zipcode, city, state
data(zipcode)
# zip, tract, address ratio
sup$zip <- read.csv(file=getDataPath(filename="ZIP_TRACT_032010.csv", dir=dir$data), colClasses=c(rep("character", times=2), rep("numeric", times=4)),
                    header = TRUE, sep = ",", stringsAsFactor = FALSE)

# ############################### #
# DATA PROCESSING ------------------
# ############################### #
# >> EXPANDING DATA SOURCE
# Date Category: 
#    a.transdate, transdate_previous --> prev_traded_ind (0 or 1), month_from_prev_trade, year_from_prev_trade
#    b.transdate, builtyear          --> year_house_age, month_house_age
# 
# Location Cateogory:
#    a. latitude, longitude --> neighbour_hood_facility (starbucks_ind, walmart_ind, costco_ind..., wholefood_ind, ..., access_to_public_park)
#    c. censustract --> population, population_increase_last_5_yaer, unit_increase, (asian_pct, black_pct, latin_pct, white_pct), (median_income, gini_index, ...)
#
# >> DATA TRANSFORMATION
# * transvalue --> log(transvalue): normalization
# * transdate_previous --> traded_prev = (0 or 1), transdate
# 
# >> VARAIABLE INVESTIGATION
#  *censustract
# unique in VAL_ZILLOW (350): 401  6600  6100 32002 23202  9000   800 30309
# unique in TRAIN_ZILLOW (349): NA(4) 28000  9300 23802  7500 26500  7900
#

# ############################
# (latitude, longitude) 
# ############################
db$TRAIN_ZILLOW$latitude  <- sapply(db$TRAIN_ZILLOW$latitude,  function(x) ifelse(abs(x) <= 1000,  x, x/ 1000000) )
db$TRAIN_ZILLOW$longitude <- sapply(db$TRAIN_ZILLOW$longitude, function(x) ifelse(abs(x) <= 1000,  x, x/ 1000000) )
db$VAL_ZILLOW$latitude    <- sapply(db$VAL_ZILLOW$latitude,    function(x) ifelse(abs(x) <= 1000,  x, x/ 1000000) )
db$VAL_ZILLOW$longitude   <- sapply(db$VAL_ZILLOW$longitude,   function(x) ifelse(abs(x) <= 1000,  x, x/ 1000000) )
# #####################
# log(y)
# #####################
db$TRAIN_ZILLOW$transvalue <- log(db$TRAIN_ZILLOW$transvalue)
db$VAL_ZILLOW$transvalue   <- log(db$VAL_ZILLOW$transvalue)

# ######################
# tract -> zip code
# ######################
db$TRAIN_ZILLOW$full_censustract <- sapply(db$TRAIN_ZILLOW$censustract, function(x) num2char(x=x, numchar=5, appendix="530330"))
db$TRAIN_ZILLOW                   <- merge(db$TRAIN_ZILLOW, sup$zip, by.x="full_censustract", by.y="TRACT", all.x=TRUE)
db$TRAIN_ZILLOW                   <- merge(db$TRAN_ZILLOW, zipcode, by.x="ZIP", by.y="zip", all.x=TRUE)

db$VAL_ZILLOW$full_censustract <- sapply(db$VAL_ZILLOW$censustract,   function(x) num2char(x=x, numchar=5, appendix="530330"))
db$VAL_ZILLOW                  <- merge(db$VAL_ZILLOW, sup$zip, by.x="full_censustract", by.y="TRACT", all.x=TRUE)
db$VAL_ZILLOW                  <- merge(db$VAL_ZILLOW,  zipcode, by.x="ZIP", by.y="zip", all.x=TRUE)

# ##########################################
# zip code to get median price via zillow
# ##########################################
temp$unique_zip  <- unique(c(unique(db$TRAIN_ZILLOW$ZIP), unique(db$VAL_ZILLOW$ZIP)))
temp$zillow_res  <- sapply(temp$unique_zip, function(zip, APIkey = zillow_api_key){ getZillowZipPrice(zip=zip, APIkey=APIkey) }, 
                           zillow_api_key = config$zillow_api_key)
sup$zillow_price <- as.data.frame(cbind("ZIP"=temp$unique_zip, t(temp$zillow_res)))
# Attach zillow price informtion to unit (ZIP_level)
db$TRAIN_ZILLOW <- merge(db$TRAIN_ZILLOW, sup$zillow_price, by.x="ZIP", by.y="ZIP", all.x=TRUE)
db$VAL_ZILLOW   <- merge(db$VAL_ZILLOW,   sup$zillow_price, by.x="ZIP", by.y="ZIP", all.x=TRUE)

# #######################################
# census data average of 2010s and 2000s
# #######################################


db$all <- rbind(cbind(db$TRAIN_ZILLOW, "is_train"=rep(1, times = nrow(db$TRAIN_ZILLOW))), cbind(db$TRAIN_ZILLOW, "is_train"=rep(0, times = nrow(db$TRAIN_ZILLOW))))
db$all <- as.data.frame(db$all)


# ############################### #
# EXPLORATERY STUDY ----------------
# ############################### #
summary$train_data      <- list() # Summarize train data
summary$validation_data <- list() # Summarize validation data

summary$train_data      <- dataSummarization(db=db$TRAIN_ZILLOW)
summary$validation_data <- dataSummarization(db=db$VAL_ZILLOW)

# ############################### #
# MODEL TRAINING ------------------
# ############################### #

# ############################### #
# MODEL SELECTION -----------------
# ############################### #

# ############################### #
# PERFORMANCE REPORTING ----------
# ############################### #


