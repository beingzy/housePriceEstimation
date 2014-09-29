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
#dir$doc    <- paste(dir$root, "doc/",    sep = "")
#dir$img    <- paste(dir$root, "images",  sep = "")

# develop code data
config$data_util_rate <- .2  # < 1 for developing purpose, =1 for training model
# config$zillow_api_key <- "X1-ZWz1dwiguygqob_agge7"
# config$census_api_key <- "bca6ac619083f4619242e65caa01ebb1f765c107"
config$id_var         <- c("propertyid")
#config$response_var   <- c("transvalue")
#config$no_predictor   <- c("latitude", "latitude.x", "latitude.y"
#                           , "longitude", "longitude.x", "longitude.y"
#                           , "viewtypeid"
#                           , config$id_var
#                           , config$response_var)

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

getZillowZipPrice <- function(zip, APIkey, puase.time = 0){
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
  Sys.sleep(time = puase.time ) # set pause time
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
  Sys.sleep(time = .5) # set pause time
  dfJSON=fromJSON(resURL)
  dfJSON=dfJSON[2:length(dfJSON)]
  dfJSON_zip=sapply(dfJSON,function(x) x[3])
  dfJSON_val=sapply(dfJSON,function(x) x[1])
  df=data.frame(dfJSON_zip,as.numeric(dfJSON_val))
  names(df)=c("zip","val")
  return(df)
}

dateExtractor <- function(date_str, comp = "year"){
  # **************************************** #
  # Extract date component from date string  #
  # input:                                   #
  #   date_str:(string), "1/01/2014"         #
  #   component:(vector), year, month, day   #
  # return:                                  #
  #   res: (vector, numeric)                 #
  # **************************************** #
  decomp <- as.numeric( strsplit(date_str, split = "/")[[1]] )
  res    <- c("month" = decomp[1], "day" = decomp[2], "year" = decomp[3])
  res    <- res[comp]
  return(res)
}

futureValueCal <- function(val, now_year, future_year, cpi){
  # **************************************** #
  # Calculate the future value               #
  # input:                                   #
  #   val: (numeric), house value            #
  #   now_year: (numeric)                    #
  #   future_year: (numeric)                 #
  #   cpi: (vector, numeric), cpi value      #
  # return:                                  #
  #   res: (numeric), future values in nyear #
  # **************************************** #
  now_year_idx    <- which(cpi$Year == now_year)
  future_year_idx <- which(cpi$Year == future_year)
  cpi_vec         <- cpi$Ave[now_year_idx:(future_year_idx - 1)] / 100
  multiplier      <- 1
  for(i in cpi_vec) multiplier <- multiplier * (1 + i)
  res             <- val * multiplier
  return(res)
}

reportPerf <- function(error_df){
  # ***************************************** #
  # Report resulets                           #
  # input:                                    #
  #   error_df: (matrix-like)                 #
  # return:                                   #
  #   res: (vector)                           #
  # ***************************************** #
  median       <- median(error_df$error_pct)
  within5_pct  <- sum(abs(error_df$error_pct) <= .05) / nrow(error_df)
  within10_pct <- sum(abs(error_df$error_pct) <= .10) / nrow(error_df)
  within20_pct <- sum(abs(error_df$error_pct) <= .20) / nrow(error_df)
  res <- c("error_pct_median" = median, "within5_pct" = within5_pct
           , "within10_pct" = within10_pct, "within20_pct" = within20_pct)
  return(res)
}

distCal <- function(point_a, point_b)
{
  # ***************************************** #
  # Calcualte the distance between two points #
  # input:                                    #
  #  point1: (vector)                         #
  #  point2: (vector)                         #
  # return:                                   #
  #  res: (numeric)                           #
  # ***************************************** #
  comp_dis <- point_a - point_b
  res      <- sqrt(sum(comp_dis ^ 2))
  return(res)
}

getExternalInfo <- function(record, info_ds){
  # **************************************** #
  # Get aggregate information for recrod     #
  # input:                                   #
  #  record: (vector), censustract, lng, lat #
  #  info_ds: (matrix-like)                  #
  # return:                                  #
  #  res: (vector)                           #
  # **************************************** #
  info_ds <- info_ds[info_ds$TRACT == as.character(record["full_censustract"]), ]
  loc     <- info_ds[, c("longitude", 'latitude')]
  dist    <- apply(info_ds[, c("longitude", "latitude")]
                   , MARGIN = 1
                   , FUN = function(x, y)
                   {
                     res <- distCal(point_a = x, point_b = y)
                     return(res)
                   }, y = loc)
  dist_order <- order(dist)
  info_ds    <- info_ds[dist_order, ]
  the_zip    <- info_ds[1, "ZIP"]
  res_ratio  <- mean(info_ds$RES_RATIO)
  bus_ratio  <- mean(info_ds$BUS_RATIO)
  oth_ratio  <- mean(info_ds$OTH_RATIO)
  tot_ratio  <- mean(info_ds$TOT_RATIO)
  res        <- as.matrix(c(the_zip, res_ratio,  bus_ratio, oth_ratio, tot_ratio), nrow = 1)
  res        <- as.numeric(res)
  names(res) <- c("zip", "res_ratio", "bus_ratio", "oth_ratio", "tot_ratio")
  return(res)
}

predError <- function(x){
  # ******************************************* #
  # Calculate the errors of predcition          #
  # input:                                      #
  #   x: (vector), "transvalue_log", "pred_val" #
  # return:                                     #
  #   res: (vector)                             #
  # ******************************************* #
  true <- 10 ^ x["transvalue_log"]
  pred <- 10 ^ x["pred_val"]
  error_dev <- pred - true
  error_abs <- abs(pred - true)
  error_pct <- round((pred - true) / true, digits = 4)
  res <- c("error_dev" = error_dev, "error_abs" = error_abs, "error_pct" = error_pct)
  return(res)
}

ensemblePrediction <- function(ds, models, weights = NA){
  # ************************************* #
  # Make Ensemble Predction               #
  # input:                                #
  #  ds: (matrix-like), new data set      #
  #  model: (list), model list            #
  #  weights: (vector), blending weights  #
  #         , sum(weights) = 1            #
  # return:                               #
  #  res: (vector/matrix-like) prediction #
  # ************************************* #
  pred      <- as.data.frame(matrix(0, nrow = nrow(ds), ncol = length(models)))
  for(i in 1:length(models)){
    pred[, i] <- predict(models[[i]], newdata = ds)
  }
  if(is.na(weights)){
    res <- apply(pred, MARGIN = 1, mean)
  }else{
    res <- apply(pred, MARGIN = , function(x, weights){ sum(x * weights) }, weights = weights)
  }
  return(res)
}

## ######################################## ##
## DATA LOADING ------------------------------
## ######################################## ##
sup <- list()

print("* Loading Data ....")
data(zipcode)

TRAIN      <- read.csv(file = getDataPath(filename="training_ZILLOW_CONFIDENTIAL.csv",   dir=dir$data), header = TRUE, sep = ",", stringsAsFactor = FALSE)
VALID      <- read.csv(file = getDataPath(filename="validation_ZILLOW_CONFIDENTIAL.csv",   dir=dir$data), header = TRUE, sep = ",", stringsAsFactor = FALSE)
cpi        <- read.csv(file = getDataPath(filename="cpi_us_1937_2005.csv", dir = dir$data), header = TRUE, sep = " ", stringsAsFactors = FALSE)
sup$zillow <- read.csv(file = getDataPath(filename="ZILLOW_ZIP_PRICE.csv", dir = dir$data), header = TRUE, sep = ",")
sup$zip    <- read.csv(file = getDataPath(filename="ZIP_TRACT_032010.csv", dir=dir$data)
                       , colClasses=c(rep("character", times=2), rep("numeric", times=4))
                       , header = TRUE, sep = ",", stringsAsFactor = FALSE)

sup$zip <- merge(sup$zip, zipcode[, c("zip", "latitude", "longitude")], by.x = "ZIP", by.y = "zip", all.x = TRUE)
#sup$zip <- subset(sup$zip, TRACT %in% unique(c(unique(ds$full_censustract, ds2$full_censustract))))

# Add an indicator to tell source of data
TRAIN$validation_idx <- 0
VALID$validation_idx <- 1
# Concatenate two data sets together
ds<- as.data.frame(rbind(TRAIN, VALID))
## Extract trade month to consider the seasonal effects in market
print("* Data transformation ...")
ds$transmonth    <- sapply(ds$transdate, function(x) dateExtractor(date_str = x, comp = "month"))
## Create indicator variable to model seasonal effects
ds$trans_spr_ind <- sapply(ds$transmonth, function(x) ifelse(x %in% c(3, 4, 5),   1, 0) ) 
ds$trans_sum_ind <- sapply(ds$transmonth, function(x) ifelse(x %in% c(6, 7, 8),   1, 0) ) 
ds$trans_fal_ind <- sapply(ds$transmonth, function(x) ifelse(x %in% c(9, 10, 11), 1, 0) ) 
ds$trans_win_ind <- sapply(ds$transmonth, function(x) ifelse(x %in% c(12, 1, 2),  1, 0) ) 
## Years since built
ds$trans_build_age <- apply(ds[, c("transdate", "builtyear")]
                            , MARGIN = 1
                            , function(x)
                            {
                              transyear <- dateExtractor(x["transdate"], comp = "year")
                              builtyear <- as.numeric(x["builtyear"])
                              res       <- transyear - builtyear
                              return(res)
                            })

## transdate_previous transvalue_previous
ds$trans_prev_ind     <- sapply(ds$transdate_previous, function(x) ifelse(x == "", 0, 1))
ds$trans_prev_numyear <- apply(ds[, c("transdate", "transdate_previous")]
                               , MARGIN = 1
                               , function(x)
                               {
                                 now_year  <- dateExtractor(x["transdate"], comp = "year")
                                 then_year <- dateExtractor(x["transdate_previous"], comp = "year")
                                 res       <- now_year - then_year
                                 res       <- ifelse(is.na(res), 0, res)
                               })
## trans_val cpi to calculate 2005's present value
ds$trans_val_2005 <- apply(ds[, c("transdate", "transdate_previous", "transvalue_previous")]
                           , MARGIN = 1
                           , function(x, cpi_db){
                             val         <- as.numeric(x["transvalue_previous"])
                             future_year <- dateExtractor(x["transdate"], comp = "year")
                             now_year    <- dateExtractor(x["transdate_previous"], comp = "year")
                             if(!(is.na(val)) & !(is.na(future_year)) & !(is.na(now_year)))
                             {
                               res <- futureValueCal(val = val, now_year = now_year, future_year = future_year, cpi = cpi_db)
                             }else{
                               res <- 0
                             }
                             return(res)
                           }, cpi_db = cpi)
## finishedsquarefeet, lotsizesquarefeet
ds$finished_lot_ratio <- apply(ds[, c("finishedsquarefeet", "lotsizesquarefeet")]
                               , MARGIN = 1
                               , function(x)
                               {
                                 res <- round(x["finishedsquarefeet"] / x["lotsizesquarefeet"], digits = 2)
                               })

## storycnt
ds$story_single_ind <- sapply(ds$storycnt, function(x) ifelse(x == 1, 1, 0))
ds$story_mult_ind   <- sapply(ds$storycnt, function(x) ifelse(x >  1,  1, 0))
## view type
ds$viewtypeid[which(is.na(ds$viewtypeid) | ds$viewtypeid == "")] <- 0
ds$view_type0 <- sapply(ds$viewtypeid, function(x) ifelse(x == 0, 1, 0))
ds$view_type1 <- sapply(ds$viewtypeid, function(x) ifelse(x == 2, 1, 0))
ds$view_type2 <- sapply(ds$viewtypeid, function(x) ifelse(x == 3, 1, 0))
ds$view_type3 <- sapply(ds$viewtypeid, function(x) ifelse(x == 4, 1, 0))
ds$view_type4 <- sapply(ds$viewtypeid, function(x) ifelse(x == 8, 1, 0))
ds$view_type5 <- sapply(ds$viewtypeid, function(x) ifelse(x == 12, 1, 0))
ds$view_type6 <- sapply(ds$viewtypeid, function(x) ifelse(x == 13, 1, 0))
## censustract 
ds$full_censustract   <- sapply(ds$censustract, function(x) num2char(x=x, numchar=5, appendix="530330"))
## latitude, longitude -->
ds$latitude           <- ds$latitude  / 1000000
ds$longitude          <- ds$longitude / 1000000 
## transvalue
ds$transvalue_log     <- log(ds$transvalue,     base = 10)
ds$trans_val_2005_log <- log(ds$trans_val_2005, base = 10)

## ######################################## ##
## MERGE WITH EXTERNAL DATA ------------------
## ######################################## ##
print("Merging with external information....")
sup$zip <- subset(sup$zip, TRACT %in% unique(ds$full_censustract))
temp    <- apply(ds[, c("full_censustract", "longitude", "latitude")]
                 , MARGIN = 1
                 , function(x, ds){ getExternalInfo(record = x, info_ds = ds) }
                 , ds = sup$zip)

temp         <- as.data.frame(t(temp))
ds$zip       <- temp$zip
ds$res_ratio <- temp$res_ratio
ds$bus_ratio <- temp$bus_ratio
ds$oth_ratio <- temp$oth_ratio
ds$tot_ratio <- temp$tot_ratio

## zillow price data
print("Attaching zillow price information via (zillow.com API) ...")
ds  <- merge(ds, sup$zillow, by.x = "zip", by.y = "ZIP", all.x = TRUE)

## Merging with demographic information
census_db <- read.csv(file = getDataPath("zbp05totals.txt", dir=dir$data), sep = ",", header = TRUE, stringsAsFactors = FALSE)
census_db <- census_db[, !(colnames(census_db) %in% c("name", "empflag"))]
ds        <- merge(ds, census_db, by.x="zip", by.y="zip", all.x=TRUE)

## ###################################### ##
## modeling configuration ----------------
## ###################################### ##
config$model0 <- list("id" = "propertyid"
                      , "reponse"   = "transvalue"
                      , "predictor" = c("transdate", "transdate_previous", "transvalue_previous"
                                        , "bathroomcnt", "bedroomcnt", "builtyear", "finishedsquarefeet"
                                        , "lotsizesquarefeet", "storycnt", "viewtypeid")
                      , "remove_vars" = c("propertyid", "usecode", "censustract"))

##
config$model1 <- list("id" = "propertyid"
                      , "reponse"   = "transvalue_log" 
                      , "predictor" = c("trans_spr_ind", "trans_sum_ind", "trans_fal_ind", "trans_win_ind"
                                        , "trans_prev_ind", "trans_prev_numyear"
                                        , "trans_val_2005"
                                        , "trans_build_age"
                                        , "bathroomcnt", "bedroomcnt"
                                        , "finishedsquarefeet", "lotsizesquarefeet", "finished_lot_ratio"
                                        , "story_single_ind", "story_mult_ind"
                                        , "view_type0", "view_type1", "view_type2", "view_type3", "view_type4", "view_type5", "view_type6"
                      ))

config$model2 <- list("id" = "propertyid"
                      , "reponse"   = "transvalue_log" 
                      , "predictor" = c("trans_spr_ind", "trans_sum_ind", "trans_fal_ind", "trans_win_ind"
                                        , "trans_prev_ind", "trans_prev_numyear"
                                        , "trans_val_2005"
                                        , "trans_build_age"
                                        , "bathroomcnt", "bedroomcnt"
                                        , "finishedsquarefeet", "lotsizesquarefeet", "finished_lot_ratio"
                                        , "story_single_ind", "story_mult_ind"
                                        , "view_type0", "view_type1", "view_type2", "view_type3", "view_type4", "view_type5", "view_type6"
                                        , "res_ratio", "bus_ratio", "oth_ratio", "tot_ratio"
                                        , "median_list_price", "median_value_per_sqft"
                                        , "emp", "qp1", "ap", "est"
                      ))

## ########################################## ##
## PREPARA DAT FOR MODELING
## ########################################## ##
## spliting data
set.seed(1234)
## Mute the missing value
print("* Data Processing for Modeling...")
## train_df <- subset(ds, validation_idx == 0)
## valid_df <- subset(ds, validation_idx == 1)
print("* Imputing missing value")
ds.imputed       <- rfImpute(transvalue_log ~ ., data=ds[, c(config$model2$reponse, config$model2$predictor)])
ds.imputed.scale <- ds.imputed
ds.imputed.scale[, config$model2$predictor[-c(1:4)]] <- scale(ds.imputed[, config$model2$predictor[-c(1:4)]], center = TRUE, scale = TRUE)
ds.imputed$propertyid       <- ds$propertyid
ds.imputed$validation_idx   <- ds$validation_idx
ds.imputed.scale$propertyid     <- ds$propertyid
ds.imputed.scale$validation_idx <- ds$validation_idx

train_ds <- subset(ds.imputed, validation_idx == 0)
valid_ds <- subset(ds.imputed, validation_idx == 1)
train_ds.scale <- subset(ds.imputed.scale, validation_idx == 0)
valid_ds.scale <- subset(ds.imputed.scale, validation_idx == 1)

rm(list = c("TRAIN", "VALID", "zipcode", "sup", "ds", "cpi", "ds.imputed", "ds.imputed.scale", "temp"))
## >>>>>>>>>>>>>>>>>>>>>>>
## Random Forest
## >>>>>>>>>>>>>>>>>>>>>>>
## input definition
model_selection <- list()
dev.temp        <- list()
## cross-validation setting
db         <- train_ds
ncross     <- 3
train_idx  <- sample(1:ncross, size = nrow(db), replace = TRUE)
response   <- config$model2$reponse
predictor  <- config$model2$predictor
model_name <- "Random Forest" 
## result container
model      <- list()
res_perf   <- list()
res_error  <- list()
res_error  <- list()

## loooping
for(i in 1:ncross){
  test_id   <- c(1:ncross)[i]
  train.x   <- db[!(train_idx %in% test_id), predictor]
  train.y   <- db[!(train_idx %in% test_id), response ]
  test.x    <- db[  train_idx %in% test_id,  predictor]
  test.y    <- db[  train_idx %in% test_id,  response ]
  ## train model...
  print(paste("training", i, "th", model_name, "model.."))
  res.model <- randomForest(x = train.x, train.y, ntree = 1000, mtry = 4)
  ## testing
  print(paste("test..."))
  pred_val  <- predict(res.model, newdata = test.x)
  pred_val  <- as.data.frame(cbind(test.y, pred_val))
  colnames(pred_val)  <- c("transvalue_log", "pred_val")
  error               <- apply(pred_val, MARGIN = 1, predError)
  error               <- as.data.frame(t(error))
  colnames(error)     <- c("error_dev", "error_abs", "error_pct")
  res_error           <- c(res_error, error$error_dev)
  res_perf[[i]]       <- reportPerf(error_df = error)
  model[[i]]          <- res.model 
}
model_selection$model    <- model
model_selection$rf_error <- res_error
model_selection$perf     <- unlist(res_perf)
rm(list = c("i", "test_id", "train.x", "train.y", "test.x", "test.y", "res.model", "pred_val", "error", "res_error", "model_name"))

## >>>>>>>>>>>>>>>>>>>>>>>
## glmboosting
## >>>>>>>>>>>>>>>>>>>>>>>
## input definition
db         <- train_ds.scale
model_name <- "Boosting: General Linear Regression" 
## cross-validation setting
res_error  <- c()
## loooping
for(i in 1:ncross){
  test_id   <- c(1:ncross)[i]
  train.x   <- db[!(train_idx %in% test_id), predictor]
  train.y   <- db[!(train_idx %in% test_id), response ]
  test.x    <- db[  train_idx %in% test_id,  predictor]
  test.y    <- db[  train_idx %in% test_id,  response ]
  ## train model...
  print(paste("training", i, "th", model_name, "model.."))
  ctrl <- boost_control(mstop = 2000)
  res.model <- glmboost(train.y ~ ., data = as.data.frame(cbind(train.y, train.x))
                        , center = TRUE, control = ctrl )
  # mstop(aic <- AIC(res.model))
  #res.model <- stepAIC(res.model, direction="both")
  ## testing
  print(paste("test..."))
  pred_val  <- predict(res.model, newdata = test.x)
  pred_val  <- as.data.frame(cbind(test.y, pred_val))
  colnames(pred_val)  <- c("transvalue_log", "pred_val")
  error               <- apply(pred_val, MARGIN = 1, predError)
  error               <- as.data.frame(t(error))
  colnames(error)     <- c("error_dev", "error_abs", "error_pct")
  res_error           <- c(res_error, error$error_dev)
  res_perf[[i]]       <- reportPerf(error_df = error)
  model[[i]]          <- res.model 
}
model_selection$glmboost_model <- model
model_selection$glmboost_error <- res_error
model_selection$glmboost_perf  <- unlist(res_perf)
rm(list = c("i", "test_id", "train.x", "train.y", "test.x", "test.y", "res.model", "pred_val", "error", "res_error", "model_name"))


## >>>>>>>>>>>>>>>>>>>>>>>
## linear model
## >>>>>>>>>>>>>>>>>>>>>>>
## input definition
db         <- train_ds
model_name <- "Linear Regression" 
## result container
res_error  <- list()
## loooping
for(i in 1:ncross){
  test_id   <- c(1:ncross)[i]
  train.x   <- db[!(train_idx %in% test_id), predictor]
  train.y   <- db[!(train_idx %in% test_id), response ]
  test.x    <- db[  train_idx %in% test_id,  predictor]
  test.y    <- db[  train_idx %in% test_id,  response ]
  ## train model...
  print(paste("training", i, "th", model_name, "model.."))
  res.model <- lm(train.y ~ ., data = as.data.frame(cbind(train.y, train.x)))
  res.model <- stepAIC(res.model, direction="both")
  ## testing
  print(paste("test..."))
  pred_val  <- predict(res.model, newdata = test.x)
  pred_val  <- as.data.frame(cbind(test.y, pred_val))
  colnames(pred_val)  <- c("transvalue_log", "pred_val")
  error               <- apply(pred_val, MARGIN = 1, predError)
  error               <- as.data.frame(t(error))
  colnames(error)     <- c("error_dev", "error_abs", "error_pct")
  res_error           <- c(res_error, error$error_dev)
  res_perf[[i]]       <- reportPerf(error_df = error)
  model[[i]]          <- res.model 
}
model_selection$lm_model <- model
model_selection$lm_error <- res_error
model_selection$lm_perf  <- unlist(res_perf)
rm(list = c("i", "test_id", "train.x", "train.y", "test.x", "test.y", "res.model", "pred_val", "error", "res_error", "model_name"))

## >>>>>>>>>>>>>>>>>>>>>>>
## SVM
## >>>>>>>>>>>>>>>>>>>>>>>
## input definition
db         <- train_ds.scale
model_name <- "SVM(kernel:linear)" 
## cross-validation setting
## result container
res_error  <- c()
## loooping
for(i in 1:ncross){
  test_id   <- c(1:ncross)[i]
  train.x   <- db[!(train_idx %in% test_id), predictor]
  train.y   <- db[!(train_idx %in% test_id), response ]
  test.x    <- db[  train_idx %in% test_id,  predictor]
  test.y    <- db[  train_idx %in% test_id,  response ]
  ## train model...
  print(paste("training", i, "th", model_name, "model.."))
  res.model <- svm(x = train.x, y = train.y, 
                   , gamma = 1
                   , scale = TRUE
                   , type = "eps"
                   , kernel = "linear")
  ## testing
  print(paste("test..."))
  pred_val  <- predict(res.model, newdata = test.x)
  pred_val  <- as.data.frame(cbind(test.y, pred_val))
  colnames(pred_val)  <- c("transvalue_log", "pred_val")
  error               <- apply(pred_val, MARGIN = 1, predError)
  error               <- as.data.frame(t(error))
  colnames(error)     <- c("error_dev", "error_abs", "error_pct")
  res_error           <- c(res_error, error$error_dev)
  res_perf[[i]]       <- reportPerf(error_df = error)
  model[[i]]          <- res.model 
}
model_selection$svm_model <- model
model_selection$svm_error <- res_error
model_selection$svm_perf  <- unlist(res_perf)
rm(list = c("i", "test_id", "train.x", "train.y", "test.x", "test.y", "res.model", "pred_val", "error", "res_error", "model_name"))
rm(list = c("train.x", "train.y", "ncross", "predictor", "response"))

## ###################################### ##
## SUMMARIZE CROSS-VALIDATION PERFORMANCE ##
## ###################################### ##
report           <- as.data.frame(matrix(NA, nrow= 3 * 4, ncol=1 + 4))
colnames(report) <- c("model", "error_pct_median", "within_5pct", "withtin10_pct", "within_20pct")
report$model     <- c(paste("RandomForest", 1:3, sep="_")
                      , paste("glmboost", 1:3, sep="_")
                      , paste("Linear Regression", 1:3, sep="_")
                      , paste("SVM", 1:3, sep="_"))
## Populate the performance information
report[1, -1]  <- model_selection$perf[1:4]
report[2, -1]  <- model_selection$perf[5:8]
report[3, -1]  <- model_selection$perf[9:12]
report[4, -1]  <- model_selection$glmboost_perf[1:4]
report[5, -1]  <- model_selection$glmboost_perf[5:8]
report[6, -1]  <- model_selection$glmboost_perf[9:12]
report[7, -1]  <- model_selection$lm_perf[1:4]
report[8, -1]  <- model_selection$lm_perf[5:8]
report[9, -1]  <- model_selection$lm_perf[9:12]
report[10, -1] <- model_selection$svm_perf[1:4]
report[11, -1] <- model_selection$svm_perf[5:8]
report[12, -1] <- model_selection$svm_perf[9:12]
write.table(report, file=getDataPath("CROSS_VALID_RESULTS.csv", dir=dir$output), sep = ",", col.names = TRUE, row.names = FALSE)

## ###################################### ##
## ERROR ANALYSIS -------------------------
## ###################################### ##
error_df <- as.data.frame((cbind("rf_error_dev"         = unlist(model_selection$rf_error)
                                 , "glmboost_error_dev" = unlist(model_selection$glmboost_error)
                                 , "lm_error_dev"       = unlist(model_selection$lm_error)
                                 , "svm_error_dev"      = unlist(model_selection$svm_error)
                                 )))
error_df$transvalue_log <- c(db$transvalue_log[train_idx == 1]
                             , db$transvalue_log[train_idx == 2]
                             , db$transvalue_log[train_idx == 3])
error_df_long <- melt(data = error_df, id.vars = c("transvalue_log"))
error_df_long <- subset(error_df_long, value < quantile(error_df_long$value, .99))
error_df_long <- subset(error_df_long, value > quantile(error_df_long$value, .01))
error_cor_mtx <- cor(error_df[, -5])
print(" ********************************************* ")
print("     ERROR CORRELATION MATRIX                  ")
print(error_cor_mtx)
print(" ********************************************* ")
write.table(x=error_cor_mtx, getDataPath("ERROR_COR_MTX.csv",    dir=dir$output), sep = ",", col.names = TRUE, row.names = FALSE)
write.table(x=error_df,      getDataPath("CROSSVALID_ERROR.csv", dir=dir$output), sep = ",", col.names = TRUE, row.names = FALSE)

print(" * Ploting scatterplots of Error vs. Log(Trans_value) per Model ")
img$error_plot <- ggplot(data = error_df_long
                         , aes(x = transvalue_log, y = value, color = variable)) + 
                  geom_point(alpha = .4, size = 2.5)                               + 
                  geom_smooth(method=lm)                                         +
                  xlab("log(Trans Value)")                                       +
                  ylab("error")
ggsave(filename=getDataPath(filename="error_scatterplot.png", dir=dir$output), 
       , plot=img$error_plot, dpi = 300, height = 10, width = 13)

## ###################################### ##
## FIT BLENDING COEFFICIENTS
## ###################################### ##
# test blending strategy
temp_4model    <- apply(error_df[, 1:4],      MARGIN = 1, mean) / (10 ^ error_df$transvalue_log)
temp_3model    <- apply(error_df[, 1:3],      MARGIN = 1, mean) / (10 ^ error_df$transvalue_log)
temp_3model_v1 <- apply(error_df[, c(1,2,4)], MARGIN = 1, function(x) sum(x * c(4/10, 3/10, 3/10)) ) / (10 ^ error_df$transvalue_log)
temp_3model_v2 <- apply(error_df[, 1:3],      MARGIN = 1, function(x) sum(x * c(4/10, 3/10, 3/10)) ) / (10 ^ error_df$transvalue_log)

temp.report <- as.data.frame(matrix(NA, nrow=2, ncol=5))

colnames(temp.report) <- c("ensembled_models", "error_pct_median", "within_5pct", "within_10pct", "within_20pct")
temp.report[1,  1]    <- c("rf, glmboost, lm, svm")
temp.report[2,  1]    <- c("rf, glmboost, lm")
temp.report[3,  1]    <- c("rf, glmboost, svm (weights:4,3,3)")
temp.report[4,  1]    <- c("rf, glmboost, lm (weights:4,3,3)")

temp.report[1, -1]    <- c(median(temp_4model)
                           , sum(temp_4model < .05)/length(temp_4model)
                           , sum(temp_4model < .10)/length(temp_4model)
                           , sum(temp_4model < .20)/length(temp_4model))

temp.report[2, -1]    <- c(median(temp_3model)
                           , sum(temp_3model < .05)/length(temp_3model)
                           , sum(temp_3model < .10)/length(temp_3model)
                           , sum(temp_3model < .20)/length(temp_3model))

temp.report[3, -1]    <- c(median(temp_3model_v1)
                           , sum(temp_3model_v1 < .05)/length(temp_3model_v1)
                           , sum(temp_3model_v1 < .10)/length(temp_3model_v1)
                           , sum(temp_3model_v1 < .20)/length(temp_3model_v1))

temp.report[4, -1]    <- c(median(temp_3model_v2)
                           , sum(temp_3model_v2 < .05)/length(temp_3model_v2)
                           , sum(temp_3model_v2 < .10)/length(temp_3model_v2)
                           , sum(temp_3model_v2 < .20)/length(temp_3model_v2))
print(" *********** Ensemble Strategies ************** ")
print(temp.report)
print(" *********************************************** ")
write.table(temp.report, file=getDataPath("ENSEMBLE_RESULTS.csv", dir=dir$output), sep = ",", col.names = TRUE, row.names = FALSE)
rm(list = c("temp_3model", "temp_4model", "temp.report"))

## ###################################### ##
## TRAINING ALL FINAL MODELS
## ###################################### ##
models          <- list()
prediction      <- list()
response        <- config$model2$reponse
predictor       <- config$model2$predictor
print("Fitting final random forest...")
models$rf       <- randomForest(x   = train_ds[, config$model2$predictor]
                                , y = train_ds[, config$model2$reponse]
                                , ntree = 3000
                                , mtry = 4)
print("Fitting final glm boosting...")
models$glmboost <- glmboost(transvalue_log ~ .
                            , data = train_ds[, c(config$model2$reponse, config$model2$predictor)]
                            , center  = TRUE
                            , control =  boost_control(mstop = 3000) ) 
print("Fitting final linear regression with stepwise selection...")
#models$lm <- lm(transvalue_log ~ .,  data = train_ds.scale[, c(config$model2$reponse, config$model2$predictor)])
#models$lm <- stepAIC(models$lm, direction="both")
models$svm <- svm(x   = train_ds.scale[, config$model2$predictor]
                  , y = train_ds.scale[, config$model2$reponse] 
                  , gamma = 1
                  , scale = TRUE
                  , type = "eps"
                  , kernel = "linear")
## ###################### ##
## FINAL PREDICITON -------
## ###################### ##
pred                <- as.data.frame(matrix(NA, nrow=nrow(valid_ds), ncol=8))
colnames(pred)      <- c("propertyid", "true_trans_val", "final_pred", "pred_rf", "pred_boost", "pred_svm", "error", "error_pct")
pred$propertyid     <- valid_ds$propertyid
pred$true_trans_val <- sapply(valid_ds$transvalue_log, function(x) 10^x )
pred$pred_rf        <- predict(object=models$rf,       newdata=valid_ds[, config$model2$predictor])       # not scaled
pred$pred_boost     <- predict(object=models$glmboost, newdata=valid_ds[, config$model2$predictor])       # not scaled
pred$pred_svm        <- predict(object=models$svm,       newdata=valid_ds.scale[, config$model2$predictor]) # scaled
pred$final_pred     <- apply(pred[, c("pred_rf", "pred_boost", "pred_svm")], MARGIN=1, function(x) sum(x * c(4/10, 3/10, 3/10)) )
pred$pred_rf        <- sapply(pred$pred_rf,    function(x) 10^x )
pred$pred_boost     <- sapply(pred$pred_boost, function(x) 10^x )
pred$pred_svm       <- sapply(pred$pred_svm,   function(x) 10^x )
pred$final_pred     <- sapply(pred$final_pred, function(x) 10^x )
pred$error          <- pred$final_pred - pred$true_trans_val
pred$error_pct      <- pred$error / pred$true_trans_val

temp.report           <- as.data.frame(matrix(NA, ncol = 5, nrow = 1))
colnames(temp.report) <- c("ensembled_models", "abs_error_median", "within_5pct", "within_10pct", "within_20pct")
temp.report$ensembled_models <- c("RF, glmboost, lm (weights: 4, 3, 3)")
temp.report$abs_error_median <- median(x=abs(pred$error))
temp.report$within_5pct      <- sum(pred$error_pct < .05) / nrow(pred)
temp.report$within_10pct     <- sum(pred$error_pct < .10) / nrow(pred)
temp.report$within_20pct     <- sum(pred$error_pct < .20) / nrow(pred)
print(" *********** Performance of Final Model on Validation Data ************** ")
print(temp.report)
print(" ************************************************************************ ")
write.table(x=pred,        file=getDataPath("VALID_PREDICTION.csv",  dir=dir$output), sep = ",", col.names = TRUE, row.names = FALSE)
write.table(x=temp.report, file=getDataPath("VALID_PERF_REPORT.csv", dir=dir$output), sep = ",", col.names = TRUE, row.names = FALSE)
