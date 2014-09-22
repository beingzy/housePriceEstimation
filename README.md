# House Pricing Modeling
    
![house_loc_map](https://github.com/beingzy/housePriceEstimation/blob/master/output/house_loc_map.png)

 > ### Data Manipulation:

 * Variable Transformation:
  1. **transdate**: **trans_spr_ind**, **trans_sum_ind**, **trans_fal_ind** and **trans_win_ind** are created to model the seasonal effects on housing market;
  2. **tranddate**, **builtyear**: **trans_build_age** indicates the number of years since the building being constructed;
  3. **transdate_previous**: **trans_prev_ind** indicates if the house had been traded before if none missing value is present;
  4. **transdate**, **transdate_previous**: **trans_prev_numyear** (= **transdate** - **transdate_previous**) is used to calculate the future value of past transaction value to account for inflation effects;
  5. **transvalue_previous**: **trans_val_2005** is calculate the future value in 2005 of previous with annual CPI data and **trans_prev_numyear**
  6. **finished_lot_ratio**: (**finishedsquarefeet** / **lotsizesquarefeet**) is computed with considering that it could provide some aspects information about the house type and neighbourhood;
  7. **storycnt**: **story_single_ind** and **story_mult_ind**, dummy variables, are created to tell if there are multiple storis in the unit. Replacing a multiple level variable with a set of dummy variables can eliminate the underlying assumption about the cardinality effect;
  8. **viewtype**: a set of 8 dummay variable **view_type[0-8]**
  9. **full_censustract**: concatenate **530330** and **censustract** 
  10. **latitude**, **longitude**: /1000000, will be used to calculate distance when merging with other data resource, if no unique key(ZIP, censustract) could match, geo location woull be utilized.
  11. **transvalue**:  **transvalue_log** = log(**transvalue**, 10) follows a normal distribution
  12. **trans_val_2005**: **trans_val_2005_log** = log(**trans_val_2005**, 10)
 
* External Data
   1. *ZIP_TRACT_032010.csv*, using 2010 data to offer a approximated information on neighborhood residential unit percentage, commercial unit precentage and etc.
   
   2. *ZILLOW_ZIP_PRICE.csv*, zillow.com developer API is used to query against ZIP to pull out the **median_list_price** and **median_list_price_per_sqft**.

* Missing Value Handling:
   1. `rfImpute()` in *"randomForest"* packages is used to mute the missing value

* Other Data Processing:
   1. `scale()` apply to make data centered and scaled to avoid some convergance issue in `lm()` and improve the fitting.

> ### Modeling

* Model Selection:

  1. *response variable*: `transvalue_log` 
  
  2. *predictor variable*: 
    ```
    "trans_spr_ind", "trans_sum_ind", "trans_fal_ind", "trans_win_ind"
    , "trans_prev_ind", "trans_prev_numyear"
    , "trans_val_2005"
    , "trans_build_age"
    , "bathroomcnt", "bedroomcnt"
    , "finishedsquarefeet", "lotsizesquarefeet", "finished_lot_ratio"
    , "story_single_ind", "story_mult_ind"
    , "view_type0", "view_type1", "view_type2", "view_type3", "view_type4","view_type5", "view_type6"
    , "res_ratio", "bus_ratio", "oth_ratio", "tot_ratio"
    , "median_list_price", "median_value_per_sqft"
    ```
    
  3. *3-fold cross validtion* is employed to do model selection
  
  4. *Random Forest* in `require('randomForest')`
    `randomForest(x = train.x, train.y, ntree = 1000, mtry = 4)`

  5. *General Linear Regression* in `require('mboost')`
     ```
     glmboost(y ~ ., data, center = TRUE, control = boost_control(mstop = 2000) )
     ```
     
  6. *Stepwise Linear Regression* in `lm()` and `require(MASS)`
    ```
    stepAIC(lm(y ~ ., data = as.data.frame(cbind(train.y, train.x))))
    ```
    
  7. *Suport Vector Machine* (kernel = 'linear') in `require('e1071')`
    ```
    svm(x, y, scale = TRUE, type = "eps", kernel = "linear")
    ``` 
  8. *Train-error correlation examination* 

* Model Ensemble
  1. Training *Random Forest*, *glmboost*, *linear regression* and *SVM* with 100% training data;

  2. Ensemble 4 models by averaging predictions genered by 4 individual models


> ### Results Summary

  * **Individual Model Performance** based on 3-fold cross-validation can be found in *output/CROSS_VALID_RESULTS.csv*:
  
  * **The scatterplot of error vs. log(trans value)** is shown as below:

    ![scatterplot_error_tranvallog](https://github.com/beingzy/housePriceEstimation/blob/master/output/error_scatterplot.png)
    
   **Discussion**: Prediction errors for extremely high/low price units are susbtantial than the erros for median price units. This issue could be addressed by learning more cases of extreme prices. Additionally, it would be helpful to increase the accuracy by feeds of different data source, which could privde different information about house price pattern.  

  * **Final prediction model** is an ensembled one by blending RandomForest, GLM Boosting and SVM(linear) with weights (4/10, 3/10, 3/10).

  * **Accuracy of Prediction** for Validation data (error data: *output/VALID_PREDICTION.csv* and results reprot: *VALID_PREF_REPORT.csv* ):

    a. **Median of Absolute Error**:  43,375.00
    
    b. **Percentage within +/- 5%**:  63.42%
    
    c. **Percentage within +/- 10%**: 73.28%
    
    d. **Percentage within +/- 20%**: 84.94%
    

> ### Future Improvement Discussion

#### 1. Current Methodology Refinement
 * **Missing Value Handling**
    In the train data set, there a big proportion of records with missing value lack value in mulitple predictor variables. By populating their records with imputed values, the inclusion of those records could contribute noise more than information. Therefore, alternatively treatment on missing value should be tested as well.

 * **Feature Selection/Extraction**
 Most non-ensembled algorithm is sensentive to the representation of variables and non-informative variables. 
With objective of finding optimistic representation of data, feature selection, eliminating redundant variables, and feature extraction (e.g. PCA, MDS ane etc.), tranforming the presentation of data worth efforts to explore. In addition to potential accuracy increase, the computation cost could be reduced as well.

 * **Ensembling Weigths based on Minimization of Error**
  In current method, weights applied in blending individual models is based on empirical choice and cross-validation results. However, a more sophisticate method can be untilized to make best of each models's superior performance on segments of units. It is to use optimization technology which can find optimistic weights to minimize overall errors. And, with adopting this optimistic weights,  some level of improvement in accuracy is expected to be achieved.

 * **Modeling Human Experts Knowledge**
  Appraisers' specialties could offer insights into determinating factors in house value. And, some special relationships among variables could be leveraged to create new variables.

#### 2. Data Source
Overall, house price is determined by a broad set of factors. In general, those factors can be classified into either of following categories: house quality information (house layout, built age, design, maintenance and etc.), neighborhood information (residents education level, culture and etc.), public infrastructure information (school quality, traffic condition)  and market force information (supply/demand, macro-economic).  The access to some information can be relatively easy to be gained, in comparison with others. With considering limited access to house quality information, the effort is recommended to be concentrated on incorporating a broad set of public information. Thanks to the advance of mobile/web technology, there are a lot of data source can be leveraged. I will discuss some of them, which I think could be helpful to boost accuracy of house price estimation.

 * **Demographic information of residents** (e.g. race, age, income and etc.)

   **Description**: it is assumed the existence of strong correlation between social status and community cultural, which has substantial influence on house price.

   **Source**: census.gov 

 

 * **Merchant information within definite proximity** (e.g. starbucks, whole foods, KFC and etc.)

   **Description**:  a particular merchant does have their own target customer base. And, their footprints represent their knowledge about the neighborhood. Therefore, their footprints are assumed to provide relevant information about customer, residents within a region. Some indicative pattern hidden in their footprints is expected.
In addition to collecting the presence of a selection of merchants, it could worth experimenting the value of mining reviews texts attached to each merchants. Those type review information is available over multiple online channels (e.g. yelp). The difference of wording, phrase, rating distribution attached to different branches of same chain store could reveal uniqueness of residents within different regions.

   **Source**: google map API, starbucks API, yelp API, foursquare API

 

 * **Education Institute information**

   **Description**: access to high quality school is considered as an influential factor in house price

   **Source**: http://nces.ed.gov/

 

 * **Traffic Information**:

   **Description**: the proximity to highway and local road could provide information about convenience of the unit location. And, it is also possible to provide the noise level in neighborhood.

 

 * **House demand/supply information**

   **Description**: new planed house units represent addition to market supply. The employments on city-level could be indicative in market demand.

 

 * **Macro-economic data**

   **Description**: CPI, interest rate, mortgage, house price increase rate, unemployment rate and etc. have provide information on the potential buying power of the entire market.  

#### 3. Algorithm Discussion
However, incorporating the broad spectrum of data raised an issue on transforming features.  The traditional algorithms are sensitive to choice on data transformation. Ensemble method can partially reduce the issues. However, supervised deep learning could be utilized to automatic construct abstract representation of data.  With feeds of more relevant information and their appropriate representation, it is expected to train a model with better accuracy.  However, some features maybe informative in some regions/cities. In elsewhere, it could turned out to be an addition to noise. Considering this, clustering analysis might be useful to categorize the nationwide cities based on their industry concentration, wealth conditions and so on. Based on this clustering effects, we can possible build region-specific model to achieve the flexibility of deployments, monitoring, maintenance of models, in addition to potential increase in accuracy.


=======
housePriceEstimation
====================

> Training Model to estimate hourse price given basic information on real estate perproty
>>>>>>> 17e852584a1a729c8aa6f9187ec4b7ff8a3bde24
