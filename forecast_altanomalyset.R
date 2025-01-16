########################################################
#
# This file provides how to obtain Out-of-sample forecast using alternative anomaly set 
#
########################################################

###############
# Load packages
###############
rm(list=ls())
library(openxlsx)
library(varhandle)
library(glmnet)
library(pls)
library(sandwich)
source('functions.R')

# Forecast horizons
h = c(1)
names(h) = c('Horizon01')

# Number of initial in-sample observations
r = 120  

# Number of holdout out-of-sample observations
pHold = 48

# Data files for anomaly returns
fileData = c('alt_anomaly_LongShort.xlsx',
             'alt_anomaly_Long.xlsx',
             'alt_anomaly_Short.xlsx')

# Portfolio types
type = c('LongShort',
         'Long',
         'Short')

# Market excess return
dataReturn = read.xlsx('./Data/dataMarketRx.xlsx')

# Convert date variable to date format
dataReturn$date = as.Date(as.character(dataReturn$date),
                          format = '%Y%m%d')

# Convert to percent return
dataReturn[ , -1 ] = 100*dataReturn[ , -1 ]

# Extract date vector
date = dataReturn[ , 1 ]

# Anomaly returns
dataAnomaly = read.xlsx('./Data/alt_anomaly_LongShort.xlsx')


# Convert date variable to date format
dataAnomaly$date = as.Date(as.character(dataAnomaly$date),
                           format = '%Y%m%d')

# Convert to percent return
dataAnomaly[ , -1 ] = 100*dataAnomaly[ , -1 ]

# Keep data frame with missing anomaly returns
dataAnomalyMissing = dataAnomaly

# Iterate over months

for ( t in 1:nrow(dataAnomaly) ){
  
  # Check for missing values
  
  if ( sum(is.na(dataAnomaly[ t , ])) > 0 ){
    
    # Fill in missing values with cross-sectional mean
    dataAnomaly[ t , which(is.na(dataAnomaly[ t , ])) ] =
      apply(dataAnomaly[ t , -1 ], 1, mean, na.rm = TRUE)
    
  }
  
}

# Anomaly names
nameAnomaly = names(dataAnomaly)[ -1 ]

# Take care of preliminaries for out-of-sample forecasting
# Forecasting strategies
nameForecast = c('Ols',
                 'Enet',
                 'Combine',
                 'Avg',
                 'Pc',
                 'Pls')

# Target variable
y = as.matrix(dataReturn[ , -1 ])

# Number of out-of-sample observations (including holdout period)
p = length(date) - r

# Storage matrix for realized market excess return

actual = matrix(NA, p, length(h))

colnames(actual) = names(h)


# Storage matrix for benchmark forecast

forecastPm = matrix(NA, p, length(h))

colnames(forecastPm) = names(h)

# Storage list object for univariate forecasts

forecastUniAll = rep(list(matrix(NA, p, length(nameAnomaly))),
                     times = length(h))

names(forecastUniAll) = names(h)

# Storage list object for forecasts based on multiple anomalies

forecastAll = rep(list(matrix(NA, p, length(nameForecast))),
                  times = length(h))

names(forecastAll) = names(h)

# Iterate over forecast horizons

for ( i in 1:length(h) ){
  
  # Anomaly names for columns
  colnames(forecastUniAll[[ i ]]) = nameAnomaly
  
  # Forecast names for columns
  colnames(forecastAll[[ i ]]) = nameForecast
  
}


# Compute out-of-sample market excess return forecasts
# Iterate over out-of-sample periods
for ( s in 1:p ){
  
  cat(sprintf('%s\n', date[ r+s ]))
  
  # Available dependent variable observations
  sy = y[ 1:(r+(s-1)) ]
  
  # Iterate over forecast horizons
  
  for ( i in 1:length(h) ){
    
    # Cumulative multi-period return for horizon h
    siy = computeMultiPeriodRetun(sy, h[ i ])
    
    # Check if past holdout period
    
    if ( s > pHold ){
      
      # Check if multi-period return available
      
      if ( s <= p-(h[ i ]-1)  ){
        
        # Realized market excess return
        actual[ s , i ] = mean(y[ (r+s):(r+s+(h[ i ]-1)) ])
        
      }
      
      # Benchmark forecast
      forecastPm[ s , i ] = mean(siy, na.rm = TRUE)
      
    }
    
    # Available anomaly return observations
    sx = as.matrix(dataAnomaly[ 1:(r+(s-1)) , -1 ])
    
    # Collect market excess return & anomaly returns
    siData = cbind(siy, sx)
    
    # Remove missing observations due to multi-period return
    siData = na.omit(siData)
    
    # Dependent variable vector
    siy = as.matrix(siData[ , 1 ])
    
    # Data matrix
    six = as.matrix(siData[ , -1 ])
    
    # Iterate over anomalies
    
    for ( j in 1:ncol(six) ){
      
      # Fit univariate predictive regression via OLS
      sijFit = lm(siy[ -1 ] ~ six[ -nrow(six) , j ])
      
      # OLS forecast for univariate preditive regression
      forecastUniAll[[ i ]][ s , j ] =
        sum(c(1, sx[ nrow(sx) , j ])*sijFit$coefficients)
      
    }
    
    # Check if past holdout period
    
    if ( s > pHold ){
      
      # Data frame for OLS
      siDataOls = data.frame(siy[ -1 ],
                             six[ -nrow(six) , ])
      
      # Clean up variable names
      colnames(siDataOls) = c('MKT',
                              nameAnomaly)
      
      # Check for linear dependence
      ldVar = attributes(alias(lm(MKT ~ .,
                                  data = siDataOls))$Complete)$dimnames[[ 1 ]]
      
      # Fix linear dependence if needed
      
      if ( length(ldVar) > 0 ){
        
        # Drop problematic predictors
        siOlsIndex = which(!(colnames(siDataOls)[ -1 ] %in% ldVar))
        
        # No need to fix linear dependence
        
      } else {
        
        # Include all predictors
        siOlsIndex = 1:length(nameAnomaly)
        
      }
      
      # Fit multiple predictive regression via OLS
      siFit = lm(siy[ -1] ~ six[ -nrow(six) , siOlsIndex ])
      
      # OLS forecast for predictive multiple regression
      forecastAll[[ i ]][ s , 'Ols' ] =
        sum(c(1, sx[ nrow(sx) , siOlsIndex ])*siFit$coefficients)
      
      # Fit multiple predictive regression via ENet
      siResult = estimateEnetAicc(six[ -nrow(six) , ],
                                  siy[ -1 ],
                                  lb = -Inf)
      
      # ENet forecast for multiple predictive regression
      forecastAll[[ i ]][ s , 'Enet' ] = sum(c(1, sx[ nrow(sx) , ])*siResult$b)
      
      # Simple combination forecast
      forecastAll[[ i ]][ s , 'Combine' ] = mean(forecastUniAll[[ i ]][ s , ])
      
      # Available anomaly return observations with missing values
      sxMissing = as.matrix(dataAnomalyMissing[ 1:(r+(s-1)) , -1 ])
      
      # Vector of cross-sectional averages
      sxAvg = apply(sxMissing, 1, mean, na.rm = TRUE)
      
      # Predictor variable vector for predictor average regression
      sixAvg = sxAvg[ 1:(r+(s-1)-(h[ i ]-1)) ]
      
      # Fit predictor average regression via OLS
      siFit = lm(siy[ -1 ] ~ sixAvg[ -length(sixAvg) ])
      
      # OLS forecast for predictor average regression
      forecastAll[[ i ]][ s , 'Avg' ] =
        sum(c(1, sxAvg[ length(sxAvg) ])*siFit$coefficients)
      
      # Standardize anomaly returns
      sxTilde = scale(sx)
      
      # Data matrix for PC/PLS regressions
      sixTilde = sxTilde[ 1:(r+(s-1)-(h[ i ]-1)) , ]
      
      # Fit principal component predictive regression
      siFitPc = pcr(siy[ -1 ] ~ sixTilde[ -nrow(sixTilde) , ],
                    ncomp = 1,
                    scale = FALSE)
      
      # Principal component predictive regression forecast
      forecastAll[[ i ]][ s , 'Pc' ] =
        predict(siFitPc,
                ncomp = 1,
                newdata = t(sxTilde[ nrow(sxTilde) , ]))
      
      # Fit predictive regression via PLS
      siFitPls = plsr(siy[ -1 ] ~ sixTilde[ -nrow(sixTilde) , ],
                      ncomp = 1,
                      scale = FALSE)
      
      # PLS forecast
      forecastAll[[ i ]][ s , 'Pls' ] =
        predict(siFitPls,
                ncomp = 1,
                newdata = t(sxTilde[ nrow(sxTilde) , ]))
      
    }
    
  }
  
}

# Create data frames & save as CSV files
# Forecast evaluation period
date = date[ -(1:(r+pHold)) ]

# Realized values for forecast evaluation period
actual = data.frame(date,
                    actual[ -(1:pHold) , ])

# Benchmark forecasts for forecast evaluation period
forecastPm = data.frame(date,
                        forecastPm[ -(1:pHold) , ])

# Save realized values
write.csv(actual,
          './Forecast/actual_alt.csv',
          row.names = FALSE)

# Save benchmark forecasts
write.csv(forecastPm,
          './Forecast/forecastPm_alt.csv',
          row.names = FALSE)

# Iterate over forecast horizons

for ( i in 1:length(h) ){
  
  # Predicted values for forecast evaluation period
  iForecast = data.frame(date,
                         forecastAll[[ i ]][ -(1:pHold) , ])
  
  # Save forecasts
  write.csv(iForecast,
            paste0('./Forecast/forecastLongShort',
                   names(h)[ i ],
                   'All_alt.csv'),
            row.names = FALSE)
  
}

# Clear up some memory
rm.all.but(keep = c('h', 'r', 'pHold'),
           keep_functions = TRUE)




# Anomaly portfolio legs
leg = c('Long',
        'Short')

# Iterate over legs

for ( i in 1:length(leg) ){

  # Anomaly portfolio leg
  nameLeg = leg[ i ]

  # Construct market excess return forecasts
  
  # Market excess return
  dataReturn = read.xlsx('./Data/dataMarketRx.xlsx')
  
  # Convert date variable to date format
  dataReturn$date = as.Date(as.character(dataReturn$date),
                            format = '%Y%m%d')
  
  # Convert to percent return
  dataReturn[ , -1 ] = 100*dataReturn[ , -1 ]
  
  # Extract date vector
  date = dataReturn[ , 1 ]
  
  # Anomaly returns
  dataAnomaly = read.xlsx(paste0('./Data/alt_anomaly_',
                                 nameLeg,
                                 '.xlsx'))
  
  # Convert date variable to date format
  dataAnomaly$date = as.Date(as.character(dataAnomaly[ , 1 ]),
                             format = '%Y%m%d')
  
  # Convert to percent return
  dataAnomaly[ , -1 ] = 100*dataAnomaly[ , -1 ]
  
  # Keep data frame with missing anomaly returns
  dataAnomalyMissing = dataAnomaly
  
  # Iterate over months
  
  for ( t in 1:nrow(dataAnomaly) ){
    
    # Check for missing values
    
    if ( sum(is.na(dataAnomaly[ t , ])) > 0 ){
      
      # Fill in missing values with cross-sectional mean
      dataAnomaly[ t , which(is.na(dataAnomaly[ t , ])) ] =
        apply(dataAnomaly[ t , -1 ], 1, mean, na.rm = TRUE)
      
    }
    
  }
  
  # Anomaly names
  nameAnomaly = names(dataAnomaly)[ -1 ]

  # Forecasting strategies
  nameForecast = c('Ols',
                   'Enet',
                   'Combine',
                   'Avg',
                   'Pc',
                   'Pls')
  
  # Target variable
  y = as.matrix(dataReturn[ , -1 ])
  
  # Number of out-of-sample observations (including holdout period)
  p = length(date) - r
  
  # Storage vector for realized market excess return
  actual = matrix(NA, p, 1)
  
  # Storage vector for benchmark forecast
  forecastPm = matrix(NA, p, 1)
  
  # Storage matrix for univariate forecasts
  
  forecastUniAll = matrix(NA, p, length(nameAnomaly))
  
  colnames(forecastUniAll) = nameAnomaly
  
  # Storage matrix for forecasts based on multiple anomalies
  
  forecastAll = matrix(NA, p, length(nameForecast))
  
  colnames(forecastAll) = nameForecast
  
  # Compute out-of-sample market excess return forecasts
  # Iterate over out-of-sample periods
  
  for ( s in 1:p ){
    
    cat(sprintf('%s\n', date[ r+s ]))
    
    # Available dependent variable observations
    sy = y[ 1:(r+(s-1)) ]
    
    # Check if past holdout period
    
    if ( s > pHold ){
      
      # Realized market excess return
      actual[ s ] = y[ r+s ]
      
      # Benchmark forecast
      forecastPm[ s ] = mean(sy)
      
    }
    
    # Available anomaly return observations
    sx = as.matrix(dataAnomaly[ 1:(r+(s-1)) , -1 ])
    
    # Iterate over anomalies
    
    for ( j in 1:ncol(sx) ){
      
      # Fit univariate predictive regression via OLS
      sjFit = lm(sy[ -1 ] ~ sx[ -nrow(sx) , j ])
      
      # OLS forecast for univariate predictive regression
      forecastUniAll[ s , j ] = sum(c(1, sx[ nrow(sx) , j ])*sjFit$coefficients)
      
    }
    
    # Check if past holdout period
    
    if ( s > pHold ){
      
      # Data frame for OLS
      sDataOls = data.frame(sy[ -1 ],
                            sx[ -nrow(sx) , ])
      
      # Clean up variable names
      colnames(sDataOls) = c('MKT',
                             nameAnomaly)
      
      # Check for linear dependence
      ldVar = attributes(alias(lm(MKT ~ .,
                                  data = sDataOls))$Complete)$dimnames[[ 1 ]]
      
      # Fix linear dependence if necessary
      
      if ( length(ldVar) > 0 ){
        
        # Drop problematic predictors
        sOlsIndex = which(!(colnames(sDataOls)[ -1 ] %in% ldVar))
        
        # No need to fix linear dependence
        
      } else {
        
        # Include all predictors
        sOlsIndex = 1:length(nameAnomaly)
        
      }
      
      # Fit multiple predictive regression via OLS
      sFit = lm(sy[ - 1] ~ sx[ -nrow(sx) , sOlsIndex ])
      
      # OLS forecast for multiple predictive regression
      forecastAll[ s , 'Ols' ] =
        sum(c(1, sx[ nrow(sx) , sOlsIndex ])*sFit$coefficients)
      
      # Fit multiple predictive regression via ENet
      sResult = estimateEnetAicc(sx[ -nrow(sx) , ],
                                 sy[ -1 ],
                                 lb = -Inf)
      
      # ENet forecast for multiple predictive regression
      forecastAll[ s , 'Enet' ] = sum(c(1, sx[ nrow(sx) , ])*sResult$b)
      
      # Simple combination forecast
      forecastAll[ s , 'Combine' ] = mean(forecastUniAll[ s , ])
      
      # Available anomaly return observations with missing values
      sxMissing = as.matrix(dataAnomalyMissing[ 1:(r+(s-1)) , -1 ])
      
      # Vector of cross-sectional averages
      sxAvg = apply(sxMissing, 1, mean, na.rm = TRUE)
      
      # Fit predictor average regression via OLS
      sFit = lm(sy[ -1 ] ~ sxAvg[ -length(sxAvg) ])
      
      # OLS forecast for predictor average regression
      forecastAll[ s , 'Avg' ] =
        sum(c(1, sxAvg[ length(sxAvg) ])*sFit$coefficients)
      
      # Standardize anomaly returns
      sxTilde = scale(sx)
      
      # Fit principal component predictive regression
      sFitPc = pcr(sy[ -1 ] ~ sxTilde[ -nrow(sxTilde) , ],
                   ncomp = 1,
                   scale = FALSE)
      
      # Principal component predictive regression forecast
      forecastAll[ s , 'Pc' ] = predict(sFitPc,
                                        ncomp = 1,
                                        newdata = t(sxTilde[ nrow(sxTilde) , ]))
      
      # Fit predictive regression via PLS
      sFitPls = plsr(sy[ -1 ] ~ sxTilde[ -nrow(sxTilde) , ],
                     ncomp = 1,
                     scale = FALSE)
      
      # PLS forecast
      forecastAll[ s , 'Pls' ] = predict(sFitPls,
                                         ncomp = 1,
                                         newdata = t(sxTilde[ nrow(sxTilde) , ]))
      
    }
    
  }
  
  cat('\n')

  # Create data frames & save as CSV files
  # Forecast evaluation period
  date = date[ -(1:(r+pHold)) ]
  
  # Realized values for forecast evaluation period
  actual = data.frame(date,
                      actual[ -(1:pHold) ])
  
  # Benchmark forecasts for forecast evaluation period
  forecastPm = data.frame(date,
                          forecastPm[ -(1:pHold) ])
  
  # Predicted values for forecast evaluation period
  forecastAll = data.frame(date,
                           forecastAll[ -(1:pHold) , ])
  
  # Save forecasts
  write.csv(forecastAll,
            paste0('./Forecast/forecast',
                   nameLeg,
                   'Horizon01All_alt.csv'),
            row.names = FALSE)
  
  # Clear up some memory
  rm.all.but(keep = c('leg', 'nameLeg', 'r', 'pHold'),
             keep_functions = TRUE)

}

