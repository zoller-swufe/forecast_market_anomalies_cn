########################################################
#
# This file provides how to obtain simulation results -- noise reduction mechanism
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
library(fs)
library(mnormt)

source('functions.R')


out<-matrix(nrow = 100, ncol = 4)
num <- 1

for (sigma_epsilon in c(0.07,0.1))  
{ 
for (delta in seq(0,0.5,0.1))  
{ 
  
  # parameter setting
  set.seed(100)
  
  phi_s <- 0.1
  phi_l <- 0.99
  sigma_ft <- 0.03 
  mean_ft <- 0.00765
  pp<- 100
  t <- 264  
  
  #simulate long-leg and short-leg anomaly portfolio return
  ft<- rnorm(n= t, mean = mean_ft, sd = sigma_ft)
  
  epsilon_l <- matrix(nrow = (t+1), ncol = pp)
  epsilon_s <- matrix(nrow = (t+1), ncol = pp)
  
  for (i in 1:pp)
  {
    epsilon_l[,i] <- rnorm(n= (t+1), mean = 0, sd = sigma_epsilon)
    epsilon_s[,i] <- rnorm(n= (t+1), mean = 0, sd = sigma_epsilon)
  }
  
  ul <- matrix(nrow = (t+1), ncol = pp)
  us <- matrix(nrow = (t+1), ncol = pp)
  
  ul[1,]<- 0
  us[1,]<- 0
  
  for (i in 1:pp)
  { 
    for (tt in 2:(t+1)) 
    {
      ul[tt,i] <- phi_l*ul[(tt-1),i]-abs(epsilon_l[tt,i])
      us[tt,i] <- phi_s*us[(tt-1),i]+abs(epsilon_s[tt,i])
    }
  }
  
  dul <- matrix(nrow = t, ncol = pp)
  dus <- matrix(nrow = t, ncol = pp)
  
  for (i in 1:pp)
  { 
    for (tt in 1:t) 
    {
      dul[tt,i] <- ul[(tt+1),i]-ul[tt,i]
      dus[tt,i] <- us[(tt+1),i]-us[tt,i]
    }
  }
  
  
  rl <- (1+delta/2)*ft + dul
  rs <- (1-delta/2)*ft + dus
  
  
  #simulate long-short anomaly portfolio return
  rls <- rl - rs
  
  #simulate market return
  sum_dul <- matrix(0,nrow = t, ncol = 1)
  sum_dus <- matrix(0,nrow = t, ncol = 1)
  for (i in 1:pp)
  {
    sum_dul<-sum_dul+dul[,i]
    sum_dus<-sum_dus+dus[,i]   
  }
  
  rmt <- ft + 0.5*(sum_dul+sum_dus)/pp 
  
  #combine date and output tables
  CH3 <- read.xlsx('./Data/dataCH3.xlsx')
  
  dataMarketRx<- data.frame(cbind(CH3[1:t,1], rmt))
  colnames(dataMarketRx)[1] <- 'date'
  
  dataLongMissing <- data.frame(cbind(CH3[1:t,1], rl)) 
  colnames(dataLongMissing)[1] <- 'date'
  
  dataShortMissing <- data.frame(cbind(CH3[1:t,1], rs))
  colnames(dataShortMissing)[1] <- 'date'
  
  dataLongShortMissing <- data.frame(cbind(CH3[1:t,1], rls))
  colnames(dataLongShortMissing)[1] <- 'date'
  
  write.xlsx(dataMarketRx,file = "./Simulation/Data/dataMarketRx.xlsx",colnames = TRUE)
  write.xlsx(dataLongMissing,file = "./Simulation/Data/dataLongMissing.xlsx",colnames = TRUE)
  write.xlsx(dataShortMissing,file = "./Simulation/Data/dataShortMissing.xlsx",colnames = TRUE)
  write.xlsx(dataLongShortMissing,file = "./Simulation/Data/dataLongShortMissing.xlsx",colnames = TRUE)
  
  # Compute/evaluate market return forecasts
  # Construct market excess return forecasts - all long-short returns
  
  # Forecast horizons
  h = c(1)
  names(h) = c('Horizon01')
  
  # Number of initial in-sample observations
  r = 120 
  
  # Number of holdout out-of-sample observations
  pHold = 48
  
  # Data files for anomaly returns
  fileData = c('dataLongShortMissing.xlsx',
               'dataLongMissing.xlsx',
               'dataShortMissing.xlsx')
  
  # Portfolio types
  type = c('LongShort',
           'Long',
           'Short')
  
  # Market excess return
  dataReturn = read.xlsx('./Simulation/Data/dataMarketRx.xlsx')
  
  # Convert date variable to date format
  dataReturn$date = as.Date(as.character(dataReturn$date),
                            format = '%Y%m%d')
  
  # Convert to percent return
  dataReturn[ , -1 ] = 100*dataReturn[ , -1 ]
  
  # Extract date vector
  date = dataReturn[ , 1 ]
  
  # Anomaly returns
  dataAnomaly = read.xlsx('./Simulation/Data/dataLongShortMissing.xlsx')
  
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
            './Simulation/Forecast/actual.csv',
            row.names = FALSE)
  
  # Save benchmark forecasts
  write.csv(forecastPm,
            './Simulation/Forecast/forecastPm.csv',
            row.names = FALSE)
  
  # Iterate over forecast horizons
  
  for ( i in 1:length(h) ){
    
    # Predicted values for forecast evaluation period
    iForecast = data.frame(date,
                           forecastAll[[ i ]][ -(1:pHold) , ])
    
    # Save forecasts
    write.csv(iForecast,
              paste0('./Simulation/Forecast/forecastLongShort',
                     names(h)[ i ],
                     'All.csv'),
              row.names = FALSE)
    
  }
  
  # Clear up some memory
  #rm.all.but(keep = c('h', 'r', 'pHold','out','num'),
  #          keep_functions = TRUE)
  
  # Anomaly portfolio legs
  leg = c('Long',
          'Short')
  
  # Iterate over legs
  
  for ( i in 1:length(leg) ){
    
    # Anomaly portfolio leg
    nameLeg = leg[ i ]
    
    # Construct market excess return forecasts
    
    # Market excess return
    dataReturn = read.xlsx('./Simulation/Data/dataMarketRx.xlsx')
    
    # Convert date variable to date format
    dataReturn$date = as.Date(as.character(dataReturn$date),
                              format = '%Y%m%d')
    
    # Convert to percent return
    dataReturn[ , -1 ] = 100*dataReturn[ , -1 ]
    
    # Extract date vector
    date = dataReturn[ , 1 ]
    
    # Anomaly returns
    dataAnomaly = read.xlsx(paste0('./Simulation/Data/data',
                                   nameLeg,
                                   'Missing.xlsx'))
    
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
              paste0('./Simulation/Forecast/forecast',
                     nameLeg,
                     'Horizon01All.csv'),
              row.names = FALSE)
    
    # Clear up some memory
    #rm.all.but(keep = c('leg', 'nameLeg', 'r', 'pHold','h','out','num'),
    #          keep_functions = TRUE)
    
  }
  
  
  
  # evaluate
  # Realized excess returns
  actual = read.csv('./Simulation/Forecast/actual.csv')
  
  # Convert date variable to date format
  actual$date = as.Date(actual$date)
  
  # Benchmark forecasts
  forecastPm = read.csv('./Simulation/Forecast/forecastPm.csv')
  
  # Convert date variable to date format
  forecastPm$date = as.Date(forecastPm$date)
  
  # Storage list object for forecasts
  
  forecastAll = rep(list(NULL),
                    times = length(h))
  
  names(forecastAll) = names(h)
  
  # Iterate over horizons
  
  for ( i in 1:length(h) ){
    
    # Forecasts based on anomaly returns
    iForecastAll = read.csv(paste0('./Simulation/Forecast/forecastLongShort',
                                   names(h)[ i ],
                                   'All.csv'))
    
    # Convert date variable to date format
    iForecastAll$date = as.Date(iForecastAll$date)
    
    # Update list object
    forecastAll[[ i ]] = iForecastAll
    
  }
  
  # Forecasting strategies
  nameForecast = names(forecastAll[[ 1 ]])[ -1 ]
  
  # Evaluate forecasts in terms of MSFE
  # Iterate over horizons
  
  for ( i in 1:length(h) ){
    
    # Storage matrix for out-of-sample results
    
    iStat = matrix(NA, length(nameForecast), 2)
    
    rownames(iStat) = nameForecast
    
    colnames(iStat) = c('r2os',
                        'cw')
    
    # Iterate over forecasts
    
    for ( j in 1:length(nameForecast) ){
      
      # Collect realized value & forecasts
      ijData = cbind(actual[ , i+1 ],
                     forecastPm[ , i+1 ],
                     forecastAll[[ i ]][ , j+1 ])
      
      # Remove missing observations due to multi-period return
      ijData = na.omit(ijData)
      
      # Evaluate forecasts
      ijResult = computeR2os(actual = ijData[ , 1 ],
                             f1 = ijData[ , 2 ],
                             f2 = ijData[ , 3 ],
                             h = h[ i ])
      
      # Store results
      iStat[ j ,  ] = c(ijResult$r2os,
                        ijResult$cw)
      
    }
    
    # Convert to table object
    iStat = as.table(round(iStat, 2))
    
    # Save table
    write.csv(iStat,
              paste0('./Simulation/Table/tableOosStatLongShort',
                     names(h)[ i ],
                     'All.csv'))
    
  }
  
  # long and short leg 
  # Anomaly portfolio legs
  leg = c('Long',
          'Short')
  
  # Iterate over legs
  
  for ( i in 1:length(leg) ){
    
    # Anomaly portfolio leg
    nameLeg = leg[ i ]
    
    # Load realized returns & forecasts
    # Realized excess returns
    actual = read.csv('./Simulation/Forecast/actual.csv')
    
    # Convert date variable to date format
    actual$date = as.Date(actual$date)
    
    # One-month horizon
    actual = actual[ , 1:2 ]
    
    # Benchmark forecast
    forecastPm = read.csv('./Simulation/Forecast/forecastPm.csv')
    
    # Convert date variable to date format
    forecastPm$date = as.Date(forecastPm$date)
    
    # One-month horizon
    forecastPm = forecastPm[ , 1:2 ]
    
    # Forecasts based on anomaly returns
    forecastAll = read.csv(paste0('./Simulation/Forecast/forecast',
                                  nameLeg,
                                  'Horizon01All.csv'))
    
    # Convert date variable to date format
    forecastAll$date = as.Date(forecastAll$date)
    
    # Forecasting strategies
    nameForecast = names(forecastAll)[ -1 ]
    
    
    # Evaluate forecasts in terms of MSFE
    # Storage matrix for out-of-sample results
    
    stat = matrix(NA, length(nameForecast), 2)
    
    rownames(stat) = nameForecast
    
    colnames(stat) = c('r2os',
                       'cw')
    
    # Iterate over forecasts
    
    for ( i in 1:length(nameForecast) ){
      
      # Collect realized value & forecasts
      iData = cbind(actual[ , 2 ],
                    forecastPm[ , 2 ],
                    forecastAll[ , i+1 ])
      
      # Evaluate forecasts
      iResult = computeR2os(actual = iData[ , 1 ],
                            f1 = iData[ , 2 ],
                            f2 = iData[ , 3 ],
                            h = 1)
      
      # Store results
      stat[ i ,  ] = c(iResult$r2os,
                       iResult$cw)
    }
    
    # Convert to table object
    stat = as.table(round(stat, 2))
    
    # Save table
    write.csv(stat,
              paste0('./Simulation/Table/tableOosStat',
                     nameLeg,
                     'Horizon01All.csv'))
  }  
  
  
  longshort <- read.csv("./Simulation/Table/tableOosStatLongShortHorizon01All.csv")
  long <- read.csv("./Simulation/Table/tableOosStatLongHorizon01All.csv")
  short <- read.csv("./Simulation/Table/tableOosStatShortHorizon01All.csv")
  
  data_long <- read.xlsx('./Simulation/Data/dataLongMissing.xlsx')
  data_short <- read.xlsx('./Simulation/Data/dataShortMissing.xlsx')
  data_longshort <- read.xlsx('./Simulation/Data/dataLongShortMissing.xlsx')
  data_mktex <- read.xlsx('./Simulation/Data/dataMarketRx.xlsx')
  
  
  if (sigma_epsilon == 0.1)
  {
    out[num,1] <- 'relatively large mispricing'
  }
  else
  {
    out[num,1] <- 'relatively small mispricing'
  }
  
  out[num+1,1] <- 'delta'
  out[num+2,1] <- delta
  
  out[num:(num+5),2:4] <- as.matrix(longshort)
  
  folders_to_delete <- c('./Simulation/Data/', './Simulation/Forecast/', './Simulation/Table/')
  
  for (folder in folders_to_delete) {
    files <- list.files(folder, full.names = TRUE)
    for (file in files) {
      if (file.exists(file)) {
        file.remove(file)
      }
    }
  }
  
  num <- num+8
}
}


write.xlsx(data.frame(out),file = "./Table/simulation_noisereduction.xlsx",colnames = TRUE)

