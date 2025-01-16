########################################################
#
# This file provides how to obtain Out-of-sample R2
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

################
# long-short 
################

# Load realized returns & forecasts
# Realized excess returns
actual = read.csv('./Forecast/actual.csv')

# Convert date variable to date format
actual$date = as.Date(actual$date)

# Benchmark forecasts
forecastPm = read.csv('./Forecast/forecastPm.csv')

# Convert date variable to date format
forecastPm$date = as.Date(forecastPm$date)

# Storage list object for forecasts

forecastAll = rep(list(NULL),
                  times = length(h))

names(forecastAll) = names(h)

# Iterate over horizons

for ( i in 1:length(h) ){

  # Forecasts based on anomaly returns
  iForecastAll = read.csv(paste0('./Forecast/forecastLongShort',
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
            paste0('./Table/tableOosStatLongShort',
                   names(h)[ i ],
                   'All.csv'))

}


#######################
# long and short leg 
#######################

# Anomaly portfolio legs
leg = c('Long',
        'Short')

# Iterate over legs

for ( i in 1:length(leg) ){
  
  # Anomaly portfolio leg
  nameLeg = leg[ i ]
  
  # Load realized returns & forecasts
  # Realized excess returns
  actual = read.csv('./Forecast/actual.csv')
  
  # Convert date variable to date format
  actual$date = as.Date(actual$date)
  
  # One-month horizon
  actual = actual[ , 1:2 ]
  
  # Benchmark forecast
  forecastPm = read.csv('./Forecast/forecastPm.csv')
  
  # Convert date variable to date format
  forecastPm$date = as.Date(forecastPm$date)
  
  # One-month horizon
  forecastPm = forecastPm[ , 1:2 ]
  
  # Forecasts based on anomaly returns
  forecastAll = read.csv(paste0('./Forecast/forecast',
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
            paste0('./Table/tableOosStat',
                   nameLeg,
                   'Horizon01All.csv'))
}  
  
  
