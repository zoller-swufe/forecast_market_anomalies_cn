########################################################
#
# This file provides how to calculate Utility gain and Sharpe ratio using alternative anomaly set 
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

# Anomaly portfolio legs
leg = c('LongShort',
        'Long',
        'Short')

for ( i in 1:length(leg) ){
  
  # Anomaly portfolio leg
  nameLeg = leg[ i ]
  
  # Coefficient of relative risk aversion
  cra = 3
  
  # Window size for sample variance
  window = 60
  
  # Portfolio weight bounds
  bound = c(-1, 2)
  
  ###########
  # Load data
  ###########
  
  # Market excess return
  dataReturn = read.xlsx('./Data/dataMarketRx.xlsx')
  
  # Convert date variable to date format
  dataReturn$date = as.Date(as.character(dataReturn$date),
                            format = '%Y%m%d')
  
  # Vector of market excess returns (decimal return)
  rx = dataReturn[ , -1 ]
  
  # Forecasts based on long-short anomaly returns
  dataForecastAnomaly = read.csv(paste0('./Forecast/forecast',nameLeg,'Horizon01All_alt.csv'))
  
  # Drop date variable & convert to decimal return
  forecastAnomaly = dataForecastAnomaly[ , -1 ]/100
  
  ##############################
  # Construct optimal portfolios
  ##############################
  
  # Number of out-of-sample periods
  p = nrow(forecastAnomaly)
  
  # Date vector for forecast evaluation period
  date = dataReturn$date[ -(1:(r+pHold)) ]
  
  # Storage vector for benchmark portfolio weights
  wPm = matrix(NA, p, 1)
  
  # Storage vector for benchmark portfolio excess return
  rxPm = matrix(NA, p, 1)
  
  # Storage matrix for competing portfolio weights
  
  wForecastAnomaly = matrix(NA, p, ncol(forecastAnomaly))
  
  colnames(wForecastAnomaly) = names(forecastAnomaly)
  
  # Storage matrix for competing portfolios excess returns
  
  rxForecastAnomaly = matrix(NA, p, ncol(forecastAnomaly))
  
  colnames(rxForecastAnomaly) = names(forecastAnomaly)
  
  # Iterate over out-of-sample periods
  
  for ( s in 1:p ){
    
    cat(sprintf('%s\n', date[ s ]))
    
    # Rolling window volatility estimate
    sSigmaHat = sd(rx[ (r+pHold+(s-1)-(window-1)):(r+pHold+(s-1)) ])
    
    # Benchmark forecast
    sForecastPm = mean(rx[ 1:(r+pHold+(s-1)) ])
    
    # Optimal equity allocation - benchmark forecast
    wPm[ s ] = sForecastPm/(cra*sSigmaHat^2)
    
    # Check lower bound
    
    if ( wPm[ s ] < bound[ 1 ] ){
      
      # Truncate from below if needed
      wPm[ s ] = bound[ 1 ]
      
      # Check upper bound
      
    } else if (wPm[ s ] > bound[ 2 ] ){
      
      # Truncate from above if needed
      wPm[ s ] = bound[ 2 ]
      
    }
    
    # Realized portfolio excess return - benchmark forecast
    rxPm[ s ] = wPm[ s ]*rx[ r+pHold+s ]
    
    # Iterate over forecasts
    
    for ( i in 1:ncol(wForecastAnomaly) ){
      
      # Optimal equity allocation - competing forecast
      wForecastAnomaly[ s , i ] = forecastAnomaly[ s , i ]/(cra*sSigmaHat^2)
      
      # Check lower bound
      
      if ( wForecastAnomaly[ s , i ] < bound[ 1 ] ){
        
        # Truncate from below if needed
        wForecastAnomaly[ s , i ] = bound[ 1 ]
        
        # Check upper bound
        
      } else if (wForecastAnomaly[ s , i ] > bound[ 2 ] ){
        
        # Truncate from above if needed
        wForecastAnomaly[ s , i ] = bound[ 2 ]
        
      }
      
      # Realized portfolio excess return - competing forecast
      rxForecastAnomaly[ s , i ] = wForecastAnomaly[ s , i ]*rx[ r+pHold+s ]
      
    }
    
  }
  
  # Market excess return observations for forecast evaluation period
  rx = rx[ -(1:(r+pHold)) ]
  
  # Collect portfolio excess returns
  rxPort = data.frame(date,
                      rx,
                      rxPm,
                      rxForecastAnomaly)
  
  # Clean up variable names
  names(rxPort) = c('date',
                    'Mkt',
                    'Pm',
                    colnames(forecastAnomaly))
  
  # Save portfolio excess returns
  write.csv(rxPort,
            paste0('./Table/tablePortfolioRx',nameLeg,'All_alt.csv'),
            row.names = FALSE)
  
  ###############################
  # Analyze portfolio performance
  ###############################
  
  # Storage matrix for average utility gains
  
  deltaAnomaly = matrix(NA, ncol(rxForecastAnomaly), 1)
  
  rownames(deltaAnomaly) = colnames(rxForecastAnomaly)
  
  colnames(deltaAnomaly) = c('Full')
  
  # Iterate over forecasts
  
  for ( i in 1:nrow(deltaAnomaly) ){
    
    # Average utility gain - full sample
    deltaAnomaly[ i , 'Full' ] =
      mean(rxForecastAnomaly[ , i ]) - 0.5*cra*var(rxForecastAnomaly[ , i ]) -
      (mean(rxPm) - 0.5*cra*var(rxPm))
  }
  
  # Average utility gains in annualized percent
  deltaAnomaly = 1200*deltaAnomaly
  
  # Convert to table object
  deltaAnomaly = as.table(round(deltaAnomaly, 2))
  
  # Save average utility gains
  write.csv(deltaAnomaly,
            paste0('./Table/tablePortfolioDelta',nameLeg,'All_alt.csv'))
  
  # Sharpe ratio
  sharpe = c(mean(rxPm)/sd(rx),
             apply(rxForecastAnomaly, 2, mean)/apply(rxForecastAnomaly, 2, sd))
  
  # Annualized Sharpe ratio
  sharpe = sqrt(12)*sharpe
  
  # Convert to table object
  
  sharpe = as.table(round(sharpe, 2))
  
  names(sharpe) = c('Pm',
                    colnames(rxForecastAnomaly))
  
  # Save Sharpe ratios
  write.csv(sharpe,
            paste0('./Table/tablePortfolioSharpe',nameLeg,'All_alt.csv'),
            row.names = FALSE)

}
