###################################################################################################################
#
# This file provides how to obtain Out-of-sample R2 and Utility gain in different arbitrage risk subsamples
#
###################################################################################################################
rm(list=ls())

source('functions.R')

# Anomaly portfolio legs
leg = c('LongShort',
        'Long',
        'Short')

for ( i in 1:length(leg) ){
  
  # Anomaly portfolio leg
  nameLeg = leg[ i ]
  subvar <- 'ivol'
  
  # Realized excess returns
  actual = read.csv('./Forecast/actual.csv')
  
  # Convert date variable to date format
  actual$date = as.Date(actual$date)
  
  # One-month horizon
  actual = actual[ , 1:2 ]
  
  # Benchmark forecast
  forecastPm = read.csv('./Forecast/forecastPm.csv')
  
  #All forecast
  forecastAll = read.csv(paste0('./Forecast/forecast',nameLeg,'Horizon01All.csv'))
  
  # Convert date variable to date format
  forecastPm$date = as.Date(forecastPm$date)
  
  # One-month horizon
  forecastPm = forecastPm[ , 1:2 ]
  
  # Load subgroup identifiers
  if (subvar=='ivol')
  {
    ivol = read.xlsx('./Data/ivch3_v.xlsx')
    
    index_high <- vector()
    index_low <- vector()
    for (ii in 1:96) 
    {
      if(ivol[ii,3]==1) 
      {
        index_high <- append(index_high, ii)
      }
      if(ivol[ii,3]==0) 
      {
        index_low <- append(index_low, ii)
      }    
    }
  }
  
  # Forecasting strategies
  nameForecast = c('Ols',
                   'Enet',
                   'Combine',
                   'Avg',
                   'Pc',
                   'Pls')
  
  # Storage matrix for out-of-sample results (subsamples)
  
  statSub = matrix(NA, length(nameForecast), 4)
  
  rownames(statSub) = nameForecast
  
  colnames(statSub) = c('r2osSub1',
                        'cwSub1',
                        'r2osSub2',
                        'cwSub2')
  
  # Iterate over forecasts (one-month horizon)
  
  for ( i in 1:length(nameForecast) ){
    
    # Collect realized value & forecasts (first subsample)
    iDataSub1 = cbind(actual[ index_high , 2 ],
                      forecastPm[ index_high , 2 ],
                      forecastAll[index_high , i+1 ])
    
    # Collect realized value & forecasts (second subsample)
    iDataSub2 = cbind(actual[ index_low , 2 ],
                      forecastPm[ index_low , 2 ],
                      forecastAll[ index_low , i+1 ])
    
    # Evaluate forecasts (first subsample)
    iResultSub1 = computeR2os(actual = iDataSub1[ , 1 ],
                              f1 = iDataSub1[ , 2 ],
                              f2 = iDataSub1[ , 3 ],
                              h = h[ 1 ])
    
    # Evaluate forecasts (second subsample)
    iResultSub2 = computeR2os(actual = iDataSub2[ , 1 ],
                              f1 = iDataSub2[ , 2 ],
                              f2 = iDataSub2[ , 3 ],
                              h = h[ 1 ])
    
    # Store results (first subsample)
    statSub[ i , 1:2 ] = c(iResultSub1$r2os,
                           iResultSub1$cw)
    
    # Store results (first subsample)
    statSub[ i , 3:4 ] = c(iResultSub2$r2os,
                           iResultSub2$cw)
    
  }
  
  # Convert to table object
  statSub = as.table(round(statSub, 2))
  
  # Save table
  write.csv(statSub, paste0('./Table/',nameLeg,'subsampleR2.csv'))
  
  
  ###################
  ###utility gains 
  ###################
  
  # Coefficient of relative risk aversion
  cra = 3
  
  # Window size for sample variance
  window = 60
  
  # Portfolio weight bounds
  bound = c(-1, 2)
  
  # Market excess return
  dataReturn = read.xlsx('./Data/dataMarketRx.xlsx')
  
  # Convert date variable to date format
  dataReturn$date = as.Date(as.character(dataReturn$date),
                            format = '%Y%m%d')
  
  # Vector of market excess returns (decimal return)
  rx = dataReturn[ , -1 ]
  
  # Forecasts based on long-short anomaly returns
  dataForecastAnomaly = read.csv(paste0('./Forecast/forecast',nameLeg,'Horizon01All.csv'))
  
  # Drop date variable & convert to decimal return
  forecastAnomaly = dataForecastAnomaly[ , -1 ]/100
  
  
  # Number of initial in-sample observations
  r = 120  # 120
  
  # Number of holdout out-of-sample observations
  pHold = 48
  
  ### Construct optimal portfolios
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
  
  
  # Storage matrix for average utility gains
  
  deltaAnomaly = matrix(NA, ncol(rxForecastAnomaly), 3)
  
  rownames(deltaAnomaly) = colnames(rxForecastAnomaly)
  
  colnames(deltaAnomaly) = c('Full',
                             'Sub1',
                             'Sub2')
  
  # Iterate over forecasts
  
  for ( i in 1:nrow(deltaAnomaly) ){
    
    # Average utility gain - full sample
    deltaAnomaly[ i , 'Full' ] =
      mean(rxForecastAnomaly[ , i ]) - 0.5*cra*var(rxForecastAnomaly[ , i ]) -
      (mean(rxPm) - 0.5*cra*var(rxPm))
    
    # Average utility gains - first subsample
    deltaAnomaly[ i , 'Sub1' ] =
      mean(rxForecastAnomaly[ index_high , i ]) -
      0.5*cra*var(rxForecastAnomaly[ index_high, i ]) -
      (mean(rxPm[ index_high ]) - 0.5*cra*var(rxPm[ index_high ]))
    
    # Average utility gain - second subsample
    deltaAnomaly[ i , 'Sub2' ] =
      mean(rxForecastAnomaly[ index_low , i ]) -
      0.5*cra*var(rxForecastAnomaly[ index_low, i ]) -
      (mean(rxPm[ index_low ]) - 0.5*cra*var(rxPm[ index_low ]))
    
  }
  
  # Average utility gains in annualized percent
  deltaAnomaly = 1200*deltaAnomaly
  
  # Convert to table object
  deltaAnomaly = as.table(round(deltaAnomaly, 2))
  
  # Save average utility gains
  write.csv(deltaAnomaly, paste0('./Table/',nameLeg,'subsampleutilitygain.csv'))
  
  
  
  # Sharpe ratio
  sharpe = c(mean(rxPm)/sd(rx),
             apply(rxForecastAnomaly, 2, mean)/apply(rxForecastAnomaly, 2, sd))
  
  sharpe_high = c(mean(rxPm[index_high])/sd(rx[index_high]),
                  apply(rxForecastAnomaly[index_high,], 2, mean)/apply(rxForecastAnomaly[index_high,], 2, sd))
  
  sharpe_low = c(mean(rxPm[index_low])/sd(rx[index_low]),
                 apply(rxForecastAnomaly[index_low,], 2, mean)/apply(rxForecastAnomaly[index_low,], 2, sd))
  
  
  # Annualized Sharpe ratio
  sharpe = sqrt(12)*sharpe
  
  sharpe_high = sqrt(12)*sharpe_high
  
  sharpe_low = sqrt(12)*sharpe_low
  
  # Convert to table object
  
  sharpe = as.table(round(sharpe, 2))
  
  names(sharpe) = c('Pm',
                    colnames(rxForecastAnomaly))
  
  sharpe_high = as.table(round(sharpe_high, 2))
  
  names(sharpe_high) = c('Pm',
                         colnames(rxForecastAnomaly))
  
  sharpe_low = as.table(round(sharpe_low, 2))
  
  names(sharpe_low) = c('Pm',
                        colnames(rxForecastAnomaly))
  
  
  Sharpeall <- rbind(sharpe, sharpe_high, sharpe_low)
  
  # Save Sharpe ratios
  write.csv(Sharpeall, paste0('./Table/',nameLeg,'subsampleSharpe.csv'))
  
  
}






