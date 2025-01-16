########################################################
#
# This file provides how to calculate slope coefficients 
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
  
  # Market excess return
  dataReturn = read.xlsx('./Data/dataMarketRx.xlsx')
  
  # Convert date variable to date format
  dataReturn$date = as.Date(as.character(dataReturn$date),
                            format = '%Y%m%d')
  
  # Extract date vector
  date = dataReturn$date
  
  # Remove date variable
  dataReturn = dataReturn[ , -1 ]
  
  # Convert to percent return
  dataReturn = 100*dataReturn
  
  # Anomaly returns
  dataAnomaly = read.xlsx(paste0('./Data/data',nameLeg,'Missing.xlsx'))
  
  # Remove date variable
  dataAnomaly = dataAnomaly[ , -1 ]
  
  # Convert to percent return
  dataAnomaly = 100*dataAnomaly
  
  # Keep data frame with missing anomaly returns
  dataAnomalyMissing = dataAnomaly
  
  # Iterate over months
  
  for ( t in 1:nrow(dataAnomaly) ){
    
    # Check for missing values
    
    if ( sum(is.na(dataAnomaly[ t , ])) > 0 ){
      
      # Fill in missing values with cross-sectional mean
      dataAnomaly[ t , which(is.na(dataAnomaly[ t , ])) ] =
        apply(dataAnomaly[ t , ], 1, mean, na.rm = TRUE)
      
    }
    
  }
  
  ############################
  # Take care of preliminaries
  ############################
  
  # Target variable
  y = dataReturn
  
  # Number of out-of-sample observations
  p = length(dataReturn) - (r + pHold)
  
  # Estimation strategies
  nameCoef = c('Avg',
               'Pc',
               'Pls')
  
  # Storage matrix for recursive Avg/PC/PLS coefficients
  
  coef = matrix(NA, p, length(nameCoef))
  
  colnames(coef) = nameCoef
  
  # Storage matrix for recursive standard errors
  
  se = matrix(NA, p, length(nameCoef))
  
  colnames(se) = nameCoef
  
  #########################################
  # Compute recursive coefficient estimates
  #########################################
  
  # Iterate over out-of-sample periods
  
  for ( s in 1:p ){
    
    cat(sprintf('%s\n', date[ r+pHold+s ]))
    
    # Available dependent variable observations (drop first observation)
    sy = y[ 2:(r+pHold+(s-1)) ]
    
    # Available long-short anomaly return observations (drop last observation)
    sx = as.matrix(dataAnomaly[ 1:(r+pHold+(s-2)) , ])
    
    # Available long-short anomaly return observations with missing values
    sxMissing = as.matrix(dataAnomalyMissing[ 1:(r+pHold+(s-2)) , ])
    
    # Standardized vector of cross-sectional averages
    sxAvg = scale(apply(sxMissing, 1, mean, na.rm = TRUE))
    
    # Fit predictor average regression via OLS
    sFitAvg = lm(sy ~ sxAvg)
    
    # Store slope coefficient
    coef[ s , 'Avg' ] = sFitAvg$coefficients[ 2 ]
    
    # Store standard error
    se[ s , 'Avg' ] = summary(sFitAvg)$coefficients[ 2 , 2 ]
    
    # Compute first principal component
    sPc = princomp(sx)$scores[ , 1 ]
    
    # Standardize principal component
    sPc = scale(sPc)
    
    # Fit principal component predictive regression via OLS
    sFitPc = lm(sy ~ sPc)
    
    # Store slope coefficient
    coef[ s , 'Pc' ] = sFitPc$coefficients[ 2 ]
    
    # Store standard error
    se[ s , 'Pc' ] = summary(sFitPc)$coefficients[ 2 , 2 ]
    
    # Estimate multiple regression via PLS (to get target-relevant factor) 
    sFitPls = plsr(sy ~ sx,
                   ncomp = 1)
    
    # Standardize target-relevant factor
    sPcPls = scale(sFitPls$scores)
    
    # Fit target-relevant factor regression via OLS
    sFitPls = lm(sy ~ sPcPls)
    
    # Store slope coefficient
    coef[ s , 'Pls' ] = -sFitPls$coefficients[ 2 ]
    
    # Store standard error
    se[ s , 'Pls' ] = summary(sFitPls)$coefficients[ 2 , 2 ]
    
  }
  
  cat('\n')
  
  ########################################
  # Create data frames & save as CSV files
  ########################################
  
  # Dates for out-of-sample period
  date = date[ -(1:(r+pHold)) ]
  
  # Data frame for slope coefficient estimates
  coef = data.frame(date,
                    coef)
  
  # Data frame for standard errors
  se = data.frame(date,
                  se)
  
  # Save data frames
  
  write.csv(coef,
            paste0('./Table/table',nameLeg,'RecursiveSlope.csv'),
            row.names = FALSE)
  
  write.csv(se,
            paste0('./Table/table',nameLeg,'RecursiveSe.csv'),
            row.names = FALSE)
  
}

