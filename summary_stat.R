############################
#
# summary statistics
#
############################

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

# Data files for anomaly returns
fileData = c('dataLongShortMissing.xlsx',
             'dataLongMissing.xlsx',
             'dataShortMissing.xlsx')

# Portfolio types
type = c('LongShort',
         'Long',
         'Short')

# Compute statistics
# Storage list object for anomaly variables
dataAnomaly = rep(list(NA),
                  times = length(fileData))

# Iterate over portfolio types

for ( i in 1:length(dataAnomaly) ){
  
  # Data for type i
  iDataAnomaly = read.xlsx(paste0('./Data/',
                                  fileData[ i ]))
  
  # Convert date variable to date format
  iDataAnomaly$date = as.Date(as.character(iDataAnomaly$date),
                              format = '%Y%m%d')
  
  # Convert to percent return
  iDataAnomaly[ , -1 ] = 100*iDataAnomaly[ , -1 ]
  
  # Update list object
  dataAnomaly[[ i ]] = iDataAnomaly
  
}

# Names for objects in list
names(dataAnomaly) = type

# Anomaly names
nameAnomaly = names(dataAnomaly$LongShort)[ -1 ]

# Extract date vector
date = iDataAnomaly$date

# CH3 factors
dataCH3 = read.xlsx('./Data/dataCH3.xlsx')

# Convert date variable to date format
dataCH3$date = date

###########################################################
# Compute alpha t-statistics for CH 3-factor model
###########################################################

# Storage vector for t-statistics
tstatAlpha = matrix(NA, length(nameAnomaly), 1)

# Iterate over long-short portfolios

for ( i in 1:(ncol(dataAnomaly$LongShort)-1) ){
  
  # Data frame for estimating 3-factor model
  iData = data.frame(dataAnomaly$LongShort[ , i+1 ],
                     dataCH3[ , -1 ])
  
  # Clean up variable name
  names(iData)[ 1 ] = 'LongShort'
  
  # Remove missing values
  iData = na.omit(iData)
  
  # Fit 3-factor model via OLS
  iFit = lm(LongShort ~ MKT + SMB + VMG,
            data = iData)
  
  # Save alpha t-statistic
  tstatAlpha[ i ] = summary(iFit)$coefficients[ 1 , 3 ]
  
}



# Thresholds for t-statistic magnitudes
threshold = c(1.645,
              1.96,
              2.58,
              3)

# Storage vector for counts
tstatAlphaCount = matrix(NA, length(threshold), 1)

# Iterate over thresholds

for ( i in 1:length(threshold) ){
  
  # Count cases
  tstatAlphaCount[ i ] = sum(abs(tstatAlpha) >= threshold[ i ])
  
}

cat('Number of significant alpha t-statistics\n')
cat(tstatAlphaCount)
cat('\n\n')

#######################################
# Compute average pairwise correlations
#######################################

# Storage list object for pairwise correlations
corrPair = rep(list(NULL),
               times = length(dataAnomaly))

# Iterate over anomalies

for ( i in 1:(length(nameAnomaly)-1) ){
  
  # Iterate again over anomalies
  
  for ( j in (i+1):length(nameAnomaly) ){
    
    # Iterate over portfolio types
    
    for ( k in 1:length(type) ){
      
      # Data frame for anomaly pair    
      ijkData = data.frame(dataAnomaly[[ k ]][ , i+1 ],
                           dataAnomaly[[ k ]][ , j+1 ])
      
      # Remove missing values
      ijkData = na.omit(ijkData)
      
      # Compute pairwise correlation
      corrPair[[ k ]] = c(corrPair[[ k ]],
                          cor(ijkData)[ 2 , 1 ])
      
    }
    
  }
  
}

# Storage vector for average pairwise correlations
corrPairAvg = matrix(NA, length(type), 1)

# Iterate over portfolio types

for ( i in 1:length(type) ){
  
  # Average paiwise correlation
  corrPairAvg[ i ] = round(mean(corrPair[[ i ]]), 2)
  
}

cat('Average pairwise correlation (long-short/long/short)\n')
cat(corrPairAvg)
cat('\n\n')

########################################
# Compute average sample mean/volatility
########################################

# Storage list object for sample mean/volatility
stat = rep(list(matrix(NA, length(nameAnomaly), 2)),
           times = length(type))

# Iterate over portfolio types

for ( i in 1:length(stat) ){
  
  # Iterate over anomalies
  
  for ( j in 1:length(nameAnomaly) ){
    
    # Data vector
    ijData = na.omit(dataAnomaly[[ i ]][ , j+1 ])
    
    # Sample mean/volatility    
    stat[[ i ]][ j , ] = c(mean(ijData),
                           sd(ijData))
    
  }
  
}

# Storage matrix for average sample mean/volatility

statAvg = matrix(NA, length(stat), 2)

rownames(statAvg) = type

colnames(statAvg) = c('Mean',
                      'Vol')

# Iterate over portfolio types

for ( i in 1:length(stat) ){
  
  # Average sample mean/volatility
  statAvg[ i , ] = apply(stat[[ i ]], 2, mean)
  
}

# Convert to table format
statAvg = as.table(round(statAvg, 2 ))

cat('Average sample mean/volatility across anomalies')
cat('\n\n')
print(statAvg)
cat('\n\n')

