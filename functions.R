##################
# Define functions
##################

computeMultiPeriodRetun = function(r, h){

  # Storage matrix for cumulative multi-period return
  rMulti = matrix(NA, length(r), 1)

  # Iterate over periods

  for ( t in 1:(length(r)-(h-1))){

    # Multi-period return
    rMulti[ t ] = mean(r[ t:(t+(h-1)) ])

  }

  return(rMulti)

}

estimateEnetAicc = function(x, y, lb){

  # Total sum of squares
  tss = sum((y - mean(y))^2)

  # ENet estimation
  fit = glmnet(x, y,
               alpha = 0.5,    
               nlambda = 200,
               lower.limits = lb)

  # Extract ENet coefficient estimates
  b = coef(fit)

  # Storage vector for R-squared statistics
  r2 = matrix(0, length(fit$lambda), 1)

  # Storage vector for corrected AIC values
  aicc = matrix(0, length(fit$lambda), 1)

  # Iterate over lambda values

  for ( j in 1:length(fit$lambda) ){

    # Residual vector
    ej = y - cbind(matrix(1, length(y), 1), x) %*% b[ , j ]

    # R-squared statistic
    r2[ j ] = 1 - sum(ej^2)/tss

    # Corrected AIC
    aicc[ j ] = length(y)*log(sum(ej^2)/length(y)) +
      2*fit$df[ j ]*length(y)/(length(y) - fit$df[ j ] - 1)

  }

  # Optimal lambda

  minIndex = which.min(aicc)

  lambda = fit$lambda[ minIndex ]

  # Vector of final ENet coefficient estimates
  b = b[ , minIndex ]

  # R-squared statistic for vector of final ENet coefficient estimates
  r2 = r2[ minIndex ]

  # List object for output
  result = list('lambda' = lambda,
                'b' = b,
                'r2' = r2,
                'minIndex' = minIndex)

  return(result)

}


computeR2os = function(actual, f1, f2, h){
  
  # MSFE for benchmark forecast (1)
  msfe1 = mean((actual-f1)^2)
  
  # MSFE for competing forecast (2)
  msfe2 = mean((actual-f2)^2)
  
  # Campbell-Thompson out-of-sample R-squared statistic
  r2os = 100*(1 - msfe2/msfe1)
  
  # Loss differential
  d = (actual - f1)^2 - (actual - f2)^2
  
  # Adjusted loss differential
  f = d + (f1 - f2)^2
  
  # Clark-West regression
  fit = lm(f ~ 1)
  
  # variance
  var <- summary(fit)[["coefficients"]][2]^2
  
  # Clark-West statistic
  cw = fit$coefficients/sqrt(var)
  
  # List object for output
  result = list('r2os' = r2os,
                'cw' = cw)
  
  return(result)
  
}

