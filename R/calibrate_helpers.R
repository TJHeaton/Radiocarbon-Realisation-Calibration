##################################
# Function FindBlockCurve
# Find mean and sd of given calibration curve between times (theta, theta + n_block - 1)
#
# Arguments:
# theta - most recent calendar date of the block
# n_block - number of years in the block
# calibration_curve - calibration curve you want to use
# independent_between_years - logical: 
#         TRUE treat pointwise values of calibration curve as independent 
#         FALSE treat pointwise values of calibration curve as entirely dependent (i.e., cov = 1)
#
# Return:
# mean and sd of the calibration curve for interval (theta, theta + nblock -1) 
FindBlockCurve <- function(theta, 
                           n_block, 
                           calibration_curve, 
                           independent_between_years = FALSE) { 
  
  block_calibration_curve_mean <- mean(
    approx(x = calibration_curve$calage, 
           y = calibration_curve$c14age, 
           x_out = theta - 1 + (1:n_block), 
           rule = 2)$y
  )
  
  if(independent_between_years) { 
    # Treat calibration curve as independent from one year to next
    block_calibration_curve_sd <- sqrt( 
      sum(
        approx(x = calibration_curve$calage, 
               y = calibration_curve$c14sig^2, 
               x_out = theta - 1 + (1:n_block), 
               rule = 2)$y
      ) / n_block^2 
    ) 
  } else { 
    # Assume covariance in the calibration curve is 1 on (short) block scale 
    # Set sd to just be just first value 
    block_calibration_curve_sd <- approx(
      x = calcurve$calage, 
      y = calcurve$c14sig, 
      x_out = theta, 
      rule = 2)$y   
  }
  
  retlist <- list(
    block_calibration_curve_mean = block_calibration_curve_mean, 
    block_calibration_curve_sd = block_calibration_curve_sd)
  return(retlist)
}



