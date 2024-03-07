###################################
# Function BlockCalibSingle
# Calibrate a single "possibly blocked" 14C determination
# Arguments are:
# calendar_age_grid - Grid of calendar dates you want to search over 
#                      (for most recent date of block)
# obs_radiocarbon_age - (Single) radiocarbon age determination you want to calibrate
# obs_radiocarbon_sigma - Reported uncertainty on the radiocarbon_age 
# n_block - number of annual years which the 14C sample represents (the block)
# calibration_curve - calibration curve you want to use
# independent_calibration_curve - logical, whether to treat pointwise values of calibration curve
#                                 as independent or not
# Returns:
# Unnormalised posterior probability for every calendar year in calendar_age_grid 
BlockCalibSingle <-  function(
    calendar_age_grid, 
    obs_radiocarbon_age, 
    obs_radiocarbon_sigma, 
    n_block, 
    calibration_curve, 
    independent_calibration_curve = FALSE) {
  
  # Find mean and sd of block-adjusted curve 
  block_calibration_curve <- sapply(
    calendar_age_grid, 
    FindBlockCurve, 
    n_block = n_block,
    calibration_curve = calibration_curve, 
    n_block = n_block, 
    independent_between_years = independent_calibration_curve)
  
  block_calibration_curve_mean <- unlist(block_calibration_curve[1,])
  block_calibration_curve_sd <- unlist(block_calibration_curve[2,]) 
  
  # Likelihood of observed 14C age for each considered calendar year in chosen grid
  likelihood <- dnorm(x = obs_radiocarbon_age, 
                      mean = block_calibration_curve_mean, 
                      sd = sqrt(block_calibration_curve_sd^2 + obs_radiocarbon_sigma^2))
  
  return(likelihood)
}