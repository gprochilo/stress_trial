#-------------------------------------------------------------------------------

  # Import long-format data for paired data points
  
  dat.long <- read.csv(here::here("data","data_long.csv"))
  
  # Change object class of id and time to factors
  
  dat.long$id = as.factor(dat.long$id)
  dat.long$time = as.factor(dat.long$time)
  
  # Convert dataset to wide format to make some tests easier to perform
  
  dat.wide <- stats::reshape(dat.long, idvar = "id", timevar = "time", direction = "wide")
  
  # At a pss t1-t0 change score to dat.wide for convenience
  
  pss.ch = dat.wide$pss.1-dat.wide$pss.0
  dat.wide = cbind(dat.wide, pss.ch)
  
#-------------------------------------------------------------------------------
  
  # Import long-format aerobic economy data
  
  dat.econ.long <- utils::read.csv(here::here("data","data_economy.csv"))
  
  # Change object class of id, time, and samp to factors
  
  dat.econ.long$id = as.factor(dat.econ.long$id)
  dat.econ.long$time = as.factor(dat.econ.long$time)
  dat.econ.long$samp = as.factor(dat.econ.long$samp)
  
  # Remove participants with missing values for velocity 1 and 2
  
  test = dat.econ.long[complete.cases(dat.econ.long),] # remove rows with NAs
  n_occur = data.frame(table(test$id)) # Count how many responses each ID has given
  keep.id = n_occur[n_occur$Freq == 8,] # Report IDs with full set of 6 responses
  dat.econ.long = test[test$id %in% n_occur$Var1[keep.id$Var1],] # Remove IDs with < 6 responses
  
#-------------------------------------------------------------------------------
  
  # Import feasibility data
  
  feas <- read.csv(here::here("data","feasibility.csv"))
  
#-------------------------------------------------------------------------------
